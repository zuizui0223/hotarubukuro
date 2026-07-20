#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(terra); library(sf); library(INLA)})
options(stringsAsFactors = FALSE); set.seed(42); INLA::inla.setOption(num.threads = "1:1")

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) > 0) args[1] else "Data_S1.csv"
env_dir <- if (length(args) > 1) args[2] else "data/processed/rasters"
sdm_dir <- if (length(args) > 2) args[3] else "sdm"
out_dir <- if (length(args) > 3) args[4] else "results/bombus_availability"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

z <- function(x) as.numeric(scale(x))
nonempty <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

ensure_raster_crs <- function(r, path) {
  current <- terra::crs(r, proj = TRUE)
  if (!is.na(current) && nzchar(trimws(current))) return(r)
  ex <- as.vector(terra::ext(r))
  looks_geographic <- length(ex) == 4L && all(is.finite(ex)) &&
    ex[[1]] >= -180 && ex[[2]] <= 180 && ex[[3]] >= -90 && ex[[4]] <= 90
  if (!looks_geographic) {
    stop("Raster has no valid CRS and its extent is not geographic: ", path,
         " [", paste(ex, collapse = ", "), "]", call. = FALSE)
  }
  warning("Raster CRS metadata was missing; assigning EPSG:4326 after geographic-extent validation: ", path,
          call. = FALSE)
  terra::crs(r) <- "EPSG:4326"
  r
}

read_raster_checked <- function(path) {
  if (!file.exists(path)) stop("Missing raster: ", path, call. = FALSE)
  r <- terra::rast(path)
  if (terra::nlyr(r) != 1L) stop("Expected one raster layer: ", path, call. = FALSE)
  ensure_raster_crs(r, path)
}

extract_values <- function(paths, layer_names, dat) {
  if (any(!file.exists(paths))) stop("Missing raster(s): ", paste(paths[!file.exists(paths)], collapse = ", "))
  layers <- lapply(paths, read_raster_checked)
  reference <- layers[[1L]]
  incompatible <- vapply(layers, function(x) {
    !isTRUE(terra::same.crs(reference, x))
  }, logical(1))
  if (any(incompatible)) {
    stop("Raster CRS differs within extraction stack: ",
         paste(paths[incompatible], collapse = ", "), call. = FALSE)
  }
  r <- do.call(c, layers); names(r) <- layer_names
  pts <- vect(dat[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
  if (!terra::same.crs(pts, r)) pts <- terra::project(pts, terra::crs(r))
  out <- as.data.frame(extract(r, pts, ID = FALSE), check.names = FALSE)
  stopifnot(nrow(out) == nrow(dat)); out
}
pca1 <- function(dat, vars, label) {
  X <- dat[vars]
  good <- vapply(X, function(x) is.numeric(x) && is.finite(sd(x, na.rm = TRUE)) && sd(x, na.rm = TRUE) > 0, logical(1))
  X <- X[good]; cc <- complete.cases(X)
  if (ncol(X) < 2 || sum(cc) < 50) stop("Invalid PCA group: ", label)
  fit <- prcomp(X[cc, , drop = FALSE], center = TRUE, scale. = TRUE)
  score <- rep(NA_real_, nrow(dat)); score[cc] <- fit$x[, 1]
  list(score = score, loadings = data.frame(group = label, variable = rownames(fit$rotation), PC1_loading = fit$rotation[, 1]))
}

# Canonical data and PR #4 identity/QC contract.
d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
required <- c("observation_id", "date", "latitude", "longitude", "R", "G", "B")
if (length(setdiff(required, names(d)))) stop("Canonical Data_S1.csv schema is incomplete")
if (anyDuplicated(d$observation_id)) stop("observation_id must be unique")
n_input <- nrow(d)
d$date <- as.Date(gsub("/", "-", trimws(as.character(d$date))))
for (nm in c("latitude", "longitude", "R", "G", "B")) d[[nm]] <- as.numeric(d[[nm]])
d$DOY <- as.integer(format(d$date, "%j"))
identity_bad <- rep(FALSE, nrow(d))
if ("duplicate_image_sha256" %in% names(d)) identity_bad <- identity_bad | nonempty(d$duplicate_image_sha256)
if ("photo_coordinate_qc_status" %in% names(d)) identity_bad <- identity_bad | grepl("duplicate", d$photo_coordinate_qc_status, ignore.case = TRUE)
input_ok <- complete.cases(d[c("date", "latitude", "longitude", "R", "G", "B")]) &
  is.finite(d$latitude) & is.finite(d$longitude) & d$latitude >= 20 & d$latitude <= 50 & d$longitude >= 120 & d$longitude <= 150
excluded_identity <- d[identity_bad, intersect(c("observation_id", "photo_id", "image_sha256", "duplicate_image_sha256", "latitude", "longitude"), names(d)), drop = FALSE]
excluded_input <- d[!input_ok, intersect(c("observation_id", "date", "latitude", "longitude", "R", "G", "B"), names(d)), drop = FALSE]
d <- d[!identity_bad & input_ok, , drop = FALSE]

# Display-referred IEC sRGB -> CIELAB D65.
rgb <- pmin(pmax(as.matrix(d[c("R", "G", "B")]) / 255, 0), 1)
lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
xyz <- lin %*% t(matrix(c(0.4124564,0.3575761,0.1804375,0.2126729,0.7151522,0.0721750,0.0193339,0.1191920,0.9503041), 3, byrow = TRUE))
xyz <- sweep(xyz, 2, c(0.95047, 1, 1.08883), "/")
f <- ifelse(xyz > (6/29)^3, xyz^(1/3), xyz/(3*(6/29)^2) + 4/29)
d$L <- 116*f[,2]-16; d$a <- 500*(f[,1]-f[,2]); d$b <- 200*(f[,2]-f[,3])

# Legacy environmental layers; only CHELSA AI is replaced by CHELSA CMI.
env_files <- c(
  chelsa_bio05="chelsa_bio05.tif", chelsa_bio10="chelsa_bio10.tif", chelsa_gdd5="chelsa_gdd5.tif",
  chelsa_cmimean="chelsa_cmimean.tif", chelsa_vpdmean="chelsa_vpdmean.tif", chelsa_bio12="chelsa_bio12.tif",
  chelsa_bio14="chelsa_bio14.tif", chelsa_bio15="chelsa_bio15.tif", chelsa_swb="chelsa_swb.tif",
  chelsa_rsdsmean="chelsa_rsdsmean.tif", soilgrids_bdod_0_5cm="soilgrids_bdod_0_5cm_mean.tif",
  soilgrids_cfvo_0_5cm="soilgrids_cfvo_0_5cm_mean.tif", soilgrids_sand_0_5cm="soilgrids_sand_0_5cm_mean.tif",
  soilgrids_silt_0_5cm="soilgrids_silt_0_5cm_mean.tif", soilgrids_nitrogen_0_5cm="soilgrids_nitrogen_0_5cm_mean.tif",
  soilgrids_ocd_0_5cm="soilgrids_ocd_0_5cm_mean.tif", soilgrids_soc_0_5cm="soilgrids_soc_0_5cm_mean.tif",
  soilgrids_phh2o_0_5cm="soilgrids_phh2o_0_5cm_mean.tif")
d <- cbind(d, extract_values(file.path(env_dir, env_files), names(env_files), d))

elev_path <- file.path(env_dir, "elevation_30s.tif")
elev <- read_raster_checked(elev_path)
topo <- c(terrain(elev, "roughness"), terrain(elev, "slope", unit = "radians"), terrain(elev, "TRI")); names(topo) <- c("roughness","slope","TRI")
if (is.na(terra::crs(topo, proj = TRUE)) || !nzchar(trimws(terra::crs(topo, proj = TRUE)))) {
  terra::crs(topo) <- terra::crs(elev)
}
pts <- vect(d[c("longitude","latitude")], geom = c("longitude","latitude"), crs = "EPSG:4326")
if (!same.crs(pts, topo)) pts <- project(pts, crs(topo))
tv <- as.data.frame(extract(topo, pts, ID = FALSE)); stopifnot(nrow(tv) == nrow(d)); d <- cbind(d, tv)

pc_temp <- pca1(d, c("chelsa_bio05","chelsa_bio10","chelsa_gdd5"), "Temperature_PC1")
pc_moist <- pca1(d, c("chelsa_cmimean","chelsa_vpdmean","chelsa_bio12","chelsa_bio14","chelsa_bio15","chelsa_swb"), "precip_PC1")
pc_phys <- pca1(d, c("soilgrids_bdod_0_5cm","soilgrids_cfvo_0_5cm","soilgrids_sand_0_5cm","soilgrids_silt_0_5cm"), "soil_phys_PC1")
pc_nutr <- pca1(d, c("soilgrids_nitrogen_0_5cm","soilgrids_ocd_0_5cm","soilgrids_soc_0_5cm"), "soil_nutrient_PC1")
pc_topo <- pca1(d, c("roughness","slope","TRI"), "topo_PC1")
d$Temperature_PC1 <- pc_temp$score; d$precip_PC1 <- pc_moist$score; d$soil_phys_PC1 <- pc_phys$score
d$soil_nutrient_PC1 <- pc_nutr$score; d$topo_PC1 <- pc_topo$score; d$soil_pH <- d$soilgrids_phh2o_0_5cm; d$RSDS <- d$chelsa_rsdsmean
pca_loadings <- do.call(rbind, lapply(list(pc_temp,pc_moist,pc_phys,pc_nutr,pc_topo), `[[`, "loadings"))

# Bombus suitability indices.
species <- c("ardens","beaticola","consobrinus","diversus","honshuensis")
d <- cbind(d, extract_values(file.path(sdm_dir, paste0(species, ".tif")), species, d))
P <- as.matrix(d[species]); sdm_ok <- complete.cases(P) & apply(P, 1, function(x) all(is.finite(x) & x >= 0 & x <= 1))
excluded_sdm <- d[!sdm_ok, c("observation_id","latitude","longitude")]; d <- d[sdm_ok, , drop = FALSE]; P <- as.matrix(d[species])
d$Bombus_suitability_sum <- rowSums(P); d$Bombus_any_availability <- 1-apply(1-P,1,prod); d$Bombus_max_availability <- apply(P,1,max)
d$spatial_widespread <- 1-apply(1-as.matrix(d[c("ardens","diversus")]),1,prod)
d$spatial_montane <- 1-apply(1-as.matrix(d[c("beaticola","consobrinus","honshuensis")]),1,prod)

# One common cohort for every comparison.
env_raw <- c("DOY","topo_PC1","Temperature_PC1","precip_PC1","soil_nutrient_PC1","soil_phys_PC1","soil_pH","RSDS")
common <- c("a","latitude","longitude",env_raw,"Bombus_suitability_sum","Bombus_any_availability","Bombus_max_availability")
M <- as.matrix(d[common]); storage.mode(M) <- "double"
model_ok <- complete.cases(M) & apply(M, 1, function(x) all(is.finite(x)))
excluded_model <- d[!model_ok, c("observation_id","latitude","longitude")]; d <- d[model_ok, , drop = FALSE]
if (nrow(d) < 50) stop("Too few common complete cases")
for (nm in c(env_raw,"Bombus_suitability_sum","Bombus_any_availability","Bombus_max_availability")) d[[paste0("z_",nm)]] <- z(d[[nm]])
d$y <- z(d$a); env_terms <- paste0("z_", env_raw)
models <- list(environment_only=env_terms,
  environment_plus_bombus_sum=c(env_terms,"z_Bombus_suitability_sum"),
  environment_plus_bombus_any=c(env_terms,"z_Bombus_any_availability"),
  environment_plus_bombus_max=c(env_terms,"z_Bombus_max_availability"))

# Reviewed SPDE-INLA.
proj_jp <- "+proj=laea +lat_0=36 +lon_0=138 +datum=WGS84 +units=m +no_defs"
loc <- st_coordinates(st_transform(st_as_sf(d, coords=c("longitude","latitude"), crs=4326, remove=FALSE), proj_jp))
mesh <- inla.mesh.2d(loc=loc, boundary=inla.nonconvex.hull(loc, convex=-0.05, resolution=100000), max.edge=c(30000,150000), cutoff=5000, offset=c(30000,150000))
spde <- inla.spde2.pcmatern(mesh, alpha=2, prior.range=c(100000,0.05), prior.sigma=c(1,0.05)); A <- inla.spde.make.A(mesh, loc=loc)
fit_model <- function(name, terms) {
  X <- data.frame(Intercept=1,d[terms],check.names=FALSE)
  stk <- inla.stack(data=list(y=d$y),A=list(A,1),effects=list(spatial=seq_len(spde$n.spde),fixed=X),tag="est")
  form <- as.formula(paste("y ~",paste(c("0 + Intercept",terms,"f(spatial, model=spde)"),collapse=" + ")))
  fit <- inla(form,family="gaussian",data=inla.stack.data(stk),control.predictor=list(A=inla.stack.A(stk),compute=TRUE),control.compute=list(waic=TRUE,dic=TRUE,cpo=TRUE,config=TRUE),control.inla=list(strategy="adaptive"),verbose=FALSE)
  if (!is.finite(fit$waic$waic)) stop("INLA failed: ",name); list(name=name,terms=terms,fit=fit)
}
fits <- Map(fit_model,names(models),models); names(fits) <- names(models)
comparison <- do.call(rbind,lapply(fits,function(o){cpo<-o$fit$cpo$cpo;data.frame(model=o$name,n=nrow(d),mesh_vertices=mesh$n,WAIC=o$fit$waic$waic,pWAIC=o$fit$waic$p.eff,DIC=o$fit$dic$dic,mean_neglogCPO=mean(-log(cpo[is.finite(cpo)&cpo>0])),failed_CPO=sum(!is.finite(cpo)|cpo<=0))}))
comparison$delta_WAIC <- comparison$WAIC-min(comparison$WAIC); comparison <- comparison[order(comparison$WAIC),]
fixed <- do.call(rbind,lapply(fits,function(o){x<-as.data.frame(o$fit$summary.fixed);x$term<-rownames(x);x$model<-o$name;rownames(x)<-NULL;x[c("model","term",setdiff(names(x),c("model","term")))]}))
hyper <- do.call(rbind,lapply(fits,function(o){x<-as.data.frame(o$fit$summary.hyperpar);x$parameter<-rownames(x);x$model<-o$name;rownames(x)<-NULL;x[c("model","parameter",setdiff(names(x),c("model","parameter")))]}))

# Same-cohort spatial block prediction sensitivity.
axis1 <- prcomp(scale(loc),center=FALSE,scale.=FALSE)$x[,1]; br <- unique(quantile(axis1,seq(0,1,length.out=6),type=1)); if(length(br)!=6)stop("Cannot form folds")
d$fold <- cut(axis1,br,include.lowest=TRUE,labels=FALSE); cv <- list()
for(nm in names(models)) for(k in 1:5){tr<-d$fold!=k;te<-d$fold==k;f0<-lm(as.formula(paste("y ~",paste(models[[nm]],collapse=" + "))),d[tr,]);p<-predict(f0,d[te,]);den<-sum((d$y[te]-mean(d$y[tr]))^2);cv[[paste(nm,k)]]<-data.frame(model=nm,fold=k,n_test=sum(te),RMSE=sqrt(mean((d$y[te]-p)^2)),MAE=mean(abs(d$y[te]-p)),R2_pred=if(den>0)1-sum((d$y[te]-p)^2)/den else NA)}
cv_fold <- do.call(rbind,cv); cv_summary <- do.call(rbind,lapply(split(cv_fold,cv_fold$model),function(x)data.frame(model=x$model[1],RMSE_mean=mean(x$RMSE),RMSE_sd=sd(x$RMSE),MAE_mean=mean(x$MAE),R2_pred_mean=mean(x$R2_pred,na.rm=TRUE))))

# Reviewer-facing exports.
write.csv(comparison,file.path(out_dir,"spde_model_comparison.csv"),row.names=FALSE); write.csv(fixed,file.path(out_dir,"spde_fixed_effects.csv"),row.names=FALSE); write.csv(hyper,file.path(out_dir,"spde_hyperparameters.csv"),row.names=FALSE)
write.csv(cv_fold,file.path(out_dir,"blocked_cv_folds.csv"),row.names=FALSE); write.csv(cv_summary,file.path(out_dir,"blocked_cv_summary.csv"),row.names=FALSE); write.csv(pca_loadings,file.path(out_dir,"environment_pca_loadings.csv"),row.names=FALSE)
write.csv(rbind(data.frame(stage="input",n=n_input),data.frame(stage="excluded_identity",n=nrow(excluded_identity)),data.frame(stage="excluded_missing_input",n=nrow(excluded_input)),data.frame(stage="excluded_invalid_sdm",n=nrow(excluded_sdm)),data.frame(stage="excluded_incomplete_model",n=nrow(excluded_model)),data.frame(stage="common_model_cohort",n=nrow(d))),file.path(out_dir,"row_flow.csv"),row.names=FALSE)
write.csv(data.frame(index=c("Bombus_suitability_sum","Bombus_any_availability","Bombus_max_availability","spatial_widespread","spatial_montane"),definition=c("sum of five species suitability values","probability-like union 1-product(1-p_i)","maximum suitability among five species","widespread-species union: ardens + diversus","montane-species union: beaticola + consobrinus + honshuensis")),file.path(out_dir,"bombus_index_definitions.csv"),row.names=FALSE)
write.csv(data.frame(group=c("widespread","montane"),species=c("ardens;diversus","beaticola;consobrinus;honshuensis"),correlation_with_temperature=c(cor(d$spatial_widespread,d$Temperature_PC1),cor(d$spatial_montane,d$Temperature_PC1)),correlation_with_topography=c(cor(d$spatial_widespread,d$topo_PC1),cor(d$spatial_montane,d$topo_PC1))),file.path(out_dir,"spatial_group_diagnostic.csv"),row.names=FALSE)
write.csv(d,file.path(out_dir,"analysis_data.csv"),row.names=FALSE)
write.csv(excluded_identity,file.path(out_dir,"excluded_identity.csv"),row.names=FALSE); write.csv(excluded_input,file.path(out_dir,"excluded_missing_input.csv"),row.names=FALSE); write.csv(excluded_sdm,file.path(out_dir,"excluded_invalid_sdm.csv"),row.names=FALSE); write.csv(excluded_model,file.path(out_dir,"excluded_incomplete_model.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out_dir,"sessionInfo.txt"))
cat("Completed common cohort:",nrow(d),"rows; outputs:",out_dir,"\n")