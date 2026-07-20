#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(terra)
})

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) args[[1L]] else "Data_S1.csv"
sdm_dir <- if (length(args) >= 2L) args[[2L]] else "sdm"
out_dir <- if (length(args) >= 3L) args[[3L]] else "results/bombus_availability"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

species <- c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")
paths <- file.path(sdm_dir, paste0(species, ".tif"))
stopifnot(file.exists(input), all(file.exists(paths)))

d <- read.csv(input, check.names = FALSE, fileEncoding = "UTF-8-BOM")
required <- c("date", "latitude", "longitude", "R", "G", "B")
if (length(setdiff(required, names(d)))) stop("Data_S1.csv is missing required columns")

d$date <- as.Date(d$date)
d$DOY <- as.integer(format(d$date, "%j"))
keep <- complete.cases(d[required]) & is.finite(d$latitude) & is.finite(d$longitude)
d <- d[keep, , drop = FALSE]

# Explicit IEC sRGB -> CIELAB D65. The response is apparent red-green colour a*.
rgb <- as.matrix(d[c("R", "G", "B")]) / 255
linear <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
xyz <- linear %*% t(matrix(c(
  0.4124564, 0.3575761, 0.1804375,
  0.2126729, 0.7151522, 0.0721750,
  0.0193339, 0.1191920, 0.9503041
), nrow = 3, byrow = TRUE))
xyz <- sweep(xyz, 2, c(0.95047, 1, 1.08883), "/")
f <- ifelse(xyz > (6 / 29)^3, xyz^(1 / 3), xyz / (3 * (6 / 29)^2) + 4 / 29)
d$a <- 500 * (f[, 1] - f[, 2])

stack <- rast(paths)
names(stack) <- species
pts <- vect(d[c("longitude", "latitude")], geom = c("longitude", "latitude"), crs = "EPSG:4326")
p <- as.data.frame(extract(stack, pts, ID = FALSE))
stopifnot(nrow(p) == nrow(d))
for (nm in species) d[[nm]] <- p[[nm]]

complete_sdm <- complete.cases(d[species])
d <- d[complete_sdm, , drop = FALSE]
P <- as.matrix(d[species])
if (any(P < 0 | P > 1)) stop("SDM values must lie in [0,1]")

d$Bombus_suitability_sum <- rowSums(P)
d$Bombus_any_availability <- 1 - apply(1 - P, 1, prod)
d$Bombus_max_availability <- apply(P, 1, max)

# Widespread/montane summaries are spatial-confounding diagnostics only.
widespread <- c("ardens", "diversus")
montane <- c("beaticola", "consobrinus", "honshuensis")
d$spatial_widespread <- 1 - apply(1 - as.matrix(d[widespread]), 1, prod)
d$spatial_montane <- 1 - apply(1 - as.matrix(d[montane]), 1, prod)

# Deterministic five geographic bands along the first spatial principal axis.
xy <- scale(as.matrix(d[c("longitude", "latitude")]))
axis1 <- prcomp(xy, center = FALSE, scale. = FALSE)$x[, 1]
q <- unique(quantile(axis1, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
if (length(q) != 6L) stop("Could not form five spatial folds")
d$fold <- cut(axis1, breaks = q, include.lowest = TRUE, labels = FALSE)

# Spatial structure is modelled directly; the three Bombus summaries are compared
# on exactly the same rows and folds.
base_terms <- c("scale(DOY)", "scale(longitude)", "scale(latitude)",
                "I(scale(longitude)^2)", "I(scale(latitude)^2)",
                "scale(longitude):scale(latitude)")
indices <- c("none", "Bombus_suitability_sum", "Bombus_any_availability", "Bombus_max_availability")

cv_one <- function(index) {
  rhs <- base_terms
  if (index != "none") rhs <- c(rhs, paste0("scale(", index, ")"))
  formula <- as.formula(paste("a ~", paste(rhs, collapse = " + ")))
  pred <- rep(NA_real_, nrow(d))
  fold_stats <- vector("list", 5)
  for (k in 1:5) {
    train <- d$fold != k
    test <- d$fold == k
    fit <- lm(formula, data = d[train, , drop = FALSE])
    pred[test] <- predict(fit, newdata = d[test, , drop = FALSE])
    rmse <- sqrt(mean((d$a[test] - pred[test])^2))
    mae <- mean(abs(d$a[test] - pred[test]))
    q2 <- 1 - sum((d$a[test] - pred[test])^2) / sum((d$a[test] - mean(d$a[train]))^2)
    fold_stats[[k]] <- data.frame(model = index, fold = k, n = sum(test), rmse = rmse, mae = mae, q2 = q2)
  }
  full <- lm(formula, data = d)
  data.frame(
    model = index,
    n = nrow(d),
    rmse = sqrt(mean((d$a - pred)^2)),
    mae = mean(abs(d$a - pred)),
    q2 = 1 - sum((d$a - pred)^2) / sum((d$a - mean(d$a))^2),
    bombus_beta = if (index == "none") NA_real_ else unname(coef(full)[paste0("scale(", index, ")")]),
    stringsAsFactors = FALSE
  ) -> summary
  list(summary = summary, folds = do.call(rbind, fold_stats), fit = full)
}

runs <- lapply(indices, cv_one)
performance <- do.call(rbind, lapply(runs, `[[`, "summary"))
baseline_q2 <- performance$q2[performance$model == "none"]
performance$delta_q2 <- performance$q2 - baseline_q2
performance <- performance[order(-performance$q2), ]
folds <- do.call(rbind, lapply(runs, `[[`, "folds"))

spatial_diagnostic <- data.frame(
  index = c("spatial_widespread", "spatial_montane"),
  cor_longitude = c(cor(d$spatial_widespread, d$longitude), cor(d$spatial_montane, d$longitude)),
  cor_latitude = c(cor(d$spatial_widespread, d$latitude), cor(d$spatial_montane, d$latitude)),
  cor_spatial_axis1 = c(cor(d$spatial_widespread, axis1), cor(d$spatial_montane, axis1)),
  cor_DOY = c(cor(d$spatial_widespread, d$DOY), cor(d$spatial_montane, d$DOY))
)

write.csv(performance, file.path(out_dir, "performance.csv"), row.names = FALSE)
write.csv(folds, file.path(out_dir, "performance_by_fold.csv"), row.names = FALSE)
write.csv(spatial_diagnostic, file.path(out_dir, "spatial_group_diagnostic.csv"), row.names = FALSE)
write.csv(d[c("date", "latitude", "longitude", "DOY", "a", "fold",
              species, "Bombus_suitability_sum", "Bombus_any_availability",
              "Bombus_max_availability", "spatial_widespread", "spatial_montane")],
          file.path(out_dir, "analysis_data.csv"), row.names = FALSE)

print(performance)
print(spatial_diagnostic)
