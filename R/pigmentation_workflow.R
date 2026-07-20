# Core functions for the publication workflow.
# No function reads from user-specific absolute paths.

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package is not installed: ", pkg, call. = FALSE)
  }
}

srgb_to_lab <- function(r, g, b) {
  rgb <- pmin(pmax(cbind(as.numeric(r), as.numeric(g), as.numeric(b)) / 255, 0), 1)
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  xyz <- lin %*% t(matrix(c(
    0.4124564, 0.3575761, 0.1804375,
    0.2126729, 0.7151522, 0.0721750,
    0.0193339, 0.1191920, 0.9503041
  ), nrow = 3, byrow = TRUE))
  xyz <- sweep(xyz, 2, c(0.95047, 1, 1.08883), "/")
  eps <- (6 / 29)^3
  f <- ifelse(xyz > eps, xyz^(1 / 3), xyz / (3 * (6 / 29)^2) + 4 / 29)
  data.frame(
    L = 116 * f[, 2] - 16,
    a = 500 * (f[, 1] - f[, 2]),
    b = 200 * (f[, 2] - f[, 3]),
    check.names = FALSE
  )
}

fit_legacy_pigment_cfa <- function(data) {
  require_package("lavaan")
  needed <- c("L", "a", "b")
  missing <- setdiff(needed, names(data))
  if (length(missing)) stop("CFA input lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  indicators <- transform(data.frame(data[needed]), Lm = -L, C = sqrt(a^2 + b^2))
  model <- "Pigment =~ 1*a + Lm + C"
  fit <- lavaan::cfa(
    model,
    data = indicators[c("a", "Lm", "C")],
    estimator = "MLR",
    std.lv = FALSE,
    missing = "fiml"
  )
  if (!isTRUE(lavaan::inspect(fit, "converged"))) stop("Legacy CFA did not converge.", call. = FALSE)
  scores <- as.numeric(lavaan::lavPredict(fit, type = "lv")[, "Pigment"])
  loadings <- lavaan::standardizedSolution(fit)
  loadings <- loadings[loadings$op == "=~", c("lhs", "rhs", "est.std", "se", "pvalue")]
  list(fit = fit, scores = scores, loadings = loadings, indicators = indicators)
}

pca_axis <- function(data, variables, label) {
  x <- data[variables]
  keep <- vapply(x, function(v) is.numeric(v) && is.finite(stats::sd(v, na.rm = TRUE)) && stats::sd(v, na.rm = TRUE) > 0, logical(1))
  x <- x[keep]
  if (ncol(x) < 2L) stop(label, " needs at least two nonconstant variables.", call. = FALSE)
  cc <- stats::complete.cases(x)
  if (sum(cc) < 50L) stop(label, " has fewer than 50 complete records.", call. = FALSE)
  fit <- stats::prcomp(x[cc, , drop = FALSE], center = TRUE, scale. = TRUE)
  score <- rep(NA_real_, nrow(data)); score[cc] <- fit$x[, 1]
  list(
    score = score,
    loadings = data.frame(group = label, variable = rownames(fit$rotation), loading = fit$rotation[, 1]),
    variance_explained = summary(fit)$importance[2, 1],
    fit = fit
  )
}

bombus_indices <- function(data, species = c("ardens", "beaticola", "consobrinus", "diversus", "honshuensis")) {
  missing <- setdiff(species, names(data))
  if (length(missing)) stop("Missing Bombus layers: ", paste(missing, collapse = ", "), call. = FALSE)
  p <- as.matrix(data[species]); storage.mode(p) <- "double"
  valid <- apply(p, 1, function(x) all(is.finite(x) & x >= 0 & x <= 1))
  out <- data.frame(
    Bombus_availability = rowSums(p),
    Bombus_any = 1 - apply(1 - p, 1, prod),
    Bombus_max = apply(p, 1, max),
    Bombus_widespread = rowSums(p[, c("ardens", "diversus"), drop = FALSE]),
    Bombus_montane = rowSums(p[, c("beaticola", "consobrinus", "honshuensis"), drop = FALSE])
  )
  out[!valid, ] <- NA_real_
  out
}

integrated_model_terms <- function(bombus = "Bombus_availability") {
  c(
    "topo_PC1", "Temperature_PC1", "precip_PC1", "soil_phys_PC1",
    "soil_nutrient_PC1", "soil_pH", "RSDS", bombus
  )
}

anthropogenic_qgam_formula <- function(human_terms, include_doy_interactions = TRUE) {
  base <- c(
    "s(longitude, latitude, k = 60)",
    "s(elevation, k = 10)",
    "s(DOY, bs = 'tp', k = 12)"
  )
  smooths <- sprintf("s(%s, k = 10)", human_terms)
  interactions <- character()
  if (include_doy_interactions) {
    interactions <- sprintf("ti(DOY, %s, k = c(8, 8))", human_terms)
  }
  stats::as.formula(paste("resid_zfit ~", paste(c(base, smooths, interactions), collapse = " + ")))
}

fit_anthropogenic_qgam <- function(data,
                                   human_terms,
                                   taus = c(0.75, 0.85, 0.90, 0.95),
                                   include_doy_interactions = TRUE) {
  require_package("qgam")
  required <- c("resid_zfit", "longitude", "latitude", "elevation", "DOY", human_terms)
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("qGAM data lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  model_data <- data[stats::complete.cases(data[required]), , drop = FALSE]
  if (nrow(model_data) < 100L) stop("Too few complete records for qGAM.", call. = FALSE)
  form <- anthropogenic_qgam_formula(human_terms, include_doy_interactions)
  models <- lapply(taus, function(tau) qgam::qgam(form, data = model_data, qu = tau))
  names(models) <- paste0("q", taus)
  list(formula = form, models = models, data = model_data, taus = taus)
}

classify_landscape_setting <- function(elevation, built_fraction, cropland_fraction, forest_edge_density) {
  out <- rep("other", length(elevation))
  out[elevation >= 1200] <- "montane"
  out[elevation < 1200 & (built_fraction >= 0.05 | cropland_fraction >= 0.15 | forest_edge_density >= 0.10)] <- "satoyama_periurban"
  factor(out, levels = c("satoyama_periurban", "montane", "other"))
}

reviewer_contract <- function(data) {
  stopifnot(!anyDuplicated(data$observation_id))
  stopifnot(all(is.finite(data$longitude)), all(is.finite(data$latitude)))
  stopifnot(all(data$longitude >= 120 & data$longitude <= 150))
  stopifnot(all(data$latitude >= 20 & data$latitude <= 50))
  invisible(TRUE)
}
