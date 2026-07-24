source(testthat::test_path("..", "..", "R", "environment_spatial.R"))
source(testthat::test_path("..", "..", "R", "natural_biotic_covariates.R"))

testthat::test_that("primary-mesh boundary flags are response blind and geometrically exact", {
  # 5339 begins at 35.333... N, 139 E. The first and last 1-km rows are flagged.
  latitude0 <- 53 / 1.5
  testthat::expect_identical(
    primary_mesh_boundary_flag(
      c(139.001, 139.5, 139.5),
      c(latitude0 + 0.001, latitude0 + 0.2, latitude0 + 79.5 / 120)
    ),
    c(TRUE, FALSE, TRUE)
  )
})

testthat::test_that("collinearity audits retain process terms and label the design", {
  set.seed(1)
  d <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
  audit <- numeric_collinearity_audit(d, c("a", "b", "c"), design = "test")
  testthat::expect_true(all(audit$vif$design == "test"))
  testthat::expect_setequal(audit$vif$term, c("a", "b", "c"))
  testthat::expect_equal(nrow(audit$correlations), 3L)
})

testthat::test_that("GBIF occurrence indices are response blind", {
  flowers <- data.frame(
    x_km = c(0, 60, 120), y_km = c(0, 0, 0), response = c(-1, 0, 1)
  )
  occurrences <- data.frame(
    x_km = c(0, 10, 60, 70, 120, 125), y_km = 0,
    short = c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis", "ardens")
  )
  a <- compute_bombus_occurrence_indices(
    flowers, occurrences, radii_km = 100, minimum_kernel_support = 0
  )$data
  flowers$response <- rev(flowers$response)
  b <- compute_bombus_occurrence_indices(
    flowers, occurrences, radii_km = 100, minimum_kernel_support = 0
  )$data
  index_cols <- grep("^(occ_|Bombus_)", names(a), value = TRUE)
  testthat::expect_equal(a[index_cols], b[index_cols])
  testthat::expect_true("Bombus_W_occ_100km" %in% names(a))
  testthat::expect_true(all(c(
    "Bombus_ardens_occ_density_100km",
    "Bombus_diversus_occ_density_100km",
    "Bombus_W_ardens_share_100km",
    "Bombus_total_occ_density_100km"
  ) %in% names(a)))
})

testthat::test_that("low-human reference is selected within region without colour", {
  d <- data.frame(
    region = factor(rep(c("West", "East"), each = 20), levels = c("West", "East")),
    z_H = rep(seq(-2, 2, length.out = 20), 2),
    response = rnorm(40)
  )
  a <- define_low_human_reference(d, fraction = 0.30)$data
  d$response <- rev(d$response)
  b <- define_low_human_reference(d, fraction = 0.30)$data
  testthat::expect_identical(a$low_human_reference, b$low_human_reference)
  testthat::expect_true(all(tapply(a$low_human_reference, a$region, sum) >= 5))
  testthat::expect_true(all(tapply(a$low_human_reference, a$region, sum) <= 7))
})

testthat::test_that("seasonal Bombus indices use location and date but not colour", {
  flowers <- data.frame(
    x_km = c(0, 50), y_km = c(0, 0), DOY = c(150, 250), response = c(-1, 1)
  )
  occurrence <- data.frame(
    x_km = c(0, 10, 45, 55, 60), y_km = 0,
    event_doy = c(145, 155, 245, 255, 180),
    short = c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
  )
  a <- compute_bombus_seasonal_indices(
    flowers, occurrence, radius_km = 100, minimum_spatial_support = 0
  )$data
  flowers$response <- rev(flowers$response)
  b <- compute_bombus_seasonal_indices(
    flowers, occurrence, radius_km = 100, minimum_spatial_support = 0
  )$data
  cols <- grep("^Bombus_", names(a), value = TRUE)
  testthat::expect_equal(a[cols], b[cols])
  testthat::expect_true("Bombus_temporal_overlap_100km_30d" %in% names(a))
})

testthat::test_that("local Bombus pairing never uses flower colour", {
  set.seed(4)
  n <- 80
  d <- data.frame(
    Bombus_test = rnorm(n),
    response = rnorm(n),
    env_a = rep(seq(-1, 1, length.out = n / 2), 2),
    env_b = rnorm(n, sd = 0.1),
    x_km = rep(seq(0, 90, length.out = n / 2), 2),
    y_km = rep(c(0, 10), each = n / 2),
    region = factor(rep(c("West", "East"), each = n / 2), levels = c("West", "East"))
  )
  a <- match_local_bombus_contrasts(
    d, "Bombus_test", c("env_a", "env_b"), radius_km = 100, env_caliper = 3
  )$pairs
  d$response <- sample(d$response)
  b <- match_local_bombus_contrasts(
    d, "Bombus_test", c("env_a", "env_b"), radius_km = 100, env_caliper = 3
  )$pairs
  testthat::expect_equal(a[c("high_source_row", "low_source_row")],
                         b[c("high_source_row", "low_source_row")])
  testthat::expect_gt(nrow(a), 0)
  testthat::expect_true(all(a$predictor_difference > 0))
})

testthat::test_that("colour dimensions are not mislabeled as imaging negative controls", {
  colour_text <- paste(deparse(body(fit_human_colour_coherence)), collapse = " ")
  image_text <- paste(deparse(body(fit_human_image_diagnostics)), collapse = " ")
  testthat::expect_match(colour_text, "not an image-quality negative control", fixed = TRUE)
  testthat::expect_false(grepl('c\\("colour_L", "colour_C"', image_text))
})

testthat::test_that("evidence ladder keeps shared and conditional estimands distinct", {
  txt <- paste(deparse(body(fit_bombus_evidence_ladder)), collapse = " ")
  testthat::expect_match(txt, "landscape")
  testthat::expect_match(txt, "environment_space_adjusted")
  testthat::expect_match(txt, "space_adjusted")
  testthat::expect_match(txt, "environment_adjusted")
})

testthat::test_that("phenology reference uses a rank-audited linear year effect", {
  txt <- paste(deparse(body(crossfit_reference_rq)), collapse = " ")
  testthat::expect_match(txt, 'c(fixed_terms, "year")', fixed = TRUE)
  testthat::expect_false(grepl("splines::ns", txt, fixed = TRUE))
  testthat::expect_match(txt, "fold_qr")
  testthat::expect_match(txt, "rq.fit.br", fixed = TRUE)
})
