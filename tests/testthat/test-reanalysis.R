source(file.path(test_project_root, "R", "reanalysis.R"))

write_reanalysis_config <- function(config) {
  path <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, path)
  path
}

testthat::test_that("reanalysis config enforces the reviewed workflow contract", {
  config_path <- file.path(test_project_root, "config", "reanalysis.yml")
  config <- read_reanalysis_config(config_path)
  testthat::expect_identical(config$primary_outcome, "a")
  testthat::expect_identical(
    config$secondary_composite,
    "primary_reference_PC1"
  )
  testthat::expect_identical(config$bombus_predictor, "Bombus_suitability_sum")
  testthat::expect_identical(
    config$population_density_source,
    "worldpop_2020_density"
  )
  testthat::expect_length(config$candidate_environment_predictors, 9L)
  testthat::expect_length(config$environment_predictors, 6L)
  testthat::expect_setequal(
    names(config$excluded_candidate_predictors),
    c("chelsa_cmimean", "chelsa_vpdmean", "worldclim_elevation_30s")
  )
  # The reviewed PCA-band algorithm is deterministic and does not consume a
  # random seed, so an absent seed is valid. A supplied seed is still checked.
  testthat::expect_null(config$spatial_block_cv$seed)
  seeded <- config
  seeded$spatial_block_cv$seed <- 42L
  testthat::expect_identical(
    read_reanalysis_config(write_reanalysis_config(seeded))$spatial_block_cv$seed,
    42L
  )

  invalid <- config
  invalid$primary_outcome <- "L"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "primary and secondary"
  )
  invalid <- config
  invalid$secondary_composite <- "L"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "primary and secondary"
  )
  invalid <- config
  invalid$bombus_predictor <- "Bee_Richness"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "Bombus_suitability_sum"
  )
  invalid <- config
  invalid$population_density_source <- "population"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "worldpop_2020_density"
  )
  invalid <- config
  invalid$qc_scenarios$strict_ok <- invalid$qc_scenarios$inclusive
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "proper subset"
  )
  invalid <- config
  invalid$environment_predictors[[1L]] <- "a"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "must not overlap"
  )
  invalid <- config
  invalid$environment_predictors <- c(
    invalid$environment_predictors,
    invalid$bombus_predictor
  )
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "must not overlap"
  )
  invalid <- config
  invalid$environment_predictors <- invalid$environment_predictors[-1L]
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "reviewed non-redundant specification"
  )
  invalid <- config
  invalid$excluded_candidate_predictors$chelsa_cmimean <- ""
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "reviewed non-redundant specification"
  )
  invalid <- config
  invalid$environment_predictors <- c(
    invalid$environment_predictors,
    invalid$population_density_source
  )
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "must not overlap"
  )
  invalid <- config
  invalid$spatial_block_cv$folds <- 1L
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "At least two"
  )
  invalid <- config
  invalid$spatial_block_cv$seed <- 1.5
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "non-negative integer"
  )
  invalid <- config
  invalid$spatial_block_cv$seed <- "42"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "non-negative integer"
  )
  invalid <- config
  invalid$spatial_block_cv$method <- "random"
  testthat::expect_error(
    read_reanalysis_config(write_reanalysis_config(invalid)),
    "Unsupported"
  )
})

testthat::test_that("canonical Data_S1 provenance is fail-closed", {
  data <- read_colour_data(
    file.path(test_project_root, "Data_S1.csv"),
    allow_missing_rgb = TRUE
  )
  audit <- validate_reviewed_data_s1(data)
  testthat::expect_equal(audit$records, 1965L)
  testthat::expect_identical(audit$extraction_version, "2.2.2")
  testthat::expect_equal(audit$duplicate_image_records, 2L)

  white_balanced <- data
  white_balanced$white_balance_applied[[1L]] <- TRUE
  testthat::expect_error(
    validate_reviewed_data_s1(white_balanced),
    "no identifiable white balance"
  )
  changed_primary <- data
  changed_primary$R[[1L]] <- changed_primary$R[[1L]] + 1
  testthat::expect_error(
    validate_reviewed_data_s1(changed_primary),
    "must equal median"
  )
  changed_hash <- data
  changed_hash$image_sha256[[1L]] <- "not-a-hash"
  testthat::expect_error(
    validate_reviewed_data_s1(changed_hash),
    "identity columns"
  )
})

testthat::test_that("stored joint-peak Lab uses the shared explicit D65 conversion", {
  data <- read_colour_data(file.path(test_project_root, "Data_S1.csv"))
  methods <- c("hsv_peak", "hsv_exposure_filtered_peak", "alpha_peak")
  for (method in methods) {
    rgb_columns <- paste0(method, "_", c("R", "G", "B"))
    lab_columns <- paste0(method, "_", c("L", "a", "b"))
    available <- stats::complete.cases(data[rgb_columns]) &
      stats::complete.cases(data[lab_columns])
    observed <- srgb_to_cielab_d65(data[available, rgb_columns, drop = FALSE])
    expected <- as.matrix(data[available, lab_columns, drop = FALSE])
    testthat::expect_equal(unname(observed), unname(expected), tolerance = 1e-9)
  }
})

testthat::test_that("real row flow and legacy crosswalk are explicit", {
  data <- read_colour_data(file.path(test_project_root, "Data_S1.csv"))
  identity <- filter_photo_coordinate_conflicts(data)
  testthat::expect_equal(nrow(identity$data), 1963L)
  testthat::expect_equal(nrow(identity$excluded), 2L)
  legacy <- select_analysis_colour_method(identity$data, "legacy")
  testthat::expect_equal(nrow(legacy$data), 1924L)
  testthat::expect_equal(nrow(legacy$excluded), 39L)
  site <- prepare_analysis_grain(
    add_colour_features(select_analysis_colour_method(identity$data, "primary")$data),
    grain = "site",
    site_id_col = "exact_site_id",
    value_cols = c("R", "G", "B", "L", "a", "b")
  )
  testthat::expect_equal(nrow(site), 1925L)
  testthat::expect_equal(sum(site$n_photos), 1963L)
})

testthat::test_that("standardized associations and spatial blocks are deterministic", {
  set.seed(42)
  data <- data.frame(
    analysis_id = sprintf("p%03d", 1:100),
    longitude = seq(130, 142, length.out = 100),
    latitude = seq(31, 41, length.out = 100) + sin(seq_len(100)) / 20,
    x1 = rnorm(100),
    x2 = rnorm(100),
    stringsAsFactors = FALSE
  )
  data$y <- 0.7 * data$x1 - 0.2 * data$x2 + rnorm(100, sd = 0.2)
  folds_a <- make_spatial_block_folds(data, folds = 5)
  folds_b <- make_spatial_block_folds(data[100:1, ], folds = 5)
  testthat::expect_identical(
    folds_a$spatial_fold,
    folds_b$spatial_fold[match(folds_a$analysis_id, folds_b$analysis_id)]
  )
  data$spatial_fold <- folds_a$spatial_fold
  fit <- fit_standardized_association(data, "y", c("x1", "x2"))
  testthat::expect_gt(
    fit$coefficients$standardized_estimate[fit$coefficients$term == "x1"],
    0
  )
  cv <- blocked_cv_association(data, "y", c("x1", "x2"))
  testthat::expect_equal(nrow(cv$predictions), nrow(data))
  testthat::expect_gt(cv$performance$q_squared_cv, 0.7)
  testthat::expect_equal(nrow(cv$fold_performance), 5L)
  testthat::expect_equal(sum(cv$fold_performance$test_records), nrow(data))
  testthat::expect_equal(cv$performance$folds_configured, 5L)
  testthat::expect_equal(cv$performance$folds_actual, 5L)
})

testthat::test_that("blocked CV preserves configured folds after complete cases", {
  data <- data.frame(
    analysis_id = sprintf("p%02d", 1:15),
    spatial_fold = rep(1:3, each = 5),
    x = seq_len(15),
    y = seq_len(15) + sin(seq_len(15)),
    stringsAsFactors = FALSE
  )
  result <- blocked_cv_association(
    data,
    "y",
    "x",
    configured_folds = 1:3
  )
  testthat::expect_equal(result$performance$folds_configured, 3L)
  testthat::expect_equal(result$performance$folds_actual, 3L)
  testthat::expect_equal(result$performance$folds, 3L)
  testthat::expect_equal(result$fold_performance$train_records, rep(10L, 3))

  missing_fold <- data
  missing_fold$y[missing_fold$spatial_fold == 3L] <- NA_real_
  testthat::expect_error(
    blocked_cv_association(
      missing_fold,
      "y",
      "x",
      configured_folds = 1:3
    ),
    "removed configured spatial fold"
  )
  testthat::expect_error(
    blocked_cv_association(data, "y", "x", configured_folds = 1:4),
    "exactly the configured"
  )
})

testthat::test_that("blocked CV fails before fitting undersized train or test folds", {
  test_too_small <- data.frame(
    analysis_id = letters[1:6],
    spatial_fold = c(rep(1L, 5), 2L),
    x = seq_len(6),
    y = seq_len(6),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    blocked_cv_association(test_too_small, "y", "x"),
    "test fold.*fewer than two"
  )

  train_too_small <- data.frame(
    analysis_id = letters[1:4],
    spatial_fold = rep(1:2, each = 2),
    x = seq_len(4),
    y = seq_len(4),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    blocked_cv_association(train_too_small, "y", "x"),
    "training fold.*fewer than 4"
  )
})

testthat::test_that("repeated photographs cannot move unique-coordinate folds", {
  grid <- expand.grid(longitude = seq(130, 135), latitude = seq(32, 37))
  grid$analysis_id <- sprintf("site-%02d", seq_len(nrow(grid)))
  base <- make_spatial_block_folds(grid, folds = 5)
  repeated <- rbind(
    grid,
    transform(
      grid[rep(1L, 100L), ],
      analysis_id = sprintf("repeat-%03d", seq_len(100L))
    )
  )
  repeated_folds <- make_spatial_block_folds(repeated, folds = 5)
  testthat::expect_identical(
    base$spatial_fold,
    repeated_folds$spatial_fold[match(base$analysis_id, repeated_folds$analysis_id)]
  )
  unique_sizes <- table(base$spatial_fold)
  testthat::expect_lte(max(unique_sizes) - min(unique_sizes), 1L)

  site_subset <- grid[c(2, 10, 20), ]
  assigned <- assign_spatial_folds(
    site_subset,
    unique(base[c("longitude", "latitude", "spatial_fold")])
  )
  testthat::expect_identical(
    assigned$spatial_fold,
    base$spatial_fold[match(site_subset$analysis_id, base$analysis_id)]
  )
})

testthat::test_that("spatial folding never silently reduces the configured fold count", {
  too_few <- data.frame(
    analysis_id = c("a", "b"),
    longitude = c(130, 131),
    latitude = c(35, 36),
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    make_spatial_block_folds(too_few, folds = 3L),
    "Too few distinct coordinates.*2 coordinates for 3 folds"
  )
  testthat::expect_error(
    make_spatial_block_folds(too_few, folds = 2.5),
    "single integer"
  )
})

testthat::test_that("colour-method comparison uses a common ID order and common Lab metric", {
  primary <- data.frame(
    analysis_id = c("b", "a"),
    L = c(50, 60), a = c(10, 20), b = c(0, 5),
    primary_reference_PC1 = c(1, 2)
  )
  candidate <- data.frame(
    analysis_id = c("a", "b"),
    L = c(63, 54), a = c(18, 13), b = c(1, 0),
    primary_reference_PC1 = c(1.8, 1.3)
  )
  summary <- pairwise_colour_method_summary(primary, candidate, "candidate")
  expected <- c(sqrt(3^2 + (-2)^2 + (-4)^2), sqrt(4^2 + 3^2))
  testthat::expect_equal(summary$common_records, 2L)
  testthat::expect_equal(summary$deltaE76_median, stats::median(expected))
})
