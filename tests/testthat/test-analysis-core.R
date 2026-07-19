testthat::test_that("Data_S1 is the standard input and its BOM is handled", {
  data <- read_colour_data(file.path(test_project_root, "Data_S1.csv"))

  testthat::expect_equal(nrow(data), 1965L)
  testthat::expect_true(all(required_colour_columns() %in% names(data)))
  testthat::expect_identical(names(data)[1:2], c("record_id", "source_row"))
  testthat::expect_s3_class(data$date, "Date")
  testthat::expect_false(anyNA(data$date))
  testthat::expect_false(anyDuplicated(data$record_id) > 0L)
  testthat::expect_equal(data$source_row, 2:1966)
  testthat::expect_true(all(data$color_statistic == "median"))
  testthat::expect_equal(data$R, data$median_R)
  testthat::expect_equal(data$G, data$median_G)
  testthat::expect_equal(data$B, data$median_B)
  qc_counts <- table(data$qc_status)
  testthat::expect_equal(
    as.integer(qc_counts[c("manual_review_required", "ok")]),
    c(785L, 1180L)
  )
})

testthat::test_that("a UTF-8 BOM and strict data contract are portable", {
  path <- tempfile(fileext = ".csv")
  bytes <- c(
    as.raw(c(0xef, 0xbb, 0xbf)),
    charToRaw(paste0(
      "date,latitude,longitude,R,G,B\n",
      "2024/06/01,35.0,137.0,255,0,0\n"
    ))
  )
  writeBin(bytes, path)

  data <- read_colour_data(path)
  testthat::expect_identical(names(data)[3], "date")
  testthat::expect_equal(data$date, as.Date("2024-06-01"))
  duplicated <- rbind(data, data)
  duplicated$record_id <- "duplicate"
  testthat::expect_error(validate_colour_data(duplicated), "unique")

  partial <- data
  partial$G <- NA_real_
  testthat::expect_error(validate_colour_data(partial), "all observed or all missing")

  outside <- data
  outside$R <- 256
  testthat::expect_error(validate_colour_data(outside), "0-255")
})

testthat::test_that("observation_id is retained as the canonical record identity", {
  path <- tempfile(fileext = ".csv")
  input <- data.frame(
    observation_id = c("photo-b", "photo-a"),
    source_row = c(20L, 10L),
    date = c("2024-06-02", "2024-06-01"),
    latitude = c(36, 35),
    longitude = c(138, 137),
    R = c(20, 10), G = c(30, 20), B = c(40, 30),
    stringsAsFactors = FALSE
  )
  utils::write.csv(input, path, row.names = FALSE)

  data <- read_colour_data(path)
  testthat::expect_identical(data$record_id, input$observation_id)
  testthat::expect_identical(data$observation_id, input$observation_id)
  testthat::expect_equal(data$source_row, input$source_row)
})

testthat::test_that("analysis QC includes accepted statuses and audits every exclusion", {
  data <- data.frame(
    record_id = paste0("p", 1:5),
    qc_status = c("ok", "manual_review_required", "NO_MASK", NA, " ok "),
    value = 1:5,
    stringsAsFactors = FALSE
  )
  filtered <- filter_analysis_qc(data)

  testthat::expect_true(filtered$status_available)
  testthat::expect_identical(filtered$data$record_id, c("p1", "p5"))
  testthat::expect_identical(filtered$excluded$record_id, c("p2", "p3", "p4"))
  testthat::expect_equal(sum(filtered$audit$records), nrow(data))
  included <- stats::setNames(filtered$audit$analysis_included, filtered$audit$qc_status)
  testthat::expect_true(unname(included[["ok"]]))
  testthat::expect_false(any(unname(included[setdiff(names(included), "ok")])))

  legacy <- data[names(data) != "qc_status"]
  legacy_filter <- filter_analysis_qc(legacy)
  testthat::expect_false(legacy_filter$status_available)
  testthat::expect_equal(nrow(legacy_filter$data), nrow(legacy))
  testthat::expect_equal(nrow(legacy_filter$excluded), 0L)
})

testthat::test_that("unresolved duplicate-photo coordinate conflicts are hard-excluded", {
  data <- data.frame(
    record_id = c("keep", "duplicate-a", "duplicate-b"),
    photo_coordinate_qc_status = c(
      "mapped_by_workbook_cell_and_image_hash",
      "manual_review_required_duplicate_photo_at_multiple_coordinates",
      "manual_review_required_duplicate_photo_at_multiple_coordinates"
    ),
    stringsAsFactors = FALSE
  )
  filtered <- filter_photo_coordinate_conflicts(data)
  testthat::expect_identical(filtered$data$record_id, "keep")
  testthat::expect_setequal(
    filtered$excluded$record_id,
    c("duplicate-a", "duplicate-b")
  )
})

testthat::test_that("colour-method selection is explicit and never mixes candidate rows", {
  data <- data.frame(
    record_id = paste0("p", 1:4),
    R = c(10, 20, 30, 40),
    G = c(11, 21, 31, 41),
    B = c(12, 22, 32, 42),
    mean_R = c(13, 23, 33, 43),
    mean_G = c(14, 24, 34, 44),
    mean_B = c(15, 25, 35, 45),
    legacy_R = c(9, 19, NA, 39),
    legacy_G = c(10, 20, NA, 40),
    legacy_B = c(11, 21, NA, 41),
    hsv_peak_R = c(100, 110, NA, Inf),
    hsv_peak_G = c(101, 111, NA, 131),
    hsv_peak_B = c(102, 112, NA, 132),
    hsv_exposure_filtered_peak_R = c(90, 100, 110, 120),
    hsv_exposure_filtered_peak_G = c(91, 101, 111, 121),
    hsv_exposure_filtered_peak_B = c(92, 102, 112, 122),
    alpha_peak_R = c(80, 90, 100, 110),
    alpha_peak_G = c(81, 91, 101, 111),
    alpha_peak_B = c(82, 92, 102, 112),
    stringsAsFactors = FALSE
  )

  primary <- select_analysis_colour_method(data)
  testthat::expect_identical(primary$method, "primary")
  testthat::expect_identical(primary$data$R, data$R)
  testthat::expect_identical(primary$data$primary_R, data$R)
  testthat::expect_true(all(primary$data$analysis_colour_method == "primary"))
  testthat::expect_equal(nrow(primary$excluded), 0L)

  mean_colour <- select_analysis_colour_method(data, "mean")
  testthat::expect_identical(mean_colour$data$R, data$mean_R)
  legacy <- select_analysis_colour_method(data, "legacy")
  testthat::expect_identical(legacy$data$record_id, c("p1", "p2", "p4"))
  testthat::expect_equal(legacy$data$R, c(9, 19, 39))

  peak <- select_analysis_colour_method(data, "hsv_peak")
  testthat::expect_identical(peak$data$record_id, c("p1", "p2"))
  testthat::expect_equal(peak$data$R, c(100, 110))
  testthat::expect_equal(peak$data$primary_R, c(10, 20))
  testthat::expect_identical(peak$excluded$record_id, c("p3", "p4"))
  testthat::expect_equal(peak$audit$rejected_missing_or_nonfinite, 2L)
  testthat::expect_true(all(peak$data$analysis_colour_method == "hsv_peak"))

  exposure <- select_analysis_colour_method(data, "hsv_exposure_filtered_peak")
  testthat::expect_equal(nrow(exposure$data), nrow(data))
  testthat::expect_equal(exposure$data$R, data$hsv_exposure_filtered_peak_R)

  alpha <- select_analysis_colour_method(data, "alpha_peak")
  testthat::expect_equal(alpha$data$R, data$alpha_peak_R)
  testthat::expect_error(
    select_analysis_colour_method(data, "hsv"),
    "exactly one of"
  )
  testthat::expect_error(
    select_analysis_colour_method(transform(data, alpha_peak_R = 300), "alpha_peak"),
    "outside the 0-255"
  )
})

testthat::test_that("RGB to Lab matches reference sRGB colours and preserves missing rows", {
  data <- data.frame(
    R = c(0, 255, 255, 0, 0, NA),
    G = c(0, 255, 0, 255, 0, NA),
    B = c(0, 255, 0, 0, 255, NA)
  )
  result <- add_colour_features(data)

  testthat::expect_equal(result$L[1], 0, tolerance = 1e-8)
  testthat::expect_equal(result$L[2], 100.000003866667, tolerance = 1e-10)
  testthat::expect_equal(result$L[3], 53.240794141307, tolerance = 1e-10)
  testthat::expect_equal(result$a[3], 80.092459596411, tolerance = 1e-10)
  testthat::expect_equal(result$b[3], 67.203196515853, tolerance = 1e-10)
  testthat::expect_equal(result$C[3], sqrt(result$a[3]^2 + result$b[3]^2))
  testthat::expect_true(all(is.na(result[6, c("L", "a", "b", "C", "H", "S", "V")])))
  testthat::expect_true(is.na(result$colour_hex[6]))
})

testthat::test_that("R and Python share one IEC sRGB to CIELAB D65 fixture", {
  fixture <- utils::read.csv(
    file.path(test_project_root, "tests", "fixtures", "srgb_cielab_d65.csv"),
    check.names = FALSE
  )
  observed <- srgb_to_cielab_d65(fixture[c("R", "G", "B")])
  expected <- as.matrix(fixture[c("L", "a", "b")])
  testthat::expect_equal(unname(observed), unname(expected), tolerance = 1e-10)
})

testthat::test_that("oriented PCA has a stable, declared PC1 direction", {
  data <- data.frame(
    warm = c(-3, -2, -1, 0, 1, 2, 3),
    dry = c(-2.8, -2.2, -0.9, 0.1, 1.2, 1.9, 3.1),
    third = c(-3.2, -1.8, -1.1, 0.2, 0.8, 2.1, 2.9)
  )
  fit <- fit_oriented_pca(data, c("warm", "dry", "third"), anchor = "warm")

  testthat::expect_s3_class(fit, "oriented_pca")
  testthat::expect_gt(unname(fit$loadings["warm"]), 0)
  testthat::expect_gt(stats::cor(fit$scores, data$warm), 0)
  testthat::expect_equal(predict(fit, data), fit$scores, tolerance = 1e-10)

  reversed <- fit_oriented_pca(data, c("warm", "dry", "third"), anchor = "warm", anchor_direction = -1)
  testthat::expect_lt(unname(reversed$loadings["warm"]), 0)
  testthat::expect_equal(reversed$scores, -fit$scores, tolerance = 1e-10)
})

testthat::test_that("photograph grain is preserved and site grain is explicit", {
  data <- data.frame(
    record_id = c("p3", "p1", "p2"),
    date = as.Date(c("2024-06-01", "2024-06-01", "2024-06-02")),
    longitude = c(137, 137, 138),
    latitude = c(35, 35, 36),
    colour_PC1 = c(1, 3, 5),
    qc_status = c("ok", "manual_review_required", "ok"),
    site_id = c("s1", "s1", "s2")
  )

  photo <- prepare_analysis_grain(data, grain = "photo")
  testthat::expect_identical(photo$record_id, data$record_id)
  testthat::expect_equal(nrow(photo), 3L)
  testthat::expect_true(all(photo$n_photos == 1L))

  no_site_id <- data[names(data) != "site_id"]
  testthat::expect_error(
    prepare_analysis_grain(no_site_id, grain = "site"),
    "explicit site_id"
  )

  site <- prepare_analysis_grain(
    data,
    grain = "site",
    site_id_col = "site_id",
    value_cols = "colour_PC1"
  )
  testthat::expect_identical(site$analysis_id, c("s1", "s2"))
  testthat::expect_equal(site$n_photos, c(2L, 1L))
  testthat::expect_equal(site$colour_PC1, c(2, 5))
  testthat::expect_equal(site$qc_ok_n, c(1L, 1L))
  testthat::expect_equal(site$qc_manual_review_required_n, c(1L, 0L))

  inconsistent <- data
  inconsistent$longitude[[2L]] <- inconsistent$longitude[[2L]] + 0.01
  testthat::expect_error(
    prepare_analysis_grain(
      inconsistent,
      grain = "site",
      site_id_col = "site_id",
      value_cols = "colour_PC1"
    ),
    "spans multiple coordinates"
  )
  testthat::expect_identical(site$record_ids, c("p3;p1", "p2"))
})

testthat::test_that("current input grain audit does not discard repeated coordinates", {
  data <- read_colour_data(file.path(test_project_root, "Data_S1.csv"))
  audit <- audit_analysis_grain(data)
  value <- stats::setNames(audit$value, audit$metric)

  testthat::expect_equal(unname(value["photograph_records"]), 1965)
  testthat::expect_equal(unname(value["exact_coordinate_sites"]), 1927)
  testthat::expect_equal(unname(value["repeated_exact_sites"]), 38)
  testthat::expect_equal(unname(value["extra_records_at_exact_sites"]), 38)
})

testthat::test_that("raster extraction preserves record IDs and row order", {
  testthat::skip_if_not_installed("terra")

  raster <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(raster) <- c(10, 20, 30, 40)
  names(raster) <- "small_value"
  points <- data.frame(
    record_id = c("third", "missing", "first"),
    longitude = c(1.5, NA, 0.5),
    latitude = c(0.5, NA, 1.5)
  )

  extracted <- extract_raster_values(points, raster)
  testthat::expect_identical(extracted$record_id, points$record_id)
  testthat::expect_equal(extracted$small_value, c(40, NA, 10))
  testthat::expect_identical(attr(extracted, "invalid_coordinate_rows"), 2L)

  shuffled <- extracted[c(3, 1, 2), ]
  attached <- attach_raster_values(points, shuffled)
  testthat::expect_identical(attached$record_id, points$record_id)
  testthat::expect_equal(attached$small_value, c(40, NA, 10))
})

testthat::test_that("Bombus suitability sum is not richness and never maps all-NA to zero", {
  species <- bombus_species_columns()
  data <- as.data.frame(matrix(
    c(
      0.1, 0.2, 0.3, 0.4, 0.5,
      NA, NA, NA, NA, NA,
      0.1, 0.2, NA, 0.4, 0.5
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(NULL, species)
  ))

  strict <- add_bombus_suitability_sum(data, require_all = TRUE)
  testthat::expect_equal(strict$Bombus_predictions_n, c(5, 0, 4))
  testthat::expect_equal(strict$Bombus_suitability_sum[1], 1.5)
  testthat::expect_true(all(is.na(strict$Bombus_suitability_sum[2:3])))
  testthat::expect_false("Bee_Richness" %in% names(strict))

  partial <- add_bombus_suitability_sum(data, require_all = FALSE)
  testthat::expect_true(is.na(partial$Bombus_suitability_sum[2]))
  testthat::expect_equal(partial$Bombus_suitability_sum[3], 1.2)
})

testthat::test_that("variance decomposition includes fixed-spatial covariance", {
  fixed <- c(1, 2, 3, 4)
  spatial <- c(4, 3, 2, 1)
  result <- variance_decomposition_with_covariance(
    fixed,
    spatial,
    residual_variance = 2,
    alignment = "position"
  )
  draw <- result$draws
  contribution <- stats::setNames(draw$contribution, draw$component)
  proportion <- stats::setNames(draw$proportion, draw$component)

  testthat::expect_equal(
    unname(contribution["fixed"] + contribution["spatial"] + contribution["fixed_spatial_covariance"]),
    stats::var(fixed + spatial),
    tolerance = 1e-12
  )
  testthat::expect_lt(contribution["fixed_spatial_covariance"], 0)
  testthat::expect_equal(unname(contribution["total"]), 2)
  testthat::expect_equal(sum(proportion[c("fixed", "spatial", "fixed_spatial_covariance", "residual")]), 1)

  draws <- variance_decomposition_with_covariance(
    unname(cbind(fixed, fixed * 2)),
    unname(cbind(spatial, spatial / 2)),
    residual_variance = c(2, 3),
    alignment = "position"
  )
  testthat::expect_equal(length(unique(draws$draws$draw)), 2L)
  testthat::expect_true(all(c("proportion_lower95", "proportion_upper95") %in% names(draws$summary)))
})

testthat::test_that("variance decomposition aligns observations, draws, and residuals by name", {
  fixed <- matrix(
    c(1, 2, 4, 8, 2, 3, 5, 9),
    nrow = 4,
    dimnames = list(c("a", "b", "c", "d"), c("draw_a", "draw_b"))
  )
  spatial <- matrix(
    c(9, 5, 3, 2, 8, 4, 2, 1),
    nrow = 4,
    dimnames = list(c("d", "c", "b", "a"), c("draw_b", "draw_a"))
  )
  residual <- c(draw_b = 7, draw_a = 3)
  result <- variance_decomposition_with_covariance(
    fixed,
    spatial,
    residual_variance = residual
  )
  testthat::expect_identical(unique(result$draws$draw_name), colnames(fixed))
  residual_rows <- result$draws[result$draws$component == "residual", ]
  testthat::expect_equal(residual_rows$contribution, c(3, 7))

  aligned_spatial <- spatial[rownames(fixed), colnames(fixed), drop = FALSE]
  fitted_expected <- vapply(seq_len(ncol(fixed)), function(draw) {
    stats::var(fixed[, draw] + aligned_spatial[, draw])
  }, numeric(1))
  fitted_observed <- vapply(seq_len(ncol(fixed)), function(draw) {
    sum(result$draws$contribution[
      result$draws$draw == draw &
        result$draws$component %in% c(
          "fixed", "spatial", "fixed_spatial_covariance"
        )
    ])
  }, numeric(1))
  testthat::expect_equal(fitted_observed, fitted_expected)
})

testthat::test_that("variance decomposition rejects ambiguous alignment", {
  fixed <- matrix(1:8, nrow = 4)
  spatial <- matrix(8:1, nrow = 4)
  testthat::expect_error(
    variance_decomposition_with_covariance(fixed, spatial, c(1, 2)),
    "Name alignment requires"
  )
  positional <- variance_decomposition_with_covariance(
    fixed,
    spatial,
    c(1, 2),
    alignment = "position"
  )
  testthat::expect_identical(
    unique(positional$draws$draw_name),
    c("draw_1", "draw_2")
  )

  dimnames(fixed) <- list(letters[1:4], c("one", "two"))
  testthat::expect_error(
    variance_decomposition_with_covariance(
      fixed,
      spatial,
      c(1, 2),
      alignment = "position"
    ),
    "only when fixed and spatial are unnamed"
  )

  spatial_named <- fixed
  rownames(spatial_named)[1L] <- "other"
  testthat::expect_error(
    variance_decomposition_with_covariance(
      fixed,
      spatial_named,
      c(one = 1, two = 2)
    ),
    "dimname sets must match"
  )
  testthat::expect_error(
    variance_decomposition_with_covariance(
      fixed,
      fixed,
      c(1, 2)
    ),
    "residual_variance must have unique names"
  )
})
