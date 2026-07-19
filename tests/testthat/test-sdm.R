locate_sdm_test_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  hit <- candidates[file.exists(file.path(candidates, "sdm", "manifest.csv"))]
  if (!length(hit)) stop("Repository root not found from test working directory.")
  normalizePath(hit[[1]], winslash = "/")
}

sdm_test_root <- locate_sdm_test_root()
source(file.path(sdm_test_root, "R", "sdm.R"))

testthat::test_that("published SDM files match their immutable manifest", {
  validation <- validate_sdm_collection(file.path(sdm_test_root, "sdm"))
  testthat::expect_true(validation$ok, info = paste(validation$issues, collapse = "; "))
  testthat::expect_true(validation$structural_ok)
  testthat::expect_false(validation$model_reproducible)
  testthat::expect_identical(validation$provenance_status, "legacy_unverifiable")
  testthat::expect_true(validation$geometry_aligned)
  testthat::expect_true(validation$nodata_masks_identical)
  testthat::expect_true(validation$hashes_unique)
  testthat::expect_true(all(validation$report$hash_ok))
  testthat::expect_true(all(validation$report$metadata_ok))
  testthat::expect_true(all(validation$report$min >= 0 & validation$report$max <= 1))
  testthat::expect_identical(names(validation$stack), validation$report$species)

  printed <- utils::capture.output(print_sdm_validation(validation))
  testthat::expect_true(any(grepl("structural validation passed", printed)))
  testthat::expect_true(any(grepl("reproducibility is unavailable", printed)))
})

testthat::test_that("SDM provenance status is explicit and validated", {
  manifest <- read_sdm_manifest(file.path(sdm_test_root, "sdm", "manifest.csv"))
  testthat::expect_false(sdm_models_reproducible(manifest))

  reproducible <- manifest
  reproducible$provenance_status <- "reproducible"
  testthat::expect_true(sdm_models_reproducible(reproducible))

  unknown <- manifest
  unknown$provenance_status[[1]] <- "unknown"
  testthat::expect_error(validate_sdm_manifest(unknown), "provenance_status")
  testthat::expect_error(
    load_sdm_stack(file.path(sdm_test_root, "sdm"), unknown),
    "provenance_status"
  )
})

testthat::test_that("summed suitability is strict and is not labelled species richness", {
  species <- expected_bombus_species()
  values <- as.data.frame(matrix(c(
    rep(0.2, 5),
    c(0.1, 0.2, NA, 0.4, 0.5),
    rep(NA_real_, 5)
  ), nrow = 3, byrow = TRUE))
  names(values) <- species
  result <- summed_suitability(values)
  testthat::expect_equal(result[[1]], 1)
  testthat::expect_true(is.na(result[[2]]))
  testthat::expect_true(is.na(result[[3]]))
  testthat::expect_error(summed_suitability(values[, -1]), "exactly")
  infinite <- values[1, , drop = FALSE]
  infinite[[1]] <- Inf
  testthat::expect_error(summed_suitability(infinite), "finite or NA")
})

testthat::test_that("species count requires five explicit validated thresholds", {
  species <- expected_bombus_species()
  values <- as.data.frame(matrix(c(0.1, 0.3, 0.5, 0.7, 0.9), nrow = 1))
  names(values) <- species
  thresholds <- stats::setNames(rep(0.5, 5), species)
  testthat::expect_identical(species_count(values, thresholds), 3L)
  testthat::expect_error(species_count(values, thresholds[-1]), "all five")
  thresholds[[1]] <- 1.1
  testthat::expect_error(species_count(values, thresholds), "in \\[0, 1\\]")
})

testthat::test_that("SDM extraction retains explicit record identity and input order", {
  records <- data.frame(
    record_id = c("third", "first", "second"),
    longitude = c(137.899999, 138.336534, 137.695011),
    latitude = c(35.256418, 35.983276, 36.546359)
  )
  extracted <- extract_sdm_by_record_id(records, file.path(sdm_test_root, "sdm"))
  testthat::expect_identical(extracted$record_id, records$record_id)
  testthat::expect_equal(nrow(extracted), nrow(records))
  testthat::expect_setequal(names(extracted)[-1], expected_bombus_species())
  testthat::expect_error(
    extract_sdm_by_record_id(rbind(records, records[1, ]), file.path(sdm_test_root, "sdm")),
    "unique"
  )

  site_records <- records
  names(site_records)[names(site_records) == "record_id"] <- "analysis_id"
  site_extracted <- extract_sdm_by_record_id(
    site_records,
    file.path(sdm_test_root, "sdm"),
    id_col = "analysis_id"
  )
  testthat::expect_identical(site_extracted$analysis_id, site_records$analysis_id)
})
