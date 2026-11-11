source(testthat::test_path("..", "..", "R", "environment_spatial.R"))

testthat::test_that("publication QC keeps the first workbook row per exact image hash", {
  raw <- data.frame(source_row = c(569, 515, 8), image_sha256 = c("x", "x", "y"),
                    duplicate_image_sha256 = c(TRUE, TRUE, FALSE),
                    median_R = 120, median_G = 100, median_B = 130)
  result <- audit_colour_qc(raw)$data
  testthat::expect_equal(result$colour_qc_primary, c(FALSE, TRUE, TRUE))
  testthat::expect_identical(result$duplicate_image_sha256, raw$duplicate_image_sha256)
})
