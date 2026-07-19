locate_analysis_cli_test_root <- function() {
  candidates <- c(".", "..", "../..", "../../..")
  hit <- candidates[file.exists(file.path(candidates, "scripts", "run_analysis.R"))]
  if (!length(hit)) stop("Repository root not found from test working directory.")
  normalizePath(hit[[1]], winslash = "/")
}

analysis_cli_test_root <- locate_analysis_cli_test_root()
analysis_cli_environment <- new.env(parent = globalenv())
source(
  file.path(analysis_cli_test_root, "scripts", "run_analysis.R"),
  local = analysis_cli_environment,
  chdir = FALSE
)

testthat::test_that("analysis CLI exposes inclusive and strict QC choices and refuses overwrite", {
  directory <- tempfile("analysis-cli-")
  dir.create(directory)
  input <- file.path(directory, "input.csv")
  output <- file.path(directory, "prepared.csv")
  loadings <- file.path(directory, "loadings.csv")
  records <- data.frame(
    observation_id = paste0("photo-", 1:6),
    source_row = 2:7,
    date = rep("2024-06-01", 6),
    latitude = 35 + seq_len(6) / 100,
    longitude = 137 + seq_len(6) / 100,
    R = c(10, 40, 80, 130, 190, 240),
    G = c(20, 70, 30, 150, 100, 220),
    B = c(30, 20, 120, 90, 200, 180),
    hsv_peak_R = c(15, 45, 85, 135, NA, 245),
    hsv_peak_G = c(25, 75, 35, 155, 105, 225),
    hsv_peak_B = c(35, 25, 125, 95, 205, 185),
    qc_status = c(rep("ok", 5), "manual_review_required"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(records, input, row.names = FALSE)

  result <- testthat::expect_warning(
    analysis_cli_environment$run_analysis_cli(
      c(
        "--input", input,
        "--no-sdm",
        "--output", output,
        "--loadings-output", loadings
      ),
      project_root = analysis_cli_test_root
    ),
    NA
  )
  prepared <- utils::read.csv(output, stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(prepared), 6L)
  testthat::expect_setequal(prepared$qc_status, c("ok", "manual_review_required"))
  testthat::expect_identical(prepared$record_id, records$observation_id)
  testthat::expect_equal(prepared$R, records$R)
  testthat::expect_equal(prepared$primary_R, records$R)
  testthat::expect_true(all(prepared$analysis_colour_method == "primary"))
  testthat::expect_equal(nrow(result$excluded_qc), 0L)
  testthat::expect_equal(nrow(result$excluded_colour_method), 0L)
  testthat::expect_null(result$sdm_validation)
  testthat::expect_true(file.exists(loadings))
  testthat::expect_length(list.files(directory, pattern = "\\.part$"), 0L)

  candidate_output <- file.path(directory, "hsv-peak.csv")
  candidate_loadings <- file.path(directory, "hsv-peak-loadings.csv")
  candidate_result <- analysis_cli_environment$run_analysis_cli(
    c(
      "--input", input,
      "--no-sdm",
      "--colour-method", "hsv_peak",
      "--output", candidate_output,
      "--loadings-output", candidate_loadings
    ),
    project_root = analysis_cli_test_root
  )
  candidate <- utils::read.csv(candidate_output, stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(candidate), 5L)
  testthat::expect_identical(candidate$record_id, records$observation_id[c(1:4, 6)])
  testthat::expect_equal(candidate$R, records$hsv_peak_R[c(1:4, 6)])
  testthat::expect_equal(candidate$primary_R, records$R[c(1:4, 6)])
  testthat::expect_true(all(candidate$analysis_colour_method == "hsv_peak"))
  testthat::expect_identical(
    candidate_result$excluded_colour_method$record_id,
    "photo-5"
  )
  testthat::expect_equal(
    candidate_result$colour_method_audit$rejected_missing_or_nonfinite,
    1L
  )

  strict_output <- file.path(directory, "strict-qc.csv")
  strict_loadings <- file.path(directory, "strict-qc-loadings.csv")
  strict_result <- analysis_cli_environment$run_analysis_cli(
    c(
      "--input", input,
      "--no-sdm",
      "--accepted-qc-status", "ok",
      "--output", strict_output,
      "--loadings-output", strict_loadings
    ),
    project_root = analysis_cli_test_root
  )
  strict <- utils::read.csv(strict_output, stringsAsFactors = FALSE)
  testthat::expect_equal(nrow(strict), 5L)
  testthat::expect_true(all(strict$qc_status == "ok"))
  testthat::expect_identical(strict_result$excluded_qc$record_id, "photo-6")

  testthat::expect_error(
    analysis_cli_environment$run_analysis_cli(
      c(
        "--input", input,
        "--no-sdm",
        "--output", output,
        "--loadings-output", file.path(directory, "other-loadings.csv")
      ),
      project_root = analysis_cli_test_root
    ),
    "Refusing to overwrite"
  )
  testthat::expect_error(
    analysis_cli_environment$run_analysis_cli(
      c(
        "--input", input,
        "--no-sdm",
        "--output", input,
        "--loadings-output", file.path(directory, "other-loadings.csv")
      ),
      project_root = analysis_cli_test_root
    ),
    "must be distinct"
  )
  testthat::expect_error(
    analysis_cli_environment$validate_analysis_output_paths(
      input,
      file.path(directory, "same.csv"),
      file.path(directory, "same.csv")
    ),
    "must be distinct"
  )

  invalid_sdm_output <- file.path(directory, "invalid-sdm.csv")
  testthat::expect_error(
    analysis_cli_environment$run_analysis_cli(
      c(
        "--input", input,
        "--sdm-dir", file.path(directory, "missing-sdm"),
        "--output", invalid_sdm_output,
        "--loadings-output", file.path(directory, "invalid-sdm-loadings.csv")
      ),
      project_root = analysis_cli_test_root
    ),
    "SDM manifest not found"
  )
  testthat::expect_false(file.exists(invalid_sdm_output))
})
