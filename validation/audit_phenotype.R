args <- commandArgs(trailingOnly = TRUE)

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/audit_phenotype.R"
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
source(file.path(repo_root, "R", "pipeline_support.R"))
arg_value <- function(flag, default = "") hb_arg_value(args, flag, default)
output_dir <- arg_value(
  "--output-dir",
  file.path(repo_root, "results", "ecological_v11_pigmentation_hurdle")
)

read_result <- function(name) {
  path <- file.path(output_dir, name)
  if (!file.exists(path)) stop("Missing v11 result: ", path, call. = FALSE)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

data <- read_result("analysis_data_pigmentation_hurdle.csv")
measurement <- read_result("pigmentation_measurement_summary.csv")
inla <- read_result("pigmentation_hurdle_inla_model_comparison.csv")
presence_held <- read_result("pigmentation_presence_bombus_heldout.csv")
presence_log <- read_result("pigmentation_presence_bombus_crossfit_log.csv")
intensity_held <- read_result("pigmented_intensity_bombus_heldout.csv")
tail_coef <- read_result("pigmentation_residual_tail_HR_coefficients.csv")
tail_held <- read_result("pigmentation_residual_tail_HR_heldout.csv")

checks <- list()
add_check <- function(name, pass, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name, status = if (isTRUE(pass)) "pass" else "fail",
    detail = as.character(detail), stringsAsFactors = FALSE
  )
}

add_check("analysis_n", nrow(data) == 1923L, nrow(data))
add_check(
  "binary_response",
  setequal(sort(unique(data$pigmented_mixture50)), c(0L, 1L)),
  paste(sort(unique(data$pigmented_mixture50)), collapse = "|")
)
add_check(
  "measurement_counts",
  sum(data$pigmented_mixture50 == 0L) == measurement$n_white &&
    sum(data$pigmented_mixture50 == 1L) == measurement$n_pigmented,
  paste0("white=", sum(data$pigmented_mixture50 == 0L),
         ";pigmented=", sum(data$pigmented_mixture50 == 1L))
)
add_check(
  "conditional_intensity_domain",
  all(is.na(data$pigment_intensity_z[data$pigmented_mixture50 == 0L])) &&
    all(is.finite(data$pigment_intensity_z[data$pigmented_mixture50 == 1L])),
  paste0("finite_intensity=", sum(is.finite(data$pigment_intensity_z)))
)
add_check(
  "zero_rule_not_primary",
  any(data$pigmented_zero_rule != data$pigmented_mixture50),
  paste0("disagreements=", sum(data$pigmented_zero_rule != data$pigmented_mixture50))
)
add_check(
  "inla_complete",
  nrow(inla) == 12L && all(is.finite(inla$WAIC)) &&
    all(inla$n_CPO_nonfinite == 0L),
  paste0("models=", nrow(inla), ";nonfinite_CPO=", sum(inla$n_CPO_nonfinite))
)
primary_held <- rbind(
  subset(presence_held, predictor %in% c("Bombus_W", "Bombus_A")),
  subset(intensity_held, predictor %in% c("Bombus_W", "Bombus_A"))
)
add_check(
  "primary_bombus_crossfit_warnings",
  all(primary_held$n_warning_folds == 0L),
  paste0("warning_rows=", sum(primary_held$n_warning_folds > 0L))
)
warning_log <- subset(
  presence_log,
  (!is.na(base_warnings) & nzchar(base_warnings)) |
    (!is.na(full_warnings) & nzchar(full_warnings))
)
add_check(
  "species_warning_isolated",
  nrow(warning_log) == 2L &&
    all(warning_log$predictor == "bee_consobrinus_ns") &&
    all(warning_log$fold == 5L),
  paste0("warning_rows=", nrow(warning_log))
)
add_check(
  "residual_tail_warnings",
  !any(!is.na(tail_coef$model_warnings) & nzchar(tail_coef$model_warnings)) &&
    all(tail_held$n_warning_folds == 0L),
  paste0(
    "coefficient_warning_rows=",
    sum(!is.na(tail_coef$model_warnings) & nzchar(tail_coef$model_warnings)),
    ";heldout_warning_rows=", sum(tail_held$n_warning_folds > 0L)
  )
)
add_check(
  "residuals_cross_fitted",
  all(is.finite(data$natural_presence_probability)) &&
    sum(is.finite(data$natural_intensity_prediction)) ==
      sum(data$pigmented_mixture50 == 1L),
  paste0(
    "presence_predictions=", sum(is.finite(data$natural_presence_probability)),
    ";intensity_predictions=", sum(is.finite(data$natural_intensity_prediction))
  )
)

validation <- do.call(rbind, checks)
utils::write.csv(
  validation, file.path(output_dir, "validation_summary.csv"),
  row.names = FALSE, na = ""
)
print(validation, row.names = FALSE)
if (any(validation$status != "pass")) {
  stop("Pigmentation v11 result validation failed.", call. = FALSE)
}
cat("All pigmentation v11 validation checks passed.\n")
