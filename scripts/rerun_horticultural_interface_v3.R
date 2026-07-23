args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/rerun_horticultural_interface_v3.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "ecological_analysis_v2.R"))
source(file.path(repo_root, "scripts", "ecological_mechanism_v3.R"))

output_dir <- arg_value(
  "--output-dir", file.path(repo_root, "results", "ecological_v10_final_mechanism_HRNA")
)
analysis_csv <- arg_value(
  "--analysis-data", file.path(output_dir, "analysis_data_mechanism_v3.csv")
)
k_space <- as.integer(arg_value("--k-space", "40"))

data <- utils::read.csv(analysis_csv, check.names = FALSE, stringsAsFactors = FALSE)
data$region <- factor(data$region, levels = c("West", "East"))
env_terms <- grep("^env_", names(data), value = TRUE)
interface <- fit_horticultural_interface_convergence(data, env_terms, k_space)

outputs <- list(
  horticultural_HR_status.csv = interface$status,
  horticultural_HR_coefficients.csv = interface$coefficients,
  horticultural_HR_heldout.csv = interface$heldout,
  horticultural_HR_warnings.csv = interface$warnings,
  horticultural_HR_collinearity_vif.csv = interface$vif,
  horticultural_HR_collinearity_correlations.csv = interface$correlations,
  horticultural_HR_collinearity_condition.csv = interface$condition,
  horticultural_HR_directional_gates.csv = interface$gates,
  horticultural_R_boundary_audit.csv = interface$boundary_audit
)
for (name in names(outputs)) {
  write_csv_safe(outputs[[name]], file.path(output_dir, name))
}

evidence_path <- file.path(output_dir, "reviewer_evidence_matrix.csv")
if (file.exists(evidence_path)) {
  evidence <- utils::read.csv(
    evidence_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  old_interface_rows <- grepl(
    paste(
      "human-forest interface axis R|H-by-R|collinearity is quantified|",
      "pooled East-minus-West"
    ),
    evidence$criterion
  )
  evidence <- rbind(
    evidence[!old_interface_rows, , drop = FALSE],
    make_interface_evidence_rows(interface)
  )
  write_csv_safe(evidence, evidence_path)
}

cat("Recomputed horticultural H-R interface block in ", output_dir, "\n", sep = "")
