args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  idx <- match(flag, args)
  if (is.na(idx) || idx == length(args)) default else args[idx + 1L]
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[1L])
} else "scripts/rerun_bombus_species_audit_v3.R"
repo_root <- normalizePath(
  file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE
)
source(file.path(repo_root, "scripts", "ecological_analysis_v2.R"))
source(file.path(repo_root, "scripts", "ecological_mechanism_v3.R"))

output_dir <- arg_value(
  "--output-dir",
  file.path(repo_root, "results", "ecological_v10_final_mechanism_HRNA")
)
analysis_csv <- arg_value(
  "--analysis-data", file.path(output_dir, "analysis_data_mechanism_v3.csv")
)
k_space <- as.integer(arg_value("--k-space", "40"))

data <- utils::read.csv(
  analysis_csv, check.names = FALSE, stringsAsFactors = FALSE
)
data$region <- factor(data$region, levels = c("West", "East"))
env_terms <- grep("^env_", names(data), value = TRUE)

species <- c("beaticola", "consobrinus", "honshuensis")
proxies <- c(
  setNames(paste0("bee_", species, "_ns"), paste0(species, "_ENMeval")),
  setNames(
    paste0("Bombus_", species, "_occ_density_100km"),
    paste0(species, "_occurrence_density")
  )
)
missing <- setdiff(unname(proxies), names(data))
if (length(missing)) {
  stop("Missing alpine-species proxies: ", paste(missing, collapse = ", "))
}

support_rows <- lapply(names(proxies), function(label) {
  variable <- proxies[[label]]
  x <- data[[variable]]
  data.frame(
    label = label,
    species = sub("_(ENMeval|occurrence_density)$", "", label),
    measurement_model = if (grepl("_ENMeval$", label)) {
      "ENMeval relative suitability within the species-specific projection support"
    } else {
      "log-standardized 100-km target-group occurrence density"
    },
    variable = variable,
    n_finite = sum(is.finite(x)),
    n_west = sum(is.finite(x) & data$region == "West"),
    n_east = sum(is.finite(x) & data$region == "East"),
    interpretation = paste(
      "species-specific exploratory audit; supports differ among ENMeval models,",
      "so coefficients are not direct between-species effect comparisons"
    ),
    stringsAsFactors = FALSE
  )
})
support <- do.call(rbind, support_rows)

fits <- lapply(unname(proxies), function(proxy) {
  fit_bombus_evidence_ladder(
    data = data, response = "response", predictor = proxy,
    env_terms = env_terms, k_space = k_space
  )
})
names(fits) <- names(proxies)

coefficients <- do.call(rbind, lapply(names(fits), function(label) {
  out <- fits[[label]]$coefficients
  out$label <- label
  out$species <- sub("_(ENMeval|occurrence_density)$", "", label)
  out$measurement_model <- if (grepl("_ENMeval$", label)) {
    "ENMeval"
  } else "occurrence_density"
  out
}))
heldout <- do.call(rbind, lapply(names(fits), function(label) {
  out <- fits[[label]]$heldout_metrics
  out$label <- label
  out$species <- sub("_(ENMeval|occurrence_density)$", "", label)
  out$measurement_model <- if (grepl("_ENMeval$", label)) {
    "ENMeval"
  } else "occurrence_density"
  out
}))
logs <- do.call(rbind, lapply(names(fits), function(label) {
  out <- fits[[label]]$log
  out$label <- label
  out
}))

conditional <- merge(
  coefficients[coefficients$rung == "environment_space_adjusted", c(
    "label", "species", "measurement_model", "predictor", "n", "estimate",
    "se", "p_value"
  )],
  heldout[heldout$rung == "environment_space_adjusted", c(
    "label", "delta_heldout_R2", "heldout_R2_base",
    "heldout_R2_with_Bombus"
  )],
  by = "label", all = TRUE
)
conditional$lower_95 <- conditional$estimate - 1.96 * conditional$se
conditional$upper_95 <- conditional$estimate + 1.96 * conditional$se
conditional$claim_ceiling <- paste(
  "exploratory species-specific gradient; not abundance, visitation, selection,",
  "or a causal species effect"
)

write_csv_safe(
  support, file.path(output_dir, "bombus_alpine_species_support.csv")
)
write_csv_safe(
  coefficients,
  file.path(output_dir, "bombus_alpine_species_ladder_coefficients.csv")
)
write_csv_safe(
  heldout,
  file.path(output_dir, "bombus_alpine_species_ladder_heldout.csv")
)
write_csv_safe(
  logs, file.path(output_dir, "bombus_alpine_species_ladder_crossfit_log.csv")
)
write_csv_safe(
  conditional,
  file.path(output_dir, "bombus_alpine_species_conditional_summary.csv")
)

cat("Completed individual alpine Bombus audit in ", output_dir, "\n", sep = "")
