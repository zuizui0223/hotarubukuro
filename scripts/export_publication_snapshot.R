args <- commandArgs(trailingOnly = TRUE)
source("R/pipeline_support.R")
hb_load_modules("final_registry")

output_dir <- hb_arg_value(
  args,
  "--output",
  file.path(
    "local_outputs",
    paste0("publication_snapshot_", format(Sys.Date(), "%Y-%m-%d"))
  )
)
overwrite <- hb_as_bool(hb_arg_value(args, "--overwrite", "false"))

if (dir.exists(output_dir) && !overwrite) {
  stop(
    "Snapshot directory already exists; use --overwrite=true: ",
    output_dir,
    call. = FALSE
  )
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

result_directories <- c(
  "results/ecological_v9_final_public_HRNA_50km",
  "results/ecological_v11_pigmentation_hurdle",
  "results/ecological_v15_multiscale_hotspots",
  "results/ecological_v16_predictive_replication",
  "results/ecological_v17_local_pair_turnover",
  "results/ecological_v19_human_landscape_extremes",
  "results/ecological_v20_local_white_isolates",
  "results/ecological_v21_local_human_neighbourhood",
  "results/ecological_v22_did_human_context",
  "results/final_analysis_pipeline",
  "results/environment_v3",
  "results/enmeval_aicc_reselected",
  "results/bombus_occurrence_phenology_cache",
  "results/public_rasters"
)

repository_files <- c(
  "Data_S1.csv",
  "data/processed/Data_S1_v2_manifest.json",
  "config/color_extraction.json",
  "README.md",
  "docs/analysis-plan.md",
  "docs/manuscript-story.md",
  "docs/data-sources/public-environment-sources.md",
  "docs/journal-target-review.md",
  "docs/methodology-literature.md",
  "docs/reproduction-guide.md",
  "manuscript/ecology-and-evolution-manuscript.md",
  "manuscript/submission-readiness-checklist.md",
  "R/pipeline_support.R",
  "R/final_registry.R",
  "scripts/build_manuscript_docx.py",
  "scripts/export_publication_snapshot.R",
  "scripts/run_publication_pipeline.R",
  "scripts/render_phenotype_report.R",
  if (file.exists("reports/phenotype-analysis.html")) {
    "reports/phenotype-analysis.html"
  } else {
    character()
  }
)

result_files <- unlist(lapply(
  result_directories,
  function(directory) {
    if (!dir.exists(directory)) return(character())
    list.files(directory, recursive = TRUE, full.names = TRUE)
  }
), use.names = FALSE)
source_files <- unique(c(repository_files, result_files))
source_files <- source_files[file.exists(source_files)]

copy_one <- function(source) {
  destination <- file.path(output_dir, "repository", source)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(source, destination, overwrite = TRUE, copy.date = TRUE)
  if (!copied) stop("Could not copy snapshot file: ", source, call. = FALSE)
  destination
}

destinations <- vapply(source_files, copy_one, character(1))
manifest <- data.frame(
  source = source_files,
  snapshot_path = sub(
    paste0("^", gsub("\\\\", "/", output_dir), "/?"),
    "",
    gsub("\\\\", "/", destinations)
  ),
  bytes = file.info(source_files)$size,
  md5 = unname(tools::md5sum(source_files)),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest,
  file.path(output_dir, "snapshot_manifest.csv"),
  row.names = FALSE
)

raw <- hb_read_csv("Data_S1.csv")
analysis <- hb_read_csv(
  paste0(
    "results/ecological_v11_pigmentation_hurdle/",
    "analysis_data_pigmentation_hurdle.csv"
  )
)
raw_year <- as.integer(format(as.Date(raw$date), "%Y"))
analysis_year <- as.integer(format(as.Date(analysis$date), "%Y"))
raw_date <- as.Date(raw$date)
analysis_date <- as.Date(analysis$date)
in_observation_window <- function(date) {
  year <- as.integer(format(date, "%Y"))
  date >= as.Date(sprintf("%d-06-01", year)) &
    date <= as.Date(sprintf("%d-08-31", year))
}
if (any(!in_observation_window(raw_date)) ||
    any(!in_observation_window(analysis_date))) {
  stop(
    "A record falls outside the registered annual window, 1 June–31 August.",
    call. = FALSE
  )
}
selection <- rbind(
  data.frame(
    dataset = "curated_colour_database",
    year = 2023:2025,
    n = as.integer(table(factor(raw_year, levels = 2023:2025))),
    first_date = vapply(
      2023:2025,
      function(year) as.character(min(raw_date[raw_year == year])),
      character(1)
    ),
    last_date = vapply(
      2023:2025,
      function(year) as.character(max(raw_date[raw_year == year])),
      character(1)
    )
  ),
  data.frame(
    dataset = "final_complete_case_analysis",
    year = 2023:2025,
    n = as.integer(table(factor(analysis_year, levels = 2023:2025))),
    first_date = vapply(
      2023:2025,
      function(year) as.character(min(analysis_date[analysis_year == year])),
      character(1)
    ),
    last_date = vapply(
      2023:2025,
      function(year) as.character(max(analysis_date[analysis_year == year])),
      character(1)
    )
  )
)
utils::write.csv(
  selection,
  file.path(output_dir, "year_selection_summary.csv"),
  row.names = FALSE
)

commit <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[1],
  error = function(error) NA_character_
)
metadata <- data.frame(
  field = c(
    "created_at", "git_commit", "r_version", "platform",
    "sns_source", "annual_observation_window",
    "sampling_scope", "source_file_count", "source_bytes"
  ),
  value = c(
    format(Sys.time(), tz = "UTC", usetz = TRUE),
    commit,
    R.version.string,
    R.version$platform,
    "YAMAP",
    "1 June through 31 August in 2023, 2024, and 2025",
    paste(
      "all eligible records identified by the author within the registered",
      "window; not a random sample of Japanese populations"
    ),
    nrow(manifest),
    sum(manifest$bytes)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  metadata,
  file.path(output_dir, "snapshot_metadata.csv"),
  row.names = FALSE
)
capture.output(
  utils::sessionInfo(),
  file = file.path(output_dir, "R_session_info.txt")
)

writeLines(
  c(
    "# Local publication-result snapshot",
    "",
    "This directory preserves the compact inputs and full adopted stage outputs",
    "used by the publication pipeline. It excludes superseded development runs.",
    "",
    "The curated database covers every eligible Campanula punctata record",
    "identified by the author on YAMAP from 1 June through 31 August in each",
    "of 2023, 2024, and 2025. See `year_selection_summary.csv`.",
    "",
    "## Restore and verify",
    "",
    "1. Check out the recorded Git commit.",
    "2. Copy `repository/*` into the repository root, preserving paths.",
    "3. Run:",
    "",
    "```powershell",
    "& 'C:\\Program Files\\R\\R-4.5.3\\bin\\Rscript.exe' `",
    "  scripts/run_publication_pipeline.R --mode=verify --tests=true",
    "```",
    "",
    "Use `snapshot_manifest.csv` to verify file sizes and MD5 hashes.",
    "See `repository/docs/reproduction-guide.md` for rebuild levels and limits."
  ),
  file.path(output_dir, "README.md"),
  useBytes = TRUE
)

cat(
  "Publication snapshot written: ",
  normalizePath(output_dir, winslash = "/"),
  " (", nrow(manifest), " files)\n",
  sep = ""
)
