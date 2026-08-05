source("R/pipeline_support.R")
hb_require_stage_packages("reporting")

if (!rmarkdown::pandoc_available()) {
  bundled_pandoc <- file.path(
    Sys.getenv("ProgramFiles", "C:/Program Files"),
    "RStudio", "resources", "app", "bin", "quarto", "bin", "tools"
  )
  if (file.exists(file.path(bundled_pandoc, "pandoc.exe"))) {
    Sys.setenv(RSTUDIO_PANDOC = bundled_pandoc)
  }
}
if (!rmarkdown::pandoc_available()) {
  stop(
    "Pandoc was not found. Install Quarto/RStudio or set RSTUDIO_PANDOC.",
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args)) args[[1L]] else "phenotype-analysis.html"

rmarkdown::render(
  input = "reports/phenotype-analysis.Rmd",
  output_file = output_file,
  knit_root_dir = normalizePath(".", winslash = "/", mustWork = TRUE),
  envir = new.env(parent = globalenv()),
  clean = TRUE,
  quiet = FALSE
)
