if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("The rmarkdown package is required to render final_v11.Rmd.", call. = FALSE)
}

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
output_file <- if (length(args)) args[[1L]] else "final_v11.html"

rmarkdown::render(
  input = "final_v11.Rmd",
  output_file = output_file,
  envir = new.env(parent = globalenv()),
  clean = TRUE,
  quiet = FALSE
)
