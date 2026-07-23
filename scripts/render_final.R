if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("The rmarkdown package is required to render final.Rmd.", call. = FALSE)
}

output_file <- Sys.getenv("HOTARUBUKURO_RENDERED_HTML", "final.html")
output_dir <- Sys.getenv("HOTARUBUKURO_RENDER_DIR", "")
as_bool <- function(x) tolower(x) %in% c("1", "true", "yes", "y")
options(
  campanula.rebuild_bombus_sdm = as_bool(Sys.getenv("HOTARUBUKURO_REBUILD_BOMBUS_SDM", "true")),
  campanula.refresh_gbif = as_bool(Sys.getenv("HOTARUBUKURO_REFRESH_GBIF", "false"))
)
render_args <- list(
  input = "final.Rmd",
  output_file = output_file,
  envir = new.env(parent = globalenv()),
  clean = TRUE,
  quiet = FALSE
)
if (nzchar(output_dir)) render_args$output_dir <- output_dir
do.call(rmarkdown::render, render_args)
