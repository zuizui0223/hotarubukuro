# Central package registry. Analysis scripts should call assert_packages() rather than repeat library() blocks.

package_groups <- list(
  core = c("dplyr", "readr", "tidyr", "tibble"),
  spatial = c("sf", "terra"),
  colour = character(),
  model = c("INLA", "mgcv", "qgam"),
  plotting = c("ggplot2", "forcats", "patchwork"),
  public_data = c("geodata"),
  sdm = c("ENMeval", "maxnet", "usdm", "rnaturalearth", "rnaturalearthdata")
)

required_packages <- function(groups = names(package_groups)) {
  unknown <- setdiff(groups, names(package_groups))
  if (length(unknown)) stop("Unknown package groups: ", paste(unknown, collapse = ", "), call. = FALSE)
  unique(unlist(package_groups[groups], use.names = FALSE))
}

assert_packages <- function(groups = names(package_groups)) {
  packages <- required_packages(groups)
  missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ". Run Rscript scripts/install_dependencies.R", call. = FALSE)
  }
  invisible(TRUE)
}
