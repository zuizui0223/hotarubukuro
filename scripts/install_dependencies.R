#!/usr/bin/env Rscript

source("R/packages.R")
cran_packages <- setdiff(required_packages(), "INLA")
missing_cran <- cran_packages[!vapply(cran_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_cran)) install.packages(missing_cran, repos = "https://cloud.r-project.org")
if (!requireNamespace("INLA", quietly = TRUE)) {
  install.packages("INLA", repos = c(CRAN = "https://cloud.r-project.org", INLA = "https://inla.r-inla-download.org/R/stable"))
}
cat("R dependencies are installed.\n")
