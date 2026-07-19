#!/usr/bin/env Rscript

# The five published SDM rasters are retained as immutable legacy outputs.
# Model fitting is intentionally not run because the original occurrence,
# predictor and evaluation artefacts are not present in this repository.
# This compatibility entry point performs the reproducible structural audit.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_argument)) sub("^--file=", "", script_argument[[1]]) else "Code_S4.R"
repo_root <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
Sys.setenv(HOTARUBUKURO_ROOT = repo_root)
source(file.path(repo_root, "scripts", "validate_sdm.R"), chdir = FALSE)
