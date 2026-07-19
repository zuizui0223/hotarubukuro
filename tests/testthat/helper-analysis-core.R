find_test_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "Data_S1.csv")) &&
        file.exists(file.path(path, "R", "analysis_core.R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) stop("Project root was not found for tests.")
    path <- parent
  }
}

test_project_root <- find_test_project_root()
source(file.path(test_project_root, "R", "analysis_core.R"))
