v24_analysis_spec_version <- "v24.0_candidate_local_doy_difference"

# Supplementary early-flowering check, in the lightest form that answers the
# question at all.
#
# It asks one thing: is a local-isolate candidate's typical flowering date
# earlier than that of the environment-similar cells around it? It answers with
# arithmetic on the cell table and the locked neighbourhood graph. There is no
# model here — no INLA, no SPDE, no predictive draws, no fitted phenology
# surface. The national phenology component was withdrawn because it does not
# complete (docs/public-reconstruction.md) and is deliberately not restored.
#
# The result feeds nothing. It is not used to select candidates, not used to
# rank them, and not the basis of any claim in the manuscript. Candidate
# selection and matching happen upstream from presence draws alone and are
# already fixed before this runs.
#
# Two neighbour sets are reported side by side, never merged:
#
#   all      every environment-similar neighbour in the locked graph
#   同年     the subset whose median observation year equals the focal cell's
#
# Reporting both makes the cost of the year restriction visible. Restricting to
# the same year removes the between-year phenological shift that would
# otherwise contaminate the difference, but it also shrinks the comparison set,
# sometimes to nothing. A candidate with no same-year neighbour is reported
# with NA and a neighbour count of zero, never dropped and never back-filled
# from the unrestricted set.

v24_require_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "Missing required ", label, " columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

# One focal cell against its neighbours. `neighbour_index` is the locked
# graph's neighbour list for this cell, so the environment-similarity and
# distance rules are exactly the ones the candidate definition used.
v24_local_doy_difference <- function(focal_index, neighbour_index,
                                     median_doy, median_year,
                                     same_year_only = FALSE) {
  if (!length(neighbour_index)) {
    return(c(difference = NA_real_, n_neighbours = 0, neighbour_doy = NA_real_))
  }
  keep <- neighbour_index
  if (same_year_only) {
    keep <- keep[
      is.finite(median_year[keep]) &
        is.finite(median_year[focal_index]) &
        median_year[keep] == median_year[focal_index]
    ]
  }
  keep <- keep[is.finite(median_doy[keep])]
  if (!length(keep) || !is.finite(median_doy[focal_index])) {
    return(c(
      difference = NA_real_, n_neighbours = length(keep),
      neighbour_doy = if (length(keep)) mean(median_doy[keep]) else NA_real_
    ))
  }
  neighbour_doy <- mean(median_doy[keep])
  c(
    # Negative means the focal cell flowers earlier than its neighbours.
    difference = median_doy[focal_index] - neighbour_doy,
    n_neighbours = length(keep),
    neighbour_doy = neighbour_doy
  )
}

# `focal_index` is a set of row positions in `cells`: the candidates, or the
# matched controls. `role` labels which, so the two land in one table and can
# be read against each other without a second run.
v24_doy_difference_table <- function(cells, graph, focal_index, role) {
  v24_require_columns(
    cells, c("exact_site_id", "median_DOY", "median_year"), "cells"
  )
  median_doy <- as.numeric(cells$median_DOY)
  median_year <- as.numeric(cells$median_year)
  if (!length(focal_index)) {
    return(data.frame(
      analysis_spec_version = character(), role = character(),
      exact_site_id = character(), median_DOY = numeric(),
      median_year = numeric(),
      all_neighbour_doy = numeric(), all_neighbour_difference = numeric(),
      all_n_neighbours = integer(),
      same_year_neighbour_doy = numeric(),
      same_year_difference = numeric(), same_year_n_neighbours = integer(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(focal_index, function(index) {
    neighbour_index <- graph$neighbours[[index]]
    all_set <- v24_local_doy_difference(
      index, neighbour_index, median_doy, median_year, same_year_only = FALSE
    )
    same_year <- v24_local_doy_difference(
      index, neighbour_index, median_doy, median_year, same_year_only = TRUE
    )
    data.frame(
      analysis_spec_version = v24_analysis_spec_version,
      role = role,
      exact_site_id = as.character(cells$exact_site_id[index]),
      median_DOY = median_doy[index],
      median_year = median_year[index],
      all_neighbour_doy = unname(all_set[["neighbour_doy"]]),
      all_neighbour_difference = unname(all_set[["difference"]]),
      all_n_neighbours = as.integer(all_set[["n_neighbours"]]),
      same_year_neighbour_doy = unname(same_year[["neighbour_doy"]]),
      same_year_difference = unname(same_year[["difference"]]),
      same_year_n_neighbours = as.integer(same_year[["n_neighbours"]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Descriptive only: counts, means, medians and the support behind them. No
# p-value and no test. With the candidate set fixed and the comparison being
# candidates against their own neighbours, a test here would be answering a
# question nobody asked and would invite reading a supplementary check as a
# finding.
v24_doy_summary <- function(differences) {
  if (!nrow(differences)) return(data.frame())
  rows <- list()
  for (role in unique(differences$role)) {
    block <- differences[differences$role == role, , drop = FALSE]
    for (set in c("all", "same_year")) {
      value <- if (identical(set, "all")) {
        block$all_neighbour_difference
      } else block$same_year_difference
      support <- if (identical(set, "all")) {
        block$all_n_neighbours
      } else block$same_year_n_neighbours
      usable <- is.finite(value)
      rows[[length(rows) + 1L]] <- data.frame(
        analysis_spec_version = v24_analysis_spec_version,
        role = role,
        neighbour_set = set,
        n_focal_cells = nrow(block),
        n_with_usable_neighbours = sum(usable),
        mean_difference_days = if (any(usable)) mean(value[usable]) else NA_real_,
        median_difference_days = if (any(usable)) {
          stats::median(value[usable])
        } else NA_real_,
        n_earlier_than_neighbours = sum(usable & value < 0),
        n_later_than_neighbours = sum(usable & value > 0),
        mean_neighbours_used = if (any(usable)) mean(support[usable]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
