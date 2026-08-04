v24_analysis_spec_version <- "v24.1_exact_year_local_doy_difference"

# Supplementary early-flowering check, in the lightest form that answers the
# question at all.
#
# It asks one thing: is a fixed local-isolate candidate earlier than the
# environment-similar cells around it? No model is fitted: no INLA, no SPDE, no
# predictive phenology surface and no p-value. The result feeds nothing and is
# never used for candidate selection, ranking or a main claim.
#
# Two descriptions are kept separate:
#
#   all neighbours  focal cell median DOY minus the median of the locked
#                   neighbours' cell-level median DOYs;
#   exact same year focal-year median DOY minus the median of neighbour-cell
#                   DOYs observed in that same calendar year, calculated once
#                   per shared year and then summarized across shared years.
#
# The second definition deliberately uses the observation table rather than a
# cell's `median_year`. A median year can equal a year in which that cell was not
# observed, so equality of two median years is not evidence of temporal overlap.

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

v24_cell_id <- function(x_km, y_km) {
  paste0("cell-1km-", floor(as.numeric(x_km)), "_", floor(as.numeric(y_km)))
}

# One row per observed 1-km cell and actual calendar year. Each cell-year first
# receives its own median DOY, preventing cells with many photographs from
# dominating the neighbour comparison.
v24_cell_year_table <- function(observations) {
  v24_require_columns(
    observations, c("x_km", "y_km", "DOY", "year"), "observations"
  )
  observations$cell_id_v24 <- v24_cell_id(
    observations$x_km, observations$y_km
  )
  observations$DOY <- as.numeric(observations$DOY)
  observations$year <- as.integer(round(as.numeric(observations$year)))
  observations <- observations[
    is.finite(observations$DOY) & is.finite(observations$year), , drop = FALSE
  ]
  if (!nrow(observations)) {
    return(data.frame(
      exact_site_id = character(), year = integer(), median_DOY = numeric(),
      n_observations = integer(), stringsAsFactors = FALSE
    ))
  }
  groups <- split(
    seq_len(nrow(observations)),
    paste(observations$cell_id_v24, observations$year, sep = "::")
  )
  rows <- lapply(groups, function(index) {
    block <- observations[index, , drop = FALSE]
    data.frame(
      exact_site_id = as.character(block$cell_id_v24[[1L]]),
      year = as.integer(block$year[[1L]]),
      median_DOY = stats::median(block$DOY),
      n_observations = nrow(block),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$exact_site_id, out$year), , drop = FALSE]
}

v24_all_neighbour_difference <- function(focal_index, neighbour_index,
                                         median_doy) {
  keep <- neighbour_index[is.finite(median_doy[neighbour_index])]
  if (!length(keep) || !is.finite(median_doy[focal_index])) {
    return(c(
      difference = NA_real_, n_neighbours = length(keep),
      focal_doy = median_doy[focal_index], neighbour_doy = NA_real_
    ))
  }
  neighbour_doy <- stats::median(median_doy[keep])
  c(
    difference = median_doy[focal_index] - neighbour_doy,
    n_neighbours = length(keep),
    focal_doy = median_doy[focal_index], neighbour_doy = neighbour_doy
  )
}

# Exact-year rows for one focal cell. A year is usable only when the focal cell
# and at least one locked neighbour both have observations in that literal year.
v24_same_year_rows <- function(focal_id, neighbour_ids, cell_year, role) {
  focal <- cell_year[cell_year$exact_site_id == focal_id, , drop = FALSE]
  if (!nrow(focal) || !length(neighbour_ids)) return(data.frame())
  rows <- lapply(seq_len(nrow(focal)), function(index) {
    year <- focal$year[[index]]
    neighbours <- cell_year[
      cell_year$exact_site_id %in% neighbour_ids & cell_year$year == year,
      , drop = FALSE
    ]
    if (!nrow(neighbours)) return(NULL)
    neighbour_doy <- stats::median(neighbours$median_DOY)
    data.frame(
      analysis_spec_version = v24_analysis_spec_version,
      role = role,
      exact_site_id = focal_id,
      year = year,
      focal_year_median_DOY = focal$median_DOY[[index]],
      neighbour_year_median_DOY = neighbour_doy,
      difference_days = focal$median_DOY[[index]] - neighbour_doy,
      n_neighbour_cells = length(unique(neighbours$exact_site_id)),
      n_neighbour_observations = sum(neighbours$n_observations),
      neighbour_cell_ids = paste(
        sort(unique(as.character(neighbours$exact_site_id))), collapse = "|"
      ),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

# Return one focal-cell table plus the exact-year detail used to build it.
v24_doy_difference_table <- function(cells, graph, focal_index, role,
                                     cell_year) {
  v24_require_columns(cells, c("exact_site_id", "median_DOY"), "cells")
  median_doy <- as.numeric(cells$median_DOY)
  if (!length(focal_index)) {
    return(list(
      differences = data.frame(), year_detail = data.frame()
    ))
  }

  detail_rows <- list()
  rows <- lapply(focal_index, function(index) {
    neighbour_index <- graph$neighbours[[index]]
    focal_id <- as.character(cells$exact_site_id[[index]])
    neighbour_ids <- as.character(cells$exact_site_id[neighbour_index])
    all_set <- v24_all_neighbour_difference(
      index, neighbour_index, median_doy
    )
    year_rows <- v24_same_year_rows(
      focal_id, neighbour_ids, cell_year, role
    )
    if (nrow(year_rows)) {
      detail_rows[[length(detail_rows) + 1L]] <<- year_rows
    }
    usable_years <- nrow(year_rows)
    data.frame(
      analysis_spec_version = v24_analysis_spec_version,
      role = role,
      exact_site_id = focal_id,
      median_DOY = median_doy[[index]],
      all_neighbour_doy = unname(all_set[["neighbour_doy"]]),
      all_neighbour_difference = unname(all_set[["difference"]]),
      all_n_neighbours = as.integer(all_set[["n_neighbours"]]),
      same_year_median_difference = if (usable_years) {
        stats::median(year_rows$difference_days)
      } else NA_real_,
      same_year_mean_difference = if (usable_years) {
        mean(year_rows$difference_days)
      } else NA_real_,
      same_year_n_years = usable_years,
      same_year_mean_neighbour_cells = if (usable_years) {
        mean(year_rows$n_neighbour_cells)
      } else NA_real_,
      same_year_min_neighbour_cells = if (usable_years) {
        min(year_rows$n_neighbour_cells)
      } else 0,
      stringsAsFactors = FALSE
    )
  })

  list(
    differences = do.call(rbind, rows),
    year_detail = if (length(detail_rows)) {
      do.call(rbind, detail_rows)
    } else data.frame()
  )
}

# Descriptive only: counts, means, medians and support. Negative differences
# mean earlier flowering. No significance test is attached to this supplement.
v24_doy_summary <- function(differences) {
  if (!nrow(differences)) return(data.frame())
  rows <- list()
  for (role in unique(differences$role)) {
    block <- differences[differences$role == role, , drop = FALSE]
    definitions <- list(
      all = list(
        value = block$all_neighbour_difference,
        support = block$all_n_neighbours
      ),
      exact_same_year = list(
        value = block$same_year_median_difference,
        support = block$same_year_mean_neighbour_cells
      )
    )
    for (set in names(definitions)) {
      value <- definitions[[set]]$value
      support <- definitions[[set]]$support
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
        mean_neighbours_used = if (any(usable)) {
          mean(support[usable], na.rm = TRUE)
        } else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
