v23_analysis_spec_version <- "v23.0_posthoc_local_state_asymmetry"

v23_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

v23_as_matrix <- function(x, nrow_expected = NULL, label = "value") {
  if (is.vector(x)) x <- matrix(x, ncol = 1L)
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (!is.null(nrow_expected) && nrow(x) != nrow_expected) {
    stop(label, " has ", nrow(x), " rows; expected ", nrow_expected, ".",
         call. = FALSE)
  }
  x
}

v23_state_rule_table <- function() {
  data.frame(
    state_rule = c("legacy_any_pigmented", "pure_cells", "share_10_90"),
    white_max_share = c(0, 0, 0.10),
    pigmented_min_share = c(.Machine$double.eps, 1, 0.90),
    interpretation = c(
      "Mirrors the locked event: any pigmented observation versus zero pigmented observations.",
      "Requires pure pigmented and pure white focal/background cells.",
      "Treats shares <=0.10 as white and >=0.90 as pigmented; intermediate cells are unclassified."
    ),
    stringsAsFactors = FALSE
  )
}

v23_classify_states <- function(counts, trials,
                                white_max_share = 0,
                                pigmented_min_share = .Machine$double.eps) {
  trials <- as.numeric(trials)
  if (any(!is.finite(trials)) || any(trials <= 0)) {
    stop("trials must be finite and positive.", call. = FALSE)
  }
  counts <- v23_as_matrix(counts, length(trials), "counts")
  if (any(!is.finite(counts)) || any(counts < 0) ||
      any(counts > matrix(trials, nrow(counts), ncol(counts)))) {
    stop("counts must be finite and lie between zero and trials.",
         call. = FALSE)
  }
  share <- sweep(counts, 1L, trials, "/")
  white <- share <= white_max_share + 1e-12
  pigmented <- share >= pigmented_min_share - 1e-12
  overlap <- white & pigmented
  if (any(overlap)) {
    stop("white and pigmented state definitions overlap.", call. = FALSE)
  }
  list(
    share = share,
    white = white,
    pigmented = pigmented,
    classified = white | pigmented
  )
}

v23_graph_support <- function(graph, n_cells) {
  if (is.null(graph$neighbours) || length(graph$neighbours) != n_cells) {
    stop("graph$neighbours must be a list with one entry per cell.",
         call. = FALSE)
  }
  supported <- if (is.null(graph$supported)) {
    vapply(graph$neighbours, length, integer(1L)) > 0L
  } else {
    as.logical(graph$supported)
  }
  if (length(supported) != n_cells || anyNA(supported)) {
    stop("graph$supported must align one-to-one with cells.", call. = FALSE)
  }
  supported
}

v23_all_neighbours_in_state <- function(state, neighbours, supported) {
  n_cells <- nrow(state)
  n_draws <- ncol(state)
  out <- matrix(FALSE, n_cells, n_draws)
  for (index in which(supported)) {
    adjacent <- as.integer(neighbours[[index]])
    adjacent <- adjacent[
      is.finite(adjacent) & adjacent >= 1L & adjacent <= n_cells &
        supported[adjacent]
    ]
    if (!length(adjacent)) next
    out[index, ] <- colSums(state[adjacent, , drop = FALSE]) == length(adjacent)
  }
  out
}

v23_directional_profiles <- function(counts, trials, graph,
                                     white_max_share = 0,
                                     pigmented_min_share = .Machine$double.eps,
                                     pseudocount = 0.5) {
  states <- v23_classify_states(
    counts, trials,
    white_max_share = white_max_share,
    pigmented_min_share = pigmented_min_share
  )
  n_cells <- nrow(states$share)
  supported <- v23_graph_support(graph, n_cells)
  support_matrix <- matrix(supported, n_cells, ncol(states$share))

  neighbours_white <- v23_all_neighbours_in_state(
    states$white, graph$neighbours, supported
  )
  neighbours_pigmented <- v23_all_neighbours_in_state(
    states$pigmented, graph$neighbours, supported
  )

  eligible_pigmented <- states$pigmented & support_matrix
  eligible_white <- states$white & support_matrix
  pigmented_in_white <- eligible_pigmented & neighbours_white
  white_in_pigmented <- eligible_white & neighbours_pigmented

  count_p_in_w <- colSums(pigmented_in_white)
  count_w_in_p <- colSums(white_in_pigmented)
  eligible_p <- colSums(eligible_pigmented)
  eligible_w <- colSums(eligible_white)

  rate_p_in_w <- (count_p_in_w + pseudocount) /
    (eligible_p + 2 * pseudocount)
  rate_w_in_p <- (count_w_in_p + pseudocount) /
    (eligible_w + 2 * pseudocount)

  summary <- data.frame(
    draw = seq_len(ncol(states$share)),
    pigmented_in_white_count = count_p_in_w,
    white_in_pigmented_count = count_w_in_p,
    eligible_pigmented_focals = eligible_p,
    eligible_white_focals = eligible_w,
    pigmented_in_white_rate = rate_p_in_w,
    white_in_pigmented_rate = rate_w_in_p,
    count_difference = count_p_in_w - count_w_in_p,
    rate_difference = rate_p_in_w - rate_w_in_p,
    log_rate_ratio = log(rate_p_in_w / rate_w_in_p),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    pigmented_in_white = pigmented_in_white,
    white_in_pigmented = white_in_pigmented,
    states = states,
    supported = supported,
    pseudocount = pseudocount
  )
}

v23_null_comparison <- function(observed, simulated) {
  simulated <- as.numeric(simulated)
  simulated <- simulated[is.finite(simulated)]
  if (!is.finite(observed) || !length(simulated)) {
    return(c(
      observed_value = observed, null_mean = NA_real_, null_sd = NA_real_,
      lower_95 = NA_real_, upper_95 = NA_real_, upper_p = NA_real_,
      lower_p = NA_real_, two_sided_p = NA_real_, percentile = NA_real_
    ))
  }
  upper <- (1 + sum(simulated >= observed)) / (length(simulated) + 1)
  lower <- (1 + sum(simulated <= observed)) / (length(simulated) + 1)
  c(
    observed_value = observed,
    null_mean = mean(simulated),
    null_sd = stats::sd(simulated),
    lower_95 = unname(stats::quantile(simulated, 0.025)),
    upper_95 = unname(stats::quantile(simulated, 0.975)),
    upper_p = upper,
    lower_p = lower,
    two_sided_p = min(1, 2 * min(upper, lower)),
    percentile = mean(simulated <= observed)
  )
}

v23_asymmetry_summary <- function(observed_profile, simulated_profile,
                                  state_rule,
                                  analysis_status = "post_hoc_diagnostic") {
  observed <- observed_profile$summary
  simulated <- simulated_profile$summary
  if (nrow(observed) != 1L) {
    stop("observed_profile must contain exactly one observed map.",
         call. = FALSE)
  }
  metrics <- c(
    "pigmented_in_white_count", "white_in_pigmented_count",
    "pigmented_in_white_rate", "white_in_pigmented_rate",
    "count_difference", "rate_difference", "log_rate_ratio"
  )
  rows <- lapply(metrics, function(metric) {
    comparison <- v23_null_comparison(observed[[metric]], simulated[[metric]])
    data.frame(
      analysis_spec_version = v23_analysis_spec_version,
      analysis_status = analysis_status,
      state_rule = state_rule,
      metric = metric,
      t(comparison),
      n_natural_maps = nrow(simulated),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

v23_observed_candidates <- function(profile, cell_ids, state_rule) {
  cell_ids <- as.character(cell_ids)
  if (length(cell_ids) != nrow(profile$pigmented_in_white)) {
    stop("cell_ids do not align with profile rows.", call. = FALSE)
  }
  if (ncol(profile$pigmented_in_white) != 1L) {
    stop("candidate extraction requires one observed map.", call. = FALSE)
  }
  rbind(
    data.frame(
      state_rule = state_rule,
      direction = "pigmented_in_white",
      exact_site_id = cell_ids[profile$pigmented_in_white[, 1L]],
      stringsAsFactors = FALSE
    ),
    data.frame(
      state_rule = state_rule,
      direction = "white_in_pigmented",
      exact_site_id = cell_ids[profile$white_in_pigmented[, 1L]],
      stringsAsFactors = FALSE
    )
  )
}
