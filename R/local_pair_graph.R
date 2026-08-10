lp_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

lp_scale_vector <- function(x) {
  x <- as.numeric(x)
  value <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (!any(keep)) return(value)
  center <- mean(x[keep])
  spread <- stats::sd(x[keep])
  if (!is.finite(spread) || spread <= 1e-12) {
    value[keep] <- 0
  } else {
    value[keep] <- (x[keep] - center) / spread
  }
  value
}

lp_environment_matrix <- function(cells) {
  columns <- c(
    "broad50km_pc1", "broad50km_pc2",
    "within50km_pc1", "within50km_pc2"
  )
  lp_require_columns(cells, columns, "cells")
  x <- as.matrix(cells[, columns, drop = FALSE])
  storage.mode(x) <- "double"
  x <- apply(x, 2, lp_scale_vector)
  if (!is.matrix(x)) x <- matrix(x, ncol = length(columns))
  colnames(x) <- columns
  x
}

lp_pair_distance <- function(x, i, j) {
  sqrt(rowMeans((x[i, , drop = FALSE] - x[j, , drop = FALSE])^2))
}

lp_pair_graph <- function(cells, radius_km = 25, k = 5L,
                          same_fold_only = FALSE,
                          common_support_only = TRUE) {
  required <- c("exact_site_id", "x_km", "y_km")
  if (same_fold_only) required <- c(required, "spatial_fold")
  if (common_support_only) required <- c(required, "bombus_fingerprint_common_support")
  lp_require_columns(cells, required, "cells")

  if (!is.finite(radius_km) || radius_km <= 0) {
    stop("radius_km must be positive.", call. = FALSE)
  }
  k <- as.integer(k)
  if (!is.finite(k) || k < 1L) stop("k must be positive.", call. = FALSE)

  coordinates <- as.matrix(cells[, c("x_km", "y_km"), drop = FALSE])
  storage.mode(coordinates) <- "double"
  dx <- outer(coordinates[, 1], coordinates[, 1], "-")
  dy <- outer(coordinates[, 2], coordinates[, 2], "-")
  distance <- sqrt(dx^2 + dy^2)
  diag(distance) <- Inf

  eligible_node <- is.finite(coordinates[, 1]) & is.finite(coordinates[, 2])
  if (common_support_only) {
    eligible_node <- eligible_node & as.logical(cells$bombus_fingerprint_common_support)
  }

  directed <- vector("list", nrow(cells))
  for (i in which(eligible_node)) {
    eligible <- eligible_node & is.finite(distance[i, ]) & distance[i, ] <= radius_km
    if (same_fold_only) {
      eligible <- eligible &
        as.integer(cells$spatial_fold) == as.integer(cells$spatial_fold[i])
    }
    candidates <- which(eligible)
    if (!length(candidates)) next
    candidates <- candidates[order(distance[i, candidates], candidates)]
    candidates <- head(candidates, k)
    directed[[i]] <- data.frame(
      i = i,
      j = candidates,
      geographic_distance_km = distance[i, candidates],
      stringsAsFactors = FALSE
    )
  }

  directed <- directed[vapply(
    directed,
    function(value) if (is.null(value)) 0L else nrow(value),
    integer(1)
  ) > 0L]
  if (!length(directed)) return(data.frame())

  edges <- do.call(rbind, directed)
  edges$i_ordered <- pmin(edges$i, edges$j)
  edges$j_ordered <- pmax(edges$i, edges$j)
  edges$key <- paste(edges$i_ordered, edges$j_ordered, sep = "::")
  edges <- edges[order(edges$geographic_distance_km, edges$key), , drop = FALSE]
  edges <- edges[!duplicated(edges$key), , drop = FALSE]
  edges$i <- edges$i_ordered
  edges$j <- edges$j_ordered
  edges <- edges[order(edges$i, edges$j), , drop = FALSE]
  rownames(edges) <- NULL

  edges$edge_id <- paste0(
    "r", radius_km, "__",
    cells$exact_site_id[edges$i], "__", cells$exact_site_id[edges$j]
  )
  edges$radius_km <- radius_km
  edges$site_i <- as.character(cells$exact_site_id[edges$i])
  edges$site_j <- as.character(cells$exact_site_id[edges$j])
  if ("spatial_fold" %in% names(cells)) {
    edges$fold_i <- as.integer(cells$spatial_fold[edges$i])
    edges$fold_j <- as.integer(cells$spatial_fold[edges$j])
  }
  if ("bombus_fingerprint_common_support" %in% names(cells)) {
    edges$both_common_support <- as.logical(
      cells$bombus_fingerprint_common_support[edges$i] &
        cells$bombus_fingerprint_common_support[edges$j]
    )
  }

  keep <- c(
    "edge_id", "radius_km", "i", "j", "site_i", "site_j",
    intersect(c("fold_i", "fold_j"), names(edges)),
    "geographic_distance_km",
    intersect("both_common_support", names(edges))
  )
  edges[, keep, drop = FALSE]
}
