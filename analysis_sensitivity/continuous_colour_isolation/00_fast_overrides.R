# Runtime-equivalent Spearman helpers for high-repetition null calculations.
#
# The descriptive table still uses v23_safe_spearman() and therefore retains
# the asymptotic cor.test P value. Repeated permutation and natural-map loops
# need only rho, so they avoid constructing hundreds of thousands of htest
# objects. This changes runtime, not the estimand or rank calculation.

v23_feature_permutation_test <- function(
    isolation, feature, strata, permutations = 2000L,
    seed = 20260725L) {
  observed <- v23_spearman_rho(isolation, feature)
  set.seed(seed)
  null <- vapply(seq_len(permutations), function(iteration) {
    permuted <- v23_permute_within_strata(feature, strata)
    v23_spearman_rho(isolation, permuted)
  }, numeric(1))
  null <- null[is.finite(null)]
  p <- if (is.finite(observed) && length(null)) {
    (1 + sum(abs(null) >= abs(observed))) / (length(null) + 1)
  } else {
    NA_real_
  }
  list(observed = observed, null = null, two_sided_p = p)
}

v23_colour_rho_difference <- function(isolation, state, feature) {
  pigmented <- v23_spearman_rho(isolation[state], feature[state])
  white <- v23_spearman_rho(isolation[!state], feature[!state])
  c(
    pigmented_rho = pigmented,
    white_rho = white,
    rho_difference = pigmented - white
  )
}
