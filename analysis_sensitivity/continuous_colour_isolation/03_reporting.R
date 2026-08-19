plot_profile <- function(path, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    grDevices::png(path, width = 1800, height = 1200, res = 180)
  } else {
    grDevices::pdf(path, width = 8, height = 5.5)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  y_range <- range(
    c(profile$lower_95, profile$upper_95, profile$rho_difference),
    finite = TRUE
  )
  graphics::plot(
    profile$radius_km, profile$rho_difference,
    type = "b", pch = 19, ylim = y_range,
    xlab = "Population-exposure radius (km; 0 = focal cell)",
    ylab = expression(rho[pigmented] - rho[white]),
    main = "Colour-specific isolation-population relationship"
  )
  graphics::segments(
    profile$radius_km, profile$lower_95,
    profile$radius_km, profile$upper_95
  )
  graphics::points(profile$radius_km, profile$null_mean, pch = 1)
  graphics::abline(h = 0, lty = 2)
  graphics::legend(
    "topright",
    legend = c("Observed difference", "Natural-map mean", "Natural 95% interval"),
    pch = c(19, 1, NA), lty = c(1, NA, 1), bty = "n"
  )
}
plot_profile(
  file.path(output_dir, "continuous_isolation_population_profile.png"),
  "png"
)
plot_profile(
  file.path(output_dir, "continuous_isolation_population_profile.pdf"),
  "pdf"
)

pigmented_median <- stats::median(raw_isolation[state])
white_median <- stats::median(raw_isolation[!state])
raw_observed_primary <- observed_for(
  "raw_same_colour_nn", "population_5km_rank"
)
relative_observed_primary <- observed_for(
  "relative_same_to_any_nn", "population_5km_rank"
)

summary_lines <- c(
  "# Continuous colour-isolation human-context analysis",
  "",
  "Status: post hoc exploratory generalisation; not preregistered.",
  "",
  sprintf(
    "- Cells: %d total; %d pigmented and %d white.",
    nrow(cells), sum(state), sum(!state)
  ),
  sprintf(
    "- Median raw same-colour isolation: %.6f km for pigmented and %.6f km for white cells.",
    pigmented_median, white_median
  ),
  sprintf(
    paste0(
      "- Raw 5-km population relationship: pigmented rho %.6f; white rho %.6f; ",
      "difference %.6f."
    ),
    raw_observed_primary$pigmented_rho,
    raw_observed_primary$white_rho,
    raw_observed_primary$rho_difference
  ),
  sprintf(
    paste0(
      "- Relative-isolation 5-km sensitivity: pigmented rho %.6f; white rho %.6f; ",
      "difference %.6f."
    ),
    relative_observed_primary$pigmented_rho,
    relative_observed_primary$white_rho,
    relative_observed_primary$rho_difference
  ),
  sprintf(
    paste0(
      "- Natural-map guardrail for the raw 5-km rho difference: observed %.6f; ",
      "null mean %.6f; central interval %.6f to %.6f; upper-tail P %.6f."
    ),
    primary_raw$observed_value, primary_raw$null_mean,
    primary_raw$lower_95, primary_raw$upper_95,
    primary_raw$empirical_p
  ),
  sprintf(
    paste0(
      "- Natural-map guardrail for the relative-isolation 5-km rho difference: ",
      "observed %.6f; null mean %.6f; central interval %.6f to %.6f; ",
      "upper-tail P %.6f."
    ),
    primary_relative$observed_value, primary_relative$null_mean,
    primary_relative$lower_95, primary_relative$upper_95,
    primary_relative$empirical_p
  ),
  sprintf(
    "- Population-scale maxT P: %.6f for raw isolation and %.6f for relative isolation.",
    maxT$all_maps_population_scale_maxT_p[
      maxT$metric == "raw_same_colour_nn"
    ],
    maxT$all_maps_population_scale_maxT_p[
      maxT$metric == "relative_same_to_any_nn"
    ]
  ),
  "",
  paste0(
    "Interpretation ceiling: the analysis describes a colour-specific human-context ",
    "overlay on spatial geometry. It does not establish horticultural origin, ",
    "introduction, establishment, plasticity, pollen movement or gene flow."
  )
)
writeLines(summary_lines, file.path(output_dir, "RESULT_SUMMARY.md"))

validation <- list(
  status = "PASS",
  spec_version = v23_analysis_spec_version,
  status_note = "post hoc exploratory generalisation; not preregistered",
  n_cells = nrow(cells),
  n_pigmented_cells = sum(state),
  n_white_cells = sum(!state),
  n_natural_maps = n_draws,
  permutations = permutations,
  seed = seed,
  observed = list(
    pigmented_median_same_colour_nn_km = pigmented_median,
    white_median_same_colour_nn_km = white_median,
    raw_population_5km = list(
      pigmented_rho = raw_observed_primary$pigmented_rho,
      white_rho = raw_observed_primary$white_rho,
      rho_difference = raw_observed_primary$rho_difference
    ),
    relative_population_5km = list(
      pigmented_rho = relative_observed_primary$pigmented_rho,
      white_rho = relative_observed_primary$white_rho,
      rho_difference = relative_observed_primary$rho_difference
    )
  ),
  natural_guardrail = list(
    raw_population_5km_upper_p = primary_raw$empirical_p,
    relative_population_5km_upper_p = primary_relative$empirical_p,
    raw_population_scale_maxT_p = maxT$all_maps_population_scale_maxT_p[
      maxT$metric == "raw_same_colour_nn"
    ],
    relative_population_scale_maxT_p = maxT$all_maps_population_scale_maxT_p[
      maxT$metric == "relative_same_to_any_nn"
    ],
    count_conditioning = as.list(selection[1L, ])
  ),
  claim_ceiling = paste(
    "No result establishes horticultural origin, introduction,",
    "establishment, plasticity, pollen movement or gene flow."
  )
)
jsonlite::write_json(
  validation,
  file.path(output_dir, "continuous_isolation_validation.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 12
)

message(
  "[continuous isolation] PASS; raw 5-km difference=",
  signif(raw_observed_primary$rho_difference, 6),
  "; natural upper P=", signif(primary_raw$empirical_p, 6)
)
