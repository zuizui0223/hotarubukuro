final_analysis_spec_version <-
  "publication_pipeline_1.0"

final_require_columns <- function(data, columns, label = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      label, " is missing: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

final_read_csv <- function(root, path) {
  full_path <- file.path(root, path)
  if (!file.exists(full_path)) {
    stop("Missing final-pipeline artifact: ", full_path, call. = FALSE)
  }
  utils::read.csv(
    full_path, check.names = FALSE, stringsAsFactors = FALSE
  )
}

final_required_artifacts <- function() {
  data.frame(
    stage = c(
      "phenotype", "national_natural_model", "national_natural_model",
      "local_bombus_turnover", "local_bombus_turnover",
      "local_isolate_definition", "local_human_context",
      "local_human_context", "did_sensitivity", "did_sensitivity"
    ),
    artifact = c(
      paste0(
        "results/ecological_v11_pigmentation_hurdle/",
        "pigmentation_measurement_summary.csv"
      ),
      paste0(
        "results/ecological_v16_predictive_replication/",
        "predictive_replication_model_performance.csv"
      ),
      paste0(
        "results/ecological_v16_predictive_replication/",
        "predictive_replication_bombus_paired_contrast.csv"
      ),
      paste0(
        "results/ecological_v17_local_pair_turnover/",
        "local_pair_predictive_summary.csv"
      ),
      paste0(
        "results/ecological_v17_local_pair_turnover/",
        "local_pair_metadata.csv"
      ),
      paste0(
        "results/ecological_v20_local_white_isolates/",
        "local_isolate_natural_null_summary.csv"
      ),
      paste0(
        "results/ecological_v21_local_human_neighbourhood/",
        "human_neighbourhood_population_scale_summary.csv"
      ),
      paste0(
        "results/ecological_v21_local_human_neighbourhood/",
        "human_neighbourhood_quality_summary.csv"
      ),
      paste0(
        "results/ecological_v22_did_human_context/",
        "did_contrast_summary.csv"
      ),
      paste0(
        "results/ecological_v22_did_human_context/",
        "did_context_composition_summary.csv"
      )
    ),
    inference_role = c(
      "confirmatory_measurement",
      "confirmatory_natural_baseline",
      "national_bombus_sensitivity",
      "planned_local_bombus_test",
      "planned_local_bombus_test",
      "candidate_definition_and_natural_null",
      "exploratory_human_context",
      "negative_control",
      "exploratory_human_context_sensitivity",
      "exploratory_human_context_sensitivity"
    ),
    stringsAsFactors = FALSE
  )
}

final_result_registry <- function(root = ".") {
  measurement <- final_read_csv(
    root,
    paste0(
      "results/ecological_v11_pigmentation_hurdle/",
      "pigmentation_measurement_summary.csv"
    )
  )
  performance <- final_read_csv(
    root,
    paste0(
      "results/ecological_v16_predictive_replication/",
      "predictive_replication_model_performance.csv"
    )
  )
  bombus <- final_read_csv(
    root,
    paste0(
      "results/ecological_v16_predictive_replication/",
      "predictive_replication_bombus_paired_contrast.csv"
    )
  )
  pair <- final_read_csv(
    root,
    paste0(
      "results/ecological_v17_local_pair_turnover/",
      "local_pair_predictive_summary.csv"
    )
  )
  isolate <- final_read_csv(
    root,
    paste0(
      "results/ecological_v20_local_white_isolates/",
      "local_isolate_natural_null_summary.csv"
    )
  )
  population <- final_read_csv(
    root,
    paste0(
      "results/ecological_v21_local_human_neighbourhood/",
      "human_neighbourhood_population_scale_summary.csv"
    )
  )
  did <- final_read_csv(
    root,
    paste0(
      "results/ecological_v22_did_human_context/",
      "did_contrast_summary.csv"
    )
  )
  did_composition <- final_read_csv(
    root,
    paste0(
      "results/ecological_v22_did_human_context/",
      "did_context_composition_summary.csv"
    )
  )
  did_candidates <- final_read_csv(
    root,
    paste0(
      "results/ecological_v22_did_human_context/",
      "did_candidate_details.csv"
    )
  )

  national_presence <- performance[
    performance$model == "national_environment_spde_presence",
    , drop = FALSE
  ]
  national_intensity <- performance[
    performance$model == "national_environment_spde_intensity",
    , drop = FALSE
  ]
  pair_primary <- pair[
    pair$radius_km == 25 &
      pair$predictor == "fingerprint_turnover",
    , drop = FALSE
  ]
  pair_presence <- pair_primary[
    pair_primary$response == "presence", , drop = FALSE
  ]
  pair_intensity <- pair_primary[
    pair_primary$response == "intensity", , drop = FALSE
  ]
  isolate_count <- isolate[
    isolate$configuration == "primary_10km_env1_all_white" &
      isolate$metric == "candidate_count",
    , drop = FALSE
  ]
  isolate_fraction <- isolate[
    isolate$configuration == "primary_10km_env1_all_white" &
      isolate$metric == "candidate_fraction",
    , drop = FALSE
  ]
  population_5km <- population[
    population$feature == "population_5km_rank", , drop = FALSE
  ]
  did_alignment <- did[
    did$feature == "did_aligned_population_score", , drop = FALSE
  ]
  did_urban <- did_composition[
    did_composition$human_context_class ==
      "did_proximate_high_population",
    , drop = FALSE
  ]
  joint_candidate <- did_candidates[
    did_candidates$joint_q10_did_proximity_spike, , drop = FALSE
  ]

  required_single_rows <- list(
    national_presence, national_intensity, pair_presence, pair_intensity,
    isolate_count, isolate_fraction, population_5km, did_alignment,
    did_urban
  )
  if (any(vapply(required_single_rows, nrow, integer(1)) != 1L)) {
    stop("Final result selectors are not one-to-one.", call. = FALSE)
  }

  add <- function(
      result_id, tier, estimand, estimate, null_reference = NA_real_,
      raw_p = NA_real_, corrected_p = NA_real_, correction_family = "",
      status, source) {
    data.frame(
      result_id = result_id,
      tier = tier,
      estimand = estimand,
      estimate = as.numeric(estimate),
      null_reference = as.numeric(null_reference),
      raw_p = as.numeric(raw_p),
      corrected_p = as.numeric(corrected_p),
      correction_family = correction_family,
      status = status,
      source = source,
      stringsAsFactors = FALSE
    )
  }
  positive_association_status <- function(estimate, corrected_p) {
    if (is.finite(estimate) && estimate > 0 &&
        is.finite(corrected_p) && corrected_p < 0.05) {
      "supported_association"
    } else {
      "not_supported_at_0.05"
    }
  }
  natural_null_status <- function(p) {
    if (is.finite(p) && p <= 0.05) {
      "upper_tail_against_natural_model"
    } else {
      "compatible_with_natural_model"
    }
  }
  exploratory_status <- function(raw_p, corrected_p) {
    if (is.finite(corrected_p) && corrected_p <= 0.05) {
      "familywise_supported_direction"
    } else if (is.finite(raw_p) && raw_p < 0.10) {
      "suggestive_not_corrected"
    } else {
      "not_supported"
    }
  }

  do.call(rbind, list(
    add(
      "phenotype_white_count", "confirmatory",
      "response-blind white-like flower count",
      measurement$n_white, status = "descriptive",
      source = "stage 01 phenotype measurement summary"
    ),
    add(
      "phenotype_pigmented_count", "confirmatory",
      "response-blind pigmented flower count",
      measurement$n_pigmented, status = "descriptive",
      source = "stage 01 phenotype measurement summary"
    ),
    add(
      "national_presence_auc", "confirmatory",
      "cross-fitted national environment-plus-SPDE presence AUC",
      national_presence$AUC, status = "model_diagnostic",
      source = "stage 02 natural-model performance"
    ),
    add(
      "national_intensity_rmse", "confirmatory",
      "cross-fitted pigmented-only intensity RMSE",
      national_intensity$RMSE, status = "model_diagnostic",
      source = "stage 02 natural-model performance"
    ),
    add(
      "national_bombus_auc_gain", "sensitivity",
      "mean fold AUC gain after adding Bombus fingerprint",
      mean(bombus$AUC_improvement),
      status = if (abs(mean(bombus$AUC_improvement)) < 0.02) {
        "small_predictive_gain"
      } else {
        "predictive_gain_ge_0.02"
      },
      source = "stage 02 national Bombus sensitivity"
    ),
    add(
      "local_bombus_presence", "planned_local_test",
      "25-km partial beta: pigmented-share turnover versus fingerprint turnover",
      pair_presence$observed_partial_beta,
      pair_presence$beta_null_mean,
      pair_presence$beta_empirical_p,
      pair_presence$BH_q_primary_25km,
      "two primary 25-km hurdle responses",
      positive_association_status(
        pair_presence$observed_partial_beta,
        pair_presence$BH_q_primary_25km
      ),
      "stage 03 local Bombus pair summary"
    ),
    add(
      "local_bombus_intensity", "planned_local_test",
      "25-km partial beta: pigmented-only intensity turnover versus fingerprint turnover",
      pair_intensity$observed_partial_beta,
      pair_intensity$beta_null_mean,
      pair_intensity$beta_empirical_p,
      pair_intensity$BH_q_primary_25km,
      "two primary 25-km hurdle responses",
      positive_association_status(
        pair_intensity$observed_partial_beta,
        pair_intensity$BH_q_primary_25km
      ),
      "stage 03 local Bombus pair summary"
    ),
    add(
      "local_isolate_count", "candidate_definition",
      "10-km environment-similar all-white-neighbour isolate count",
      isolate_count$observed_value, isolate_count$null_mean,
      isolate_count$empirical_p, correction_family = "prespecified primary",
      status = natural_null_status(isolate_count$empirical_p),
      source = "stage 04 local-isolate natural null"
    ),
    add(
      "local_isolate_fraction", "candidate_definition",
      "10-km environment-similar all-white-neighbour isolate fraction",
      isolate_fraction$observed_value, isolate_fraction$null_mean,
      isolate_fraction$empirical_p,
      correction_family = "prespecified primary",
      status = natural_null_status(isolate_fraction$empirical_p),
      source = "stage 04 local-isolate natural null"
    ),
    add(
      "local_population_5km", "exploratory_human_context",
      "focal-minus-white-neighbour 5-km population rank",
      population_5km$observed_focal_minus_white_neighbour,
      population_5km$null_mean,
      population_5km$directional_or_two_sided_p,
      population_5km$maxT_FWER_p,
      "five population scales",
      exploratory_status(
        population_5km$directional_or_two_sided_p,
        population_5km$maxT_FWER_p
      ),
      "stage 05 population-scale summary"
    ),
    add(
      "local_population_did_alignment",
      "exploratory_human_context_sensitivity",
      "focal-minus-white-neighbour population-DID alignment",
      did_alignment$observed_focal_minus_white_neighbour,
      did_alignment$null_mean,
      did_alignment$directional_or_two_sided_p,
      did_alignment$maxT_FWER_p,
      "five DID-context features",
      exploratory_status(
        did_alignment$directional_or_two_sided_p,
        did_alignment$maxT_FWER_p
      ),
      "stage 05 DID contrast summary"
    ),
    add(
      "did_proximate_candidate_fraction",
      "exploratory_human_context_sensitivity",
      "fraction of isolates in DID-proximate high-population context",
      did_urban$observed_candidate_fraction,
      did_urban$null_mean_fraction,
      did_urban$two_sided_p,
      did_urban$maxT_FWER_p,
      "four response-blind context classes",
      exploratory_status(did_urban$two_sided_p, did_urban$maxT_FWER_p),
      "stage 05 DID context composition"
    ),
    add(
      "joint_human_followup_count",
      "candidate_followup",
      "q10 unexpected-pigmentation plus top10 DID-proximity-spike cells",
      nrow(joint_candidate),
      status = "followup_only",
      source = "stage 05 DID candidate details"
    )
  ))
}

final_claim_registry <- function(results) {
  value <- function(id, column) {
    result <- results[[column]][match(id, results$result_id)]
    if (length(result) != 1L) {
      stop("Missing or duplicated final result: ", id, call. = FALSE)
    }
    result
  }

  bombus_presence_beta <- value("local_bombus_presence", "estimate")
  bombus_intensity_beta <- value("local_bombus_intensity", "estimate")
  bombus_presence_q <- value("local_bombus_presence", "corrected_p")
  bombus_intensity_q <- value("local_bombus_intensity", "corrected_p")
  bombus_supported <- all(c(
    bombus_presence_beta > 0,
    bombus_intensity_beta > 0,
    bombus_presence_q < 0.05,
    bombus_intensity_q < 0.05
  ))

  isolate_count <- value("local_isolate_count", "estimate")
  isolate_count_p <- value("local_isolate_count", "raw_p")
  isolate_fraction <- value("local_isolate_fraction", "estimate")
  isolate_fraction_p <- value("local_isolate_fraction", "raw_p")
  isolates_compatible <- isolate_count_p > 0.05 && isolate_fraction_p > 0.05
  isolate_status <- if (isolates_compatible) {
    "not_excessive_under_natural_model"
  } else if (isolate_count_p <= 0.05 && isolate_fraction_p <= 0.05) {
    "count_and_fraction_upper_tail"
  } else if (isolate_count_p <= 0.05) {
    "count_upper_tail_only"
  } else {
    "fraction_upper_tail_only"
  }
  isolate_claim <- if (isolates_compatible) {
    sprintf(
      paste(
        "The %d primary local pigmented isolates are useful follow-up",
        "units; neither count (p=%.3f) nor fraction (%.4f; p=%.3f)",
        "exceeded the natural-map reference."
      ),
      as.integer(round(isolate_count)), isolate_count_p,
      isolate_fraction, isolate_fraction_p
    )
  } else {
    sprintf(
      paste(
        "The reconstruction identified %d primary local pigmented isolates.",
        "The count test gave p=%.3f and the candidate fraction was %.4f",
        "with p=%.3f; the upper-tail result is reported without assigning",
        "horticultural provenance."
      ),
      as.integer(round(isolate_count)), isolate_count_p,
      isolate_fraction, isolate_fraction_p
    )
  }

  population_corrected <- value("local_population_5km", "corrected_p")
  did_corrected <- value("local_population_did_alignment", "corrected_p")
  human_familywise_supported <- any(c(
    population_corrected, did_corrected
  ) <= 0.05)

  data.frame(
    claim_id = c(
      "C1_two_part_phenotype", "C2_national_natural_baseline",
      "C3_national_bombus_gain", "C4_local_bombus_turnover",
      "C5_local_isolates", "C6_local_human_context",
      "C7_horticultural_origin"
    ),
    manuscript_role = c(
      "confirmatory_core", "confirmatory_core", "sensitivity",
      "planned_local_mechanism", "candidate_definition",
      "exploratory_extension", "claim_ceiling"
    ),
    status = c(
      "supported", "supported",
      if (abs(value("national_bombus_auc_gain", "estimate")) < 0.02) {
        "small_and_inconsistent"
      } else {
        "gain_ge_0.02"
      },
      if (bombus_supported) "supported_association" else "not_jointly_supported",
      isolate_status,
      if (human_familywise_supported) {
        "familywise_supported_context_association"
      } else {
        "suggestive_not_familywise_significant"
      },
      "not_demonstrated"
    ),
    claim = c(
      paste(
        "Flower colour is represented hierarchically as pigmentation",
        "presence and pigmented-only optical intensity."
      ),
      paste(
        "Environment plus continuous spatial structure provides the",
        "nationwide natural predictive baseline."
      ),
      sprintf(
        paste(
          "Adding the Bombus fingerprint yields a mean cross-fitted",
          "AUC change of %.4f."
        ),
        value("national_bombus_auc_gain", "estimate")
      ),
      sprintf(
        paste(
          "At the prespecified 25-km pair scale, Bombus fingerprint",
          "turnover had presence beta %.3f (q %.3f) and pigmented-only",
          "intensity beta %.3f (q %.3f)."
        ),
        bombus_presence_beta, bombus_presence_q,
        bombus_intensity_beta, bombus_intensity_q
      ),
      isolate_claim,
      sprintf(
        paste(
          "Local 5-km population and DID alignment had corrected",
          "p-values %.3f and %.3f; these characterize post-selection",
          "human context rather than provenance."
        ),
        population_corrected, did_corrected
      ),
      paste(
        "Images and public landscape layers do not demonstrate planting,",
        "horticultural origin, escape, introgression, or genetic pollution."
      )
    ),
    claim_ceiling = c(
      "measurement hierarchy; not anthocyanin chemistry",
      "predictive baseline; not complete causal partition",
      "habitat-support fingerprint; not abundance or visitation",
      "local association beyond fitted natural-map expectation; not causal selection",
      "candidate definition; not introduction evidence",
      "exploratory local placement tendency; not human-mediated origin",
      "requires field history, specimens, and population genetics"
    ),
    stringsAsFactors = FALSE
  )
}

final_exclusion_registry <- function() {
  data.frame(
    excluded_item = c(
      "legacy monolithic R Markdown workflow",
      "all-flower continuous a-star as one biological response",
      "white-flower a-star as anthocyanin amount",
      "natural-model horticultural tiers as final evidence",
      "random-forest or extreme-tail inference",
      "matched-landscape and early-dark horticultural diagnostics",
      "model residual as a primary response",
      "individual Bombus species coefficients",
      "causal pollinator-selection claim",
      "horticultural origin or introgression claim"
    ),
    reason = c(
      "removed; stage-specific publication modules supersede it",
      "mixes pigment absence with conditional pigment intensity",
      "primarily optical and background variation",
      "superseded by fixed local-isolate and held-out human-context workflow",
      "exploratory development paths not retained in final inference",
      "small or weakly supported and superseded by stage 05 design",
      "avoids second-stage residual bias and unclear estimands",
      "species predictions are correlated habitat support, not independent effects",
      "observational habitat fingerprints cannot establish causal selection",
      "requires provenance and genetic evidence"
    ),
    stringsAsFactors = FALSE
  )
}

final_write_lock <- function(
    root = ".", output_dir = "results/final_analysis_pipeline") {
  dir.create(file.path(root, output_dir), recursive = TRUE,
             showWarnings = FALSE)
  required <- final_required_artifacts()
  required$exists <- file.exists(file.path(root, required$artifact))
  if (!all(required$exists)) {
    stop(
      "Missing required final artifacts: ",
      paste(required$artifact[!required$exists], collapse = ", "),
      call. = FALSE
    )
  }
  results <- final_result_registry(root)
  claims <- final_claim_registry(results)
  exclusions <- final_exclusion_registry()
  checksums <- data.frame(
    artifact = required$artifact,
    md5 = unname(tools::md5sum(file.path(root, required$artifact))),
    stringsAsFactors = FALSE
  )
  output_path <- file.path(root, output_dir)
  utils::write.csv(
    required, file.path(output_path, "final_required_artifacts.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    results, file.path(output_path, "final_result_registry.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    claims, file.path(output_path, "final_claim_registry.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    exclusions, file.path(output_path, "final_exclusion_registry.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    checksums, file.path(output_path, "final_input_checksums.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    hb_publication_stage_registry(),
    file.path(output_path, "publication_stage_registry.csv"),
    row.names = FALSE
  )
  metadata <- data.frame(
    field = c(
      "analysis_spec_version", "generated_at",
      "confirmatory_core", "local_mechanism_test",
      "human_context_role", "residual_as_primary_response",
      "horticultural_origin_claim"
    ),
    value = c(
      final_analysis_spec_version, as.character(Sys.time()),
      paste(
        "stage 01 hierarchical phenotype plus stage 02 national",
        "environment-and-SPDE natural predictive replication"
      ),
      paste(
        "stage 03 response-blind 25-km local pairs with Bombus",
        "community-fingerprint turnover"
      ),
      paste(
        "stage 04 natural isolate definition plus stage 05 population",
        "and DID post-selection characterization; exploratory"
      ),
      "false", "not demonstrated"
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    metadata, file.path(output_path, "final_pipeline_metadata.csv"),
    row.names = FALSE
  )
  writeLines(
    c(
      "# Final locked analysis pipeline",
      "",
      paste(
        "The confirmatory core is stage 01 hierarchical phenotype and",
        "stage 02 national environment-plus-SPDE predictive replication."
      ),
      "",
      paste(
        "The planned local mechanism test is the stage 03 response-blind",
        "25-km pair analysis of flower-colour turnover and predicted",
        "Bombus community-fingerprint turnover."
      ),
      "",
      paste(
        "Stage 04 defines local pigmented isolates under the natural maps.",
        "Stage 05 population and DID analyses characterize human context",
        "after candidate selection and remain exploratory."
      ),
      "",
      paste(
        "Residuals are not primary responses. Individual Bombus species",
        "effects, abundance, visitation, horticultural origin, planting,",
        "escape, and introgression are not inferred."
      ),
      "",
      paste(
        "Stable stage modules live under R/, executable entry points under",
        "scripts/, and independent checks under validation/."
      )
    ),
    file.path(output_path, "README.md"),
    useBytes = TRUE
  )
  invisible(list(
    required = required, results = results, claims = claims,
    exclusions = exclusions, checksums = checksums, metadata = metadata
  ))
}
