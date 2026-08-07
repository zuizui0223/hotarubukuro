from pathlib import Path
import re


def read(path):
    return Path(path).read_text(encoding="utf-8")


def write(path, text):
    Path(path).write_text(text, encoding="utf-8")


def replace_once(path, old, new):
    text = read(path)
    n = text.count(old)
    if n != 1:
        raise RuntimeError(
            f"{path}: expected one literal match, found {n}: {old[:90]!r}"
        )
    write(path, text.replace(old, new, 1))


def regex_once(path, pattern, replacement):
    text = read(path)
    out, n = re.subn(pattern, replacement, text, count=1, flags=re.S)
    if n != 1:
        raise RuntimeError(
            f"{path}: expected one regex match, found {n}: {pattern[:90]!r}"
        )
    write(path, out)


manuscript = "manuscript/ecology-and-evolution-manuscript.md"
replace_once(
    manuscript,
    "A five-species predicted *Bombus* fingerprint added essentially no national discrimination (mean ΔAUC=-0.0016). We therefore tested a directional local hypothesis instead of treating the SDMs as a national causal predictor:",
    "We then tested a directional local hypothesis without treating the SDMs as a national causal predictor:",
)
replace_once(
    manuscript,
    "\nThe national *Bombus* analysis was retained only as a sensitivity. A three-axis fingerprint of predicted total support and composition was added to the national natural model on common support, and the held-out AUC change was calculated. A near-zero gain would indicate that predicted bumblebee geography adds little broad-scale discrimination beyond environment and space.\n",
    "\n",
)
replace_once(
    manuscript,
    "### 3.3 | National *Bombus* information was weak, but local limitation was directionally associated with pigmentation state",
    "### 3.3 | Local *Bombus* limitation was directionally associated with pigmentation state",
)
replace_once(
    manuscript,
    "\nAdding the predicted *Bombus* fingerprint to the national environment-plus-SPDE reference changed held-out AUC by only -0.0016 on average. Thus the predicted bumblebee surfaces did not improve broad-scale pigmentation discrimination after the national natural geography was represented.\n",
    "\n",
)
replace_once(
    manuscript,
    "### 4.3 | Broad environment and space remain the dominant national description\n\nThe near-zero national ΔAUC from adding the predicted *Bombus* fingerprint shows that bumblebee SDMs do not provide substantial independent broad-scale predictive information beyond the national environment-plus-SPDE model. This is unsurprising because the bee distributions and the plant phenotype share large climatic and geographical gradients. The SPDE field also retains omitted environment, demographic history, population structure and sampling geography.",
    "### 4.3 | The *Bombus* hypothesis is intentionally local\n\nWe do not estimate an independent national *Bombus* effect. The bumblebee SDMs are themselves derived from environmental geography and share broad climatic and spatial structure with the plant phenotype, so a national environment-plus-space-plus-*Bombus* regression would be difficult to interpret mechanistically. The national models therefore serve only as the natural flower-colour reference; the pollinator hypothesis begins at the local matched comparison.",
)
replace_once(
    manuscript,
    "Predicted *Bombus* geography adds essentially no national discrimination. Locally, however, environmentally matched cells where all five focal bumblebees have low predicted availability tend to have less pigmentation than nearby cells where at least one focal bumblebee is moderately supported.",
    "The *Bombus* hypothesis is evaluated only locally: environmentally matched cells where all five focal bumblebees have low predicted availability tend to have less pigmentation than nearby cells where at least one focal bumblebee is moderately supported.",
)
replace_once(
    manuscript,
    "| National *Bombus* increment | ΔAUC=-0.0016 | same-support blocked comparison | essentially no additional national information |\n",
    "",
)
replace_once(
    manuscript,
    "**Figure 3. National *Bombus* sensitivity and the local bumblebee-limitation hypothesis.** (a) Foldwise held-out change in national presence AUC after adding the predicted *Bombus* fingerprint. (b) Pigmented share in the 22 one-to-one lower-third limitation pairs, oriented from *Bombus*-limited to *Bombus*-available before flower colour was read. (c) Observed available-minus-limited pigmentation contrasts across the retained low-support gate grid; open points show natural-map means. (d) The lower-third observed contrast relative to 1,000 cross-fitted natural maps. The 0.33 gate was adopted after exploratory design development; the full threshold grid and across-grid multiplicity are retained. Predicted *Bombus* support is an availability proxy, not abundance or visitation.",
    "**Figure 3. Local bumblebee-limitation hypothesis.** (a) Pigmented share in the 22 one-to-one lower-third limitation pairs, oriented from *Bombus*-limited to *Bombus*-available before flower colour was read. (b) Observed available-minus-limited pigmentation contrasts across the retained low-support gate grid; open points show natural-map means. (c) The lower-third observed contrast relative to 1,000 cross-fitted natural maps. The 0.33 gate was adopted after exploratory design development; the full threshold grid and across-grid multiplicity are retained. Predicted *Bombus* support is an availability proxy, not abundance or visitation.",
)
text = read(manuscript)
forbidden = ["ΔAUC", "National *Bombus* increment", "National *Bombus* sensitivity"]
if any(item in text for item in forbidden):
    raise RuntimeError("manuscript still contains a national Bombus manuscript claim")


figure_script = r'''# Figure 3: local Bombus limitation gate only.
source("R/pipeline_support.R")
hb_require_packages(c("ggplot2", "patchwork"))
output_dir <- file.path("manuscript", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

gate <- hb_read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_summary.csv"
)
pairs <- hb_read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_pairs.csv"
)
null <- hb_read_csv(
  "results/ecological_v17_bombus_limitation_gate/bombus_limitation_gate_null.csv"
)
cells <- hb_read_csv(
  "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"
)

primary_pairs <- pairs[pairs$is_primary_gate, , drop = FALSE]
share <- cells$n_pigmented / pmax(cells$n_observations, 1)
paired <- data.frame(
  pair = seq_len(nrow(primary_pairs)),
  limited = share[primary_pairs$low_i],
  available = share[primary_pairs$high_i]
)
paired_long <- rbind(
  data.frame(pair = paired$pair, state = "Bombus-limited", share = paired$limited),
  data.frame(pair = paired$pair, state = "Bombus-available", share = paired$available)
)
paired_long$state <- factor(
  paired_long$state, levels = c("Bombus-limited", "Bombus-available")
)

p1 <- ggplot2::ggplot(
  paired_long, ggplot2::aes(x = state, y = share, group = pair)
) +
  ggplot2::geom_line(alpha = 0.35) +
  ggplot2::geom_point(size = 1.7) +
  ggplot2::stat_summary(
    ggplot2::aes(group = 1), fun = mean, geom = "line", linewidth = 1.1
  ) +
  ggplot2::stat_summary(fun = mean, geom = "point", size = 3) +
  ggplot2::labs(
    title = "Lower-third Bombus gate",
    subtitle = sprintf(
      "%d one-to-one pairs within 25 km; environmentally matched",
      nrow(primary_pairs)
    ),
    x = NULL, y = "Pigmented share"
  ) +
  ggplot2::theme_minimal(base_size = 9)

presence <- gate[gate$response == "pigmentation_share", , drop = FALSE]
presence <- presence[order(presence$low_threshold), , drop = FALSE]
presence$threshold_label <- paste0("≤", presence$low_threshold)
p2 <- ggplot2::ggplot(
  presence,
  ggplot2::aes(
    x = factor(threshold_label, levels = threshold_label),
    y = observed_directed_difference
  )
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_point(ggplot2::aes(y = natural_null_mean), shape = 1, size = 2.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("p=", sprintf("%.3f", upper_tail_p))),
    nudge_y = 0.035, size = 2.7
  ) +
  ggplot2::labs(
    title = "Retained limitation-gate sensitivity",
    subtitle = "Filled = observed; open = natural-map mean",
    x = "All-species low-support threshold",
    y = "Available − limited pigmentation"
  ) +
  ggplot2::theme_minimal(base_size = 9)

primary_null <- null[
  abs(null$low_threshold - 0.33) < 1e-12 &
    null$response == "pigmentation_share", , drop = FALSE
]
primary <- gate[
  gate$is_primary_gate & gate$response == "pigmentation_share", , drop = FALSE
]
p3 <- ggplot2::ggplot(primary_null, ggplot2::aes(x = statistic)) +
  ggplot2::geom_histogram(bins = 35, boundary = 0) +
  ggplot2::geom_vline(
    xintercept = primary$observed_directed_difference,
    linewidth = 0.8, linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Observed contrast against natural maps",
    subtitle = sprintf(
      "Observed %.3f; p %.3f; grid q %.3f",
      primary$observed_directed_difference,
      primary$upper_tail_p,
      primary$BH_q_all_gate_tests
    ),
    x = "Available − limited pigmentation contrast",
    y = "Predictive maps"
  ) +
  ggplot2::theme_minimal(base_size = 9)

figure_3 <- (p1 + p2) / p3 +
  patchwork::plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(heights = c(1, 1.05))

for (ext in c("png", "pdf")) {
  path <- file.path(output_dir, paste0("figure_3_bombus_limitation.", ext))
  if (ext == "png") {
    ggplot2::ggsave(
      path, figure_3, width = 7.2, height = 6.5, units = "in", dpi = 600
    )
  } else {
    ggplot2::ggsave(
      path, figure_3, width = 7.2, height = 6.5, units = "in",
      device = grDevices::cairo_pdf
    )
  }
}
'''
write("scripts/internal/build_bombus_limitation_figure.R", figure_script)


wrapper = "scripts/build_publication_figures.R"
text = read(wrapper)
marker = "parsed <- parse(text = text, keep.source = FALSE)\n"
if marker not in text:
    raise RuntimeError("figure wrapper parse marker missing")
insertion = r'''# The retained core still contains the archived national/turnover Figure 3.
# Remove that block before evaluation; the active Figure 3 is local-only.
text <- sub(
  "(?s)# Figure 3: incremental national information and local turnover tests\\..*?# Figure 4: local isolates and human-context follow-up\\.",
  "# Figure 4: local isolates and human-context follow-up.",
  text, perl = TRUE
)
text <- gsub(
  "figure_3_bombus_turnover", "figure_3_bombus_limitation",
  text, fixed = TRUE
)

'''
text = text.replace(marker, insertion + marker, 1)
text = text.replace(
    "# Replace the manuscript-facing Figure 3 with the directional limitation gate.",
    "# Build the manuscript-facing Figure 3 from the local limitation gate only.",
    1,
)
write(wrapper, text)


pipeline = "scripts/run_publication_pipeline.R"
regex_once(
    pipeline,
    r'''\n# The former unsigned turnover test is no longer an active claim\. It is rerun as\n# a sensitivity so historical comparisons remain available and the retained\n# plotting core can be reproduced; final registries do not read its result\.\nif \(mode %in% c\("extensions", "full"\)\) \{\n  run_stage\("03S_run_unsigned_turnover_sensitivity", "scripts/run_local_bombus_turnover\.R", role = "superseded_turnover_sensitivity"\)\n\}\n''',
    "\n",
)


registry = "R/final_registry.R"
replace_once(
    registry,
    'final_analysis_spec_version <- "publication_pipeline_1.1_limitation_gate"',
    'final_analysis_spec_version <- "publication_pipeline_1.2_local_bombus_only"',
)
replace_once(
    registry,
    '      "phenotype", "national_natural_model", "national_natural_model",\n      "local_bombus_limitation", "local_bombus_limitation",',
    '      "phenotype", "national_natural_model",\n      "local_bombus_limitation", "local_bombus_limitation",',
)
replace_once(
    registry,
    '      "results/ecological_v16_predictive_replication/predictive_replication_model_performance.csv",\n      "results/ecological_v16_predictive_replication/predictive_replication_bombus_paired_contrast.csv",',
    '      "results/ecological_v16_predictive_replication/predictive_replication_model_performance.csv",',
)
replace_once(
    registry,
    '      "confirmatory_natural_baseline",\n      "national_bombus_sensitivity",\n      "local_pollinator_limitation_test",',
    '      "confirmatory_natural_baseline",\n      "local_pollinator_limitation_test",',
)
regex_once(
    registry,
    r'''\n  bombus <- final_read_csv\(\n    root, "results/ecological_v16_predictive_replication/predictive_replication_bombus_paired_contrast\.csv"\n  \)''',
    "",
)
regex_once(
    registry,
    r'''\n    add\(\n      "national_bombus_auc_gain", "sensitivity",\n      "mean fold AUC gain after adding the predicted Bombus fingerprint",\n      mean\(bombus\$AUC_improvement\),\n      status = if \(abs\(mean\(bombus\$AUC_improvement\)\) < 0\.02\) \{\n        "small_predictive_gain"\n      \} else "predictive_gain_ge_0\.02",\n      source = "stage 02 national Bombus sensitivity"\n    \),''',
    "",
)
claim_block = '''  data.frame(
    claim_id = c(
      "C1_two_part_phenotype", "C2_national_natural_baseline",
      "C4_local_bombus_limitation", "C5_local_isolates",
      "C6_local_human_context", "C7_horticultural_origin"
    ),
    manuscript_role = c(
      "confirmatory_core", "confirmatory_core",
      "local_mechanistic_sensitivity", "candidate_definition",
      "exploratory_extension", "claim_ceiling"
    ),
    status = c(
      "supported", "supported", gate_status, isolate_status,
      if (human_familywise_supported) {
        "familywise_supported_context_association"
      } else "suggestive_not_familywise_significant",
      "not_demonstrated"
    ),
    claim = c(
      paste(
        "Flower colour is represented hierarchically as pigmentation presence",
        "and pigmented-only optical intensity."
      ),
      paste(
        "Environment plus continuous spatial structure provides the nationwide",
        "natural predictive baseline."
      ),
      sprintf(
        paste(
          "In the lower-third Bombus-limitation gate, environmentally matched",
          "Bombus-available cells had pigmentation share %.3f higher than",
          "Bombus-limited cells (upper-tail p=%.3f; across-grid BH q=%.3f).",
          "Pigmented-only intensity differed by %.3f (p=%.3f)."
        ),
        gate_presence, gate_presence_p, gate_presence_q,
        gate_intensity, gate_intensity_p
      ),
      isolate_claim,
      sprintf(
        paste(
          "Local 5-km population and DID alignment had corrected p-values",
          "%.3f and %.3f; these characterize post-selection human context",
          "rather than provenance."
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
      paste(
        "predicted Bombus-availability contrast after local environmental matching;",
        "lower-third gate adopted after exploratory design development; not",
        "abundance, visitation, pigment-cost measurement, or causal selection"
      ),
      "candidate definition; not introduction evidence",
      "exploratory local placement tendency; not human-mediated origin",
      "requires field history, specimens, and population genetics"
    ),
    stringsAsFactors = FALSE
  )
}

final_exclusion_registry'''
regex_once(
    registry,
    r'''  data\.frame\(\n    claim_id = c\(.*?\n    stringsAsFactors = FALSE\n  \)\n\}\n\nfinal_exclusion_registry''',
    claim_block,
)
replace_once(
    registry,
    '      "model residual as a primary response",\n      "unsigned Bombus community-turnover as the active mechanism test",',
    '      "model residual as a primary response",\n      "national Bombus fingerprint as a manuscript-facing analysis",\n      "unsigned Bombus community-turnover as the active mechanism test",',
)
replace_once(
    registry,
    '      "avoids second-stage residual bias and unclear estimands",\n      paste(\n        "superseded as the active biological test by the directional Bombus",',
    '      "avoids second-stage residual bias and unclear estimands",\n      "removed from active inference; the pollinator hypothesis is tested only in local matched contrasts",\n      paste(\n        "superseded as the active biological test by the directional Bombus",',
)
text = read(registry)
if "national_bombus_auc_gain" in text or "C3_national_bombus_gain" in text:
    raise RuntimeError("final registry still exposes national Bombus inference")


validator = "validation/validate_publication_pipeline.R"
regex_once(
    validator,
    r'''national_bombus <- results\[results\$result_id == "national_bombus_auc_gain", , drop = FALSE\]\nadd_check\(\n  "national_and_local_bombus_not_conflated",\n  nrow\(national_bombus\) == 1L && is\.finite\(national_bombus\$estimate\) &&\n    grepl\("limitation", metadata_value\[\["local_mechanism_test"\]\], ignore\.case = TRUE\),\n  paste\("national mean AUC gain=", round\(national_bombus\$estimate, 4\)\)\n\)''',
    '''add_check(
  "national_bombus_not_active",
  !any(results$result_id == "national_bombus_auc_gain") &&
    !any(claims$claim_id == "C3_national_bombus_gain") &&
    grepl("limitation", metadata_value[["local_mechanism_test"]], ignore.case = TRUE),
  "Bombus inference is restricted to the stage-03 local limitation contrast."
)''',
)
replace_once(
    validator,
    '          "turnover retained only as sensitivity")',
    '          "turnover excluded from the active pipeline")',
)


audit = "validation/audit_publication_claims.R"
replace_once(audit, 'national_bombus <- get_result("national_bombus_auc_gain")\n', "")
regex_once(
    audit,
    r'''add_claim\(\n  "National Bombus gain remains small",\n  abs\(national_bombus\$estimate\) < 0\.02,\n  sprintf\("mean AUC change=%.4f", national_bombus\$estimate\)\n\)''',
    '''add_check(
  "National Bombus increment is not an active claim",
  !any(results$result_id == "national_bombus_auc_gain") &&
    !any(claims$claim_id == "C3_national_bombus_gain"),
  "Bombus is tested only in the local limitation stage"
)''',
)
replace_once(
    audit,
    '  "turnover retained only as documented sensitivity"',
    '  "turnover is outside the active pipeline"',
)


readme = "README.md"
replace_once(
    readme,
    "### Bombus inference ceiling\n\nStage 03 is directional and biologically motivated.",
    "### Bombus inference ceiling\n\nThe active paper contains **no national Bombus increment analysis**. Bombus enters only at stage 03, where the hypothesis is local, directional and biologically motivated.",
)
replace_once(
    readme,
    "The older unsigned flower-colour/*Bombus*-community turnover analysis is retained only as a sensitivity and is not read by the final result or claim registries.",
    "The older unsigned flower-colour/*Bombus*-community turnover analysis is not run by the active pipeline and remains only as historical method-development code.",
)


dag = "docs/pipeline-dag.md"
replace_once(
    dag,
    "The previous unsigned *Bombus*-community turnover analysis is rerun only as a documented sensitivity. It is not read by the final result or claim registries.",
    "The previous unsigned *Bombus*-community turnover analysis is outside the active pipeline. The active paper tests *Bombus* only through the local limitation gate.",
)


figure_map = "manuscript/figure-map.md"
replace_once(
    figure_map,
    "| 3 | `figure_3_bombus_limitation` | near-zero national Bombus increment plus the local environmentally matched Bombus-limitation gate, threshold-grid sensitivity and natural-map reference |",
    "| 3 | `figure_3_bombus_limitation` | local environmentally matched Bombus-limitation gate, threshold-grid sensitivity and natural-map reference |",
)
replace_once(
    figure_map,
    "Figure 3 is the manuscript-facing Bombus figure. The older unsigned-turnover figure may still be generated as a sensitivity artifact, but it is not cited as an active main-text figure and is not read by the final result or claim registries.",
    "Figure 3 is entirely local. No national Bombus increment or unsigned-turnover panel is generated for the active manuscript.",
)


manuscript_readme = "manuscript/README.md"
replace_once(
    manuscript_readme,
    "Stage 03 does not call SDM suitability abundance, visitation or selection pressure.",
    "Bombus is used only in the local stage-03 limitation test; there is no national Bombus increment in the active manuscript. Stage 03 does not call SDM suitability abundance, visitation or selection pressure.",
)
replace_once(
    manuscript_readme,
    "the old unsigned-turnover figure may be generated as a sensitivity artifact but is not the active Figure 3.",
    "the old unsigned-turnover figure is not generated by the active publication pipeline.",
)


si = "manuscript/supporting-information-plan.md"
replace_once(
    si,
    "- the superseded unsigned 10-, 25- and 50-km *Bombus*-community turnover analysis as a sensitivity rather than the active mechanism claim;\n",
    "",
)
replace_once(
    si,
    "- directional all-species opportunity and environment-residualized community analyses as exploratory design-development results, clearly separated from the active stage;\n",
    "",
)


sdm_doc = "docs/bombus-sdm-inference.md"
regex_once(
    sdm_doc,
    r'''\n## Useful secondary analyses\n\nThe previously developed analyses remain useful as sensitivities but are not the active causal story:.*?\n## Stronger than any SDM-only test: field validation''',
    "\n## Stronger than any SDM-only test: field validation",
)


numeric_ref = "reproducibility/submission_reference/manuscript_numeric_reference.csv"
text = read(numeric_ref)
lines = [line for line in text.splitlines() if "national_bombus_auc_gain" not in line]
write(numeric_ref, "\n".join(lines) + "\n")

claim_ref = "reproducibility/submission_reference/submission_claim_registry.csv"
text = read(claim_ref)
lines = [line for line in text.splitlines() if "C3_national_bombus_gain" not in line]
write(claim_ref, "\n".join(lines) + "\n")

ref_readme = "reproducibility/submission_reference/README.md"
replace_once(
    ref_readme,
    "Conditional intensity differed by -0.6145 (p=0.9051), providing no support for darker flowers under greater predicted Bombus availability. National Bombus addition remained negligible (mean held-out AUC change -0.00187).",
    "Conditional intensity differed by -0.6145 (p=0.9051), providing no support for darker flowers under greater predicted Bombus availability. No national Bombus increment is part of the active manuscript or claim registry.",
)


obsolete = [
    ".github/workflows/bombus-directional-opportunity.yml",
    ".github/workflows/bombus-interaction-sensitivity.yml",
    "scripts/run_bombus_directional_opportunity.R",
    "scripts/run_bombus_interaction_sensitivity.R",
    "docs/bombus-directional-opportunity-results.md",
    "docs/bombus-interaction-sensitivity-results.md",
]
for item in obsolete:
    p = Path(item)
    if p.exists():
        p.unlink()

manifest = "config/code_manifest.csv"
text = read(manifest)
lines = [line for line in text.splitlines() if not any(item in line for item in obsolete)]
write(manifest, "\n".join(lines) + "\n")


for path in [
    manuscript,
    "manuscript/figure-map.md",
    "manuscript/README.md",
    "reproducibility/submission_reference/submission_claim_registry.csv",
]:
    txt = read(path)
    if "national_bombus_auc_gain" in txt or "C3_national_bombus_gain" in txt:
        raise RuntimeError(f"{path}: stale national Bombus registry token")

# Remove the one-off patch machinery from the resulting source commit.
for temporary in [
    ".github/workflows/local-bombus-only-patch.yml",
    "scripts/internal/apply_local_bombus_only_patch.py",
]:
    p = Path(temporary)
    if p.exists():
        p.unlink()

print("Local-only Bombus cleanup applied.")
