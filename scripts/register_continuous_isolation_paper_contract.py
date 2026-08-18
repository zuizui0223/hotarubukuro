#!/usr/bin/env python3
"""Synchronise the current JBI paper contract with continuous colour isolation.

The script is idempotent. It updates the canonical lock, paper overview, active
file map, submission validator/checklist/captions, Figure 4 renderer hook and
figure workflow/validator without changing any scientific estimate.
"""

from __future__ import annotations

import csv
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ARTIFACT_ID = 9317087893
ARTIFACT_SHA256 = "6fd26d9a938b68d3f0c56512cd1620597c740d44ba91ab5a7ccbb9daa99d5386"
RESULT_ROOT = "results/continuous_colour_isolation_human_context"
RESULT_FILES = [
    "RESULT_SUMMARY.md",
    "continuous_isolation_cell_metrics.csv",
    "continuous_isolation_colour_contrasts.csv",
    "continuous_isolation_correlations.csv",
    "continuous_isolation_count_conditioning.csv",
    "continuous_isolation_feature_registry.csv",
    "continuous_isolation_fold_label_null.csv",
    "continuous_isolation_fold_label_summary.csv",
    "continuous_isolation_fold_specific.csv",
    "continuous_isolation_leave_one_fold_out.csv",
    "continuous_isolation_natural_null_draws.csv",
    "continuous_isolation_natural_null_summary.csv",
    "continuous_isolation_output_sha256.txt",
    "continuous_isolation_population_maxT.csv",
    "continuous_isolation_population_profile.csv",
    "continuous_isolation_population_profile.pdf",
    "continuous_isolation_population_profile.png",
    "continuous_isolation_top10_summary.csv",
    "continuous_isolation_validation.json",
]
REQUIRED_OUTPUTS = [f"{RESULT_ROOT}/{name}" for name in RESULT_FILES]


def words(text: str) -> list[str]:
    return re.findall(r"\b[\wÀ-ž*'’-]+\b", text)


def write_if_changed(path: Path, text: str) -> None:
    current = path.read_text(encoding="utf-8")
    if current != text:
        path.write_text(text, encoding="utf-8")


def replace_once(text: str, old: str, new: str, label: str) -> str:
    count = text.count(old)
    if count != 1:
        raise RuntimeError(f"Expected one {label}; found {count}")
    return text.replace(old, new, 1)


def replace_regex(text: str, pattern: str, replacement: str, label: str) -> str:
    updated, count = re.subn(pattern, replacement, text, count=1, flags=re.MULTILINE | re.DOTALL)
    if count != 1:
        raise RuntimeError(f"Expected one {label}; found {count}")
    return updated


def update_lock() -> None:
    path = ROOT / "config/paper_pipeline.lock.json"
    lock = json.loads(path.read_text(encoding="utf-8"))
    lock["paper_version"] = "jbi-2026-08-18-continuous-isolation-integrated"
    lock["provenance_boundary"]["interpretation"] = (
        "The reproduce profile regenerates the accepted Broad spatial-null sensitivity, "
        "local Bombus and event-based human-context analyses, restores the validated "
        "continuous colour-isolation result, and rebuilds figures and the JBI review bundle "
        "from checksum-locked inputs. The all-cell isolation result is explicitly post hoc; "
        "its density and 10,000-map guardrails are frozen and checksum locked."
    )

    lock["artifacts"]["accepted_continuous_colour_isolation"] = {
        "id": ARTIFACT_ID,
        "sha256": ARTIFACT_SHA256,
        "destination": RESULT_ROOT,
        "merge": False,
        "required": REQUIRED_OUTPUTS,
        "role": (
            "validated post hoc all-cell colour-isolation human-context output; the dedicated "
            "workflow performs full recomputation and the paper pipeline restores this "
            "checksum-locked accepted result"
        ),
        "references": [
            ".github/workflows/continuous-colour-isolation-human-context.yml",
            "reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md",
            "reproducibility/continuous_colour_isolation_manuscript_integration_2026-08-18.md",
        ],
    }

    lock["stages"]["restore_continuous_isolation"] = {
        "kind": "artifact",
        "artifact": "accepted_continuous_colour_isolation",
        "outputs": REQUIRED_OUTPUTS,
    }
    reproduce = list(lock["profiles"]["reproduce"])
    reproduce = [name for name in reproduce if name != "restore_continuous_isolation"]
    insert_at = reproduce.index("run_human_context") + 1
    reproduce.insert(insert_at, "restore_continuous_isolation")
    lock["profiles"]["reproduce"] = reproduce

    render_outputs = lock["stages"]["render_figures"]["outputs"]
    lock["stages"]["render_figures"]["outputs"] = [
        output.replace(
            "Figure_4_calibrated_local_departures.pdf",
            "Figure_4_continuous_colour_isolation_human_context.pdf",
        )
        for output in render_outputs
    ]

    alignment = lock["alignment"]
    for required in [
        "R/continuous_colour_isolation.R",
        "analysis_sensitivity/run_continuous_colour_isolation_human_context.R",
        "analysis_sensitivity/continuous_colour_isolation/00_fast_overrides.R",
        "analysis_sensitivity/continuous_colour_isolation/01_observed_geometry.R",
        "analysis_sensitivity/continuous_colour_isolation/02_natural_map_guardrail.R",
        "analysis_sensitivity/continuous_colour_isolation/03_reporting.R",
        ".github/workflows/continuous-colour-isolation-human-context.yml",
        "scripts/render_jbi_figure4_continuous_isolation.R",
        "reproducibility/continuous_colour_isolation_human_context_2026-08-18.md",
        "reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md",
        "reproducibility/continuous_colour_isolation_manuscript_integration_2026-08-18.md",
    ]:
        if required not in alignment["required_files"]:
            alignment["required_files"].append(required)

    remove_labels = {
        "calibrated departures and human follow-up",
        "paper overview mirrors all current results",
    }
    checks = [check for check in alignment["checks"] if check["label"] not in remove_labels]
    checks.extend(
        [
            {
                "label": "continuous colour-isolation human context",
                "path": "submission/jbi/JBI_main_manuscript_anonymized.md",
                "patterns": [
                    "674 pigmented",
                    "3\\.61",
                    "rho=0\\.252",
                    "mean of \\+0\\.133",
                    "P=0\\.0002",
                    "rho=\\+0\\.285",
                    "mean of \\+0\\.154",
                    "P=0\\.0009",
                    "does not establish horticultural origin",
                ],
            },
            {
                "label": "supplementary event calibration and field targets",
                "path": "submission/jbi/supporting/Appendix_S6_event_departures_human_context.md",
                "patterns": [
                    "\\*\\*16\\*\\*",
                    "0\\.27897",
                    "0\\.12609",
                    "0\\.06744",
                    "0\\.05479",
                    "field/provenance targets",
                ],
            },
            {
                "label": "paper overview mirrors continuous and event roles",
                "path": "paper/README.md",
                "patterns": [
                    "1,922",
                    "0\\.106802",
                    "0\\.03393",
                    "67",
                    "0\\.03590",
                    "674",
                    "0\\.251980",
                    "0\\.285498",
                    "0\\.000900",
                    "16",
                    "0\\.05479",
                ],
            },
        ]
    )
    alignment["checks"] = checks
    path.write_text(json.dumps(lock, ensure_ascii=False, separators=(",", ":")) + "\n", encoding="utf-8")


def update_paper_overview() -> None:
    readme = """# Current paper

This directory maps the single active JBI paper and its reproducible evidence chain. The current argument is not a list of predictors. It connects distinct processes at the scale and comparison unit where each is biologically defensible.

## Current scientific sequence

1. **Trait construction:** 1,922 author-screened YAMAP observations are converted into pigmentation state and pigmented-only intensity.
2. **Broad natural geography:** response-specific INLA-SPDE models separate measured environment from coherent residual geography. In the cross-fitted space-only sensitivity, pigmentation-state divergence across environmental difference is 0.106802 versus a spatial-null median of 0.058240, an excess of 0.048562 (upper P=0.03393); conditional intensity shows no excess.
3. **Local Bombus boundary test:** 67 independently fixed white-pigmented boundaries show a mean focal-Bombus contrast of +0.03590 at 5 km. Its heterogeneity and attenuation define a local state-maintenance hypothesis; equal-elevation highland analyses prevent broad overlap from being misread as an independent pollinator mechanism.
4. **Continuous human-context geometry:** all 1,305 cells are used. Among 674 pigmented cells, median nearest-pigmented distance is 3.605551 km. Pigmented isolation correlates with 5-km population exposure (rho=0.251980) more strongly than 10,000 natural maps expect (mean 0.132980; upper P=0.000200). After scaling by local flower-cell spacing, rho=0.285498 versus natural mean 0.153616 (P=0.000900). The raw white negative sign does not survive density correction, so the guarded conclusion is a pigmented-specific human-context overlay, not reciprocal colour displacement.
5. **Event calibration and field targets:** the earlier 16 pigmented-among-white events are not excessive under natural maps (count P=0.27897; fraction P=0.12609). Their 5-km local population contrast is +0.06744 (directional P=0.00800; global maxT P=0.05479). They remain reproducible field/provenance targets, not the foundation of the main human-origin claim.

## Interpretation ceiling

The continuous result does not establish horticultural origin, planting, escape, establishment, phenotypic plasticity, pollen movement, gene flow or human causation. It identifies where human context overlays the spatial geometry of pigmentation and sharpens the populations and measurements needed for field history, voucher and genomic tests.

## Current entry points

- Manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- Supporting human-context details: `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`
- Analysis map: `paper/analysis-map.md`
- Canonical audit: `python run_pipeline.py audit`
- Canonical reproduction: `python run_pipeline.py reproduce`
- Continuous analysis entry: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Validated result lock: `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`

The continuous analysis is explicitly labelled post hoc. Its sampling-density correction and 10,000-map natural guardrails are fixed and checksum locked.
"""
    write_if_changed(ROOT / "paper/README.md", readme)

    analysis_map = """# Current analysis map

The active paper follows one dependent inference chain. Previous stages create the comparison unit required by later stages; integration connects scales rather than collapsing them into one national regression.

| Layer | Active question | Comparison unit | Current role | Main evidence |
|---|---|---|---|---|
| Trait construction | Can an incidental hiking-photo stream recover quantitative flower-colour geography? | observation | Main | 1,922 screened observations; state and intensity separated |
| Broad environment + space | Which measured environmental associations and residual spatial scales structure each response? | observation / 1-km cell | Main | response-specific INLA-SPDE models; state-only environment-aligned excess over cross-fitted space-only expectation |
| Local focal Bombus | Does habitat opportunity rise from white to pigmented across independently fixed local boundaries? | 67 non-overlapping pairs | Main | mean +0.03590 at 5 km; heterogeneity and scale attenuation; highland overlap rejected as an independent mechanism after elevation matching |
| Continuous human context | Does isolation from the same colour covary with human exposure beyond natural geography and local flower-cell spacing? | all 1,305 cells; 674 pigmented | Main exploratory geometry | raw pigmented rho=0.251980, natural mean=0.132980, P=0.000200; relative rho=0.285498, natural mean=0.153616, P=0.000900 |
| Event calibration | Are restrictive pigmented-among-white configurations excessive, and which populations should be revisited? | 16 event-defined cells | Supporting / field targeting | count P=0.27897; fraction P=0.12609; local population contrast +0.06744, global maxT P=0.05479 |

## Continuous-isolation implementation

- Core geometry: `R/continuous_colour_isolation.R`
- One-shot entry: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Observed geometry: `analysis_sensitivity/continuous_colour_isolation/01_observed_geometry.R`
- Natural-map replay: `analysis_sensitivity/continuous_colour_isolation/02_natural_map_guardrail.R`
- Reporting: `analysis_sensitivity/continuous_colour_isolation/03_reporting.R`
- Dedicated workflow: `.github/workflows/continuous-colour-isolation-human-context.yml`
- Design/status note: `reproducibility/continuous_colour_isolation_human_context_2026-08-18.md`
- Validated result: `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`

The analysis is post hoc because its motivating raw correlations were inspected before specification. It is not relabelled as preregistered. The density correction, fold diagnostics and 10,000-map guardrail are nevertheless frozen and independently validated.

## Event-family role

The retained event scripts and v20-v22 results remain active because they answer different questions: frequency calibration, local candidate-versus-white contrast and reproducible selection of provenance targets. They no longer define the primary human-context estimand.

## Canonical execution

`python run_pipeline.py audit` validates manuscript, figure, file-map and claim contracts. `python run_pipeline.py reproduce` restores checksum-locked evidence, regenerates the accepted Broad and local analyses, restores the validated continuous-isolation output, renders the four Main figures and builds the JBI review package.
"""
    write_if_changed(ROOT / "paper/analysis-map.md", analysis_map)


def update_active_map() -> None:
    path = ROOT / "paper/active-file-map.csv"
    with path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    new_rows = [
        ("analysis", "Main3", "R/continuous_colour_isolation.R", "component"),
        ("analysis", "Main3", "analysis_sensitivity/run_continuous_colour_isolation_human_context.R", "component"),
        ("analysis", "Main3", "analysis_sensitivity/continuous_colour_isolation/00_fast_overrides.R", "component"),
        ("analysis", "Main3", "analysis_sensitivity/continuous_colour_isolation/01_observed_geometry.R", "component"),
        ("analysis", "Main3", "analysis_sensitivity/continuous_colour_isolation/02_natural_map_guardrail.R", "component"),
        ("analysis", "Main3", "analysis_sensitivity/continuous_colour_isolation/03_reporting.R", "component"),
        ("analysis", "figures", "scripts/render_jbi_figure4_continuous_isolation.R", "component"),
        ("workflow", "Main3", ".github/workflows/continuous-colour-isolation-human-context.yml", "component"),
        ("reproducibility", "Main3", "reproducibility/continuous_colour_isolation_human_context_2026-08-18.md", "provenance"),
        ("reproducibility", "Main3", "reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md", "provenance"),
        ("reproducibility", "integration", "reproducibility/continuous_colour_isolation_manuscript_integration_2026-08-18.md", "provenance"),
        ("testing", "R", "tests/testthat/test-continuous-colour-isolation.R", "component"),
        ("infrastructure", "contract", "scripts/register_continuous_isolation_paper_contract.py", "component"),
    ]
    existing = {row["path"] for row in rows}
    for role, section, item, status in new_rows:
        if item not in existing:
            rows.append({"role": role, "section": section, "path": item, "status": status})
            existing.add(item)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=["role", "section", "path", "status"])
        writer.writeheader()
        writer.writerows(rows)


def update_submission_text_contracts() -> None:
    captions_path = ROOT / "submission/jbi/JBI_main_figure_captions.md"
    captions = captions_path.read_text(encoding="utf-8")
    captions = replace_regex(
        captions,
        r"## Figure 4\..*\Z",
        """## Figure 4. Pigmented isolation carries a human-context overlay beyond natural geography

Spatial geometry and human context of *Campanula punctata* across Japan. (a) All 1,305 flower-colour cells; grey points show white cells, while 674 pigmented cells are sized by distance to the nearest other pigmented cell and filled by population-exposure rank within 5 km. The map uses WGS84 longitude/latitude and includes a 100-km bar scale. Median nearest-pigmented distance was 3.61 km. (b) Population exposure versus density-corrected same-colour isolation, displayed in within-colour rank space. Pigmented rho was 0.285, whereas white rho was 0.079; local flower-cell spacing has therefore been removed before the comparison. (c) Observed 5-km correlations (filled points) against means and 95% intervals from 10,000 final-eight-axis natural maps (open points and bars). The pigmented relationship exceeded natural expectation for raw isolation (rho=0.252; natural mean 0.133; P=0.0002) and relative isolation (rho=0.285; natural mean 0.154; P=0.0009). The raw pigmented-minus-white difference was elevated, but its relative-isolation form was not, so a robust reciprocal colour displacement is not claimed. (d) Observed pigmented correlations and natural intervals across focal, 5-, 10-, 25- and 50-km population scales. The result indicates a pigmented-specific human-context overlay; it does not establish horticultural origin, introduction or gene flow. The 16 event-defined field/provenance targets and their calibration are retained in Appendix S6.
""",
        "Figure 4 caption",
    )
    write_if_changed(captions_path, captions)

    plan_path = ROOT / "submission/jbi/JBI_main_figure_plan.md"
    plan = plan_path.read_text(encoding="utf-8")
    plan = replace_regex(
        plan,
        r"## Figure 4 —.*?(?=\n## Figure rule)",
        """## Figure 4 — Reveal a human overlay on pigmented spatial geometry

**Question created by Figure 3:** Once broad environment, coherent geography and a local Bombus hypothesis are separated, does human context alter where isolated pigmented occurrences sit?

Panels:

- (a) all 1,305 flower cells, with 674 pigmented cells sized by nearest-pigmented distance and filled by 5-km population exposure;
- (b) population exposure versus relative same-colour isolation for pigmented and white cells;
- (c) observed raw and density-corrected 5-km correlations against 10,000 natural-map intervals;
- (d) observed pigmented correlations and natural expectations across the population-scale ladder.

**Reveal:** the positive isolation-population relationship within pigmented occurrences is stronger than the fitted natural geography predicts and survives local flower-cell density correction. The raw white negative sign does not survive that correction, so the result is not sold as reciprocal colour displacement.

**Supporting calibration:** the former 16-event analysis moves to Appendix S6 as an alternative estimand and a reproducible selector of extreme field/provenance targets.

**Ending:** the figures identify where physiology, population history, realized pollination and provenance should be tested directly, while preserving the claim ceiling that a human-context overlay is not evidence of horticultural origin.
""",
        "Figure 4 plan",
    )
    write_if_changed(plan_path, plan)

    cover_path = ROOT / "submission/jbi/JBI_cover_letter.md"
    cover = """# Cover letter draft — Journal of Biogeography

Dear Senior Editors,

We submit our Research Article, “From broad geography to local boundaries: biogeography of flower-colour polymorphism from hiking photographs.” From screened YAMAP images, we built a national quantitative flower-colour dataset and resolved its geography into environmental, residual-spatial, local focal-Bombus and contemporary human-context layers. The conceptual advance is scale-matched attribution: changing comparison unit separates processes a national regression would confound. Continuous all-cell geometry further shows that isolated pigmented occurrences are more population-exposed than natural maps predict, while density correction prevents an apparent reciprocal white pattern from being overstated. The study uses positive, heterogeneous and null results to generate testable physiological, reproductive and provenance hypotheses without assigning causality beyond the evidence.

Sincerely,

[Corresponding author]
[Affiliation]
[Email]
"""
    write_if_changed(cover_path, cover)


def update_checklist() -> None:
    path = ROOT / "submission/jbi/JBI_submission_checklist.md"
    text = path.read_text(encoding="utf-8")
    text = text.replace(
        "and apparent departures are calibrated against natural maps before human context is read.",
        "and continuous same-colour isolation is tested against local sampling density and 10,000 natural maps before event-defined populations are retained as field targets.",
    )
    text = text.replace(
        "calibrated field targets and a leading provenance hypothesis.",
        "a guarded pigmented human-context overlay, calibrated field targets and a leading provenance hypothesis.",
    )
    text = text.replace(
        "- **Departure killing part:** predictive replay converts 16 apparent exceptions into reproducibly calibrated field/provenance targets rather than erasing them as a null result.\n- **Human killing part:** short-range population exposure is identified as the leading human-context hypothesis and its likely spatial scale, without claiming provenance.",
        "- **Human-geometry killing part:** across 674 pigmented cells, the 5-km isolation-population relationship exceeds 10,000-map natural expectation in raw and density-corrected form; the raw white sign reversal is explicitly rejected after density correction.\n- **Departure killing part:** predictive replay converts 16 apparent exceptions into reproducibly calibrated event-defined field/provenance targets rather than using them as the main human-context sample.",
    )
    text = text.replace(
        "- No claim that the **16 current-Broad departures** are anthropogenic or more frequent than the finalized natural model expects.",
        "- No claim that the **674 pigmented cells** establish horticultural origin, or that the **16 event-defined field targets** are anthropogenic or more frequent than the finalized natural model expects.",
    )
    main = (ROOT / "submission/jbi/JBI_main_manuscript_anonymized.md").read_text(encoding="utf-8")
    abstract = main.split("## Abstract", 1)[1].split("**Keywords:**", 1)[0]
    body = main.split("## 1. Introduction", 1)[1].split("## Acknowledgements", 1)[0]
    abstract_n = len(words(abstract))
    body_n = len(words(body))
    text = re.sub(
        r"Current Introduction-through-Discussion count = \*\*[\d,]+ words\*\* by repository validator\.",
        f"Current Introduction-through-Discussion count = **{body_n:,} words** by repository validator.",
        text,
    )
    text = re.sub(
        r"Current abstract = \*\*\d+ words\*\* by repository validator\.",
        f"Current abstract = **{abstract_n} words** by repository validator.",
        text,
    )
    write_if_changed(path, text)


def update_submission_validator() -> None:
    path = ROOT / "submission/jbi/validate_jbi_submission.py"
    text = path.read_text(encoding="utf-8")
    text = text.replace(
        '"### 3.5 Post-selection analysis identified a short-range human-context clue",',
        '"### 3.5 Predictive replay retained extreme populations as field targets",',
    )
    text = replace_regex(
        text,
        r"require_tokens\(\n    \"Main manuscript science locks\",.*?\n\)\n\nfor citation",
        '''require_tokens(
    "Main manuscript science locks",
    text,
    (
        "1,922",
        "3.81 times",
        "67 sharp transitions",
        "674 pigmented",
        "rho=0.252",
        "P=0.0002",
        "rho=+0.285",
        "P=0.0009",
        "Sixteen pigmented cells",
        "calibrated field targets",
    ),
)

for citation''',
        "Main science-lock block",
    )
    text = replace_once(
        text,
        'require_tokens("Appendix S6", s6, ("**16**", "0.27897", "0.05479"))',
        'require_tokens("Appendix S6", s6, ("674 pigmented", "0.251980", "0.000200", "0.285498", "0.000900", "**16**", "0.27897", "0.05479"))',
        "Appendix S6 token lock",
    )
    text = replace_once(
        text,
        'require_tokens("Main-figure captions", figure_text, ("1,922", "Sixty-seven", "**16**", "0.05479"))',
        'require_tokens("Main-figure captions", figure_text, ("1,922", "Sixty-seven", "674 pigmented", "rho=0.252", "P=0.0002", "rho=0.285", "P=0.0009", "16 event-defined"))',
        "figure-caption token lock",
    )
    text = replace_once(
        text,
        'require_tokens("Japanese translated abstract", translated, ("1,922", "67", "16地点", "0.0548"))',
        'require_tokens("Japanese translated abstract", translated, ("1,922", "67", "674", "0.252", "0.0002", "0.285", "0.0009", "16地点"))',
        "translated-abstract token lock",
    )
    text = text.replace(
        '"16 current-Broad departures",',
        '"674 pigmented cells",\n        "16 event-defined field targets",',
    )
    write_if_changed(path, text)


def update_alignment_claim_aliases() -> None:
    path = ROOT / "validation/validate_jbi_repository_alignment.py"
    text = path.read_text(encoding="utf-8")
    text = text.replace(
        'r"does not establish an additional anthropogenic process)"',
        'r"does not establish an additional anthropogenic process|"\n        r"does not establish horticultural origin|"\n        r"not as proof of reciprocal colour displacement or horticultural origin)"',
    )
    write_if_changed(path, text)


def update_figure_workflow() -> None:
    path = ROOT / ".github/workflows/jbi-main-figures.yml"
    text = path.read_text(encoding="utf-8")
    for trigger in (
        "      - scripts/render_jbi_figure4_continuous_isolation.R\n",
        "      - reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md\n",
    ):
        if trigger not in text:
            text = text.replace(
                "      - scripts/render_jbi_main_figures.R\n",
                "      - scripts/render_jbi_main_figures.R\n" + trigger,
            )
    env_marker = '  HUMAN_CURRENT_BROAD_ARTIFACT_SHA256: "f5f7f3633d43a62fbef1f5142a77a21e766a5d85ab4f17dc43b062dadf4803c4"\n'
    env_add = (
        env_marker
        + f'  CONTINUOUS_ISOLATION_ARTIFACT_ID: "{ARTIFACT_ID}"\n'
        + f'  CONTINUOUS_ISOLATION_ARTIFACT_SHA256: "{ARTIFACT_SHA256}"\n'
    )
    if "CONTINUOUS_ISOLATION_ARTIFACT_ID" not in text:
        text = replace_once(text, env_marker, env_add, "figure workflow continuous env")

    insertion_marker = "          test -s results/human_context_current_broad/current_broad_primary_human_maxT.csv\n"
    restore_add = insertion_marker + f'''          mkdir -p {RESULT_ROOT}
          curl --fail --location --retry 5 --retry-all-errors \\
            -H "Authorization: Bearer ${{GITHUB_TOKEN}}" -H "Accept: application/vnd.github+json" \\
            "https://api.github.com/repos/${{GITHUB_REPOSITORY}}/actions/artifacts/${{CONTINUOUS_ISOLATION_ARTIFACT_ID}}/zip" -o /tmp/continuous-isolation.zip
          [[ "$(sha256sum /tmp/continuous-isolation.zip | awk '{{print $1}}')" == "$CONTINUOUS_ISOLATION_ARTIFACT_SHA256" ]]
          unzip -q -o /tmp/continuous-isolation.zip -d {RESULT_ROOT}
          test -s {RESULT_ROOT}/continuous_isolation_cell_metrics.csv
          test -s {RESULT_ROOT}/continuous_isolation_natural_null_summary.csv
          test -s {RESULT_ROOT}/continuous_isolation_validation.json
'''
    if "/tmp/continuous-isolation.zip" not in text:
        text = replace_once(text, insertion_marker, restore_add, "continuous figure artifact restore")

    provenance_marker = "          current_broad_human_artifact_sha256=${HUMAN_CURRENT_BROAD_ARTIFACT_SHA256}\n"
    provenance_add = provenance_marker + "          continuous_isolation_artifact_id=${CONTINUOUS_ISOLATION_ARTIFACT_ID}\n          continuous_isolation_artifact_sha256=${CONTINUOUS_ISOLATION_ARTIFACT_SHA256}\n"
    if "continuous_isolation_artifact_id=" not in text:
        text = replace_once(text, provenance_marker, provenance_add, "continuous figure provenance")
    write_if_changed(path, text)


def update_figure_renderer() -> None:
    path = ROOT / "scripts/render_jbi_main_figures.R"
    text = path.read_text(encoding="utf-8")
    text = replace_regex(
        text,
        r"# Figure 4a must reflect.*?(?=# Overwrite the adapter previews)",
        'source("scripts/render_jbi_figure4_continuous_isolation.R")\n\n',
        "Figure 4 renderer block",
    )
    text = text.replace(
        '"Figure_4_calibrated_local_departures",',
        '"Figure_4_continuous_colour_isolation_human_context",',
    )
    text = text.replace(
        '"calibrate natural departures before contemporary provenance follow-up"',
        '"test pigmented isolation against natural geography and sampling density"',
    )
    write_if_changed(path, text)


def update_figure_validator() -> None:
    path = ROOT / "validation/validate_jbi_figure_bundle.R"
    text = path.read_text(encoding="utf-8")
    required_marker = '  file.path(output_dir, "figure_data", "figure3_final8_environment_balance.csv")\n'
    required_add = required_marker.rstrip("\n") + ',\n  file.path(output_dir, "figure_data", "figure4_continuous_cell_metrics.csv"),\n  file.path(output_dir, "figure_data", "figure4_continuous_natural_summary.csv"),\n  file.path(output_dir, "figure_data", "figure4_continuous_population_profile.csv")\n'
    if "figure4_continuous_cell_metrics.csv" not in text:
        text = replace_once(text, required_marker, required_add, "continuous figure required data")

    expected_marker = "  bombus_final8_all_edge_env_distance = 0.317519973222642\n"
    expected_add = "  bombus_final8_all_edge_env_distance = 0.317519973222642,\n  continuous_pigmented_cells = 674,\n  continuous_white_cells = 631,\n  continuous_pigmented_median_isolation_km = 3.60555127546399,\n  continuous_raw_population_5km_pigmented_rho = 0.25197993694926,\n  continuous_raw_population_5km_natural_mean = 0.132979942281226,\n  continuous_raw_population_5km_upper_p = 0.0001999800019998,\n  continuous_relative_population_5km_pigmented_rho = 0.285497536507115,\n  continuous_relative_population_5km_natural_mean = 0.153615552893768,\n  continuous_relative_population_5km_upper_p = 0.0008999100089991,\n  continuous_raw_population_5km_direct_difference = 0.323523940415984,\n  continuous_relative_population_5km_direct_difference = 0.20699177378826\n"
    if "continuous_pigmented_cells = 674" not in text:
        text = replace_once(text, expected_marker, expected_add, "continuous figure numerical expectations")

    tolerance_marker = "  bombus_final8_selected_env_distance=1e-10, bombus_final8_all_edge_env_distance=1e-10\n"
    tolerance_add = "  bombus_final8_selected_env_distance=1e-10, bombus_final8_all_edge_env_distance=1e-10,\n  continuous_pigmented_cells=0, continuous_white_cells=0,\n  continuous_pigmented_median_isolation_km=1e-10,\n  continuous_raw_population_5km_pigmented_rho=1e-10,\n  continuous_raw_population_5km_natural_mean=1e-10,\n  continuous_raw_population_5km_upper_p=1e-12,\n  continuous_relative_population_5km_pigmented_rho=1e-10,\n  continuous_relative_population_5km_natural_mean=1e-10,\n  continuous_relative_population_5km_upper_p=1e-12,\n  continuous_raw_population_5km_direct_difference=1e-10,\n  continuous_relative_population_5km_direct_difference=1e-10\n"
    if "continuous_raw_population_5km_pigmented_rho=1e-10" not in text:
        text = replace_once(text, tolerance_marker, tolerance_add, "continuous figure tolerances")

    check_marker = "\nbombus_balance <- hb_read_csv(file.path(output_dir, \"figure_data\", \"figure3_final8_environment_balance.csv\"))\n"
    continuous_check = '''
continuous_cells <- hb_read_csv(file.path(output_dir, "figure_data", "figure4_continuous_cell_metrics.csv"))
continuous_natural <- hb_read_csv(file.path(output_dir, "figure_data", "figure4_continuous_natural_summary.csv"))
continuous_primary <- continuous_natural[
  continuous_natural$metric == "relative_same_to_any_nn" &
    continuous_natural$feature == "population_5km_rank" &
    continuous_natural$component == "pigmented_rho" &
    continuous_natural$null_mode == "all_nondegenerate_maps",
  , drop = FALSE
]
add_check(
  "figure4_continuous_isolation_human_context",
  nrow(continuous_cells) == 1305L &&
    sum(as.character(continuous_cells$colour) == "pigmented") == 674L &&
    nrow(continuous_primary) == 1L &&
    abs(as.numeric(continuous_primary$observed_value) - 0.285497536507115) < 1e-10 &&
    abs(as.numeric(continuous_primary$null_mean) - 0.153615552893768) < 1e-10 &&
    abs(as.numeric(continuous_primary$empirical_p) - 0.0008999100089991) < 1e-12,
  if (nrow(continuous_primary)) sprintf(
    "cells=%d pigmented=%d rho=%.6f null=%.6f P=%.6f",
    nrow(continuous_cells),
    sum(as.character(continuous_cells$colour) == "pigmented"),
    as.numeric(continuous_primary$observed_value),
    as.numeric(continuous_primary$null_mean),
    as.numeric(continuous_primary$empirical_p)
  ) else "missing continuous primary row"
)

bombus_balance <- hb_read_csv(file.path(output_dir, "figure_data", "figure3_final8_environment_balance.csv"))
'''
    if '"figure4_continuous_isolation_human_context"' not in text:
        text = replace_once(text, check_marker, "\n" + continuous_check, "continuous Figure 4 validation")
    write_if_changed(path, text)


def main() -> None:
    update_lock()
    update_paper_overview()
    update_active_map()
    update_submission_text_contracts()
    update_checklist()
    update_submission_validator()
    update_alignment_claim_aliases()
    update_figure_workflow()
    update_figure_renderer()
    update_figure_validator()


if __name__ == "__main__":
    main()
