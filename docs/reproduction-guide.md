# Reproduction guide

This guide is for checking the JBI manuscript numbers or rebuilding the accepted review package.

If you only want to understand the biology, start at [`paper/README.md`](../paper/README.md).

## Canonical commands

Fast repository/manuscript audit:

```bash
python run_pipeline.py audit
```

One-shot paper reproduction:

```bash
python run_pipeline.py reproduce
```

The canonical GitHub Actions entry is `.github/workflows/paper-pipeline.yml` (**Paper pipeline**). A manual `reproduce` dispatch prepares the declared Ubuntu, Python and R environment, restores checksum-locked inputs, reruns the accepted downstream analyses, rebuilds the four figures, assembles six editable DOCX files, renders them through LibreOffice and writes a machine-readable run manifest.

Useful controls:

```bash
python run_pipeline.py reproduce --dry-run
python run_pipeline.py reproduce --from-stage run_local_bombus
python run_pipeline.py reproduce --only-stage validate_alignment
python run_pipeline.py reproduce --no-resume
```

All artifact IDs, SHA-256 checksums, commands, seeds, declared outputs and manuscript locks live in [`config/paper_pipeline.lock.json`](../config/paper_pipeline.lock.json). Do not copy those values into a new ad hoc script.

## What `audit` checks

`audit` does not download artifacts or fit models. It checks that:

1. the JBI manuscript, `paper/README.md` and `paper/analysis-map.md` carry the same accepted numbers;
2. inferential ceilings remain explicit: Bombus SDMs are not visitation/selection, and local departures are not proof of human origin;
3. artifact IDs and checksums agree across the lock, component workflows and evidence map;
4. the JBI source package passes its format/anonymity validator;
5. the repository exposes one primary execution entry.

Output: `results/paper_pipeline/jbi_repository_alignment.json` and `results/paper_pipeline/run_manifest.json`.

## What `reproduce` means

The exact route starts from three checksum-locked accepted inputs:

- accepted Broad flower-colour/local-graph/human baseline;
- seeded five-species Bombus SDMs;
- final-eight-axis posterior predictive draws.

It then regenerates:

1. occurrence-referenced Bombus support;
2. 67 fixed local white-pigmented boundary tests;
3. the final-eight-axis environmental-balance audit;
4. 10,000-map natural-departure and human-context adjudication;
5. the four JBI main figures and their numerical validation;
6. the editable JBI review bundle and rendered PDF smoke tests;
7. the final manuscript/repository alignment report and provenance manifest.

The exact pipeline is intentionally checksum-locked. It is not a live source refresh.

### Explicit provenance boundary

Original YAMAP photographs are third-party content and are not redistributed. The public derived table is `Data_S1.csv`; the accepted Broad evidence is restored by checksum. Consequently, `reproduce` rebuilds the manuscript-facing analyses and delivery package from accepted evidence, but it does not claim to redownload the original photographs or silently replace the accepted Broad model.

GBIF, CHELSA, SoilGrids and other public sources can change. Reacquiring them is therefore a **new analysis** and belongs to the focused source-build workflows, especially `.github/workflows/rebuild-bombus-sdm.yml`, not to exact paper reproduction.

## Stage contracts

### 1. Flower-colour analysis population

Public table and construction code:

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`

Expected manuscript population:

- 1,922 observations;
- 1,305 1-km cells;
- 966 white-like;
- 956 pigmented.

Details: Appendices S1-S2.

### 2. Accepted Broad environment-spatial evidence

Final model record:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

Expected structure:

- pigmentation state: eight abiotic axes + East/West + stationary SPDE;
- conditional intensity: the same terms + Temperature PC1 × temperature seasonality.

The interaction screen and alternative spatial models remain focused diagnostic components. They are not rerun as a hidden alternative to the accepted model during paper reproduction.

### 3. Bombus support and local boundaries

Commands are declared in the lock and call:

- `scripts/build_bombus_occurrence_reference_support.R`
- `scripts/run_bombus_local_sharp_transition.R`
- `analysis_sensitivity/audit_bombus_final8_environment_distance.R`

Expected primary result:

- 67 non-overlapping pure transitions within 5 km;
- mean focal contrast +0.03590;
- median -0.00277;
- 49.3% positive pairs;
- one-sided P=0.02716;
- q=0.08148 across the 5/10/25-km primary family.

The SDM-derived surface is habitat support, not bee abundance, visitation, pollen transfer or selection. The environmental-distance audit is run after the pairs are fixed; it does not choose or weight pairs.

Details: Appendices S4-S5.

### 4. Natural departures and human follow-up

The accepted detector uses:

- pigmented focal cell;
- at least three neighbours within 10 km;
- root-mean-square environmental distance <=1 across the final eight abiotic axes;
- all eligible observed neighbours white.

The same detector is replayed on 10,000 predictive maps.

Expected natural-calibration result:

- 16 observed candidates;
- count P=0.27897;
- candidate-fraction P=0.12609.

Expected leading human-context result:

- population exposure within 5 km: +0.06744;
- directional P=0.00800;
- global maxT FWER P=0.05479.

This is a provenance/field-work signal, not proof of human origin. Details: Appendix S6.

### 5. Figures and review package

The canonical reproduction calls:

- `scripts/render_jbi_main_figures_final_broad.R`
- `validation/validate_jbi_figure_bundle_final_broad.R`
- `scripts/build_jbi_submission_bundle.py`
- `scripts/validate_jbi_submission_bundle.py`
- `submission/jbi/validate_jbi_submission.py`

It renders every generated DOCX through LibreOffice and checks the first page of each PDF. Human visual approval and author-controlled portal fields remain outside automation.

## Local prerequisites

The orchestrator installs declared Python and R packages. System libraries must already be available; the canonical GitHub Actions workflow installs them from:

- `dependencies/apt-packages.txt`
- `dependencies/submission-apt-packages.txt`

Local artifact restoration requires `GITHUB_TOKEN` or `GH_TOKEN` with read access to Actions artifacts. The canonical workflow supplies its own token.

## Focused component workflows

The older stage-specific workflows remain available for development, diagnostics and live-source refresh. They are components, not competing paper entry points. Their artifact locks are checked automatically against `config/paper_pipeline.lock.json`.

## What counts as success?

A successful run writes `results/paper_pipeline/run_manifest.json` and recovers the same:

- analysis population and response definitions;
- accepted model and event definitions;
- local-boundary and local-departure rules;
- manuscript-level numerical results;
- figure and submission-bundle validation status.

Exact stochastic draws do not need to be bit-for-bit identical across platforms; the scientific result and declared numerical locks must agree.

## Remaining durability gate

The current evidence is checksum-locked but stored as retention-bound GitHub Actions artifacts. Before public archival release, the locked input bundle should be copied to a durable release asset or external repository with a DOI. The validator reports this as a durability warning rather than weakening the checksum rule.
