# Reproducing the current manuscript-facing analyses

Start with [`../paper/README.md`](../paper/README.md) and [`../paper/analysis-map.md`](../paper/analysis-map.md). They define the current scientific hierarchy and the checksum-locked numerical references used by the JBI manuscript.

The current paper is **not** reproduced through the historical 1,909-observation pipeline. That architecture is preserved under `legacy/` for provenance.

## What is current

The manuscript has four linked analytical layers:

1. **YAMAP / iEcology data layer** — source reconstruction, author screening, image audit and two-part quantitative flower-colour phenotype;
2. **Main 1: broad natural template** — national environment + INLA-SPDE for pigmentation state and pigmented-only intensity;
3. **Main 2: local focal-Bombus test** — sharp nearby white-pigmented transitions versus occurrence-referenced *B. ardens* + *B. diversus* availability;
4. **Main 3: event-based departures** — repeated natural-map calibration of local pigmented-in-white configurations followed by post-selection human-context characterization.

Five-species Bombus turnover and the montane/elevation analysis are Supporting Information guardrails, not additional Main mechanisms.

## Recommended route: GitHub Actions

### Broad natural template + anomaly/human-context stages

Run:

`.github/workflows/reanalysis-current-inputs.yml`

This workflow:

1. restores the pinned software environment;
2. checks out the exact frozen upstream implementation used to rebuild the fresh phenotype/environment boundary;
3. restores the checksum-locked fresh Bombus source artifact required by downstream source tables;
4. rebuilds the 1,965-row source boundary and the broad v11/v15 phenotype/environment products;
5. runs the natural predictive reference, event-based departure calibration and human-context stages once; and
6. uploads the resulting tables and provenance.

The expected manuscript population is 1,922 phenotype observations in 1,305 1-km cells, with 966 white-like and 956 pigmented observations.

### Main 2 local focal-pollinator test

The manuscript-facing local test is built in two steps:

1. create occurrence-referenced support from the selected fresh SDMs with `scripts/build_bombus_occurrence_reference_support.R`;
2. test sharp nearby white-pigmented boundaries with `scripts/run_bombus_local_sharp_transition.R`.

Corresponding workflows:

- `.github/workflows/bombus-occurrence-reference-support.yml`;
- `.github/workflows/bombus-local-sharp-transition.yml`.

The primary contrast uses pure non-overlapping transitions within 5 km and occurrence-referenced support from *B. ardens* + *B. diversus*. Pair selection is Bombus-blind and sign-blind. The current interpretation and claim ceiling are documented in `docs/bombus-inference-current.md`.

### Supporting Bombus biogeography

Run:

- `scripts/run_bombus_spatial_replication_test.R`;
- `.github/workflows/bombus-spatial-replication-test.yml`.

This produces the five-species community-boundary correspondence and near-equal-elevation montane guardrails used only in Supporting Information.

### YAMAP public-source benchmark

The frozen descriptive comparison is defined by:

- `.github/workflows/yamap-public-database-benchmark.yml`;
- `.github/workflows/yamap-public-database-overlap-audit.yml`;
- `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`;
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`.

The manuscript-ready table is `submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md`.

## Frozen numerical references

Use the run/artifact/SHA records in `paper/analysis-map.md`. The current manuscript deliberately references frozen successful artifacts rather than silently changing results when workflows or external databases are updated.

Key frozen references include:

- broad + anomaly: run `31258851297`, artifact `9022276431`;
- occurrence-referenced focal Bombus support: run `31262211605`, artifact `9023137743`;
- local sharp-transition test: run `31263324505`, artifact `9023416810`;
- supplementary Bombus boundary guardrails: run `31285234317`, artifact `9029595037`;
- YAMAP benchmark: run `31289927019`, artifact `9031041034`.

Exact SHA-256 checksums are in `paper/analysis-map.md`.

## Local execution

A full local rebuild is possible but is not the preferred audit route because several current stages depend on large public raster inputs and frozen workflow artifacts. For development, install the declared environment first:

```bash
Rscript scripts/setup_r_environment.R \
  --report-dir reproducibility \
  --scopes analysis,reproducibility,acquisition,testing,figures,reporting
```

Then use the individual manuscript-facing scripts listed in `paper/active-file-map.csv`. Do not use scripts under `legacy/` as current entry points.

## Data and source-build boundary

- `Data_S1.csv` is the curated derived trait/source table distributed with the repository.
- Original YAMAP photographs are third-party material and are not redistributed.
- `source_build/` contains current source-construction utilities for visible colour, environmental inputs, Bombus occurrences/SDMs and human rasters.
- External services such as GBIF can change. Live acquisition is therefore distinct from the frozen artifacts used by the manuscript.

## What moved to legacy

Historical 1,909 and 1,923 analyses, the all-five Bombus limitation gate, national Bombus null developments, relaxation/local-contrast variants, old E&E manuscript drafts and superseded publication/figure machinery are under `legacy/`.

See:

- `legacy/README.md`;
- `legacy/MOVED_2026-08-09.md`.

Nothing in `paper/active-file-map.csv` points into `legacy/`.

## Statistical rather than bitwise reproducibility

INLA posterior samples and external public-data queries need not reproduce bit-for-bit across platforms or dates. The manuscript therefore locks input identities, seeds, definitions, run provenance and claim ceilings, and reports realized effect sizes and uncertainty. Live source refreshes must be treated as new analyses rather than silently substituted for the frozen manuscript evidence.
