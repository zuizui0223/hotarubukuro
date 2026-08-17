# Reproduction guide

This guide checks the current JBI paper or rebuilds its accepted review package. For the biology first, use [`paper/README.md`](../paper/README.md).

## Canonical commands

Fast repository/manuscript audit:

```bash
python run_pipeline.py audit
```

One-shot paper reproduction:

```bash
python run_pipeline.py reproduce
```

Useful controls:

```bash
python run_pipeline.py reproduce --dry-run
python run_pipeline.py reproduce --from-stage run_broad_space_null_excess
python run_pipeline.py reproduce --only-stage validate_alignment
python run_pipeline.py reproduce --no-resume
```

The canonical GitHub Actions entry is `.github/workflows/paper-pipeline.yml` (**Paper pipeline**). Artifact IDs, SHA-256 checksums, commands, seeds, expected outputs and manuscript locks are declared once in [`config/paper_pipeline.lock.json`](../config/paper_pipeline.lock.json).

## What `audit` checks

`audit` does not download artifacts or fit models. It checks that:

1. the current manuscript, paper overview and evidence map carry the same accepted numbers;
2. the merged PR #50 spatial-null result remains a supporting Broad sensitivity with its non-causal claim ceiling;
3. the merged PR #51 local-boundary Bombus result remains the primary biotic story and the highland overlap remains a guardrail;
4. artifact IDs and checksums agree across the lock, component workflows and evidence map;
5. the JBI source package passes format/anonymity validation;
6. the repository exposes one active execution front door.

Outputs:

- `results/paper_pipeline/jbi_repository_alignment.json`
- `results/paper_pipeline/run_manifest.json`

## What `reproduce` does

Exact reproduction starts from three checksum-locked inputs:

- accepted Broad flower-colour, graph and human-context evidence;
- seeded five-species Bombus SDMs;
- final-eight-axis posterior predictive draws.

It then executes, in order:

1. `run_broad_space_null_excess`: five-fold cross-fitted space-only SPDE sensitivity for state and conditional intensity;
2. occurrence-referenced Bombus support reconstruction;
3. 67 fixed local white-pigmented boundary tests and their final-eight-axis balance audit;
4. 10,000-map natural-departure and human-context adjudication;
5. the four current JBI figures;
6. the editable six-file JBI review bundle and PDF render smoke tests;
7. final manuscript/repository alignment and a machine-readable provenance manifest.

This route is checksum-locked. It does not refresh live public sources or replace the accepted Broad model family.

## Stage contracts

### 1. Flower-colour analysis population

Expected manuscript population:

- 1,922 observations;
- 1,305 1-km cells;
- 966 white-like;
- 956 pigmented.

Public derived table and construction code:

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`

Original YAMAP photographs are third-party content and are not redistributed.

### 2. Accepted Broad environment-spatial models

The observation-level model remains the current JBI model:

- pigmentation state: eight abiotic axes + East/West + stationary Matérn SPDE;
- conditional intensity: the same structure + Temperature PC1 × temperature seasonality.

Primary records:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

### 3. Cross-fitted Broad spatial-null sensitivity from PR #50

Command component:

- `scripts/fit_broad_space_null_phenotype_excess.R`
- `scripts/run_broad_space_null_phenotype_excess_pipeline.R`
- `.github/workflows/broad-spatial-inertia-environment-tracking.yml`

The canonical pipeline repairs the known post-computation metadata-row mismatch only when every scientific output exists, then validates the result against numerical tolerances.

Expected result from 500 posterior-predictive realizations, seed 20260725, five geographical folds and five geographical-distance strata per fold:

| Response | Observed high-env − low-env divergence | Space-null median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.106802 | 0.058240 | +0.048562 | 0.03393 |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

Interpretation: pigmentation-state divergence is aligned with environmental difference beyond the fitted continuous-spatial expectation. This is not FST, PST or QST and does not establish selection, local adaptation or a unique causal environment.

### 4. Local focal-Bombus boundaries from the current PR #51 narrative

Expected primary result:

- 67 non-overlapping pure transitions within 5 km;
- mean focal contrast +0.03590;
- median -0.00277;
- 49.3% positive pairs;
- one-sided P=0.02716;
- q=0.08148 across the 5/10/25-km primary family.

The local 5-km comparison is the primary ecological test. It is the finest predeclared replicated scale, not an exact foraging-distance estimate. The national highland overlap is secondary: it disappears under near-equal-elevation comparison and demonstrates the danger of shared mountain geography. SDM support is habitat opportunity, not abundance, visitation, pollen transfer or realized selection.

### 5. Natural departures and human follow-up

Expected natural calibration:

- 16 observed candidates;
- count P=0.27897;
- candidate-fraction P=0.12609.

Expected leading human-context result:

- population exposure within 5 km: +0.06744;
- directional P=0.00800;
- global maxT FWER P=0.05479.

The 16 sites remain field/provenance targets, not demonstrated anthropogenic anomalies.

### 6. Figures and review package

The pipeline rebuilds four figures, assembles six editable DOCX files, validates hashes/anonymity/package structure, renders all DOCX files through LibreOffice and checks first-page PDF output. Human visual approval and author-controlled portal fields remain outside automation.

## Local prerequisites

The orchestrator installs declared Python and R packages. The canonical workflow installs system libraries from:

- `dependencies/apt-packages.txt`
- `dependencies/submission-apt-packages.txt`

Local artifact restoration requires `GITHUB_TOKEN` or `GH_TOKEN` with Actions-artifact read access. The canonical workflow supplies its own token.

## What counts as success?

A successful run writes `results/paper_pipeline/run_manifest.json` and recovers the same:

- analysis population and response definitions;
- PR #50 spatial-null direction and numerical lock;
- PR #51 local-boundary hierarchy and claim ceilings;
- 16-site natural calibration and human follow-up;
- current figures and JBI review package.

Exact stochastic draws need not be bit-for-bit identical across platforms; the declared scientific result and numerical tolerances must agree.

## Remaining durability gate

The evidence is checksum-locked but currently referenced through retention-bound GitHub Actions artifacts. Before archival release, the locked inputs should be copied to a durable release asset or DOI-backed repository. The validator reports this without weakening the checksum rule.
