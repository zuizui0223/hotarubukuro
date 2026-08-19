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
python run_pipeline.py reproduce --from-stage restore_broad_supported_term_distance
python run_pipeline.py reproduce --only-stage validate_alignment
python run_pipeline.py reproduce --no-resume
```

The canonical GitHub Actions entry is `.github/workflows/paper-pipeline.yml` (**Paper pipeline**). Artifact provenance IDs, SHA-256 checksums, commands, seeds, expected outputs and manuscript locks are declared in [`config/paper_pipeline.lock.json`](../config/paper_pipeline.lock.json).

## What `audit` checks

`audit` does not fit models. It checks that:

1. the current manuscript, paper overview and evidence map carry the same accepted numbers;
2. the final Broad coefficient models and supported-environmental-distance comparison retain their response-specific conclusions and non-causal claim ceilings;
3. the local-boundary Bombus result is reported as heterogeneous rather than pervasive, with the highland overlap retained as a confounding guardrail;
4. continuous all-cell isolation is the Main human-context analysis and the 16-event family remains in Appendix S6;
5. the pooled *Campanula punctata* sensu lato taxonomic scope is stated in Supporting Information;
6. frozen-input and accepted-artifact identities remain traceable;
7. the JBI source package passes format and anonymity validation;
8. the repository exposes one active execution front door.

Outputs:

- `results/paper_pipeline/jbi_repository_alignment.json`
- `results/paper_pipeline/run_manifest.json`

## What `reproduce` does

Exact reproduction starts from checksum-locked inputs and accepted outputs:

- the observation-level Broad flower-colour, graph and human-context baseline;
- the accepted supported-environmental-distance comparison against fixed space-only maps;
- seeded focal Bombus SDMs;
- final-eight-axis posterior predictive draws;
- the validated continuous colour-isolation result.

The pipeline then executes or restores, in order:

1. the accepted Broad observation-level evidence;
2. the supported-term environmental-distance comparison, without refitting the final environment or spatial models;
3. occurrence-referenced Bombus support reconstruction;
4. 67 fixed local white-pigmented boundary tests and their environmental-balance audit;
5. the supplementary 16-event calibration and its human-context sensitivity;
6. the validated continuous all-cell isolation analysis used in the Main paper;
7. the four current JBI figures;
8. the editable six-file JBI review bundle and PDF render smoke tests;
9. final manuscript/repository alignment and a machine-readable provenance manifest.

This route is checksum locked. It does not refresh live public sources or replace the accepted Broad model family.

## Stage contracts

### 1. Flower-colour analysis population

Expected manuscript population:

- 1,922 observations;
- 1,305 1-km cells;
- 966 white-like observations;
- 956 pigmented observations.

Public derived table and construction code:

- `Data_S1.csv`
- `Code_S1.py`
- `source_build/build_data_s1.py`
- `source_build/extract_color.py`

Original YAMAP photographs are third-party content and are not redistributed.

The analysis pools the forms commonly called ホタルブクロ and ヤマホタルブクロ as *C. punctata* sensu lato. Appendix S1 explains that their image-diagnostic distinction is concentrated in calyx morphology, which is not consistently visible, and that preliminary unpublished data found no clear genetic differentiation. This is an analytical scope decision, not a formal taxonomic revision.

### 2. Accepted Broad environment-spatial models

The observation-level models remain the primary Broad analysis:

- pigmentation state: eight abiotic axes + East/West + stationary Matérn SPDE;
- conditional intensity: the same structure + Temperature PC1 × temperature seasonality.

Primary records:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`

Expected directional results:

- pigmentation state: Temperature PC1 = -0.542, 95% CrI -1.033 to -0.049;
- conditional intensity: precipitation PC1 = -0.174, temperature seasonality = +0.207, Topography PC1 = -0.134 and Temperature PC1 × seasonality = -0.204;
- residual ranges: 132.8 km for state and 65.7 km for conditional intensity.

### 3. Supported environmental-term distance versus fixed spatial continuity

The final models are not rebuilt for this comparison. The analysis reuses already fixed held-out pairs, five geographical-distance strata and 500 cached space-only posterior-predictive maps.

Environmental distance is response specific:

- pigmentation state: Temperature PC1;
- conditional intensity: an unweighted Euclidean distance across precipitation PC1, temperature seasonality, Topography PC1 and Temperature PC1 × temperature seasonality.

Expected result:

| Response | Observed high-minus-low divergence | Space-only median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | **0.100608** | **0.048475** | **+0.052133** | **0.00998** |
| Conditional intensity | 0.047416 | 0.020897 | +0.026519 | 0.26347 |

The comparison asks whether supported environmental separation orders held-out phenotype divergence beyond fitted spatial continuity. It does not identify the underlying mechanism or demonstrate selection or local adaptation.

### 4. Local focal-Bombus boundaries

Expected primary result:

- 67 non-overlapping pure transitions within 5 km;
- mean focal contrast +0.03590;
- median -0.00277;
- 49.3% positive pairs;
- one-sided P=0.02716;
- q=0.0815 across the 5/10/25-km family.

The positive mean is driven by a subset of boundaries. The distribution, scale attenuation, raw-support failure and equal-elevation guardrail must be reported together. SDM support is habitat opportunity, not abundance, visitation, pollen transfer or pollinator-mediated selection.

### 5. Continuous human-context geometry

The Main analysis uses all 1,305 cells: 674 pigmented and 631 white.

Expected focal result at 5 km:

| Pigmented isolation measure | Observed rho | Natural mean | Upper-tail P |
|---|---:|---:|---:|
| raw same-colour nearest distance | **0.251980** | **0.132980** | **0.000200** |
| relative to local all-flower spacing | **0.285498** | **0.153616** | **0.000900** |

The robust conclusion is an excess positive isolation-population relationship within pigmented occurrences. The apparent negative white relationship does not survive density correction. The analysis is explicitly post hoc and does not establish horticultural origin or causation by people.

### 6. Supplementary event calibration

Appendix S6 retains the earlier restrictive event family:

- 16 observed event cells;
- count P=0.27897;
- candidate-fraction P=0.12609;
- leading 5-km population contrast +0.06744;
- global maxT FWER P=0.05479.

These cells are supplementary extreme field targets. They are not excessive under natural maps and are not the statistical foundation of the Main human-context result.

### 7. Figures and review package

The pipeline rebuilds four figures, assembles six editable DOCX files, validates hashes, anonymity and package structure, renders all DOCX files through LibreOffice and checks first-page PDF output. Human visual approval and author-controlled portal fields remain outside automation.

## Local prerequisites

The orchestrator installs declared Python and R packages. The canonical workflow installs system libraries from:

- `dependencies/apt-packages.txt`
- `dependencies/submission-apt-packages.txt`

## What counts as success?

A successful run writes `results/paper_pipeline/run_manifest.json` and recovers the same:

- analysis population and response definitions;
- final Broad coefficients and residual spatial ranges;
- supported-term distance result for state and the null result for conditional intensity;
- heterogeneous local Bombus boundary hierarchy and claim ceilings;
- continuous pigmented isolation-population relationship and its density correction;
- supplementary 16-event calibration;
- current figures and JBI review package;
- pooled *C. punctata* sensu lato scope in Supporting Information.

Exact stochastic draws need not be bit-for-bit identical across platforms; the declared scientific result and numerical tolerances must agree.

## Durability status

Accepted artifact IDs and SHA-256 values are recorded in `paper/analysis-map.md` and `config/paper_pipeline.lock.json`. A future DOI-backed mirror can add independent preservation, but it is not a prerequisite for reproducing the frozen repository state.