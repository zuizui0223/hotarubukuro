# hotarubukuro

Reproducible nationwide analysis of floral-colour geography in *Campanula
punctata* using author-reviewed social-media photographs, environmental
rasters, spatial models, predicted *Bombus* communities, and exploratory human
landscape context.

## Biological response

Each focal flower was cropped and its retained petal colour region was visually
confirmed by the author before scripted colour extraction. The analysis then
uses a two-part response:

1. optical pigmentation presence across all observations; and
2. CIELAB a* intensity conditional on a flower being classified as pigmented.

White-flower a* variation is not interpreted as anthocyanin quantity. Extreme
dark or pink flowers are retained because they may be biologically real and are
relevant to follow-up of possible human-mediated introductions.

## Publication analysis

The pipeline separates nationwide pattern, local biotic turnover, and
exploratory human context so that they do not share a single causal claim.

| Stage | Manuscript role | Main analysis |
|---|---|---|
| `01_phenotype` | Measurement model | White/pigmented classification and pigmented-only intensity |
| `02_natural_model` | Confirmatory natural baseline | Response-blind environment plus PC-prior INLA-SPDE and spatial cross-validation |
| `03_local_bombus` | Planned local biotic test | Near-neighbour flower-colour turnover versus predicted *Bombus* community turnover |
| `04_candidate_definition` | Candidate definition | Pigmented isolates among environment-similar observed-white neighbours |
| `05_human_context` | Exploratory characterization | Population and DID contrasts without using human variables to select candidates |
| `06_final_lock` | Reproducibility and claim control | Result, exclusion, checksum, validation, and claim registries |

Fresh ENMeval model selection and prediction are used for the five *Bombus*
species. Previously generated *Bombus* TIFF files are not accepted as analysis
inputs. Predicted suitability is treated as a community fingerprint, not as
abundance, visitation, pollination efficiency, or direct selection pressure.

See `docs/analysis-plan.md` for the complete inferential design and
`docs/manuscript-story.md` for the reviewer-facing narrative and claim
ceilings.

## Repository layout

```text
R/                         stage-specific analysis functions
R/pipeline_support.R       package groups, module registry, shared helpers
scripts/                   executable data-build and stage runners
validation/                independent validation and claim audits
tests/testthat/             unit and interface tests
reports/                   analysis reports, separate from executable modules
docs/                      current analysis and data-source documentation
results/final_analysis_pipeline/
                           publication registries and locked handoff
Data_S1.csv                analysis data
scripts/extract_color.py   deterministic colour-extraction CLI
Code_S1.py                 supplementary GPX georeferencing code
```

Stable code filenames do not contain development version numbers. Versioned
result directories such as `ecological_v11_*` and `ecological_v22_*` remain as
provenance identifiers for frozen analyses.

## Run and verify

From the repository root, verify all locked artifacts and run the test suite:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/run_publication_pipeline.R --mode=verify --tests=true
```

Rebuild the post-baseline extensions from the frozen natural-model checkpoint:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/run_publication_pipeline.R --mode=extensions --tests=true
```

Use `--mode=full` to rebuild the natural predictive model and subsequent
stages. Image extraction and manually reviewed colour regions remain fixed
upstream inputs.

Render the phenotype-analysis report:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/render_phenotype_report.R
```

On Windows, INLA may require `TEMP` and `TMP` to point to an ASCII-only
directory.

## Dependencies

R package requirements and their stage assignments are centralized in
`R/pipeline_support.R`. Major dependencies are `INLA`, `sf`, `terra`,
`ENMeval`, `maxnet`, `mclust`, `mgcv`, `qgam`, `ranger`, `testthat`, and
`rmarkdown`. Python dependencies for supplementary colour extraction are
declared in `pyproject.toml`.

## Interpretation ceiling

The natural model quantifies environmental and unresolved spatial structure.
The local *Bombus* analysis evaluates correspondence between predicted
community turnover and flower-colour turnover. The human-context analysis ranks
follow-up candidates. None of these analyses alone establishes pollinator
selection, horticultural origin, planting, garden escape, introgression, or
genetic contamination; those require field, provenance, and genetic evidence.
