# Reproducing the publication analysis

## What is reproducible

The repository separates three reproducibility levels.

1. **Locked-result verification** recalculates the final result and claim
   registries from adopted stage outputs, reruns independent validators and
   audits, and runs the software tests.
2. **Post-baseline reconstruction** starts from the frozen national predictive
   checkpoints and regenerates the local *Bombus*, candidate-definition, and
   human-context stages.
3. **Full model reconstruction** refits the national natural model before
   regenerating all downstream stages. It additionally requires the public
   environmental rasters, MLIT layers, occurrence cache, and freshly selected
   ENMeval predictions described below.

The raw SNS photographs cannot be redistributed automatically. `Data_S1.csv`
contains the derived colour measurements, source identifiers, image hashes,
manual-review provenance, and QC fields needed to audit the published
phenotypes.

The public environmental and human-landscape inputs, including the exact
dataset versions, variables, spatial resolutions, provider pages, and roles in
the analysis, are listed in
[`docs/data-sources/public-environment-sources.md`](data-sources/public-environment-sources.md).

## Software

The locked local run used R 4.5.3. R package groups and their assignment to
analysis stages are declared in `R/pipeline_support.R`. Major dependencies are
INLA, sf, terra, ENMeval, maxnet, mclust, mgcv, qgam, ranger, testthat, and
rmarkdown.

Python 3.9 or later is required for the supplementary colour-extraction
utilities. Install the declared package with:

```powershell
python -m pip install -e ".[excel,test]"
```

On Windows, set `TEMP` and `TMP` to an ASCII-only directory before fitting
INLA models if the default temporary path contains non-ASCII characters.

## Verify the locked results

From the repository root:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/run_publication_pipeline.R --mode=verify --tests=true
```

Success requires:

- all stages in `results/final_analysis_pipeline/final_stage_manifest.csv` to
  report `PASS`;
- all checks in `final_independent_validation.csv` to report `PASS`; and
- all checks in `final_claim_audit.csv` to report `PASS`.

## Rebuild downstream analyses

To regenerate stages 03–06 from the frozen national predictive checkpoints:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/run_publication_pipeline.R --mode=extensions --tests=true
```

This reproduces the 25-km local *Bombus* turnover test, local pigmented-isolate
definition, population/DID characterization, and final lock.

## Refit the national natural model

To refit stage 02 and then rebuild all downstream stages:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/run_publication_pipeline.R --mode=full --tests=true
```

The full run expects:

- the two-part phenotype input under
  `results/ecological_v11_pigmentation_hurdle/`;
- fixed 1-km cell context under
  `results/ecological_v15_multiscale_hotspots/`;
- freshly selected ENMeval predictions under
  `results/enmeval_aicc_reselected/predictions/`;
- the frozen public *Bombus* occurrence cache;
- MLIT-derived human-context rasters under `results/public_rasters/`; and
- the public environmental cache supplied through the runner arguments or
  environment variables documented by the individual build scripts.

Previously generated *Bombus* TIFFs are not accepted as substitutes for the
fresh ENMeval selection workflow.

## Create a dated local snapshot

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' `
  scripts/export_publication_snapshot.R `
  --output=local_outputs/publication_snapshot_2026-07-24 `
  --overwrite=true
```

The snapshot contains the adopted full stage outputs, compact upstream inputs,
the final registries, a file-level MD5 manifest, R session information, and
year-specific sample counts. Copy its `repository/` directory into a checkout
of the recorded commit to repeat the locked-result verification.

## Rebuild the manuscript

The Word draft is generated from the versioned manuscript rather than edited
from the superseded DOCX:

```powershell
& 'C:\Users\zuizui\.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe' `
  scripts/build_manuscript_docx.py
```

The default output is
`local_outputs/manuscript_review/revised.docx`. Supply `--output=<path>` to
write a separate submission copy elsewhere. The generated document uses A4
pages, 2.54-cm margins, Times New Roman 12 pt, double-spaced body text,
continuous line numbering, page numbers, semantic headings, and marked table
header rows.

## Sampling and interpretation limits

The database contains every eligible *Campanula punctata* record identified by
the author on YAMAP from 1 June through 31 August in each of 2023, 2024, and
2025; it is not a random sample of Japanese populations. Before database
construction, the author manually confirmed the focal taxon and flower region,
removed repeated photographs of the same individual, and excluded taxonomic
errors including *Adenophora triphylla* misidentified as *C. punctata*. Those
pre-database exclusions were not retained as separate count categories and
must not be assigned reconstructed counts.

YAMAP is a GPS-enabled hiking and outdoor-activity platform. Its activity
records provide a stronger route-linked georeferencing affordance than ordinary
social-media photographs that lack usable coordinates. The analysis uses the
reviewed coordinates carried in the curated source workbook; it does not claim
that the publication pipeline independently reconstructed every coordinate from
GPX. Because the occurrence records are concentrated on accessible hiking
trails and activity routes, accessibility, photographer choice, and observation
opportunity remain part of the sampling process.

Unlike iNaturalist, whose basic data unit is intentionally an organismal
observation, YAMAP is organized around hiking maps and recorded activities.
The comparison does not imply that YAMAP has higher positional accuracy than a
purpose-built biodiversity application. It identifies a different reuse case:
an incidental biological image can inherit a route-linked geographic context
and become a supervised trait record after taxon, flower region, and coordinate
review.

The curated colour database contains 1,965 records (642, 687, and 636 in
2023–2025). Two exact duplicate images were excluded programmatically. The
final complete-case ecological analysis contains 1,923 observations (629, 669,
and 625 by year); the remaining difference reflects missing joint
environmental or nationwide *Bombus* prediction support. Automated QC warnings
were retained in the primary analysis after author review and were excluded
only in sensitivity analyses.

The restricted hiking-route sampling frame also limits the human-context
analysis. Population, roads, and land use can affect both plant exposure and
the probability that a YAMAP user records a flower. Moreover, when candidate
and comparison observations are already drawn from accessible trails, the
range of human accessibility can be narrower than it would be in a
trail-independent survey. Weak human-context contrasts therefore do not rule
out human influence, but they also cannot be interpreted as evidence for it.
