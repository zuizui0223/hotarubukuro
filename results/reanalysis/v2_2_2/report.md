# Reviewed public-data reanalysis

Analysis version: `public-reanalysis-v1`.

## Scope

The analysis starts from Data_S1 extraction version 2.2.2, hard-excludes 2 unresolved duplicate-photo/different-coordinate record(s), verifies the complete 30 arc-second public raster manifest, and attaches every raster by `analysis_id`.

The primary response is display-referred CIELAB a* (red-green). L* and b* are illumination/exposure diagnostics. The global-reference colour PC1 is retained only for descriptive method comparison and is not cross-validated, because fitting it on all records would leak test-fold outcome information. No result is a calibrated pigment, reflectance, pollinator-vision, adaptation, or causal estimate.

## Row flow

- Primary inclusive photograph records: 1963.
- Primary strict-OK photograph records: 1180.
- Exact-site sensitivity records: 1925.
- Public predictor layers validated: 19.
- Candidate predictor maximum VIF: 95.56; retained six-predictor maximum VIF: 4.786.
- Bombus-complete primary records: 1887; missing: 76.
- All-colour-method common inclusive records: 1922.
- Unique-coordinate fold sizes: 385, 385, 385, 385, 385.

## Descriptive model status

Full-cohort environment-only blocked Q2 for primary a: 0.2156. On the identical Bombus-complete cohort, environment-only Q2 is 0.2337 and environment-plus-Bombus Q2 is 0.2521.

These are descriptive standardized linear models. Naive standard errors are included only as diagnostics and do not correct spatial dependence. 5 equal-site, principal-geographic-axis bands assess prediction sensitivity; no buffer is imposed and the result is not causal. Pooled Q2 uses each fold's training mean as its baseline, and fold-level/macro metrics are retained.

## Required caveats

- White balance is not identifiable from the post-processed cut-outs and was not applied.
- Manual adjudication of 785 automated review flags is incomplete; inclusive and strict-OK results are both retained.
- Workbook coordinates were remapped to images but not independently recomputed from GPX.
- The five Bombus rasters are structurally valid but their model-generation provenance is legacy_unverifiable; their sum is suitability, not richness.
- Bombus suitability reuses environmental information and is therefore shown only as an additional sensitivity predictor.
- WorldPop 2020 is treated mainly as a human access/sampling-intensity proxy, not a causal ecological driver.
- CHELSA 1981–2010 climatology and WorldPop 2020 do not match individual photograph dates; season, camera, illumination, and non-random citizen-photo sampling remain uncontrolled.
- SoilGrids uses a mutable `latest` endpoint; this run is pinned by the retrieved-file hashes in the raster manifest.
