# Appendix, archive, and optional Supporting Information plan

Target: *Ecology and Evolution*, Research Article

Fallback: *AoB PLANTS*, Study

Decision date: 2026-07-24

## Packaging decision

The initial *Ecology and Evolution* submission uses three in-article appendices
for information required to evaluate the headline inferences:

- **Appendix A — Sampling and optical-measurement safeguards**
- **Appendix B — National and predicted-community model specification**
- **Appendix C — Local reference tests and human-context separation**

These appendices are included and cross-referenced in
`ecology-and-evolution-manuscript.md`. Do not create a large general supplement
that duplicates them. Large machine-readable diagnostics belong in the
DOI-bearing archive.

Create an optional Supporting Information S1 PDF only if the editor or upload
limit requires secondary visual diagnostics to be separated. No method, result,
or caveat needed to evaluate a main claim may exist only in S1.

## DOI archive inventory

### Observation and optical phenotype

- permitted observation-level `Data_S1.csv`;
- data dictionary, missing-value rules, units, QC fields, and release limits;
- sampling years and window, provenance status, duplicate audit, and manual
  taxon/region confirmation protocol;
- mixture components, a* boundary, ambiguity flags, and warning-free
  sensitivity;
- no automatic redistribution of original YAMAP photographs.

### Environment and national models

- public-raster versions, URLs, native resolution, preprocessing, and checksums;
- response-blind PC loadings, retained-axis rationale, correlation and VIF
  diagnostics;
- INLA formulae, priors, mesh settings, posterior fixed effects, and spatial
  hyperparameters;
- geographical fold definitions, foldwise performance, calibration, and
  predictive coverage;
- 25-, 50-, and 100-km environmental-scale sensitivity outputs.

### Predicted *Bombus* community

- GBIF query metadata and cleaned occurrence counts;
- ENMeval tuning grid, selected feature classes and regularization, AICc, and
  spatial validation;
- prediction hashes and evidence that final predictions were generated from
  selected models rather than reused TIFF files;
- fingerprint definitions, common support, environmental orthogonalization,
  and block-level national comparison;
- explicit statement that suitability is not abundance, visitation, richness,
  or pollination effectiveness.

### Local and human-context tests

- fixed pair graphs at 10, 25, and 50 km and pair-support audit;
- observed partial statistics, all 1,000 natural-map reference values,
  Monte Carlo probabilities, and BH family;
- isolate definition, matching settings, candidate table, count/fraction
  reference distributions, and membership sensitivity;
- WorldPop, MLIT land-use, road/interface, and DID contrasts;
- early-flowering and dark-colour follow-up facets;
- raw and maxT-adjusted probabilities with feature-family definitions.

### Reproduction and validation

- complete `R/`, `scripts/`, `validation/`, and test code;
- seeds, input and prediction hashes, stage manifest, and final claim registry;
- `R_session_info.txt` and an environment lock or complete package versions;
- repository commit SHA and dated release manifest;
- clean-run instructions for `verify`, `extensions`, and `full`;
- final figure source data and production figure files.

## Optional S1 contents

If the journal asks for a separate S1 file, keep it lean:

1. optical-classification density and ambiguity sensitivity;
2. environmental correlation/VIF and SPDE mesh diagnostics;
3. foldwise calibration and predictive performance;
4. ENMeval tuning and fingerprint common-support map;
5. local-pair support and 10/25/50-km sensitivity;
6. isolate-definition and maxT human-context sensitivity.

Every S1 item must be independently numbered, cited in the manuscript, checked
against the locked archive, and supplied with accessible captions. Do not add
cross-references until the S1 file actually exists.
