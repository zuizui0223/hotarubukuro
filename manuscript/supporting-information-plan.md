# Supporting Information plan

## Submission unit

Prepare one self-contained **Supporting Information S1** file containing the
extended methods, Tables S1-S12, and Figures S1-S9 below. Number supplementary
items independently from the main article. Publish analysis-ready derived data
and code in the DOI-archived repository rather than duplicating the full
machine-readable release inside the Supporting Information file.

The Supplement should support audit and sensitivity assessment. It should not
carry stronger biological claims than the main text. In particular, predicted
*Bombus* suitability is not abundance or visitation, and the human-context
analyses do not establish horticultural origin, escape, introgression, or
genetic contamination.

## Extended Methods

### Appendix S1 | YAMAP sampling frame and manual screening

- State that the frame was all eligible YAMAP hiking-activity records from
  1 June through 31 August in 2023, 2024, and 2025.
- Document manual confirmation of the extracted subject and petal region,
  duplicate-individual removal, and taxonomic-error removal, including
  *Adenophora triphylla* records misidentified as *Campanula punctata*.
- Explain that the retained occurrences are route-linked hiking observations,
  not an areally balanced vegetation survey.
- Do not reconstruct exclusion counts that were not retained.

### Appendix S2 | Supervised colour extraction and optical phenotype

- Give the deterministic image-decoding, alpha/HSV candidate-pixel, channel-
  wise median RGB, and D65 CIELAB conversion steps.
- State that author-confirmed dark observations and warning-flagged cases were
  retained in the primary data; only non-formable measurements and exact image
  duplicates were programmatic exclusions.
- Describe the response-blind Gaussian-mixture boundary, classification
  confidence flag, and the two-part response: pigmentation presence for all
  observations and standardized a* only among pigmented observations.
- Reiterate that white-flower a* was not analysed as pigment quantity and that
  uncalibrated sRGB is neither anthocyanin concentration nor pollinator colour
  contrast.

### Appendix S3 | Public rasters and scale decomposition

- Give source URL, provider/version, native resolution, download date, checksum,
  resampling rule, common grid, and derived variable for every CHELSA,
  SoilGrids, WorldClim, WorldPop, and MLIT layer.
- Document the response-blind environmental principal components and fixed
  axis signs.
- Show how 50-km broad-scale means and cell-minus-neighbourhood deviations were
  calculated; retain 25- and 100-km decompositions as fixed sensitivities.

### Appendix S4 | National INLA-SPDE models and predictive replication

- Give formulas, likelihoods, links, predictor scaling, mesh construction,
  penalised-complexity priors, and zero-mean field constraint.
- Document five 100-km geographic folds, foldwise standardization, held-out
  predictions, 1,000 outcome replications, and recombination into the
  cross-fitted national mosaics.
- Explain why fixed environmental coefficients are conditional on the spatial
  field and why the SPDE field is unresolved continuous geography rather than
  disposable noise.

### Appendix S5 | ENMeval and the predicted *Bombus* fingerprint

- Give GBIF filtering, coordinate-uncertainty limit, cleaning, deduplication,
  background definition, spatial partitions, feature-class and regularization
  grid, and finite-AICc selection for each of the five species.
- Explicitly state that the final pipeline generated predictions from the
  selected ENMeval models and did not use pre-existing *Bombus* TIFF files.
- Document within-species rank standardization, Hellinger composition,
  principal components, common support, foldwise environmental
  orthogonalization, and same-support national model comparison.
- Treat suitability axes as a predicted community fingerprint, not abundance,
  richness, visitation, or pollination effectiveness.

### Appendix S6 | Local turnover, natural-map events, and human context

- Give the fixed nearest-neighbour graph construction at 25 km and the fixed
  10- and 50-km sensitivity radii.
- Define both flower-colour turnover responses, fingerprint turnover,
  environmental/natural-map adjustment, shared-node handling, and one-sided
  Monte Carlo reference tests.
- Define the 10-km environment-similar all-white-neighbour pigmented-isolate
  event and show that the identical extractor was replayed on every natural
  predictive map.
- Describe the post-selection WorldPop, land-use, DID, early-flowering, and
  dark-colour contrasts and their maxT families.

## Supplementary Tables

| Item | Content | Locked source |
|---|---|---|
| Table S1 | Observation counts by year, retained provenance status, and final analytical exclusions | `Data_S1.csv`; phenotype audit |
| Table S2 | Observation-level data dictionary, units, allowed values, QC flags, and release restrictions | `Data_S1.csv`; `docs/data-dictionary.md` if created |
| Table S3 | Complete public-data provenance, native and analysis resolution, preprocessing, and checksum | `docs/data-sources/public-environment-sources.md`; stage metadata |
| Table S4 | Mixture components, a* boundary, ambiguous-class count, warning-free and rule-sensitivity results | `pigmentation_mixture_components.csv`; `pigmentation_classification_sensitivity.csv`; rule-sensitivity outputs |
| Table S5 | Correlation matrix summary, VIFs, condition indices, and retained predictor rationale | `pigmentation_hurdle_collinearity_*.csv` |
| Table S6 | INLA formulas, priors, mesh settings, posterior fixed effects, and spatial hyperparameters | `pigmentation_hurdle_inla_*.csv`; natural-model metadata |
| Table S7 | Foldwise predictive performance, calibration, and predictive coverage for both response parts | `predictive_replication_model_fold_performance.csv`; `predictive_replication_presence_calibration.csv` |
| Table S8 | Five-species GBIF counts, ENMeval tuning grid, selected feature class and regularization, AICc, and spatial evaluation | `ENMeval_AICc_selection.csv`; ENMeval reports and query metadata |
| Table S9 | Fingerprint-axis definitions, inter-axis correlations, environmental orthogonalization diagnostics, and same-support national contrasts | `predictive_replication_fingerprint_orthogonalization.csv`; paired contrasts |
| Table S10 | Local-pair support and presence/intensity turnover statistics at 10, 25, and 50 km, including pair counts, null replicates, raw p, and BH q | `local_pair_predictive_summary.csv`; `local_pair_predictive_null.csv`; metadata |
| Table S11 | Isolate-definition settings, candidate list, graph support, count/fraction predictive checks, and rank sensitivity | `local_isolate_*.csv`; predictive-replication candidate sensitivity |
| Table S12 | Population-scale, land-use, DID, early-flowering, and dark-colour contrasts with raw and maxT-adjusted probabilities | `human_neighbourhood_*.csv`; `did_*.csv`; auxiliary-facet summaries |

## Supplementary Figures

### Figure S1 | Sampling and measurement audit

Show annual retained counts, the manual-to-script measurement workflow, the
distribution of mask/QC flags, and the final duplicate exclusions. Do not show
or redistribute original YAMAP photographs without confirmed permission.

### Figure S2 | Optical classification robustness

Show fitted mixture-component densities, posterior class confidence, and the
number of records whose class changes across the pre-specified boundary and
warning-free sensitivities. The main Figure 1 histogram need not be repeated.

### Figure S3 | Predictor collinearity and scale structure

Combine a labelled correlation heatmap, VIF/condition diagnostics, and the
loadings of broad-scale and within-neighbourhood environmental axes. This is
the direct reviewer-facing check that biologically interesting predictors were
not removed merely because they share environment or geography.

### Figure S4 | Spatial design

Map the INLA mesh, observation support, and five 100-km geographic folds in a
common WGS84 frame. Add a small panel showing train/test counts per fold.

### Figure S5 | National predictive diagnostics

Show foldwise presence calibration, AUC/Brier distributions, intensity
observed-versus-predicted values, RMSE/MAE, predictive coverage, and simulation
stability. Keep performance interpretation predictive rather than causal.

### Figure S6 | ENMeval and fingerprint construction

For each *Bombus* species, show the selected tuning point and a common-scale
rank-suitability map. Add fingerprint-axis distributions and a correlation
panel. Do not present summed suitability as abundance or species richness.

### Figure S7 | Local-pair design and null support

Show edge-distance and environmental-difference distributions, pair counts for
each response and radius, observed partial statistics against natural-map null
distributions, and fixed 10/25/50-km sensitivity. This figure explains why the
local test is a change-to-change comparison rather than a national species
co-occurrence regression.

### Figure S8 | Isolate-event stability

Show how candidate count, fraction, and membership change across fixed
neighbour-distance, environmental-similarity, support, and extremeness
settings. Overlay the observed primary event on its 1,000-map predictive
reference distribution.

### Figure S9 | Post-selection human-context sensitivity

Show population contrasts at 5, 10, 25, and 50 km, land-use feature families,
DID distance/alignment, raw and maxT probabilities, and the early/dark
candidate facets. Visually separate selection-free candidate definition from
post-selection characterization.

## Repository files accompanying Supporting Information S1

- **Data S1:** `Data_S1.csv`, the curated observation-level derived
  measurement table, subject to the final coordinate/source-identifier release
  decision.
- **Derived cell and edge data:** release the exact 1-km cell, local-pair, and
  candidate tables used for the paper in the DOI archive.
- **Code:** release the complete publication pipeline, environment lock,
  seeds, input/prediction hashes, validation scripts, and figure builders.
  `Code_S1.py` may remain a named georeferencing utility, but the repository
  archive is the authoritative executable record.
- **Excluded material:** do not redistribute original YAMAP photographs
  automatically; document access, permission, and audit-hash policy instead.

## Main-text cross-references to add when the Supplement is built

- Methods 2.1-2.3: Appendices S1-S2, Tables S1-S4, Figures S1-S2.
- Methods 2.4-2.5: Appendices S3-S4, Tables S3 and S5-S7, Figures S3-S5.
- Methods 2.6: Appendix S5, Tables S8-S9, Figure S6.
- Methods 2.7: Appendix S6, Table S10, Figure S7.
- Methods 2.8: Appendix S6, Tables S11-S12, Figures S8-S9.

Add these citations only after the listed items are generated and checked; do
not cite planned supplementary items as if they already exist.
