# Broad residual-spatial structure — biological rationale and adopted rules

Date: 2026-08-11; current interpretation aligned 2026-08-12.

## Purpose

The Broad analysis uses a residual spatial field because measured climate, terrain, soil and radiation do not exhaust the geographical processes that can structure flower colour. The spatial field is deliberately **agnostic**: it adjusts for coherent residual geography without being labelled as a dispersal kernel, genetic history or one specific unmeasured environmental process.

## Species-specific rationale

### Mating system and pollen movement

Mainland Honshu populations in classic Izu comparisons were predominantly self-incompatible and strongly outcrossing, with bumblebees as principal pollinators. Allozyme work reported outcrossing rates of approximately 0.62–0.79 in self-incompatible mainland and Oshima populations. Population connectivity can therefore involve pollen as well as seed movement.

- Inoue, K. & Amano, M. (1986). *Plant Species Biology* 1:89–97. DOI: 10.1111/j.1442-1984.1986.tb00018.x.
- Inoue, K. & Kawahara, T. (1990). *American Journal of Botany* 77:1440–1448. DOI: 10.1002/j.1537-2197.1990.tb12554.x.

### Regional differentiation and island history

The allozyme study found clear mainland–Izu differentiation and stronger among-population differentiation in the island group. Genetic and geological evidence also supported progressive southward island colonization. These results make residual regional structure biologically plausible, while not identifying the latent field in the current photographic data with those historical populations.

### Seed biology and dispersal uncertainty

Direct quantitative seed-dispersal kernels for Japanese *C. punctata* are not available in the evidence base used here. Geographic variation in thermal germination responses is documented, but germination ecology does not supply a nationwide dispersal parameter.

- Inoue, K. & Washitani, I. (1989). Geographical variation in thermal germination responses in *Campanula punctata*. *Plant Species Biology* 4:69–74. DOI: 10.1111/j.1442-1984.1989.tb00049.x.

The Broad model therefore does not impose a species-specific migration or resistance surface unsupported by the data.

## Adopted stationary SPDE interpretation

The observation-level model uses a stationary, approximately isotropic Matérn field in a Japan-centred equal-area projection. It provides a conservative residual-geography adjustment because it:

- allows continuously decaying spatial covariance without defining arbitrary genetic clusters;
- separates measured environmental coefficients from broad unresolved geography;
- does not require unobserved dispersal or population-genetic parameters.

Limitations are explicit:

- covariance is based on projected distance rather than realized connectivity;
- one range/variance process applies across the domain;
- coastline, sea gaps and corridors are not explicit in the adopted field;
- the field may absorb unmeasured environment, population history, sampling geography and their combination.

The inferred range is a residual-correlation scale, not a seed-, pollen- or colonization-distance estimate.

## Spatial alternatives evaluated

### East/West structural adjustment

The 136.5°E East/West factor is treated as a broad structural adjustment rather than a demonstrated genetic boundary. It is retained because removing it did not provide sufficiently robust geographical-transfer improvement and worsened the conditional-intensity full-fit evidence.

### Coastline-barrier SPDE

A coastline-barrier field was biologically motivated by island separation and population-genetic evidence. It was evaluated as a covariance sensitivity, not as a claim that all movement follows terrestrial paths.

For pigmentation state, barrier formulations worsened both full-fit and held-out evidence. For conditional intensity, the barrier model produced a small WAIC improvement but not a robust, geographically transferable predictive gain. The stationary field is therefore retained.

### More complex spatial structures

Free anisotropy, nonstationary range, genetic-cluster random effects and resistance surfaces are not included because the current data lack independent evidence needed to identify those additional structures without conflating them with unmeasured environment or sampling geography.

## Model-retention rule

A more complex spatial specification is promoted only if it jointly:

1. fits successfully;
2. improves full-fit evidence meaningfully;
3. improves geographically blocked primary predictive loss;
4. has a spatial-block bootstrap interval supporting positive predictive gain;
5. improves most geographical folds;
6. preserves acceptable fixed-effect collinearity and numerical stability;
7. has a biological interpretation supported independently of the flower-colour response.

This rule makes predictive transfer and biological defensibility necessary, rather than choosing a covariance model from fit statistics alone.

## Adopted spatial results

### Pigmentation state

- stationary Matérn + East/West retained;
- posterior range: **132.76 km**;
- 95% CrI: **88.78–195.68 km**;
- spatial SD: **2.105**;
- 95% CrI: **1.629–2.696**.

### Conditional visible intensity

- stationary Matérn + East/West retained;
- Temperature PC1 × temperature-seasonality retained in the fixed effects;
- posterior range: **65.72 km**;
- 95% CrI: **31.05–132.63 km**;
- spatial SD: **0.357**;
- 95% CrI: **0.236–0.501**.

The range contrast between state and intensity is descriptive because the responses use different likelihoods and analysis subsets.

## Relationship to the rest of the paper

The adopted analysis separates:

- fixed Broad environmental terms: measured long-term abiotic geography;
- stationary Matérn field: unresolved coherent geography after measured environment;
- local Bombus test: a directional biotic hypothesis tested at sharp nearby boundaries;
- local-departure analysis: final-eight-axis ecological events calibrated on 10,000 predictive maps;
- human context: post-selection characterization only.

The downstream event analysis uses the finalized eight-axis natural reference and yields **16 observed candidates**. Human variables do not enter event selection.

## Claim ceiling

The residual field improves spatially responsible inference but does not identify its biological source. Population history, dispersal limitation, lineage structure, unmeasured microenvironment and sampling structure remain testable explanations that require independent data.

## Evidence records

Final Broad coefficients and hyperparameters are frozen in:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`;
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`;
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`;
- `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`.
