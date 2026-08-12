# Appendix S3. Broad environmental and spatial flower-colour model

## Purpose and inferential boundary

This Appendix documents the finalized broad geographical component of the paper and the model-selection audit used to decide what belongs in that component. The Broad stage asks how long-term abiotic geography and unresolved continuous spatial structure organize two biologically distinct visible-colour responses in *Campanula punctata*:

1. **pigmentation state** — whether an observation lies in the white-like or visibly pigmented regime; and
2. **conditional visible intensity** — how strong the visible red–green signal is after the flower has entered the pigmented regime.

The Broad stage contains measured environment and continuous space only. Bombus SDMs are not inserted as national environmental covariates, and human-context variables do not enter this stage.

Two modelling layers remain conceptually distinct but are now explicitly aligned where the paper requires them to connect.

- The **observation-level INLA-SPDE models** estimate broad environmental associations, response-specific interactions and residual spatial covariance.
- The **1-km-cell cross-fitted pigmentation-state reference** generates natural predictive maps for Main 3. Its primary environmental basis is now the same finalized eight measured abiotic axes used by the Broad state analysis. The current downstream event detector therefore uses the finalized eight-axis state space and yields 16 observed local-departure candidates. The historical four-PC broad/within-neighbourhood formulation and its 17-candidate set are retained only as sensitivity/provenance in Appendix S6 and under `legacy/`.

The image response is a reproducible display-referred CIELAB phenotype. Neither pigmentation state nor conditional a* intensity is interpreted as a direct assay of anthocyanin concentration, anthocyanin identity, vacuolar pH, spectral reflectance, ultraviolet contrast, petal temperature or Bombus colour contrast.

## Analysis population and record flow

All records recovered within the predefined 2023–2025 YAMAP keyword frame were taken through author visual screening rather than subsampled. Taxonomic misidentifications and non-focal campanuloid subjects were excluded before the screened source table was frozen. The focal flower and usable petal region were confirmed; identical-coordinate records were checked against photographs, dates and activity provenance; exact-image duplication was audited separately by SHA-256.

The final record flow was:

- 1,965 author-screened eligible source records;
- minus one later exact-image duplicate;
- minus 40 records without complete topographic raster support;
- minus two records without complete soil support;
- **1,922 observations in the environment-complete integrated analysis**.

The final population comprised 966 white-like and 956 pigmented observations in 1,305 unique 1-km cells. Among these, 674 cells contained at least one pigmented observation and therefore had a finite cell-level conditional-intensity response.

The observation-level responses were:

- **pigmentation state:** Bernoulli white-like/pigmented classification for all 1,922 observations;
- **conditional visible intensity:** standardized `max(a* - 4.968780, 0)` among the 956 pigmented observations only.

White-like observations do not receive a conditional-intensity value.

## Finalized abiotic predictor space

Environmental compression preceded the flower-colour models and was response-blind. Component signs are arbitrary but frozen. All terms were standardized over the corresponding analysis population.

**Table S3.1. Final eight measured abiotic axes.**

| Model term | Source variables / definition | Increasing score represents |
|---|---|---|
| Temperature PC1 | BIO5 0.574; BIO10 0.583; GDD5 0.575 | warmer warm-season conditions and greater thermal accumulation |
| Precipitation PC1 | climatic moisture index 0.593; BIO12 0.594; BIO14 0.544 | wetter/moister annual and dry-period climate |
| Temperature seasonality | standardized BIO4 | greater annual temperature variability |
| Precipitation seasonality | standardized BIO15 | greater annual precipitation variability |
| Topography PC1 | roughness 0.590; slope 0.571; TRI 0.571 | steeper terrain and greater local elevation relief |
| Soil PC1 | frozen SoilGrids resource/chemistry rotation | higher organic/nutrient values and lower bulk density/pH at the positive end |
| Soil PC2 | frozen SoilGrids texture rotation | mainly silt-positive, sand-negative texture |
| RSDS | standardized surface downwelling shortwave radiation | greater incident shortwave radiation |

A structural East/West factor was retained at the observation level, with longitude >=136.5° E assigned to `East`. It is a geographical adjustment rather than an ecological mechanism or inferred genetic boundary. Elevation was not added as a further fixed effect because it jointly proxies thermal, hydrological, radiative and regional gradients that the model attempts to distinguish.

The ecological interpretation of the predictor set was fixed before model extension. Temperature represented the primary directional hypothesis; precipitation/moisture represented climatic water supply; BIO4 and BIO15 represented long-term variability; RSDS represented shortwave/light context rather than UV-B; Topography PC1 represented terrain relief rather than elevation itself; and the soil PCs represented broad resource/texture context. These national layers are not flower-level measurements of developmental weather, water status, illumination or root-zone chemistry.

## Observation-level INLA-SPDE specification

Pigmentation state used a Bernoulli likelihood with logit link and conditional intensity a Gaussian likelihood. Coordinates were transformed to a Japan-centred Lambert azimuthal equal-area projection and expressed in kilometres.

The reference stationary SPDE used:

- inner/outer mesh maximum edge lengths: 20/100 km;
- point cutoff: 5 km;
- Matérn alpha = 2;
- PC prior `P(range < 100 km)=0.05`;
- PC prior `P(spatial SD > 1)=0.05`.

The state mesh contained 5,753 vertices and the intensity mesh 5,144 vertices.

A model extension was promoted only when it had an ecological interpretation, acceptable collinearity, improved geographically blocked predictive loss, positive spatial-block bootstrap support and improvement in at least four of five response-blind geographical folds. WAIC or a concentrated posterior alone was insufficient.

### Collinearity policy

VIF was a graded stability diagnostic rather than a mechanical deletion threshold:

- **<5:** preferred;
- **5–10:** admissible only with explicit coefficient, blocked-transfer and spatial-hyperparameter stability;
- **>10:** not promoted without exceptional mechanistic and predictive justification.

The final pigmentation-state model remained below the preferred range. In the final conditional-intensity model the maximum VIF was approximately 6.34, driven mainly by Temperature PC1 (6.34) and Soil PC1 (5.23); the retained Temperature PC1 × temperature-seasonality interaction itself had VIF 1.66. Removing the East/West structural adjustment reduced maximum VIF below 5 but worsened WAIC by about 5.9 units and did not provide robust transfer gain. Terms were therefore not deleted merely to cross an arbitrary VIF=5 threshold.

## Interaction audit

Two complementary screens were used:

1. a mechanism-prioritized set of ten interactions motivated by thermal regulation, climatic variability, water/radiation co-stress, terrain context and substrate buffering; and
2. an exhaustive guardrail fitting all `choose(8,2)=28` pairwise products among the eight environmental axes one at a time.

Both screens retained the same response-specific likelihood, fixed main effects, SPDE mesh/priors and five geographical folds.

### Pigmentation state

No interaction satisfied the complete promotion rule. The strongest mechanism-prioritized candidate was climatic dryness × RSDS: posterior mean 0.317 (95% CrI 0.115–0.519) and held-out log loss improved in four of five folds, but the spatial-block bootstrap interval for predictive gain crossed zero. The all-28 audit likewise did not justify replacing the additive state model.

The final pigmentation-state model is therefore **additive**. Dryness × radiation is retained only as a suggestive co-stress sensitivity; RSDS is not UV-B and long-term climatic dryness is not flower-level water stress.

### Conditional visible intensity

The mechanism screen identified a transferable Temperature PC1 × temperature-seasonality interaction. Its posterior mean was -0.204 (95% CrI -0.302 to -0.107; mechanism-screen BH=0.00043), WAIC improved by approximately 5.9 units relative to the additive model, held-out squared error improved in four of five folds, and the spatial-block bootstrap interval remained above zero.

The exhaustive 28-pair audit also identified precipitation PC1 × temperature seasonality. A narrow joint adjudication showed that this exhaustive-only interaction collapsed after the predeclared thermal interaction was included: Temperature PC1 × temperature seasonality remained -0.196 (95% CrI -0.319 to -0.075), whereas precipitation PC1 × temperature seasonality became +0.015 (95% CrI -0.125 to +0.153). The final intensity extension therefore contains **Temperature PC1 × temperature seasonality only**.

The negative interaction means that the warm-climate decline in visible intensity becomes stronger as long-term temperature seasonality increases. This is an interacting geographical climate context, not evidence that temperature fluctuations directly caused anthocyanin expression in the photographed flowers.

## Hydroclimate-completeness sensitivity

CHELSA VPD and site water balance (SWB) were the highest-priority same-resolution omissions because the primary precipitation PC represents water supply more directly than atmospheric demand.

For pigmentation state, VPD did not improve held-out prediction and raised maximum VIF to approximately 25.9. SWB, VPD+SWB and hydroclimate replacement variants likewise failed to improve geographical transfer.

For conditional intensity, adding VPD improved in-sample WAIC in an additive model but did not improve held-out prediction and raised maximum VIF to approximately 25.8. SWB worsened both WAIC and transfer; VPD+SWB retained severe collinearity; hydroclimate replacement also worsened transfer. VPD is biologically plausible but does not provide stable independent information within the present Japanese sampling geography, so no extra atmospheric-demand mechanism was promoted.

## Final fixed effects

**Table S3.2. Final pigmentation-state fixed effects.** Posterior mean and 95% CrI on the logit scale.

| Term | Mean | 95% CrI |
|---|---:|---:|
| Intercept | -0.597 | -1.942 to 0.727 |
| East versus West | -0.020 | -1.422 to 1.336 |
| Temperature PC1 | **-0.542** | **-1.033 to -0.049** |
| Precipitation PC1 | -0.409 | -0.848 to 0.026 |
| Temperature seasonality | 0.409 | -0.070 to 0.893 |
| Precipitation seasonality | 0.158 | -0.368 to 0.675 |
| Topography PC1 | -0.188 | -0.417 to 0.042 |
| Soil PC1 | -0.181 | -0.630 to 0.270 |
| Soil PC2 | 0.120 | -0.103 to 0.345 |
| RSDS | 0.004 | -0.209 to 0.218 |

A one-SD shift toward warmer Temperature PC1 corresponds to an odds ratio of approximately `exp(-0.542)=0.58`. The precipitation coefficient points toward greater pigmentation at the drier end but remains uncertain after continuous space is included.

**Table S3.3. Final conditional-intensity fixed effects.** Posterior mean and 95% CrI in standardized visible-intensity units; main effects are conditional on interacting variables being at their standardized reference values.

| Term | Mean | 95% CrI |
|---|---:|---:|
| Intercept | 0.257 | -0.017 to 0.513 |
| East versus West | **-0.379** | **-0.709 to -0.003** |
| Temperature PC1 | -0.084 | -0.274 to 0.106 |
| Precipitation PC1 | **-0.174** | **-0.323 to -0.024** |
| Temperature seasonality | **0.207** | **0.044 to 0.369** |
| Precipitation seasonality | -0.057 | -0.197 to 0.089 |
| Topography PC1 | **-0.134** | **-0.224 to -0.043** |
| Soil PC1 | 0.063 | -0.114 to 0.239 |
| Soil PC2 | 0.012 | -0.081 to 0.104 |
| RSDS | 0.026 | -0.048 to 0.099 |
| Temperature PC1 × temperature seasonality | **-0.204** | **-0.302 to -0.107** |

There is therefore no single constant national temperature slope for already-pigmented flowers. The warm-climate decline strengthens with increasing temperature seasonality. Conditional intensity is also lower toward wetter/moister climatic geography and toward steeper, greater-relief terrain after other measured terms and continuous space are included.

## Spatial-model audit and residual geography

The stationary + East/West reference was compared with stationary models without East/West and coastline-barrier SPDE variants with and without East/West.

For pigmentation state, removing East/West produced only a very small held-out improvement whose spatial-block bootstrap interval crossed zero. Coastline-barrier models worsened both WAIC and held-out prediction; barrier + East/West worsened held-out log loss by approximately 0.0049 and improved only one of five folds.

For conditional intensity, using the retained thermal interaction, removing East/West worsened WAIC by approximately 5.9 units. A coastline barrier improved full-data WAIC by approximately 2.5 units but improved held-out squared error by only 0.00065; its spatial-block bootstrap interval crossed zero and only three of five folds improved. The final observation-level models therefore retain the stationary Matérn field + East/West structural adjustment.

**Table S3.4. Spatial hyperparameters for the final observation-level models.**

| Response | Final fixed-effect structure | Mean range, km | 95% CrI, km | Mean spatial SD | 95% CrI |
|---|---|---:|---:|---:|---:|
| Pigmentation state | additive environmental model | 132.76 | 88.78–195.68 | 2.105 | 1.629–2.696 |
| Conditional visible intensity | environment + Temperature PC1 × temperature seasonality | 65.72 | 31.05–132.63 | 0.357 | 0.236–0.501 |

The ranges are descriptive residual spatial scales, not seed or pollen dispersal distances. Historical allozyme work documenting mainland–Izu differentiation, regional mating-system differences and progressive island colonization makes unresolved biogeographic structure biologically plausible, but the present spatial field may also contain unmeasured environment and sampling geography and is not assigned to a single mechanism.

## Current 1-km natural reference passed to Main 3

The downstream predictive layer is distinct from the observation-level coefficient model but has now been deliberately aligned with the finalized pigmentation-state environment.

The primary 1-km model uses:

- binomial pigmentation counts for the 1,305 environment-complete cells;
- the same eight measured abiotic axes listed in Table S3.1;
- five response-blind approximately 100-km geographical folds;
- a cross-fitted SPDE natural reference; and
- **10,000 checksum-locked predictive maps** under the frozen analysis geometry.

Main 3 defines environmental similarity using root-mean-square Euclidean distance across those same eight standardized axes, with radius 10 km, RMS caliper <=1 and at least three eligible neighbours. East/West is not an abiotic matching dimension; it remains a structural geographical adjustment. Human variables do not enter the natural model, matching graph, candidate selection or ranking.

Under this current-Broad definition, the observed data contain **16** local pigmented departures. Replaying the identical event detector over 10,000 natural maps gives a null mean candidate count of 13.5908 (95% interval 7–21; Monte Carlo P=0.27897) and a candidate-fraction upper-tail P=0.12609. Full event and post-selection human results are reported in Appendix S6.

The former broad/within-50-km four-PC representation, its 17-candidate set and associated 2026-08-09/11 outputs are not alternative current primaries. They are retained only as sensitivity/provenance under Appendix S6 and `legacy/reproducibility-development/`.

## Ecological interpretation and claim ceiling

The final Broad result is response specific.

- **Pigmentation state:** the clearest measured environmental signal is a broad cool-climate association; no interaction passed the complete promotion rule.
- **Conditional intensity:** the temperature association is context dependent, becoming more negative with increasing thermal seasonality; wetter/moister climate and greater terrain relief are associated with weaker visible intensity after spatial adjustment.
- **Residual geography:** substantial continuous regional structure remains and may combine unmeasured environment, population history, dispersal and sampling geography.

The model does not show that long-term climate directly caused the colour of an individual photographed flower, does not infer genetic adaptation from spatial association, and does not turn an environment-derived Bombus SDM into an independent national causal predictor. Those limits motivate the separate local Bombus design in Appendix S5 and the natural-map/post-selection human design in Appendix S6.

## Remaining biological gaps

The current Broad model does not resolve petal anthocyanin chemistry, flower-level UV/light, dated developmental weather, root-zone soil chemistry, lineage/genomic structure or species-specific dispersal kernels. Spectroscopy and pigment assays, weather-window analyses, field microenvironment measurements and population-genetic sampling are required for those mechanisms.

## Reproducibility resources

Current Broad evidence is locked in:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`;
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`;
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`;
- `reproducibility/environment_interaction_inla_screen_spec_2026-08-11.md`;
- `reproducibility/broad_environment_spatial_audit_spec_2026-08-11.md`;
- `reproducibility/broad_environment_variable_evidence_registry_2026-08-11.csv`.

The integrated downstream handoff is locked in:

- `reproducibility/final_integrated_pipeline_2026-08-12.md`;
- `reproducibility/current_broad_human_primary_2026-08-12.md`;
- Appendix S6.

Superseded four-PC/17-candidate implementations and numerical records have been moved out of the current interface to `legacy/reproducibility-development/`.
