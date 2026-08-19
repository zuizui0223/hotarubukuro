# Appendix S2. Image colour extraction and two-part flower-colour phenotype

## Scope and inference ceiling

The image pipeline estimates a reproducible **human-visible, display-referred colour phenotype** from the validated focal flower/petal region. It does not measure calibrated spectral reflectance, ultraviolet contrast, pigment concentration or anthocyanin chemistry. The primary colour values therefore describe optical appearance in the available post-processed sRGB images; physiological and pollinator-vision interpretations require spectroscopy or pigment assays.

The phenotype was constructed before geography, environmental predictors, Bombus surfaces, human-context variables or model residuals were used. The primary representation separates:

1. **pigmentation state** — white-like versus visibly pigmented; and
2. **conditional visible intensity** — variation in a* above the operational pigmentation boundary, analysed only among observations classified as pigmented.

This separation prevents small red-green variation among white photographs from being treated as a continuous pigment-amount scale.

## Taxonomic scope: hotarubukuro and yamahotarubukuro

The image analysis did not distinguish the Japanese forms commonly called ホタルブクロ (hotarubukuro) and ヤマホタルブクロ (yamahotarubukuro; *Campanula punctata* var. *hondoensis*). Their traditional morphological distinction is concentrated in calyx characters rather than the corolla-colour trait analysed here, especially the presence versus absence of appendages between the calyx lobes. Those calyx characters are not consistently visible in opportunistic flower photographs, whereas the corolla used for colour phenotyping is visible by design. We therefore treated both forms as one *C. punctata* sensu lato analytical unit rather than assigning image records from incomplete calyx views.

This pooling is also consistent with preliminary unpublished genetic data from the study system, in which morphologically assigned hotarubukuro and yamahotarubukuro did not show clear genetic differentiation. Those unpublished data are not analysed or presented as a result in this paper; they are stated only as supporting rationale for not imposing a calyx-based taxonomic split on the image dataset. This analytical treatment is not a formal taxonomic revision, and voucher-based genomic work could later reassess whether the forms differ in ancestry or flower-colour geography.

## Data lineage and auditability

The public derived table retains source-row, date, coordinate, observation, image-hash, colour-method and quality-control provenance while excluding local file paths, diagnostic-image paths and raw activity URLs. Coordinates are carried from the source workbook under an assumed EPSG:4326 reference and are not presented as independently re-georeferenced because no separate GPX/photo-timestamp manifest was available.

**Table S2.1. Source-to-phenotype record flow.** Counts distinguish the complete source table from the YAMAP-only benchmark subset.

| Stage | Records / images | Interpretation |
|---|---:|---|
| Complete eligible source table | 1,965 records | 1,964 YAMAP activity-photo rows plus one field/other record |
| Unique image hashes in complete source table | 1,964 | One later exact-hash copy is marked by stable source-row order |
| YAMAP-only benchmark subset | 1,964 rows; 1,963 unique hashes | Counts used in Appendix S1 |
| Final phenotype-analysis observations | 1,922 | Current reconstructed two-part phenotype population |
| White-like observations | 966 | Primary univariate a* mixture classification |
| Pigmented observations | 956 | Primary univariate a* mixture classification |
| Ambiguity-flagged observations | 124 | Posterior class probability between 0.2 and 0.8; retained in the primary classification |

Exact-image duplicate semantics are deterministic: observations are ordered by numeric source row and original row order, the first occurrence of a SHA-256 image hash is retained as canonical, and only later exact copies are marked. Image-response warning flags were not used as an automatic exclusion rule in the current source reconstruction.

## Deterministic visible-colour extraction

The active extraction configuration is version 2.2.2. Images are decoded as 0–255 sRGB. The primary region combines the validated alpha-bounded flower cut-out with an HSV mask designed to retain white, pink, red and purple petal pixels. The primary statistic is the channel-wise median RGB value within that mask. Candidate joint-peak and exposure-filtered measurements are retained for diagnostics but are not substituted for the primary value.

**Table S2.2. Principal extraction settings.** These settings are fixed in `config/color_extraction.json`.

| Component | Fixed setting |
|---|---|
| Alpha threshold | 128 |
| Morphological kernel | 3 pixels |
| Pink/purple hue range | OpenCV H 90–179 |
| Red hue range | OpenCV H 0–10 |
| Coloured-pixel minimum | saturation 40; value 40 |
| White-pixel rule | saturation <=70; value >=150 |
| Primary mask/statistic | alpha-bounded HSV; channel-wise median RGB |
| Colour conversion | sRGB to CIELAB, D65, 2-degree observer |
| White balance | not applied: no neutral reference or reliable camera white-balance metadata |
| Candidate peak space | CIELAB; DeltaE76 distance |

The extraction records image dimensions, alpha availability, mask size and coverage, component structure, clipping, near-white and shadow fractions, possible warm/cool contamination, multimodality and disagreement among candidate peak estimators. These fields make image limitations inspectable rather than treating the derived RGB triplet as error-free.

The source manifest classified 1,180 records as automated-QC `ok` and 785 as `manual_review_required`. These are extraction diagnostics, not the taxonomic/subject screening described in Appendix S1. All recovered candidates had already been checked for the focal organism and usable flower/petal region; automated warning strata were retained for audit and sensitivity rather than used to define the white-pigmented response.

## CIELAB variables

Median RGB values were converted to CIELAB. The primary boundary variable was a*, the red-green axis. L* and b* were retained as optical descriptors, and chroma was calculated as

`C* = sqrt(a*^2 + b*^2)`.

The use of a* is operational: positive values alone were not assumed to indicate biologically meaningful pigmentation. White photographs can have small positive or negative a* values because of lighting, image processing, petal texture and display-referred colour variation.

## Response-blind pigmentation-state model

A univariate Gaussian mixture was fitted to the 1,922 finite a* values with `mclust`. Candidate models used 1–8 components and equal- or variable-variance univariate parameterizations. BIC selected a four-component variable-variance model. Components were ordered by their fitted a* means, and the largest adjacent mean gap separated two white-colour-noise components from two pigmented components.

For each observation, posterior probabilities of the components assigned to the pigmented regime were summed. The operational decision boundary was the a* value between the two regimes at which the fitted pigmented posterior probability was closest to 0.5. The resulting boundary was **a*=4.968780**. An observation was classified as pigmented when its fitted pigmented probability was at least 0.5.

**Table S2.3. Selected univariate a* mixture.**

| Component | Mean a* | SD a* | Mixture proportion | Assigned regime |
|---:|---:|---:|---:|---|
| 1 | -2.380341 | 4.140661 | 0.296566 | white-colour noise |
| 2 | -1.037856 | 1.453589 | 0.180553 | white-colour noise |
| 3 | 17.269477 | 10.381630 | 0.256821 | pigmented |
| 4 | 29.926855 | 12.056672 | 0.266059 | pigmented |

The gap between the second and third component means was 18.307333 a* units. The classification yielded 966 white-like and 956 pigmented observations.

## Ambiguity and alternative optical classifications

The primary 0.5 posterior classification retains all observations. A separate confidence flag identifies observations with pigmented probability >=0.8 or <=0.2; 124 observations between those bounds are marked ambiguous rather than deleted.

**Table S2.4. Classification diagnostics.** Agreement is calculated against the primary four-component univariate a* classification.

| Diagnostic rule | White | Pigmented | Ambiguous / unavailable | Agreement with primary |
|---|---:|---:|---:|---:|
| Primary univariate a* mixture, posterior >=0.5 | 966 | 956 | 0 | reference |
| High-confidence subset, posterior >=0.8 or <=0.2 | 885 | 913 | 124 | 100% among classified observations |
| Joint standardized a*, -L* and C* mixture | 953 | 969 | 0 | 99.0% |
| Naive a*>0 rule | 713 | 1,209 | 0 | 86.8% |

The naive zero rule labelled 253 non-negative observations below the fitted a*=4.968780 boundary as pigmented. This is why a*>0 is retained only as a sensitivity rule and not used as the biological state definition. A multivariate optical mixture based on standardized a*, darkness (-L*) and C* differed from the primary state classification for only 19 of 1,922 observations.

## Conditional visible intensity

For each observation, visible pigment excess was defined as

`pigment excess = max(a* - 4.968780, 0)`.

This quantity was standardized only across the 956 observations assigned to the pigmented regime, producing the conditional intensity response used in the broad geographical model. White-like observations have no value for this second response. Consequently, a process can correspond to whether a visible pigmentation state is maintained without implying progressively darker colour among already pigmented flowers.

## Reproducibility resources

Primary data and source-build files:

- `Data_S1.csv` — public derived trait/provenance table;
- `Code_S1.py` — public extraction entry point;
- `config/color_extraction.json` — fixed extraction parameters;
- `source_build/extract_color.py` — deterministic colour extraction;
- `source_build/build_data_s1.py` — public table, hashes and manifest construction;
- `data/processed/Data_S1_v2_manifest.json` — record counts, schema, hashes, software versions and colour-method configuration.

Current phenotype outputs are produced by the checksum-locked fresh reconstruction and include:

- `pigmentation_measurement_summary.csv`;
- `pigmentation_mixture_components.csv`;
- `pigmentation_joint_lab_components.csv`;
- `pigmentation_measurement_observations.csv`;
- `pigmentation_classification_sensitivity.csv`.

The manuscript claim is limited to an auditable two-part visible-colour phenotype. Mapping these classes to anthocyanin concentration, floral reflectance under controlled illumination or Bombus visual contrast remains a field/laboratory validation task.