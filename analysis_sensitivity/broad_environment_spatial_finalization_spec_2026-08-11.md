# Broad residual-spatial structure: biological rationale and finalization rules

Date: 2026-08-11

## Why a residual spatial field is biologically necessary

The broad flower-colour analysis cannot treat residual geography as disposable autocorrelation. In *Campanula punctata*, population history, mating system and dispersal can generate persistent spatial covariance in traits even after measured climate and terrain are fitted. At the same time, available natural-history evidence is not rich enough to encode one explicit nationwide migration graph or genetic cluster map. The Matérn field is therefore used as an **agnostic residual biogeographic component**, not as a dispersal kernel or a direct estimate of genetic history.

## Species-specific evidence

### Mainland mating and pollen movement potential

Mainland Honshu populations in the classic Izu comparison were predominantly highly self-incompatible and strongly outcrossing, with bumblebees as principal pollinators. The allozyme study reported mean outcrossing rates of approximately 0.62-0.79 in self-incompatible mainland and Oshima populations. This means that realized population connectivity cannot be inferred from seed movement alone: pollen-mediated gene flow is biologically important.

- Inoue, K. & Amano, M. (1986). *Plant Species Biology* 1:89-97. DOI: 10.1111/j.1442-1984.1986.tb00018.x.
- Inoue, K. & Kawahara, T. (1990). *American Journal of Botany* 77:1440-1448. DOI: 10.1002/j.1537-2197.1990.tb12554.x.

### Genetic differentiation and island colonization

The same allozyme study found clear mainland-versus-Izu differentiation. Nei genetic identity was approximately 0.97 among mainland populations, 0.95 among island populations and 0.84 between mainland and island groups. About 14% of gene diversity was among mainland populations versus 31% among island populations. Island genetic diversity declined with distance from the mainland, and genetic plus geological evidence supported an older northern-island foundation followed by progressive southward dispersal.

This evidence establishes two points relevant to the present spatial model:

1. mainland populations can retain broad spatial genetic structure even in an outcrossing system;
2. sea gaps and stepping-stone island history can produce discontinuities that Euclidean distance alone may smooth across.

It does **not** imply that the current photographic sample contains the historic Izu allozyme populations or that a current SPDE field can be labelled that genetic structure.

### Seed biology and dispersal uncertainty

Direct quantitative seed-dispersal distances for Japanese *C. punctata* were not located in the literature audit. A recent Korean seed study measured very small seeds (about 1.05 × 0.67 mm; 1000-seed mass about 0.0577 g) with underdeveloped embryos and strong improvement of germination after cold stratification. Earlier work specifically documented geographic variation in thermal germination responses in *C. punctata*. These observations support strong environmental filtering across the recruitment stage but do not supply a dispersal kernel.

- Inoue, K. & Washitani, I. (1989). Geographical variation in thermal germination responses in *Campanula punctata*. *Plant Species Biology* 4:69-74. DOI: 10.1111/j.1442-1984.1989.tb00049.x.
- Comparison of seed germination traits of five herbaceous perennial Campanulaceae species native to the Korean Peninsula (2025), *Agronomy* 15:2884.

Related Campanula studies show that small capsule-dispersed seeds can be deposited largely near parent plants and that pollen can dominate interpopulation gene flow, but those distances are not transferred numerically to *C. punctata*.

## What the current stationary SPDE assumes

The observation-level model currently uses a stationary, approximately isotropic Matérn field in a Japan-centred equal-area projection. This is a reasonable default nuisance model because it:

- allows continuously decaying spatial covariance without defining arbitrary genetic clusters;
- separates measured environmental coefficients from broad unresolved geography;
- does not require unobserved dispersal or population-genetic parameters.

Its limitations are equally explicit:

- covariance depends on Euclidean distance rather than land connectivity;
- the same range/variance process applies across the study domain;
- coastlines, sea gaps and mountain corridors are not explicit barriers;
- the field can absorb unmeasured environment, population history, sampling geography and their combination.

The inferred spatial range is therefore a model-based residual-correlation scale, not a seed-dispersal, pollen-dispersal or colonization distance.

## Spatial alternatives considered

### 1. Structural East/West factor

The current 136.5°E East/West factor is a broad adjustment and has no demonstrated genetic boundary at that longitude. It should remain only if it improves transfer or stabilizes the residual model after the continuous spatial field is present. The finalization sensitivity compares stationary models with and without this factor.

### 2. Coastline-barrier SPDE

A barrier SPDE is the most biologically defensible alternative to test because sea is a real connectivity discontinuity and species-specific island genetics demonstrate strong mainland-island differentiation. The barrier model assigns sea triangles a reduced effective range rather than treating them as complete walls. The finalization sensitivity compares:

- stationary SPDE + East/West;
- stationary SPDE without East/West;
- coastline-barrier SPDE + East/West;
- coastline-barrier SPDE without East/West.

The barrier is a spatial covariance sensitivity, not a claim that all dispersal follows terrestrial paths.

### 3. Anisotropy

Japan has an elongated geography, but the literature audit did not identify a species-specific nationwide directional dispersal process or genetic anisotropy that would justify estimating a free anisotropic covariance solely from the colour data. Anisotropy is therefore not promoted unless stationary/barrier residual diagnostics reveal a systematic directional failure.

### 4. Nonstationary spatial range

Regional mating-system differences and island history make nonstationarity biologically possible. However, a free nonstationary field would be difficult to distinguish from unmeasured environment and platform sampling with the present opportunistic imagery. It is not used as a primary model without independent genetic/environmental covariates.

### 5. Genetic clusters or resistance surfaces

Nationwide population-genomic data linked to the 1,922 observations are not available in the current paper. No genetic-cluster random effect or landscape-resistance surface is therefore introduced post hoc. The spatial field remains deliberately agnostic until genetic sampling can test whether residual flower-colour geography follows isolation by distance, range history or particular barriers.

## Final spatial-model selection rule

The stationary + East/West model remains the reference. A coastline-barrier or no-region alternative is adopted only when it:

1. improves WAIC by at least 2 relative to the reference;
2. improves geographically blocked primary predictive loss;
3. has a spatial-block bootstrap 95% interval of paired predictive gain above zero;
4. improves at least four of five geographical folds;
5. keeps maximum fixed-effect VIF below 10;
6. does not materially reverse the core environmental conclusions solely through spatial reallocation;
7. yields stable spatial hyperparameters and no numerical pathologies.

If no alternative satisfies this rule, the stationary Matérn field is retained because its purpose is conservative spatial adjustment, not mechanistic reconstruction of dispersal.

## Interpreting state versus intensity spatial scales

The current posterior mean ranges are about 133 km for pigmentation state and 61 km for pigmented-only intensity, but their intervals overlap and the responses use different likelihoods and analysis subsets. The comparison is therefore descriptive only. It is compatible with, but does not establish, broader regional organization of the threshold-like pigment state and more local modulation of intensity after pigmentation is present.

A formal shared latent-field hurdle model would be required to compare cross-response spatial structure on a common scale. That model is not necessary for the present JBI claim and would introduce substantial additional identifiability assumptions.

## Consequence for the paper

The Broad act should distinguish:

- fixed environmental terms: measured long-term abiotic geography;
- retained environmental interactions: context dependence that also transfers geographically;
- Matérn/barrier field: unresolved biogeographic continuity after measured environment;
- later local Bombus test: a separate biotic hypothesis at 5 km;
- later event/human analysis: departures relative to the separately cross-fitted natural predictive reference.

No spatial-model sensitivity in this file automatically changes that separate cell-level natural reference or the 17 fixed local-departure candidates.
