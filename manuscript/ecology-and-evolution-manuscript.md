# From hiking records to two-part floral-colour geography: scale-aware inference from YAMAP photographs of *Campanula punctata*

Ruiqi Zhang

Kyoto University, Kyoto, Japan

Article type: Research Article

Target journal: *Ecology and Evolution*

## Abstract

Geographical analyses of intraspecific plant traits are often limited by a
lack of quantitative observations from wild populations. Community
photographs can fill part of this gap, but flower-colour studies must
distinguish pigment absence from variation among pigmented flowers and must not
confuse shared geography with biotic effects. We used every eligible
*Campanula punctata* record identified by the author on YAMAP, a GPS-enabled
hiking platform, from 1 June to 31 August in 2023–2025. The focal taxon and
petal extraction region were manually verified; repeated photographs of the
same individual and misidentifications, including *Adenophora triphylla*, were
removed before database construction. The curated database contained 1,965
records, and 1,923 had joint environmental and predicted-*Bombus* support. A
response-blind mixture on CIELAB a* separated 966 white-like and 957 pigmented
flowers at a*=4.9445. We modelled pigmentation presence and visible intensity
conditional on pigmentation separately. National INLA-SPDE models detected
temperature associations in both components, while soil was associated with
pigmentation presence and topographic heterogeneity with pigmented-only
intensity. Five geographically blocked predictive folds yielded AUC=0.862 for
pigmentation presence and RMSE=0.914 for conditional intensity. A five-species
ENMeval *Bombus*-community fingerprint added little national discrimination
(mean AUC gain=0.0061), but its local turnover was associated with both
pigmentation-share turnover (partial beta=0.084) and pigmented-only intensity
turnover (beta=0.092) at the pre-specified 25-km scale (both BH q=0.028).
Sixteen locally isolated pigmented cells were not excessive across 1,000 maps
replicated from the natural model. Their population and dense-settlement
contrasts were directionally positive but did not pass familywise correction.
Thus, route-linked hiking photographs can help close a quantitative-trait data
gap when the response hierarchy, process scale, predictive uncertainty, and
platform-specific sampling are kept explicit. The *Bombus* result is a
scale-specific correspondence, not evidence of pollinator-mediated selection,
and the human-context result prioritizes follow-up sites rather than
demonstrating horticultural origin.

## Keywords

bumblebee community; community photographs; digital phenotyping; flower colour;
INLA-SPDE; intraspecific trait variation

## 1 | Introduction

Intraspecific trait variation contributes to population responses,
eco-evolutionary dynamics, and community assembly, but its geographical
distribution remains poorly measured for many plant traits (Westerband et al.,
2021). Floral colour is a particularly informative example because it can be
associated with pollinator interactions, reproductive isolation, physiological
stress, and population history (Rausher, 2008; Koski & Ashman, 2015;
Trunschke et al., 2021). Yet broad-scale data often record colour as a small
set of categories. This can reveal the distribution of morphs, but it cannot
show whether already pigmented flowers vary quantitatively along environmental
or biotic gradients.

Large collections of community and social photographs offer a partial solution
to the geographical trait-data gap. Visible colour can be recovered from such
images at aggregate scales, and recent workflows have quantified flower colour
from community photographs across landscapes (Laitly et al., 2021; Luong et
al., 2023; McKenzie et al., 2026). These images nevertheless combine biological
variation with heterogeneous cameras, illumination, exposure, photographer
choice, access, and platform use. Supervised identification of the focal
organism and measurement region can reduce target and region-of-interest error,
but it cannot make opportunistic images equivalent to a calibrated field
survey.

YAMAP provides a distinctive source within this broader class of digital
records. It is a hiking and outdoor-activity platform whose application uses
GPS for maps and activity recording, and whose activity diaries can contain
photographs associated with a hiking trajectory (YAMAP, 2026a, 2026b).
Compared with ordinary social-media images lacking usable coordinates, this
route-linked structure provides a stronger georeferencing affordance. It also
creates a clear selection process: observations are concentrated along
accessible hiking trails and activity routes. Repurposing YAMAP can therefore
expand geographical trait coverage, provided that trail access and observation
opportunity are treated as limitations rather than as random sampling.
This differs from purpose-built biodiversity platforms such as iNaturalist,
whose basic data unit is explicitly an organismal observation at a time and
location (iNaturalist, 2026).

A second difficulty concerns the biological meaning of a continuous colour
score. Loss or strong downregulation of anthocyanin-pathway expression can
produce a threshold-like white–pigmented contrast, whereas quantitative
variation among pigmented flowers can arise through different regulatory and
optical processes (Dick et al., 2011; Tasaki et al., 2022). Treating all
flowers on one continuous axis consequently mixes two questions: whether
visible pigmentation is expressed and how intense the human-visible colour is
when pigmentation is expressed. Furthermore, CIELAB a* is an optical red–green
axis, not a chemical assay. Small a* differences among white flowers can reflect
tissue scattering, camera processing, shadows, or other optical variation and
should not be interpreted as anthocyanin amount (van der Kooi et al., 2016).
Nor can ordinary visible photographs measure ultraviolet reflectance or
receptor-based bumblebee colour contrast (Chittka, 1992; Renoult et al., 2017).

A third difficulty is scale. Climate, topography, dispersal history, and
population structure can generate broad geographical trait clines, while
pollinator-mediated processes act through encounters at much finer scales.
Predicted pollinator distributions are themselves structured by climate and
space. Putting environmental, spatial, and pollinator variables into a single
national model therefore does not automatically identify an independent biotic
effect. Spatial Gaussian fields can quantify continuous geography left after
measured covariates, but they do not uniquely separate environment, history,
and genetic structure or eliminate spatial confounding (Paciorek, 2010; Hanks
et al., 2015). A more defensible design matches each hypothesis to the scale at
which it is expected to leave information: national models for broad
environment–space structure and nearby comparisons for local community
turnover (Soberón, 2007; Araújo & Rozenfeld, 2014; Mod et al., 2020).

*Campanula punctata* Lam. is well suited to this design. The perennial herb is
widely distributed in Japan and includes conspicuous white-to-pink floral
variation. Bumblebees are important pollinators, and changes in pollinator
assemblages and breeding systems have been described in the Izu Islands
(Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). The occurrence of a
white-flowered island relative where bumblebees are absent or scarce motivates,
but does not prove, a mainland pollinator-community hypothesis. The species is
also cultivated, and horticultural forms can include unusually dark or
early-flowering phenotypes. Isolated pigmented observations in otherwise
white-flowered regions may therefore motivate a human-movement hypothesis, but
horticultural origin cannot be established without provenance or genetic data.

We developed a scale-aware workflow with four ordered aims. First, we converted
an author-reviewed census of eligible YAMAP records into a two-part optical
phenotype: pigmentation presence and pigmented-only intensity. Second, we
estimated national environmental associations and continuous spatial structure
with INLA-SPDE and built a geographically cross-fitted natural predictive
baseline. Third, we tested whether local turnover in the two colour components
corresponded to turnover in a predicted five-species *Bombus* community
fingerprint. Fourth, we replayed a pre-specified local-isolate event on 1,000
natural predictive maps, then characterized human context only after candidates
and white-flowered comparison neighbourhoods had been fixed. This ordering,
rather than any single model, is the central methodological contribution.

## 2 | Materials and Methods

### 2.1 | Study system, YAMAP sampling frame, and manual screening

The sampling frame comprised all eligible *C. punctata* records identified by
the author in YAMAP hiking-activity records from 1 June through 31 August in
each of 2023, 2024, and 2025. Thus, the occurrences were hiking-trail and
activity-route observations, not an areally balanced plot survey. Records were
not subsampled by year or region. Before the analytical database was
constructed, the author inspected the extracted subject, confirmed the focal
flower and the petal region to be measured, removed repeated photographs of the
same individual, and removed taxonomic errors. The latter included
*Adenophora triphylla* photographs posted as *C. punctata*. Counts for these
pre-database exclusions were not retained as separate categories and were not
reconstructed.

The curated table contained 1,965 observations: 642 in 2023, 687 in 2024, and
636 in 2025. All source images were identified through YAMAP. The public
provenance table retains a YAMAP activity label for 1,964 records; one otherwise
eligible record lacks the activity-level label and is marked
`field_survey_or_other`. Coordinates were inherited from the manually reviewed
source workbook in WGS84. The present publication pipeline did not independently
recalculate every coordinate from image time stamps and GPX files. The
geographical unit should therefore be interpreted as a reviewed route-linked
occurrence, not a formal positional-accuracy experiment.

YAMAP users can hide photograph locations, and trails differ in user activity.
The resulting observations are a census of eligible records within the stated
platform, dates, and screening rules, not a random census of Japanese
populations. Trail accessibility, flower conspicuousness, photographer
decisions, and platform use can affect inclusion.

### 2.2 | Supervised colour extraction and quality control

After subject extraction and cropping, the author visually confirmed the
retained flower and petal extraction region before scripted measurement. The
deterministic extraction script decoded display-referred sRGB values, bounded
candidate pixels by visible alpha and HSV petal-colour criteria, and used the
channel-wise median RGB value as the primary colour statistic. The primary
colour was converted to CIELAB under a D65, 2° standard-observer convention.
CIELAB a* was the optical red–green phenotype.

No image-specific white balance was applied because camera metadata and a
neutral reference were unavailable. Component shape, mask coverage,
overexposure, shadows, multimodality, and disagreement among candidate colour
summaries generated audit flags. These flags were not used to remove dark or
otherwise extreme colours from the primary analysis after author confirmation.
Only a non-formable colour measurement or an exact duplicate image constituted
a primary programmatic exclusion. Two exact duplicate-image records were
removed. Warning-flagged observations were excluded only in sensitivity
analyses. This preserves dark observations that are biologically relevant to
the human-movement hypothesis while separating measurement warnings from
exclusion.

The phenotype represents uncalibrated human-visible image colour. We did not
interpret a* as anthocyanin concentration, ultraviolet reflectance, or
bumblebee chromatic contrast.

### 2.3 | Two-part flower-colour response

We estimated a response-blind univariate Gaussian mixture from CIELAB a*.
Geography, environmental predictors, *Bombus* predictions, human-context
variables, flowering date, and fitted-model departures were not used to set the
boundary. The fitted boundary a*=4.944542 separated a white-like measurement
regime from a pigmented regime. Observations with mixture-classification
confidence below 0.8 were flagged as ambiguous, not deleted.

We then defined:

1. pigmentation presence, a binary indicator used for all observations; and
2. conditional visible colour intensity, standardized a* used only among
   pigmented observations.

White-flower a* variation was not modelled as pigment intensity. We use
“two-part response hierarchy” rather than claiming two experimentally
validated developmental stages.

### 2.4 | Public environmental predictors

Climate variables were obtained from CHELSA v2.1 climatologies for 1981–2010 at
30 arc-seconds (Karger et al., 2017; Brun et al., 2022). They included maximum
temperature of the warmest month (BIO5), mean temperature of the warmest
quarter (BIO10), growing degree-days above 5 °C, climate moisture index, annual
precipitation (BIO12), precipitation of the driest month (BIO14),
precipitation seasonality (BIO15), temperature seasonality (BIO4), and surface
downwelling short-wave radiation. Mean 0–5-cm bulk density, coarse fragments,
sand, silt, total nitrogen, organic-carbon density, soil organic carbon, and pH
were obtained from ISRIC SoilGrids 2.0 at 250 m and aligned to the common grid
(Poggio et al., 2021). Elevation came from WorldClim 2.1 at 30 arc-seconds
(Fick & Hijmans, 2017); slope, terrain ruggedness index, and roughness were
derived from it.

All continuous layers were cropped to Japan and aligned to an EPSG:4326,
30-arc-second grid. Values at observations were bilinearly extracted.
Response-blind principal components summarized temperature, precipitation,
soil, and topography. Axis signs were fixed before modelling for reproducible
interpretation. The source versions, provider URLs, and raster MD5 hashes are
provided with the data package.

For the replicated natural baseline, we used elevation, CHELSA BIO10, CHELSA
BIO12, and CHELSA radiation at two spatial scales. For each observed 1-km cell,
we calculated a 50-km neighbourhood mean and the cell-minus-neighbourhood
deviation. Two response-blind principal components summarized the four
broad-scale variables, and two summarized the four within-neighbourhood
deviations. Equivalent 25- and 100-km decompositions were retained as
sensitivity analyses.

### 2.5 | National environmental and spatial models

We used two complementary national analyses. First, observation-level
environment-plus-space models estimated conditional environmental clines for
both response parts. The presence outcome used a binomial likelihood and logit
link; conditional intensity used a Gaussian likelihood. Standardized
environmental axes and an INLA-SPDE Matérn field were included. The triangular
mesh used maximum inner and outer edge lengths of 20 and 100 km and a 5-km
cutoff. Penalised-complexity priors specified P(spatial range <100 km)=0.05 and
P(spatial standard deviation >1)=0.05 (Lindgren et al., 2011; Simpson et al.,
2017). Fixed-effect estimates are conditional on this spatial specification;
the SPDE field can contain omitted environment, dispersal history, genetic
structure, and other coherent geography.

Second, we built the predictive natural baseline at the 1-km-cell level. Counts
of pigmented flowers were modelled as binomial trials; the median standardized
intensity was modelled with a Gaussian likelihood only in cells containing
pigmented observations. Five response-blind geographical folds based on
100-km blocks assessed transfer to withheld regions (Roberts et al., 2017;
Valavi et al., 2019). Environmental predictors were standardized within
training folds. The SPDE field was constrained to zero mean at training
locations in each fold. We drew 1,000 replicated outcomes per held-out cell,
preserving observed trial counts. The foldwise predictions were combined into
cross-fitted national mosaics.

For presence, primary performance was mean negative log predictive mass, with
image-weighted AUC, Brier score, calibration intercept and slope, and cell-level
AUC as additional metrics. For conditional intensity, we report RMSE, MAE, and
95% predictive coverage. Spatially blocked AUC is interpreted as geographic
discrimination within the sampling frame, not as causal model adequacy (Hijmans,
2012; Lobo et al., 2008).

### 2.6 | ENMeval models and the predicted *Bombus* community

We considered five Japanese bumblebees: *Bombus ardens* and *B. diversus* as
widespread taxa, and *B. beaticola*, *B. consobrinus*, and *B. honshuensis* as
more montane taxa. Georeferenced present records were downloaded from GBIF for
Japan with maximum coordinate uncertainty of 10 km, cleaned and deduplicated,
and stored with query metadata. Sample sizes after filtering were 2,814,
7,309, 939, 286, and 1,084, respectively.

Presence-background models were fitted from the public environmental layers
using ENMeval 2.0 and maxnet (Kass et al., 2021). Feature classes and
regularization multipliers were tuned, and the minimum finite AICc setting was
selected for each species. Spatial validation metrics and prediction hashes
were stored. The final pipeline generated fresh predictions from these selected
models; pre-existing *Bombus* TIFF files were not treated as source data.

Raw suitability values are not abundances and are not necessarily comparable
among species (Renner & Warton, 2013; Guillera-Arroita, 2015). We therefore
rank-standardized support within species and constructed a predicted-community
fingerprint on the five-species common-support region. It comprised total
ranked habitat support plus two principal components of Hellinger-transformed
predicted composition. The fingerprint represents relative habitat support and
composition, not observed richness, visitation, pollen transfer, pollination
effectiveness, or selection.

As a national sensitivity analysis, the three fingerprint axes were
orthogonalized against the four environmental predictors inside each training
fold and added as a block to the presence model on common support. We compared
held-out AUC with an environment-plus-SPDE reference fitted to exactly the same
cells.

### 2.7 | Local flower-colour and predicted-community turnover

The local analysis asked a different question from the national incremental
model: whether changes between nearby flower cells corresponded to changes in
the predicted community more than expected under the fitted natural baseline.
On the five-species common support, we built a response-blind undirected graph
by linking each 1-km cell to at most five nearest neighbours within 25 km.
Edges crossed neither held-out fold boundaries nor the common-support boundary.
Duplicate undirected edges were removed. Radii of 10 and 50 km were fixed
sensitivities.

The two responses were the absolute endpoint difference in pigmented share and,
when both endpoints contained pigmented observations, the absolute difference
in conditional intensity. The primary predictor was Euclidean turnover across
the standardized three-axis *Bombus* fingerprint. Endpoint distance, natural
expected colour difference, environmental difference, observation support,
year as a nuisance term, and shared-node dependence were included in the same
partial-statistic construction for observed and replicated maps.

For each of 1,000 cross-fitted natural maps, we recalculated the pair responses
on the fixed graph and refitted the identical partial association. One-sided
upper-tail Monte Carlo p-values asked whether observed positive correspondence
exceeded that expected from the natural model and observation design.
Benjamini–Hochberg correction was applied across the two pre-specified
25-km colour responses. The design weakens broad shared geography but cannot
remove fine-scale unmeasured environment, history, observation processes, or
the uncertainty of the *Bombus* SDMs.

### 2.8 | Repeated local-isolate events and post-selection human context

We did not regress first-stage residuals on human predictors. Instead, we fixed
a local event using only flower colour, response-blind distance, natural
environment, and sampling support. A primary pigmented isolate was a pigmented
1-km focal cell with at least three cells within 10 km, root-mean-square
distance ≤1 across the four standardized 50-km environmental components, and
all eligible observed neighbours white. Population, land use, roads, dense
settlement, flowering date, and colour intensity did not select cases or
neighbours.

The identical event extractor was replayed on each of the 1,000 replicated
natural maps. We compared observed candidate count and candidate fraction with
the replicated distributions. These are predictive model checks conditional on
the fitted natural baseline, not ordinary residual-regression coefficients or
proof of cause (Meng, 1994; Freckleton, 2002; Gabry et al., 2019).

Only after candidates had been selected did we compare each focal cell with its
own environment-similar white neighbourhood. Population counts came from
WorldPop Japan 2020 and were summed within 5, 10, 25, and 50 km. Land-use
classes came from the 2021 MLIT National Land Numerical Information L03-b
100-m layer and were summarized as built-up, cultivation, artificial-land,
forest–managed-land interface, and major-road context. Distance to 2015 MLIT
Densely Inhabited District (DID) polygons was a separate sensitivity analysis.
The identical post-selection contrasts were calculated for candidate sets from
every natural map. Directional Monte Carlo p-values followed pre-specified
human-exposure directions, and maxT controlled familywise error within each
feature family. Early flowering and dark pigmented intensity were evaluated
only as held-out follow-up facets.

Because all flowers originated from hiking-activity records, candidate and
comparison cells already shared a trail-access sampling condition. Population,
roads, and land use can influence both plant exposure and observation
opportunity, and the restricted frame can attenuate contrasts by reducing the
available range of accessibility. Human-context tests therefore characterize
the sampled route network; they do not estimate a population-wide human effect.

### 2.9 | Software, reproducibility, and claim control

Analyses were conducted in R 4.5.3 using INLA, sf, terra, ENMeval, maxnet,
mclust, mgcv, ranger, and supporting packages; colour extraction used Python
3.12 with Pillow and NumPy. Seeds, package assignments, input and prediction
hashes, complete stage outputs, and independent validations are stored in the
publication snapshot. Every final numerical claim was read from the locked
result registry rather than copied from the obsolete earlier manuscript.

## 3 | Results

### 3.1 | Sampling flow and two-part optical phenotype

The curated YAMAP-derived database comprised 1,965 records spanning the
registered annual windows. Counts were 642, 687, and 636 in 2023–2025. After
two exact duplicate-image records and joint environmental/*Bombus*
complete-case support were considered, the final ecological analysis contained
1,923 observations: 629, 669, and 625 by year. All 1,923 observations had
author-confirmed extraction regions.

The response-blind a* mixture separated 966 white-like and 957 pigmented
observations at a*=4.9445. One hundred twenty-four observations had
classification confidence <0.8 and were retained with an ambiguity flag.
Conditional intensity was defined only for the 957 pigmented observations.
Aggregation produced 1,307 unique 1-km cells, including 88 mixed cells and 674
cells with a conditional-intensity value (Figure 1).

### 3.2 | National environmental clines and continuous geography

The two-part environmental models detected partially shared but non-identical
clines. In the presence model, the temperature composite had posterior
mean −0.567 (95% credible interval −1.058 to −0.076), indicating lower
pigmentation probability toward the warmer end of the pre-oriented axis. The
joint soil PC1 coefficient was −0.404 (−0.712 to −0.097). Precipitation,
seasonality, topography, soil PC2, and radiation intervals overlapped zero.

Among pigmented flowers, temperature was again negative
(−0.287, −0.454 to −0.122), and the topographic composite was negative
(−0.136, −0.233 to −0.040); the remaining reported environmental intervals
overlapped zero. Because the topographic axis loaded positively on roughness,
slope, and terrain ruggedness, its coefficient describes lower standardized
visible intensity toward more heterogeneous terrain after the spatial field and
other covariates were included. These are conditional optical-colour
associations, not evidence of a pigment-synthesis mechanism.

The SPDE fields retained coherent structure in both response parts. Estimated
spatial range was 131.5 km (95% credible interval 88.1–193.4) for pigmentation
presence and 61.4 km (31.5–116.3) for conditional intensity. Thus, the
presence boundary and pigmented-only variation retained different characteristic
spatial scales after environmental adjustment.

The cross-fitted national presence model had AUC=0.862, Brier score=0.150,
calibration intercept=−0.133, and calibration slope=1.141. Conditional
intensity had RMSE=0.914, MAE=0.710, and 95% predictive coverage=0.947.
These values quantify withheld-geography prediction inside the YAMAP sampling
frame (Figure 2).

### 3.3 | National and local predicted-*Bombus* information

Five-species common support contained 584 cells and 876 observations. On these
same cells, the environment-plus-SPDE reference had AUC=0.858. Adding the
environment-orthogonal *Bombus* fingerprint increased AUC to 0.864, a gain of
0.0061. The small increment did not support a strong independent national
contribution.

At the primary 25-km local scale, the response-blind graph contained 1,742
presence edges among 577 nodes and 1,183 pigmented-only intensity edges among
440 nodes. Fingerprint turnover was positively associated with observed
pigmentation-share turnover (partial beta=0.084; upper-tail Monte Carlo
p=0.028) and conditional-intensity turnover (beta=0.092; p=0.014). Both
pre-specified responses had BH q=0.028. The observed statistics lay at the
97.3rd and 98.7th percentiles of their replicated natural-map distributions.
The correspondence was scale-sensitive: the intensity association was also
evident at 10 and 50 km, whereas presence was weaker at 10 km and marginal at
50 km (Figure 3).

### 3.4 | Local isolates were compatible with the natural predictive baseline

The primary event definition identified 16 locally isolated pigmented cells.
Across 1,000 replicated natural maps, the mean was 13.507
(95% predictive interval 7–21), giving upper-tail p=0.273. The observed
candidate fraction was 0.0448 versus a replicated mean of 0.0341
(95% interval 0.0169–0.0537; p=0.125). The observed number and fraction of
isolates were therefore not unusually large under the fitted natural baseline
(Figure 4a,b).

### 3.5 | Human-context signals were directional but not familywise significant

Within each candidate's own white-flowered neighbourhood, the focal cell had a
0.059 higher rank in 5-km population context than its neighbours. The
directional p-value was 0.022, but the maxT p-value across five population
scales was 0.076. The 10-km difference was smaller (0.048; raw p=0.044);
25- and 50-km contrasts were weak.

The population–DID alignment contrast was 0.054 (raw p=0.029; maxT p=0.084).
Nine of 16 candidates (0.5625) were classified as DID-proximate with high
population, compared with a natural-map mean candidate fraction of 0.313
(raw p=0.082; maxT p=0.117). Built-up fraction and settlement composites
pointed in the same direction but no land-use, road, interface, population, or
DID family passed its corrected threshold. Early-flowering and unusually dark
pigmented colour did not converge: their primary case–control contrasts were
not positive relative to the replicated baseline. One leading joint candidate
combined an unexpected-pigmentation tail with local DID proximity, but was
neither unusually early nor unusually dark (Figure 4c,d).

## 4 | Discussion

### 4.1 | A scale-aware digital-phenotyping workflow

The main advance is not a claim that community photographs have never been
used to measure flower colour. Instead, the study links four decisions that
address distinct inferential failures: a hiking platform was repurposed to fill
a geographical quantitative-trait gap; visible pigmentation expression was
separated from intensity among pigmented flowers; broad environment and
continuous geography were estimated nationally while the biotic hypothesis was
tested locally; and a pre-defined event was replayed on replicated natural maps
before human variables were examined. This sequence limits circularity and
keeps each conclusion tied to the scale and proxy that generated it.

YAMAP is central to that contribution. Unlike biodiversity applications such
as iNaturalist (iNaturalist, 2026), YAMAP is designed for hiking, maps, and
activity recording. Biological images are incidental to its primary purpose.
Their route-linked context nevertheless makes them useful digital specimens
when occurrence coordinates and taxon identity are reviewed. The novelty is
not that YAMAP coordinates are intrinsically more accurate than those of a
purpose-built biodiversity application; it is that photographs generated
during GPS-linked hiking activities can be repurposed as a supervised,
geographically explicit trait census. The nearly even annual counts and
explicit annual window show that the database was not created by selecting only
the most colourful observations. Manual removal of repeated individuals and
misidentified *A. triphylla* further protects the taxonomic and sampling unit.
This supervised reuse of a domain-specific outdoor platform can complement,
not replace, formal field surveys and purpose-built biodiversity repositories.

### 4.2 | Quantification revealed two geographical colour components

The white–pigmented boundary and the variation among pigmented flowers did not
have identical environmental or spatial structure. Temperature was associated
with both components, but soil was retained for pigmentation presence whereas
topography was retained for conditional intensity. The estimated spatial range
was also larger for presence than intensity. A single continuous white-to-pink
score would have mixed white-flower optical noise with meaningful variation
among pigmented flowers and obscured these differences.

These findings support “two-part geographical clines,” not two proven
developmental mechanisms. The mixture boundary is operational, and a* remains
an uncalibrated visible-colour measure. Chemical extraction, standardized
reflectance, or expression assays are needed to determine whether the two
statistical components correspond to anthocyanin-pathway thresholds and
pigment concentration. Nevertheless, the hierarchy illustrates why
quantitative data add information beyond the location of white and pink
morphs: conditional intensity can vary even where the categorical state is
unchanged.

### 4.3 | Space was an estimand, not disposable noise

The broad colour geography could not be reduced to the measured raster
variables. The SPDE fields quantified continuous structure that remained after
environmental adjustment, with characteristic ranges differing between
presence and intensity. This spatial signal may reflect unmeasured
microclimate, dispersal and colonization history, population genetic structure,
or correlated sampling. It is therefore a substantive result, but not an
identified mechanism.

The same caution applies to the environmental coefficients. A spatial field can
compete with smooth covariates and change their magnitude or precision
(Paciorek, 2010; Hanks et al., 2015). Adding SPDE does not “remove spatial
bias,” and coefficient persistence is not proof of adaptation. Reporting the
conditional clines, spatial scale, collinearity diagnostics, and geographically
held-out predictions is more informative than choosing between “environment”
and “space” as mutually exclusive explanations.

### 4.4 | Weak national addition and local *Bombus* turnover are compatible

The five-species fingerprint added only 0.0061 AUC nationally after
environment and space. This is expected when predicted bumblebee distributions
share the same climatic and geographical gradients as the plant phenotype; it
also bounds any claim that *Bombus* explains the national map independently.
Deleting the community predictor solely because of collinearity would discard
the biological question, whereas treating its coefficient as an independent
pollinator effect would overstate the data. The common-support comparison
instead measured incremental held-out information.

The local pair analysis asked whether changes in predicted community
composition and support coincided with sharper nearby changes in two flower
colour components. Both correspondences exceeded those produced by the natural
model at the pre-specified 25-km scale. The national and local results are not
contradictory because their estimands differ: one tests additional broad
discrimination, and the other tests scale-specific turnover correspondence.
The latter is consistent with a pollinator-community hypothesis after reducing
broad shared geography.

It is not an estimate of pollinator-mediated selection. Presence-background
SDM output is relative suitability, not bee abundance, visitation, pollen
deposition, or reproductive success (Guillera-Arroita, 2015). CIELAB is not a
bumblebee visual model. Direct tests should target the identified colour
transition zones with flower reflectance, receptor-based visual contrasts,
species-resolved visitation, pollen transfer and seed set, and experimental
colour manipulation. The Izu-island observation is best treated as motivation
for such comparative work, not as a causal replicate of the mainland analysis.

### 4.5 | Human-context analysis generated priorities, not provenance

Replaying the same local-isolate definition on every natural map changed the
question from “what predicts a large residual?” to “how often would this kind
of locally discordant event occur under the fitted natural baseline?” This
avoids treating a fitted residual as a newly measured trait and propagates
model and observation variation through the event definition. The result was
instructive because it could fail: 16 isolates were not more numerous than
expected, and their human-context contrasts did not pass familywise correction.

The raw 5-km population and population–DID directions are compatible with a
local human-movement hypothesis, but several alternatives remain. Population
is an incomplete proxy for planting or propagule pressure. DID describes dense
settlement rather than all rural and satoyama land. More importantly, the
YAMAP hiking-trail frame already selects accessible routes. This can weaken
candidate–neighbour differences by conditioning both groups on trail access:
mountain routes near settlements and more isolated routes enter the dataset
only when users traverse and photograph them. The restriction can also make
population or roads proxies for observation opportunity. The weak corrected
results may therefore reflect a small biological association, restricted
predictor range created by route sampling, incomplete human proxies, or no
association; the current data cannot distinguish these explanations.

Dark colour and early flowering also failed to converge on the leading
human-context candidate. This absence of multilateral convergence makes a
strong horticultural interpretation less, not more, defensible. Conversely, it
does not prove natural origin. Planting histories, repeated site visits,
voucher specimens, nearby population surveys, and assignment of candidates
against wild populations and commercial cultivars are required to test
horticultural introduction, escape, or introgression (Trusty et al., 2007).

### 4.6 | Limitations and transferability

The study has eight principal limitations. First, YAMAP observations are
opportunistic and trail-biased. Second, user decisions and the visibility of
photographs and locations affect coverage. Third, image colour was not
standardized by a neutral reference, camera calibration, or illumination
experiment. Fourth, the operational pigmentation boundary has uncertainty.
Fifth, public environmental rasters and *Bombus* predictions are much coarser
than individual plant–pollinator encounters. Sixth, SPDE cannot identify the
source of residual geography or eliminate confounding. Seventh, replicated
departures are conditional on the fitted natural model. Eighth, there is no
historical or genetic test of horticultural origin.

These limits define the claim ceiling but do not erase the value of the data.
When quantitative field trait databases are sparse, a supervised platform
census can reveal geographical hypotheses and prioritize field sites at a
scale unavailable to a small local survey. The workflow is transferable to
other conspicuous traits if researchers report the platform selection process,
separate biologically different response components, validate in withheld
geography, and change inferential scale rather than treating all correlated
processes as coefficients in one saturated model.

## 5 | Conclusions

Author-reviewed YAMAP photographs provided a geographically extensive
quantitative flower-colour dataset for *C. punctata*. Separating pigmentation
presence from conditional intensity revealed distinct environmental and
spatial components. A predicted *Bombus* fingerprint added little national
discrimination but corresponded to local turnover in both colour components.
Locally isolated pigmented events and their human context did not exceed
corrected thresholds, so horticultural origin remains a field and genetic
hypothesis. The broader contribution is a scale-aware workflow for extracting
bounded ecological inference from non-random, georeferenced image records.

## Data Accessibility Statement

The analysis code, derived flower-colour table, manual-review and QC
provenance, public-data source registry, model-selection tables, prediction
hashes, seeds, complete adopted stage outputs, and independent validations are
available in the project repository and dated local publication snapshot.
Before submission, the repository release will be archived in a DOI-issuing
repository. Original YAMAP photographs are third-party user content and are
not redistributed automatically. `Data_S1.csv` contains source identifiers,
image hashes, reviewed coordinates, derived colour measurements, and QC fields
needed to audit the analytical phenotypes. Provider versions and source URLs
for CHELSA, SoilGrids, WorldClim, WorldPop, MLIT, and GBIF are listed in
`docs/data-sources/public-environment-sources.md`.

## Author Contributions

Ruiqi Zhang: Conceptualization; Data curation; Formal analysis; Investigation;
Methodology; Software; Validation; Visualization; Writing – original draft;
Writing – review and editing.

## Conflict of Interest

The author declares no conflict of interest.

## References

Araújo, M. B., & Rozenfeld, A. (2014). The geographic scaling of biotic
interactions. *Ecography*, 37, 406–415.
https://doi.org/10.1111/j.1600-0587.2013.00643.x

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N. (2022).
Global climate-related predictors at kilometer resolution for the past and
future. *Earth System Science Data*, 14, 5573–5603.
https://doi.org/10.5194/essd-14-5573-2022

Chittka, L. (1992). The colour hexagon: A chromaticity diagram based on
photoreceptor excitations as a generalized representation of colour opponency.
*Journal of Comparative Physiology A*, 170, 533–543.
https://doi.org/10.1007/BF00188925

Dick, C. A., Buenrostro, J., Butler, T., Carlson, M. L., Kliebenstein, D. J.,
& Whittall, J. B. (2011). Arctic mustard flower color polymorphism controlled
by petal-specific downregulation at the threshold of the anthocyanin
biosynthetic pathway. *PLoS ONE*, 6, e18230.
https://doi.org/10.1371/journal.pone.0018230

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: New 1-km spatial resolution
climate surfaces for global land areas. *International Journal of
Climatology*, 37, 4302–4315. https://doi.org/10.1002/joc.5086

Freckleton, R. P. (2002). On the misuse of residuals in ecology: Regression of
residuals vs. multiple regression. *Journal of Animal Ecology*, 71, 542–545.
https://doi.org/10.1046/j.1365-2656.2002.00618.x

Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019).
Visualization in Bayesian workflow. *Journal of the Royal Statistical Society:
Series A*, 182, 389–402. https://doi.org/10.1111/rssa.12378

Guillera-Arroita, G. (2015). Is my species distribution model fit for purpose?
Matching data and models to applications. *Global Ecology and Biogeography*,
24, 276–292. https://doi.org/10.1111/geb.12268

Hanks, E. M., Schliep, E. M., Hooten, M. B., & Hoeting, J. A. (2015).
Restricted spatial regression in practice: Geostatistical models, confounding,
and robustness under model misspecification. *Environmetrics*, 26, 243–254.
https://doi.org/10.1002/env.2331

iNaturalist. (2026). What is an observation? iNaturalist Help. Accessed
24 July 2026.
https://help.inaturalist.org/en/support/solutions/articles/151000169927-what-is-an-observation-

Hijmans, R. J. (2012). Cross-validation of species distribution models:
Removing spatial sorting bias and calibration with a null model. *Ecology*,
93, 679–688. https://doi.org/10.1890/11-0826.1

Inoue, K. (1988). Pattern of breeding-system change in the Izu Islands in
*Campanula punctata*: Bumblebee-absence hypothesis. *Plant Species Biology*,
3, 125–128. https://doi.org/10.1111/j.1442-1984.1988.tb00178.x

Inoue, K., & Amano, M. (1986). Evolution of *Campanula punctata* in the Izu
Islands: Changes of pollinators and evolution of breeding systems. *Plant
Species Biology*, 1, 89–97.
https://doi.org/10.1111/j.1442-1984.1986.tb00018.x

Karger, D. N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H.,
Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017).
Climatologies at high resolution for the earth's land surface areas.
*Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

Kass, J. M., et al. (2021). ENMeval 2.0: Redesigned for customizable and
reproducible modeling of species' niches and distributions. *Methods in Ecology
and Evolution*, 12, 1602–1608.
https://doi.org/10.1111/2041-210X.13628

Koski, M. H., & Ashman, T.-L. (2015). Floral pigmentation patterns provide an
example of Gloger's rule in plants. *Nature Plants*, 1, 14007.
https://doi.org/10.1038/nplants.2014.7

Laitly, A., Callaghan, C. T., Delhey, K., & Cornwell, W. K. (2021). Is color
data from citizen science photographs reliable for biodiversity research?
*Ecology and Evolution*, 11, 4071–4083.
https://doi.org/10.1002/ece3.7307

Lindgren, F., Rue, H., & Lindström, J. (2011). An explicit link between
Gaussian fields and Gaussian Markov random fields: The stochastic partial
differential equation approach. *Journal of the Royal Statistical Society:
Series B*, 73, 423–498.
https://doi.org/10.1111/j.1467-9868.2011.00777.x

Lobo, J. M., Jiménez-Valverde, A., & Real, R. (2008). AUC: A misleading measure
of the performance of predictive distribution models. *Global Ecology and
Biogeography*, 17, 145–151.
https://doi.org/10.1111/j.1466-8238.2007.00358.x

Luong, Y., Gasca-Herrera, A., Misiewicz, T. M., & Carter, B. E. (2023). A
pipeline for the rapid collection of color data from photographs.
*Applications in Plant Sciences*, 11, e11546.
https://doi.org/10.1002/aps3.11546

McKenzie, P. F., Church, S. H., & Hopkins, R. (2026). High-throughput
iNaturalist image analysis reveals flower color divergence in *Monarda
fistulosa*. *The American Naturalist*, 208, 101–109.
https://doi.org/10.1086/739413

Meng, X.-L. (1994). Posterior predictive p-values. *The Annals of Statistics*,
22, 1142–1160. https://doi.org/10.1214/aos/1176325622

Mod, H. K., et al. (2020). Scale dependence of ecological assembly rules:
Insights from empirical datasets and joint species distribution modelling.
*Journal of Ecology*, 108, 1967–1977.
https://doi.org/10.1111/1365-2745.13434

Nagano, Y., Abe, K., Kitazawa, T., Hattori, M., Hirao, A. S., & Itino, T.
(2014). Changes in pollinator fauna affect altitudinal variation of floral size
in a bumblebee-pollinated herb. *Ecology and Evolution*, 4, 3395–3407.
https://doi.org/10.1002/ece3.1191

Paciorek, C. J. (2010). The importance of scale for spatial-confounding bias
and precision of spatial regression estimators. *Statistical Science*, 25,
107–125. https://doi.org/10.1214/10-STS326

Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B.,
Ribeiro, E., & Rossiter, D. (2021). SoilGrids 2.0: Producing soil information
for the globe with quantified spatial uncertainty. *SOIL*, 7, 217–240.
https://doi.org/10.5194/soil-7-217-2021

Rausher, M. D. (2008). Evolutionary transitions in floral color.
*International Journal of Plant Sciences*, 169, 7–21.
https://doi.org/10.1086/523358

Renner, I. W., & Warton, D. I. (2013). Equivalence of MAXENT and Poisson point
process models for species distribution modeling in ecology. *Biometrics*, 69,
274–281. https://doi.org/10.1111/j.1541-0420.2012.01824.x

Renoult, J. P., Kelber, A., & Schaefer, H. M. (2017). Colour spaces in ecology
and evolutionary biology. *Biological Reviews*, 92, 292–315.
https://doi.org/10.1111/brv.12230

Roberts, D. R., et al. (2017). Cross-validation strategies for data with
temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40,
913–929. https://doi.org/10.1111/ecog.02881

Simpson, D., Rue, H., Riebler, A., Martins, T. G., & Sørbye, S. H. (2017).
Penalising model component complexity: A principled, practical approach to
constructing priors. *Statistical Science*, 32, 1–28.
https://doi.org/10.1214/16-STS576

Soberón, J. (2007). Grinnellian and Eltonian niches and geographic
distributions of species. *Ecology Letters*, 10, 1115–1123.
https://doi.org/10.1111/j.1461-0248.2007.01107.x

Tasaki, K., et al. (2022). Identification of candidate genes responsible for
flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*,
13, 906879. https://doi.org/10.3389/fpls.2022.906879

Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower
color evolution and the evidence of pollinator-mediated selection. *Frontiers
in Plant Science*, 12, 617851.
https://doi.org/10.3389/fpls.2021.617851

Trusty, J. L., Goertzen, L. R., Zipperer, W. C., & Lockaby, B. G. (2007).
Invasive *Wisteria* in the southeastern United States: Genetic diversity,
hybridization and the role of urban centers. *Urban Ecosystems*, 10, 379–395.
https://doi.org/10.1007/s11252-007-0030-y

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019).
blockCV: An R package for generating spatially or environmentally separated
folds for k-fold cross-validation of species distribution models. *Methods in
Ecology and Evolution*, 10, 225–232.
https://doi.org/10.1111/2041-210X.13107

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016).
How to colour a flower: On the optical principles of flower coloration.
*Proceedings of the Royal Society B*, 283, 20160429.
https://doi.org/10.1098/rspb.2016.0429

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait
variation in plants: A renewed focus on its role in ecological processes.
*Annals of Botany*, 127, 397–410.
https://doi.org/10.1093/aob/mcab011

YAMAP. (2026a). YAMAP application and website. YAMAP Help Center. Accessed
24 July 2026.
https://help.yamap.com/hc/ja/articles/900000929903

YAMAP. (2026b). How to record an activity. YAMAP Help Center. Accessed
24 July 2026.
https://help.yamap.com/hc/ja/articles/900000911666

## Tables

### Table 1. Public spatial data used in the analysis

| Domain | Dataset and version | Variables or role | Analysis grain |
|---|---|---|---|
| Climate | CHELSA v2.1, 1981–2010 | temperature, degree-days, moisture, precipitation, seasonality, radiation | 30 arc-seconds |
| Soil | ISRIC SoilGrids 2.0 | 0–5-cm physical, nutrient, carbon, and pH properties | 250 m aligned to 30 arc-seconds |
| Elevation | WorldClim 2.1 | elevation and derived terrain | 30 arc-seconds |
| Population | WorldPop Japan 2020, unadjusted count | post-selection population context | native approximately 1 km, summed within 5–50 km |
| Land use | MLIT L03-b 2021 | settlement, cultivation, artificial land, forest interface, major roads | 100 m summarized to 1 km |
| Dense settlement | MLIT A16 2015 DID | post-selection proximity sensitivity | polygons summarized on 1-km grid |
| Bumblebee occurrence | GBIF | five-species ENMeval inputs | point records; predictions aligned to 30 arc-seconds |

### Table 2. Locked primary results and claim ceilings

| Analysis | Estimate | Uncertainty or reference | Interpretation ceiling |
|---|---:|---:|---|
| Pigmentation presence | 966 white-like; 957 pigmented | a* boundary=4.9445 | optical class, not chemical pigment |
| National presence prediction | AUC=0.862 | five 100-km folds | withheld-geography discrimination |
| Conditional intensity prediction | RMSE=0.914 | coverage=0.947 | optical intensity among pigmented flowers |
| National *Bombus* increment | ΔAUC=0.0061 | same common-support cells | little additional national information |
| 25-km presence turnover | beta=0.084 | p=0.028; BH q=0.028 | local correspondence, not selection |
| 25-km intensity turnover | beta=0.092 | p=0.014; BH q=0.028 | local correspondence, not selection |
| Local-isolate count | 16 | null mean=13.507; p=0.273 | not excessive under natural baseline |
| Local-isolate fraction | 0.0448 | null mean=0.0341; p=0.125 | not excessive under natural baseline |
| 5-km population contrast | 0.059 | raw p=0.022; maxT p=0.076 | suggestive human context |
| Population–DID alignment | 0.054 | raw p=0.029; maxT p=0.084 | suggestive human context |
| DID-proximate high-population fraction | 0.5625 | raw p=0.082; maxT p=0.117 | not corrected-significant |

## Figure legends

**Figure 1. Observation-level extracted colours and the two-part flower-colour
response.** (a) Every final observation represented by its extracted
channel-wise median sRGB colour from the author-confirmed petal region,
separated into white-like and pigmented classes and ordered by a* from left to
right and then top to bottom.
(b) Distribution of CIELAB a* with the response-blind mixture boundary at
4.9445. (c) National locations of white-like and pigmented observations.
(d) Conditional intensity among pigmented flowers. Displayed colours are
uncalibrated human-visible sRGB, not anthocyanin concentration or *Bombus*
colour contrast.

**Figure 2. National environmental and continuous spatial structure of the two
flower-colour components.** (a) Posterior environmental coefficients with 95%
credible intervals for pigmentation presence and pigmented-only intensity.
(b) Posterior SPDE ranges retained after environmental adjustment. (c)
Cross-fitted pigmentation probabilities and (d) cross-fitted conditional
intensity predictions at sampled 1-km cells. The maps show withheld-geography
predictions, not causal process attribution or isolated spatial-field effects.

**Figure 3. National incremental and local turnover tests of the predicted
*Bombus* community fingerprint.** (a) Same-support held-out AUC for national
environment-plus-SPDE models with and without the environment-orthogonal
fingerprint; lines join the same spatial fold and diamonds denote fold means.
(b–c) Observed 25-km partial turnover statistics over their
1,000-map natural reference distributions for pigmentation share and
pigmented-only intensity. (d) Fixed 10-, 25-, and 50-km sensitivity results.
The fingerprint represents relative predicted habitat suitability across five
species, not abundance or visitation.

**Figure 4. Repeated local-isolate events and post-selection human context.**
(a) Locations of the 16 pigmented focal 1-km cells isolated among
environment-similar white neighbours. (b) Observed candidate count and fraction
relative to 1,000 replicated natural maps. (c) Population-scale and
densely-inhabited-district (DID) contrasts with natural-map 95% intervals; no
familywise maxT test passed 0.05. (d) Candidate-level early-flowering and
dark-colour predictive-tail checks; neither met the pre-specified q=0.10
criterion. Candidate locations are follow-up priorities, not horticultural
provenance assignments.
