# Manuscript story and claim architecture

## Working title

**A scale-aware workflow links YAMAP flower phenotypes to national
environment–space structure and local predicted pollinator turnover**

Short alternative:

**Two-part floral-colour geography from georeferenced hiking records**

## One-sentence advance

An author-reviewed census of eligible YAMAP flower records can help fill a
geographical quantitative-trait gap when optical pigmentation expression is
separated from pigmented-only intensity, national environmental and continuous
spatial structure define the natural baseline, local biotic turnover is tested
at a local scale, and unusual events are replayed on replicated predictive maps
before human context is examined.

## Why YAMAP is methodologically useful

YAMAP is a hiking and outdoor-activity platform, not a biodiversity-recording
application. Its activity records use GPS-enabled maps and can associate
photographs with a recorded hiking trajectory. This gives route-linked
photographs a stronger georeferencing affordance than ordinary social-media
images whose coordinates are absent or coarse. The present database uses the
reviewed coordinates carried in the curated source workbook and does not claim
that the publication pipeline independently reconstructed every coordinate
from GPX.

The sampling strength and limitation have the same source. YAMAP provides
geographically informative flower photographs from places that conventional
trait databases undersample, but observations are concentrated on accessible
hiking trails and activity routes. They are therefore not a random sample of
Japanese plant populations.

The comparison with iNaturalist is functional, not a contest in positional
accuracy. iNaturalist is designed around organismal observations, whereas the
biological photographs used here were incidental records within GPS-linked
hiking activities. The methodological novelty is the supervised reuse of that
route context for geographical trait phenotyping.

## Sampling statement

The author examined every eligible *Campanula punctata* record identified on
YAMAP from 1 June through 31 August in each of 2023, 2024, and 2025. Before the
analytical database was constructed, the author:

1. confirmed the focal taxon and manually specified or confirmed the petal
   extraction region;
2. removed repeated photographs of the same individual; and
3. removed taxonomic errors, including *Adenophora triphylla* posted as
   *C. punctata*.

The counts of these pre-database exclusions were not retained separately and
must not be reconstructed. `Data_S1.csv` contains 1,965 curated records
(642, 687, and 636 by year). Two exact duplicate images were then removed
programmatically. Joint environmental and nationwide *Bombus* support yielded
the final 1,923-observation complete-case analysis (629, 669, and 625 by year).

## Why this species

*Campanula punctata* links four otherwise separate questions:

1. mainland flowers show conspicuous white-to-pink variation;
2. *Bombus* is a principal pollinator group;
3. a white-flowered Izu-island relative occurs where bumblebees are scarce or
   absent; and
4. horticultural forms include dark and early-flowering phenotypes.

These observations motivate a mainland pollinator-community hypothesis and a
human-movement follow-up hypothesis. They do not establish pollinator-mediated
selection or horticultural escape.

## Results narrative

### 1. Supervised digital phenotyping fills a trait-data gap

The workflow converted hiking-route images into quantitative optical
phenotypes after author confirmation of the flower and extraction region.
Colour-shape masks were not used to reject observations, and unusually dark
flowers were retained. A response-blind mixture on CIELAB a* separated 966
white-like from 957 pigmented observations at a*=4.9445. White-flower a*
variation was treated as optical and photographic variation, not anthocyanin
amount. Pigmentation presence and intensity among pigmented flowers were then
analysed separately.

This is not claimed as the first use of community photographs for flower-colour
phenotyping. The novelty is the use of a hiking platform as a route-linked
digital specimen source and the ordering of the subsequent two-part and
scale-specific inference.

### 2. Environment and continuous geography are both results

In the national two-part INLA analyses, warmer temperature conditions were
associated with lower pigmentation presence (posterior mean −0.567, 95%
credible interval −1.058 to −0.076) and lower pigmented-only intensity
(−0.287, −0.454 to −0.122). A soil composite was also associated with
presence (−0.404, −0.712 to −0.097), whereas a topographic-ruggedness
composite was associated with pigmented-only intensity (−0.136, −0.233 to
−0.040). These coefficients are conditional associations and depend on the
orientation of response-blind PCA axes.

The SPDE field is not generic noise and is not claimed to remove spatial
confounding. It represents coherent geography left after measured predictors,
compatible with omitted environment, dispersal and colonization history,
population structure, or other spatial processes.

The locked predictive baseline represented elevation, temperature,
precipitation, and radiation as 50-km broad means plus within-neighbourhood
deviations. Five response-blind 100-km folds yielded AUC=0.862 for
pigmentation presence and RMSE=0.914 for conditional intensity. AUC describes
held-out geographic discrimination, not causal explanatory power.

### 3. The weak national and positive local *Bombus* results answer different questions

Adding an environment-orthogonal five-species ENMeval community fingerprint on
common support improved held-out AUC by only 0.0061. This does not support a
large independent national contribution.

The planned 25-km local analysis instead asked whether colour change between
nearby cells corresponded to change in the predicted *Bombus* community more
than expected across 1,000 natural predictive maps. It found parallel
correspondence for pigmentation-share turnover (partial beta=0.084,
upper-tail p=0.028) and pigmented-only intensity turnover (beta=0.092,
p=0.014); both BH q=0.028.

The fingerprint summarizes within-species ranked habitat support and predicted
composition. It is not *Bombus* abundance, observed richness, visitation,
pollination effectiveness, visual preference, or selection strength. Local
matching reduces broad shared geography but cannot eliminate fine-scale
environmental, historical, or observation confounding.

### 4. Replayed local events replace residual-as-response inference

A pigmented isolate was defined before human variables were examined: a
pigmented 1-km cell with at least three cells within 10 km, close on the four
natural environmental axes, and only white observed neighbours. The identical
event extractor was applied to each natural predictive map. Sixteen cases were
observed versus a null mean of 13.507 (p=0.273); their proportion was 0.0448
versus 0.0341 (p=0.125). Local isolates were therefore not excessive under the
fitted natural baseline.

### 5. Human context is suggestive, weak, and restricted by trail sampling

Candidates lay in a slightly higher 5-km population context than their own
white-flowered neighbourhoods (rank difference=0.059, raw p=0.022), but this
did not cross the five-scale maxT threshold (p=0.076). Population–DID alignment
showed the same pattern (difference=0.054, raw p=0.029, maxT p=0.084).
Nine of 16 candidates occurred in the DID-proximate high-population class
versus a natural-map mean fraction of 0.313 (raw p=0.082, maxT p=0.117).
Early flowering and dark pigmented colour did not converge on the leading
candidate.

Weak human-context contrasts are compatible with at least three explanations:
the effect is small, the proxies are incomplete, or the YAMAP sampling frame
has already restricted observations to accessible hiking routes and therefore
reduced the contrast between candidate and comparison cells. Population and
road proximity also affect observation opportunity. The analysis does not
distinguish these possibilities and cannot establish horticultural origin.

## Novelty statement

The defensible novelty is the complete workflow, not any single component:

1. repurposing a GPS-enabled hiking platform to fill a quantitative
   geographical trait gap;
2. treating visible pigmentation expression and pigmented-only optical
   intensity as a two-part response;
3. treating environmental and SPDE components as national-scale findings while
   testing the predicted pollinator-community hypothesis in nearby pairs;
4. replaying a pre-specified local event on 1,000 fitted-natural maps instead
   of turning first-stage residuals into a new trait; and
5. examining population, land use, and dense-settlement context only after
   candidates and comparison neighbourhoods were fixed.

This design limits circularity and aligns each proxy with a biologically
plausible spatial scale. It does not remove all confounding.

## Reviewer-resistant wording

Prefer:

- “YAMAP hiking-activity records”;
- “route-linked georeferenced photographs”;
- “optical pigmentation expression”;
- “conditional visible colour intensity”;
- “continuous residual geography”;
- “predicted *Bombus*-community fingerprint”;
- “scale-specific turnover correspondence”;
- “repeated departure from the fitted natural baseline”; and
- “candidate consistent with anthropogenic influence.”

Avoid:

- “first SNS flower-colour quantification”;
- “anthocyanin amount” for uncalibrated image colour;
- “SPDE removed spatial bias”;
- “*Bombus* effect,” abundance, richness, or selection pressure;
- “natural-model residual is the trait”; and
- “horticultural escape,” introduced genotype, or gene pollution without
  provenance and genetic evidence.
