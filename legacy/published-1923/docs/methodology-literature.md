# Methodological literature for the publication workflow

This note maps primary research and authoritative methodological papers to the
claims in the planned manuscript. It is not a claim that every cited method is
implemented. In particular, papers on Spatial+ and posterior predictive checks
define interpretation or sensitivity-analysis options; they should not be cited
as if the present analysis had used those methods unless the code is updated.

## Reviewer-facing conclusions

1. The defensible phenotype is **two-part optical colour**: pigmentation
   expression across all flowers, followed by visible colour intensity
   conditional on being classified as pigmented. It is not a chemical
   anthocyanin assay and it is not a direct model of Bombus colour perception.
2. The two outcomes may reveal two distinct geographical clines, but this
   should be called a **two-part response hierarchy**, not two proven
   developmental or evolutionary stages.
3. National environment–space models and local near-neighbour Bombus tests
   estimate different scale-specific questions. This is a principled
   multi-scale design, not an attempt to make a weak coefficient significant
   by changing models.
4. An SPDE field estimates structured geography left after the measured
   covariates. It neither “removes spatial bias” nor uniquely identifies
   dispersal history, genes, or an omitted environmental variable.
5. Spatially blocked validation evaluates transfer to withheld geography and
   is normally harsher than random validation. A moderate or low AUC for a
   widespread species can reflect limited discrimination against the accessible
   background as well as model weakness.
6. Presence-background SDM outputs are relative suitability or relative
   occurrence intensity. A standardized five-species Bombus fingerprint can be
   used as a community-composition proxy, but not as abundance, visitation,
   pollination effectiveness, or directly comparable probabilities across
   species.
7. Replaying a pre-specified event definition on model-replicated maps is more
   coherent than regressing first-stage residuals on new predictors. It
   characterizes observations that are rare under the fitted natural model; it
   does not by itself explain why they occurred.
8. Population density, land use, early flowering, and local isolation may make
   a candidate more consistent with human introduction. Without planting
   records, field verification, or population genetics, none establishes
   horticultural provenance.

## Citation matrix

| Workflow issue | What the literature supports | What it does **not** support | Recommended insertion point and cautious language | Primary or methodological sources |
|---|---|---|---|---|
| Pigmentation presence versus intensity | Natural flower-colour polymorphisms can arise from threshold-like disruption or downregulation of the anthocyanin pathway, whereas variation among pigmented flowers can be quantitative. A separate binary expression component and pigmented-only intensity component are therefore biologically interpretable. | A Gaussian mixture boundary is not proof of a biochemical threshold in *Campanula punctata*. Two fitted outcomes do not prove two causal developmental stages. | **Introduction, final problem paragraph:** “We treated optical pigmentation expression and conditional colour intensity as distinct response components, because pigment loss and quantitative variation among pigmented flowers need not represent the same biological process.” **Methods, Phenotype construction:** call this a “two-part response hierarchy” or “hurdle-like representation,” not a chemically validated hurdle mechanism. | Dick et al. (2011), [doi:10.1371/journal.pone.0018230](https://doi.org/10.1371/journal.pone.0018230); Tasaki et al. (2022), [doi:10.3389/fpls.2022.906879](https://doi.org/10.3389/fpls.2022.906879) |
| Meaning of CIELAB and white-flower values | Petal appearance depends on pigment absorption and tissue scattering. Controlled work shows that visible colour metrics can covary with anthocyanin content within an already pigmented system. | Positive or noisy a* values in visually white flowers are not a calibrated anthocyanin concentration. White appearance is not proof that all flavonoids or UV-absorbing compounds are absent. Uncalibrated sRGB-to-CIELAB values cannot be translated directly to pigment mass. | **Methods, Colour measurement, immediately after a* definition:** “CIELAB a* was used as an optical red–green phenotype. Because the images were not chemically or spectrally calibrated, a* was not interpreted as anthocyanin concentration; in particular, small a* variation among white-like flowers was treated as measurement and optical variation.” **Discussion limitation:** chemical extraction or spectrophotometry is required to validate the proxy. | van der Kooi et al. (2016), [doi:10.1098/rspb.2016.0429](https://doi.org/10.1098/rspb.2016.0429); Tasaki et al. (2022), [doi:10.3389/fpls.2022.906879](https://doi.org/10.3389/fpls.2022.906879) |
| Human optical phenotype versus pollinator perception | Colour spaces are observer-dependent; receptor-based bee models require reflectance and visual-system assumptions. CIELAB remains useful for repeatable human-visible colour phenotyping. | CIELAB a* is not Bombus chromatic contrast, detectability, or preference, and ordinary photographs do not recover ultraviolet reflectance. | **Methods, last sentence of Colour measurement:** “The phenotype quantifies human-visible image colour and was not used as a model of Bombus visual contrast.” **Discussion, Bombus paragraph:** any visual-attraction mechanism remains a hypothesis for reflectance and behavioural experiments. | Chittka (1992), [doi:10.1007/BF00188925](https://doi.org/10.1007/BF00188925); Renoult et al. (2017), [doi:10.1111/brv.12230](https://doi.org/10.1111/brv.12230) |
| Value and bias of community-science photographs | Community-science photographs provide otherwise unavailable spatial and temporal coverage. Empirical validation shows that visible-spectrum colour signals can be recovered at aggregate scales, and published pipelines demonstrate landscape-scale flower-colour extraction. Manual confirmation of the focal flower and petal region directly addresses target/ROI errors. | Published validation does not make a single uncalibrated photograph an unbiased intraspecific colour measurement. Camera, exposure, saturation, illumination, photographer choice, trails, roads, season, and population density can all affect the sample. Manual ROI review does not correct colour balance or sampling effort. | **Introduction:** use photographs to address the geographical trait-data gap. **Methods:** explicitly state that the author visually confirmed the focal subject and specified the petal extraction region before scripted measurement. **Discussion limitation:** “Coverage was broad but opportunistic; observation density and image colour were subject to accessibility, observer, camera, and illumination processes.” | Laitly et al. (2021), [doi:10.1002/ece3.7307](https://doi.org/10.1002/ece3.7307); Luong et al. (2023), [doi:10.1002/aps3.11546](https://doi.org/10.1002/aps3.11546); Silva et al. (2018), [doi:10.1371/journal.pone.0197325](https://doi.org/10.1371/journal.pone.0197325); Geurts et al. (2023), [doi:10.1002/ecs2.4582](https://doi.org/10.1002/ecs2.4582) |
| Novelty relative to existing image-colour studies | Recent studies show that community-science images can reveal broad floral-colour divergence. The present contribution can be distinguished by its response hierarchy and its sequence of national natural baseline, local biotic turnover, and replicated-event characterization. | “The first quantitative use of social-media flower images” is no longer defensible. A novelty claim requires comparison to current image-phenotyping work, not only older qualitative floras. | **Introduction, final paragraph:** “We extend image-based geographical colour phenotyping by separating optical pigmentation expression from conditional intensity and by matching environmental, spatial, biotic, and human-context questions to different inferential scales.” Use “to our knowledge” only after a documented systematic search. | Luong et al. (2023), [doi:10.1002/aps3.11546](https://doi.org/10.1002/aps3.11546); McKenzie et al. (2026), [doi:10.1086/739413](https://doi.org/10.1086/739413) |
| Why broad environment–space and local biotic analyses are separated | Niche variables and biotic interactions have different natural measurement scales, and empirical work shows that the apparent importance of environmental filtering and interactions changes with grain and extent. A nationwide model can estimate broad environmental and continuous geographical pattern, whereas environment-matched near-neighbour pairs ask whether local turnover co-occurs. | Scale separation does not automatically remove confounding. Local pairs remain observational, share nodes, and may contain fine-scale environmental, historical, or sampling differences. A local association is not a causal pollinator effect. | **Introduction, design rationale:** “Rather than requiring one saturated model to identify processes acting at different scales, we estimated broad abiotic and spatial structure nationally and evaluated the biotic hypothesis among pre-defined nearby pairs.” **Methods, Local Bombus test:** state pair radius, response-blind construction, environmental-distance adjustment, node dependence, spatial restriction, and multiplicity control. **Discussion:** call the result “scale-specific correspondence.” | Soberón (2007), [doi:10.1111/j.1461-0248.2007.01107.x](https://doi.org/10.1111/j.1461-0248.2007.01107.x); Araújo and Rozenfeld (2014), [doi:10.1111/j.1600-0587.2013.00643.x](https://doi.org/10.1111/j.1600-0587.2013.00643.x); Mod et al. (2020), [doi:10.1111/1365-2745.13434](https://doi.org/10.1111/1365-2745.13434) |
| SPDE and spatial confounding | The SPDE approach provides a computational representation of a continuous Matérn Gaussian field. PC priors make the base model and prior tail statements explicit. Spatial random effects can absorb variation aligned with smooth covariates, so coefficient changes and uncertainty must be reported and interpreted as conditional on the spatial specification. | The SPDE field is not generic “noise,” but neither is it a uniquely identified biological mechanism. Adding SPDE does not guarantee unbiased causal environmental coefficients. A smaller coefficient after adding space is not proof that the original association was spurious, and an interval excluding zero is not proof of causality. | **Methods, National natural model:** describe mesh, coordinate reference system, likelihood, link, PC prior tail probabilities, and sensitivity to mesh/prior. **Results:** report non-spatial and SPDE-adjusted estimates as an evidence ladder, not competing truth claims. **Discussion:** “The field represents coherent residual geography compatible with unmeasured environment, demography, dispersal history, or genetic structure.” Consider a Spatial+ or alternative-scale sensitivity if environmental coefficient interpretation is central. | Lindgren et al. (2011), [doi:10.1111/j.1467-9868.2011.00777.x](https://doi.org/10.1111/j.1467-9868.2011.00777.x); Simpson et al. (2017), [doi:10.1214/16-STS576](https://doi.org/10.1214/16-STS576); Paciorek (2010), [doi:10.1214/10-STS326](https://doi.org/10.1214/10-STS326); Hanks et al. (2015), [doi:10.1002/env.2331](https://doi.org/10.1002/env.2331); Dupont et al. (2022), [doi:10.1111/biom.13656](https://doi.org/10.1111/biom.13656) |
| Multicollinearity and process proxies | Strong correlation is expected when temperature, elevation, geography, and predicted Bombus composition share biogeography. Prespecified composite axes can be retained to represent distinct hypotheses if uncertainty, condition diagnostics, and predictive increments are reported. | VIF-based deletion does not reveal which correlated process is causal. Separate models for highly correlated Bombus summaries are not independent replications, and individual species coefficients cannot be interpreted as independent pollination effects. | **Methods, Predictor diagnostics:** report correlations, VIF/condition diagnostics in the actual analysis subsets, common-support restrictions, and the reason for analyzing guild summaries separately. **Discussion:** “The analysis distinguishes information added by each proxy, not the independent effect of each Bombus species.” | Dormann et al. (2013), [doi:10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x) |
| Spatial block validation and predictive maps | Random cross-validation is optimistic for structured ecological data. Spatial blocking tests geographic transfer and should be aligned with the intended prediction task. ENMeval supports spatial partitions and complexity tuning for Maxent-type models. | Spatial CV does not make a map causal or guarantee transfer outside sampled environmental space. AUC is sensitive to geographic extent, background definition, and spatial sorting; it is not an absolute score of ecological truth. | **Methods, Validation:** state fold geometry, block size rationale, whether tuning was nested, and the target of transfer. **Results:** use “held-out geographic discrimination/prediction,” not “explanatory power.” Explain modest Bombus AUC using model limitations and broad accessible-range occupancy, without presenting breadth as the only possible cause. | Hijmans (2012), [doi:10.1890/11-0826.1](https://doi.org/10.1890/11-0826.1); Roberts et al. (2017), [doi:10.1111/ecog.02881](https://doi.org/10.1111/ecog.02881); Valavi et al. (2019), [doi:10.1111/2041-210X.13107](https://doi.org/10.1111/2041-210X.13107); Muscarella et al. (2014), [doi:10.1111/2041-210X.12261](https://doi.org/10.1111/2041-210X.12261); Lobo et al. (2008), [doi:10.1111/j.1466-8238.2007.00358.x](https://doi.org/10.1111/j.1466-8238.2007.00358.x) |
| Meaning of the Bombus SDMs and fingerprint | Presence-background models rank relative suitability or relative occurrence intensity under their sampling assumptions. Within-species standardization followed by community axes can summarize geographical turnover without selecting one highly correlated species as “the” driver. | Raw suitability values are not abundance, visitation rate, pollen transfer, selection strength, or necessarily comparable probabilities among species. A sum of uncalibrated raw species outputs is not observed species richness. | **Methods, Bombus SDMs:** “We used fresh ENMeval predictions to derive a standardized predicted-community fingerprint; previously generated TIFFs were not analysis inputs.” Specify output scale and within-species standardization before PCA/summing. **Results/Discussion:** use “predicted suitability turnover” or “predicted community fingerprint,” never “Bombus abundance/richness” unless independently observed. | Renner and Warton (2013), [doi:10.1111/j.1541-0420.2012.01824.x](https://doi.org/10.1111/j.1541-0420.2012.01824.x); Guillera-Arroita (2015), [doi:10.1111/geb.12268](https://doi.org/10.1111/geb.12268); Kass et al. (2021), [doi:10.1111/2041-210X.13628](https://doi.org/10.1111/2041-210X.13628) |
| Replicated-map event characterization versus residual regression | Posterior/predictive checking compares an observed statistic with the same statistic calculated on data replicated under the fitted model and can propagate parameter and process uncertainty. This supports pre-defining a local-isolate or extreme-event rule, replaying it on every natural-model replicate, and comparing candidate frequency/features with the replicated distribution. | A raw residual is not a new measured biological trait. Regressing first-stage residuals on correlated second-stage predictors can give biased estimates and incorrect uncertainty. Posterior predictive tail areas are model checks, not ordinary confirmatory p-values; using the data twice can make them conservative. Replication cannot attribute an anomaly to horticulture. | **Methods, Candidate definition:** specify the event without population, land use, DOY, colour extremeness chosen after inspection, or other human variables; in each of 1,000 natural-model replicates, rerun the identical extraction. State whether both latent-field uncertainty and observation/likelihood variation were sampled. **Methods, Human-context characterization:** compare pre-specified summary statistics with their replicated distributions; keep candidate identity out of model fitting. **Discussion:** call these “repeated departures from the natural baseline” and “follow-up candidates.” | Meng (1994), [doi:10.1214/aos/1176325622](https://doi.org/10.1214/aos/1176325622); Freckleton (2002), [doi:10.1046/j.1365-2656.2002.00618.x](https://doi.org/10.1046/j.1365-2656.2002.00618.x); Chambert et al. (2014), [doi:10.1002/ece3.993](https://doi.org/10.1002/ece3.993); Gabry et al. (2019), [doi:10.1111/rssa.12378](https://doi.org/10.1111/rssa.12378) |
| Limits of pollinator-selection inference | Pollinator-mediated selection on continuous flower colour requires evidence linking colour to pollinator behaviour and ultimately to differential fitness. Community-level colour preferences do not imply that similarly coloured species have the same pollinator assemblage. | Correspondence between flower-colour turnover and predicted Bombus suitability does not demonstrate preference, visitation, pollen transfer, reproductive success, or selection. The Izu-island pattern is motivation, not a mainland experimental replicate. | **Discussion, first Bombus paragraph:** “The local correspondence is consistent with a pollinator-community hypothesis but is not an estimate of pollinator-mediated selection.” **Future work:** standardized reflectance, Bombus visual models, visitation observations, pollen deposition/seed set, and colour-manipulation or reciprocal-transplant experiments at transition zones. | Reverté et al. (2016), [doi:10.1093/aob/mcw103](https://doi.org/10.1093/aob/mcw103); Trunschke et al. (2021), [doi:10.3389/fpls.2021.617851](https://doi.org/10.3389/fpls.2021.617851) |
| Limits of horticultural-origin inference | Human-associated landscape features and repeated local departures can prioritize sites. Studies that establish garden escape or source populations use historical records together with chloroplast, nuclear, microsatellite, or genomic assignment. | An isolated pigmented flower, dark colour, early date, or proximity to population is not proof of cultivar ancestry, planting, escape, introgression, or gene pollution. Failure to find these signatures also does not prove natural origin. | **Discussion, Human context:** “Several features may be jointly consistent with human-mediated introduction, but they are non-diagnostic exposure proxies.” **Final limitation/future work:** verify ranked sites, photograph habitat and neighbouring flowers, obtain vouchers and cultivation histories, and genotype candidates together with nearby wild populations and commercial cultivars. | Trusty et al. (2007), [doi:10.1007/s11252-007-0030-y](https://doi.org/10.1007/s11252-007-0030-y) |

## Exact manuscript placement

### Introduction

**Paragraph 1 — trait-data gap.** Cite Laitly et al. (2021), Luong et al.
(2023), and McKenzie et al. (2026). State that image collections expand
geographical coverage but contain heterogeneous camera and observation
processes.

**Paragraph 2 — why colour needs two responses.** Cite Dick et al. (2011), van
der Kooi et al. (2016), and Tasaki et al. (2022). End with:

> Treating every flower on one continuous axis can conflate the expression of
> visible pigmentation with quantitative variation among flowers that express
> it. We therefore estimated a pigmentation-expression cline across all
> observations and a conditional intensity cline among pigmented observations.

**Paragraph 3 — scale and confounding problem.** Cite Soberón (2007), Araújo
and Rozenfeld (2014), and Paciorek (2010). End with:

> We matched each hypothesis to its expected scale: environmental and
> continuous geographical structure were estimated across Japan, whereas the
> Bombus hypothesis was tested as correspondence in turnover among pre-defined
> nearby flower-colour pairs.

**Paragraph 4 — study aims and novelty.** Use:

> Our contribution is a scale-aware inferential workflow rather than a single
> saturated model: (i) author-reviewed image regions yield a two-part optical
> phenotype, (ii) environmental and SPDE components define a national natural
> baseline, (iii) nearby pairs test whether flower-colour turnover corresponds
> to turnover in a predicted Bombus-community fingerprint, and (iv) replicated
> natural-model maps identify repeatedly unusual local events for independent
> human-context characterization.

Avoid “first study” unless a formal search establishes it. The strong,
defensible novelty is the **combination and ordering** of the four parts.

### Methods

1. **Image review and colour extraction:** describe manual confirmation before
   scripted calculation; cite Laitly et al. (2021) and Luong et al. (2023).
2. **Two-part phenotype:** distinguish operational mixture classification from
   chemical validation; cite Dick et al. (2011) and Tasaki et al. (2022).
3. **National environment–SPDE model:** cite Lindgren et al. (2011), Simpson et
   al. (2017), and Paciorek (2010); report mesh and prior sensitivity.
4. **Spatial validation:** cite Roberts et al. (2017), Valavi et al. (2019),
   Hijmans (2012), and ENMeval papers.
5. **Predicted Bombus community:** cite Renner and Warton (2013),
   Guillera-Arroita (2015), and Kass et al. (2021); state explicitly that raw
   outputs are neither abundance nor visitation.
6. **Near-neighbour turnover:** cite the scale literature; explain response-blind
   pair creation, shared-node adjustment, environmental similarity, spatial
   restriction, and permutation/null design.
7. **Repeated departure definition:** cite Chambert et al. (2014), Gabry et al.
   (2019), and Freckleton (2002); state that the event extractor is replayed
   unchanged on each replicated map.
8. **Human-context characterization:** say candidate generation and
   characterization use separate variable sets. Pre-specify the summaries and
   multiplicity correction. Do not use residual magnitude as a second-stage
   continuous response for a causal coefficient.

### Results

Organize results in the same order as the estimands:

1. optical threshold and classification uncertainty;
2. national pigmentation-expression cline;
3. pigmented-only intensity cline;
4. environment and SPDE field, with spatially held-out performance;
5. local flower-colour/Bombus fingerprint turnover correspondence;
6. frequency and recurrence of local departure events under replicated maps;
7. human-context feature convergence or lack of convergence.

For the two colour outcomes, “two-part clines” is safer than “two-stage
mechanism.” Report environmental estimates before and after SPDE, their
intervals, and predictive change. Do not rank mechanisms from WAIC differences
alone.

### Discussion

**Paragraph 1 — principal advance.**

> Quantitative image phenotyping exposed two geographical components hidden by
> a single white-to-pink score: the probability that visible pigmentation was
> expressed and, conditional on expression, variation in optical intensity.

Immediately add that optical intensity is not chemically calibrated pigment
mass.

**Paragraph 2 — environment and space.** Treat both as findings. Environmental
coefficients are conditional associations at their measured resolution; the
SPDE field is unresolved structured geography. Cite Paciorek (2010) and Hanks et
al. (2015).

**Paragraph 3 — scale-aware Bombus result.**

> The weak national incremental contribution and the local turnover
> correspondence are not contradictory: they address different estimands and
> scales. The latter indicates correspondence with a predicted community
> fingerprint after local natural-geographical controls, not direct selection.

Cite Soberón (2007), Mod et al. (2020), Guillera-Arroita (2015), and Trunschke
et al. (2021).

**Paragraph 4 — natural-model departures.**

> Repeated map replication converted “large residuals” into a pre-defined event
> question: how often would the same kind of locally discordant flower-colour
> event arise under the fitted natural baseline? This retains model dependence
> but avoids treating fitted residuals as a new phenotype.

Cite Freckleton (2002), Chambert et al. (2014), and Gabry et al. (2019).

**Paragraph 5 — human context and limits.** State whether multiple
pre-specified features converged, did not converge, or were inconsistent. Even
under convergence, use “candidate anthropogenic influence.” Cite Trusty et al.
(2007) to explain why historical and genetic source assignment is the necessary
next test.

**Final limitations paragraph.** Include all of the following:

- non-random sites and photographs;
- uncalibrated visible colour, with no UV/reflectance or chemical assay;
- uncertainty in the operational pigmentation boundary;
- environmental rasters and Bombus SDMs measured at coarser resolution than
  individual interactions;
- spatial confounding and unmeasured historical/genetic structure;
- Bombus suitability not abundance, visitation, or fitness;
- replicated departures conditional on the fitted natural model;
- no causal or genetic test of horticultural origin.

## Suggested claim vocabulary

Prefer:

- “optical pigmentation expression”;
- “conditional visible colour intensity”;
- “two-part geographical clines”;
- “continuous residual geography”;
- “predicted Bombus-community fingerprint”;
- “scale-specific turnover correspondence”;
- “incremental held-out information”;
- “repeated departure from the fitted natural baseline”;
- “candidate consistent with anthropogenic influence”;
- “prioritized for field and genetic validation.”

Avoid:

- “anthocyanin amount” for uncalibrated image a*;
- “bee-visible redness” or “Bombus attraction” from CIELAB;
- “SPDE removed spatial bias”;
- “Bombus effect” or “pollinator selection pressure”;
- “Bombus abundance/richness” from summed SDM suitability;
- “natural-model residual is the trait”;
- “horticultural escape,” “introduced genotype,” or “gene pollution” without
  provenance evidence.

## Reference list

- Araújo, M. B., & Rozenfeld, A. (2014). The geographic scaling of biotic
  interactions. *Ecography*, 37, 406–415.
  [https://doi.org/10.1111/j.1600-0587.2013.00643.x](https://doi.org/10.1111/j.1600-0587.2013.00643.x)
- Chambert, T., Rotella, J. J., & Higgs, M. D. (2014). Use of posterior
  predictive checks as an inferential tool for investigating individual
  heterogeneity in animal population vital rates. *Ecology and Evolution*, 4,
  1389–1397.
  [https://doi.org/10.1002/ece3.993](https://doi.org/10.1002/ece3.993)
- Chittka, L. (1992). The colour hexagon: a chromaticity diagram based on
  photoreceptor excitations as a generalized representation of colour
  opponency. *Journal of Comparative Physiology A*, 170, 533–543.
  [https://doi.org/10.1007/BF00188925](https://doi.org/10.1007/BF00188925)
- Dick, C. A., Buenrostro, J., Butler, T., Carlson, M. L., Kliebenstein, D. J.,
  & Whittall, J. B. (2011). Arctic mustard flower color polymorphism controlled
  by petal-specific downregulation at the threshold of the anthocyanin
  biosynthetic pathway. *PLoS ONE*, 6, e18230.
  [https://doi.org/10.1371/journal.pone.0018230](https://doi.org/10.1371/journal.pone.0018230)
- Dormann, C. F., et al. (2013). Collinearity: a review of methods to deal with
  it and a simulation study evaluating their performance. *Ecography*, 36,
  27–46.
  [https://doi.org/10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)
- Dupont, E., Wood, S. N., & Augustin, N. H. (2022). Spatial+: a novel approach
  to spatial confounding. *Biometrics*, 78, 1279–1290.
  [https://doi.org/10.1111/biom.13656](https://doi.org/10.1111/biom.13656)
- Freckleton, R. P. (2002). On the misuse of residuals in ecology: regression
  of residuals vs. multiple regression. *Journal of Animal Ecology*, 71,
  542–545.
  [https://doi.org/10.1046/j.1365-2656.2002.00618.x](https://doi.org/10.1046/j.1365-2656.2002.00618.x)
- Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019).
  Visualization in Bayesian workflow. *Journal of the Royal Statistical
  Society: Series A*, 182, 389–402.
  [https://doi.org/10.1111/rssa.12378](https://doi.org/10.1111/rssa.12378)
- Geurts, E. M., et al. (2023). Turning observations into biodiversity data:
  broadscale spatial biases in community science. *Ecosphere*, 14, e4582.
  [https://doi.org/10.1002/ecs2.4582](https://doi.org/10.1002/ecs2.4582)
- Guillera-Arroita, G. (2015). Is my species distribution model fit for purpose?
  Matching data and models to applications. *Global Ecology and Biogeography*,
  24, 276–292.
  [https://doi.org/10.1111/geb.12268](https://doi.org/10.1111/geb.12268)
- Hanks, E. M., Schliep, E. M., Hooten, M. B., & Hoeting, J. A. (2015).
  Restricted spatial regression in practice: geostatistical models, confounding,
  and robustness under model misspecification. *Environmetrics*, 26, 243–254.
  [https://doi.org/10.1002/env.2331](https://doi.org/10.1002/env.2331)
- Hijmans, R. J. (2012). Cross-validation of species distribution models:
  removing spatial sorting bias and calibration with a null model. *Ecology*,
  93, 679–688.
  [https://doi.org/10.1890/11-0826.1](https://doi.org/10.1890/11-0826.1)
- Kass, J. M., et al. (2021). ENMeval 2.0: redesigned for customizable and
  reproducible modeling of species' niches and distributions. *Methods in
  Ecology and Evolution*, 12, 1602–1608.
  [https://doi.org/10.1111/2041-210X.13628](https://doi.org/10.1111/2041-210X.13628)
- Laitly, A., Callaghan, C. T., Delhey, K., & Cornwell, W. K. (2021). Is color
  data from citizen science photographs reliable for biodiversity research?
  *Ecology and Evolution*, 11, 4071–4083.
  [https://doi.org/10.1002/ece3.7307](https://doi.org/10.1002/ece3.7307)
- Lindgren, F., Rue, H., & Lindström, J. (2011). An explicit link between
  Gaussian fields and Gaussian Markov random fields: the stochastic partial
  differential equation approach. *Journal of the Royal Statistical Society:
  Series B*, 73, 423–498.
  [https://doi.org/10.1111/j.1467-9868.2011.00777.x](https://doi.org/10.1111/j.1467-9868.2011.00777.x)
- Lobo, J. M., Jiménez-Valverde, A., & Real, R. (2008). AUC: a misleading
  measure of the performance of predictive distribution models. *Global
  Ecology and Biogeography*, 17, 145–151.
  [https://doi.org/10.1111/j.1466-8238.2007.00358.x](https://doi.org/10.1111/j.1466-8238.2007.00358.x)
- Luong, Y., Gasca-Herrera, A., Misiewicz, T. M., & Carter, B. E. (2023). A
  pipeline for the rapid collection of color data from photographs.
  *Applications in Plant Sciences*, 11, e11546.
  [https://doi.org/10.1002/aps3.11546](https://doi.org/10.1002/aps3.11546)
- McKenzie, P. F., Church, S. H., & Hopkins, R. (2026). High-throughput
  iNaturalist image analysis reveals flower color divergence in *Monarda
  fistulosa*. *The American Naturalist*, 208, 101–109.
  [https://doi.org/10.1086/739413](https://doi.org/10.1086/739413)
- Meng, X.-L. (1994). Posterior predictive p-values. *The Annals of Statistics*,
  22, 1142–1160.
  [https://doi.org/10.1214/aos/1176325622](https://doi.org/10.1214/aos/1176325622)
- Mod, H. K., et al. (2020). Scale dependence of ecological assembly rules:
  insights from empirical datasets and joint species distribution modelling.
  *Journal of Ecology*, 108, 1967–1977.
  [https://doi.org/10.1111/1365-2745.13434](https://doi.org/10.1111/1365-2745.13434)
- Muscarella, R., et al. (2014). ENMeval: an R package for conducting spatially
  independent evaluations and estimating optimal model complexity for Maxent
  ecological niche models. *Methods in Ecology and Evolution*, 5, 1198–1205.
  [https://doi.org/10.1111/2041-210X.12261](https://doi.org/10.1111/2041-210X.12261)
- Tasaki, K., et al. (2022). Identification of candidate genes responsible
  for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant
  Science*, 13, 906879.
  [https://doi.org/10.3389/fpls.2022.906879](https://doi.org/10.3389/fpls.2022.906879)
- Paciorek, C. J. (2010). The importance of scale for spatial-confounding bias
  and precision of spatial regression estimators. *Statistical Science*, 25,
  107–125.
  [https://doi.org/10.1214/10-STS326](https://doi.org/10.1214/10-STS326)
- Renner, I. W., & Warton, D. I. (2013). Equivalence of MAXENT and Poisson
  point process models for species distribution modeling in ecology.
  *Biometrics*, 69, 274–281.
  [https://doi.org/10.1111/j.1541-0420.2012.01824.x](https://doi.org/10.1111/j.1541-0420.2012.01824.x)
- Renoult, J. P., Kelber, A., & Schaefer, H. M. (2017). Colour spaces in
  ecology and evolutionary biology. *Biological Reviews*, 92, 292–315.
  [https://doi.org/10.1111/brv.12230](https://doi.org/10.1111/brv.12230)
- Reverté, S., Retana, J., Gómez, J. M., & Bosch, J. (2016). Pollinators show
  flower colour preferences but flowers with similar colours do not attract
  similar pollinators. *Annals of Botany*, 118, 249–257.
  [https://doi.org/10.1093/aob/mcw103](https://doi.org/10.1093/aob/mcw103)
- Roberts, D. R., et al. (2017). Cross-validation strategies for data with
  temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40,
  913–929.
  [https://doi.org/10.1111/ecog.02881](https://doi.org/10.1111/ecog.02881)
- Silva, S. J., Barbieri, L. K., & Thomer, A. K. (2018). Observing vegetation
  phenology through social media. *PLoS ONE*, 13, e0197325.
  [https://doi.org/10.1371/journal.pone.0197325](https://doi.org/10.1371/journal.pone.0197325)
- Simpson, D., Rue, H., Riebler, A., Martins, T. G., & Sørbye, S. H. (2017).
  Penalising model component complexity: a principled, practical approach to
  constructing priors. *Statistical Science*, 32, 1–28.
  [https://doi.org/10.1214/16-STS576](https://doi.org/10.1214/16-STS576)
- Soberón, J. (2007). Grinnellian and Eltonian niches and geographic
  distributions of species. *Ecology Letters*, 10, 1115–1123.
  [https://doi.org/10.1111/j.1461-0248.2007.01107.x](https://doi.org/10.1111/j.1461-0248.2007.01107.x)
- Trusty, J. L., Goertzen, L. R., Zipperer, W. C., & Lockaby, B. G. (2007).
  Invasive Wisteria in the southeastern United States: genetic diversity,
  hybridization and the role of urban centers. *Urban Ecosystems*, 10, 379–395.
  [https://doi.org/10.1007/s11252-007-0030-y](https://doi.org/10.1007/s11252-007-0030-y)
- Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower
  color evolution and the evidence of pollinator-mediated selection.
  *Frontiers in Plant Science*, 12, 617851.
  [https://doi.org/10.3389/fpls.2021.617851](https://doi.org/10.3389/fpls.2021.617851)
- Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019).
  blockCV: an R package for generating spatially or environmentally separated
  folds for k-fold cross-validation of species distribution models. *Methods in
  Ecology and Evolution*, 10, 225–232.
  [https://doi.org/10.1111/2041-210X.13107](https://doi.org/10.1111/2041-210X.13107)
- van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016).
  How to colour a flower: on the optical principles of flower coloration.
  *Proceedings of the Royal Society B*, 283, 20160429.
  [https://doi.org/10.1098/rspb.2016.0429](https://doi.org/10.1098/rspb.2016.0429)
