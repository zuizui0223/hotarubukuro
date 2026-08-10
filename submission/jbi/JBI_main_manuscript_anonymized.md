# From broad geography to local boundaries: flower-colour biogeography from hiking photographs

**Running title:** Layered flower-colour biogeography

## Abstract

**Aim:** Geographical trait variation matters because it can mediate local adaptation and species interactions, yet abiotic, biotic, historical and human processes often share the same spatial structure. We asked whether a quantitative flower-colour polymorphism could be resolved through an integrated design that changes spatial scale and comparison unit with each ecological question.

**Location:** Japan.

**Taxon:** *Campanula punctata* (Campanulaceae).

**Methods:** We repurposed GPS-linked YAMAP hiking photographs from 2023–2025 and derived a two-part phenotype: white versus pigmented state and visible intensity conditional on pigmentation. National environment-plus-INLA-SPDE models established the broad template. Rather than treating environmentally derived bumblebee SDMs as independent national predictors, we tested Bombus-blind white–pigmented transitions within 5 km against occurrence-referenced availability of the focal pollinators *Bombus ardens* and *B. diversus*. Finally, we defined pigmented cells embedded among environmentally similar white neighbours as ecological events, replayed the same detector on repeated natural predictive maps and only then examined human context.

**Results:** The dataset contained 1,922 photographs. Broad models showed distinct environmental and spatial structure for pigmentation state and pigmented-only intensity. Across 67 sharp 5-km transitions, mean focal-bumblebee availability was higher on the pigmented side, but the pattern was magnitude-driven and not robust across exposure or scale sensitivities. Apparent high-elevation Bombus–pigmentation correspondence disappeared in near-equal-elevation comparisons. Seventeen local departures were reproducibly identified, but neither their frequency nor human-context associations supported a robust additional anthropogenic process.

**Main conclusions:** Broad environment and space define the principal flower-colour template; any pollinator contribution appears, at most, highly local and concerns pigmentation state rather than colour intensity; and visually unusual populations should not automatically be treated as anthropogenic anomalies. The study shows why integrative trait biogeography must connect, rather than collapse, processes operating at different scales, and how repurposed recreational imagery can make that integration possible.

**Keywords:** digital phenotyping, flower colour, hiking photographs, iEcology, intraspecific variation, pollination, spatial confounding, trait biogeography

## Introduction

Geographical variation within species is not merely descriptive noise around a species mean. Intraspecific trait variation can alter individual performance, population persistence, species interactions and the ecological breadth of populations across heterogeneous environments (Westerband et al., 2021). Understanding how such variation is organized across a species range is therefore central to explaining local adaptation and the geographical mosaic of ecological interactions. Yet the same phenotype can be shaped by several processes at once: abiotic conditions, dispersal and population history, biotic interactions and, increasingly, human movement or habitat modification. These alternatives matter because they imply different mechanisms and different expectations for how trait distributions should respond when environments or interacting species change.

Flower colour is an unusually integrative trait for addressing this problem. Floral pigmentation is simultaneously a physiological phenotype and a reproductive signal: pigments and floral optics can covary with abiotic conditions, while visible colour can influence pollinator attraction, mating patterns and reproductive success (Rausher, 2008; Koski & Ashman, 2015; Trunschke et al., 2021). Geographic colour polymorphisms therefore provide a tractable window into how multiple selective agents and spatial history combine within one conspicuous phenotype. But that integrative potential creates an inferential danger. A climate gradient, a pollinator turnover and a historical population boundary can generate similar spatial patterns, so studying each process separately—or forcing them into one same-scale regression—can mistake shared geography for mechanism. The ecological need is therefore not simply for more predictors, but for an integrated design in which each process is examined at the spatial scale and comparison unit at which its biological meaning is most defensible.

That integration first requires a phenotype measured densely enough to reveal the geography that subsequent mechanisms must explain. Range-wide flower-colour studies often rely on discrete morph frequencies, whereas quantitative reflectance or pigment measurements are usually available from fewer populations (Arista et al., 2013; Koski & Galloway, 2020). This matters because a single white-to-pink axis can merge biologically distinct transitions. A threshold-like loss or activation of anthocyanin-pathway expression can separate white and visibly pigmented flowers, whereas variation among already pigmented flowers can additionally reflect pigment amount and floral optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). Distinguishing whether pigmentation is expressed from how intense that pigmentation becomes is therefore necessary not only for measurement precision, but also for asking whether different ecological processes act on different components of the phenotype.

Digital imagery offers a way to bridge the gap between broad categorical maps and small-sample quantitative measurements. Biodiversity platforms increasingly provide photographs from which visible phenotypes can be recovered (Laitly et al., 2021; Luong et al., 2023; McKenzie et al., 2026), while iEcology extends this idea to digital material originally created for other purposes (Jarić et al., 2020). We used YAMAP, a hiking-navigation and activity-diary platform, because public hiking activities can retain route-linked photograph locations across mountain systems. In a matched 2023–2025 Supplementary benchmark, the author-screened YAMAP retrieval yielded 1,964 georeferenced focal-species records compared with 516 iNaturalist observations with photographs; the matched GBIF image set was almost entirely syndicated from iNaturalist (Appendix S1). This does not make YAMAP unbiased. Its observation process is different: users document hikes rather than submit focal-species records, potentially reducing selection conditioned specifically on taxon reporting while retaining route choice, access, flower conspicuousness and subject-selection biases. For a mountain herb, this sampling frame is also deliberately concentrated in natural and semi-natural mountain environments relevant to wild-population geography, although it cannot establish wild provenance for every photograph. Exhaustive author screening and deterministic image phenotyping then convert that recreational image stream into a quantitative trait dataset rather than a direct occurrence export.

Once this measurement problem is solved, integration becomes more—not less—important. A detailed national flower-colour map exposes the fact that climate, topography, residual spatial history and predicted pollinator distributions can all share the same broad geography. Species-distribution models (SDMs) are themselves generated from environmental predictors, so a national flower-colour–pollinator association can be statistically precise while remaining biologically ambiguous (Soberón, 2007; Paciorek, 2010; Hanks et al., 2015). At the same time, genuine biotic selection is expected to arise through encounters among local plant and pollinator populations rather than through abstract overlap of two national surfaces (Araújo & Rozenfeld, 2014). An integrated analysis must therefore connect scales rather than collapse them: broad environment and continuous space should first establish the geographical template, after which a local biotic hypothesis can be asked at abrupt nearby boundaries. In this sense, integration means linking complementary pieces of evidence while preserving the ecological scale of each process.

*Campanula punctata* provides unusually strong natural-history grounds for such a local test. Its large tubular flowers are effectively pollinated by bumblebees, and realized Bombus assemblages vary geographically. In this species, flower–pollinator size matching can affect pollen removal and floral trait selection (Nagano et al., 2014), while in the Izu Islands the loss of bumblebee-dominated pollination is associated with major changes in reproductive ecology and breeding system (Inoue & Amano, 1986; Inoue, 1988). These studies do not show that bumblebees determine white versus pigmented flowers, but they make a directional colour hypothesis biologically meaningful. If visible pigmentation contributes to attraction or detection by important bumblebee visitors, the reproductive benefit of maintaining a pigmented state should be greater where those pollinators are locally available and may relax where their availability declines. This prediction concerns the maintenance of a visible pigment state more directly than progressive darkening among flowers that are already pigmented. We therefore focus the directional test on the broadly distributed, directly documented focal pollinators *B. ardens* and *B. diversus*. Montane and alpine Bombus species are not pooled into the primary availability metric because their niches overlap the same high-elevation geography in which pigmented flowers are common; they instead provide a guardrail against mistaking shared mountain biogeography for a pollinator mechanism.

The broad natural template is also ecologically important because it defines what should count as exceptional. Without such a reference, a locally unusual colour state can be interpreted too readily as evidence of a new process. A large fitted residual is not automatically a biological anomaly, because residual magnitude also reflects model uncertainty, leverage and sampling geometry. This distinction is especially relevant in *C. punctata*, which is cultivated ornamentally and may therefore experience planting, escape or introgression in addition to natural environmental and biotic processes. Human influence is consequently not treated as a competing national predictor. We first ask whether a locally discordant colour configuration is unusual relative to repeated natural predictive maps, and only then examine whether independently defined candidates occupy distinctive human context. This final layer broadens the integrated framework from natural trait biogeography to the question of when human activity may perturb an otherwise spatially structured phenotype.

These questions are therefore linked by ecological necessity rather than assembled as parallel analyses. Quantitative image phenotyping makes the national environmental–spatial template possible; that template reveals why same-scale Bombus overlap is difficult to interpret and motivates a local boundary test; and the same template supplies the predictive reference needed to calibrate local departures before human context is considered. We ask: (1) what broad environmental and spatial structure organizes pigmentation state and conditional visible intensity across Japan; (2) within that template, whether the sharpest nearby white–pigmented transitions align directionally with availability of focal bumblebee pollinators; and (3) whether locally discordant pigmented populations are more frequent than expected under the broad natural model and, once defined independently, occupy distinctive human context. By integrating these layers without forcing them into one scale, we aim to explain not only where flower-colour variation occurs, but how different ecological processes can be distinguished within the same geographical mosaic.

## Materials and Methods

### Digital sampling and two-part flower-colour phenotype

The predefined sampling frame covered the flowering seasons of 2023–2025. All recovered YAMAP candidate records in that frame were taken through author screening rather than subsampled. The source table contained 1,965 eligible records, including 1,964 YAMAP activity-photo rows representing 1,963 unique image hashes. Annual YAMAP counts were 642, 687 and 635 for 2023–2025, respectively. All retained YAMAP rows had complete calendar dates and finite coordinates in the frozen source table. The study treated these as route-linked photographs, not as an areally random population survey.

Before inclusion, every recovered candidate was visually checked for the focal organism and flower; incorrect or similar campanuloid subjects were excluded, the usable petal region was confirmed, and repeated images and photo-coordinate mappings were audited. The deterministic image pipeline retained source-row provenance, dates, coordinates and SHA-256 image hashes, summarized display-referred sRGB pixels within the confirmed petal region and converted median RGB to CIELAB under D65. CIELAB a* was used as a reproducible human-visible red–green phenotype, not as calibrated reflectance, ultraviolet contrast or anthocyanin concentration. Full data-source and image-QC details, including the YAMAP/iNaturalist/GBIF benchmark, are in Appendices S1–S2.

The white–pigmented boundary was estimated from a* alone without geography, environment, Bombus predictions or human context. Univariate Gaussian mixtures were compared by BIC and ordered by fitted mean; the selected four-component variable-variance model was collapsed at the largest adjacent mean gap. The operational boundary was a*=4.9688. The final analysis contained 966 white-like and 956 pigmented observations; 124 observations with intermediate regime-membership probability were retained with an ambiguity flag. We analysed (i) binary pigmentation state for all 1,922 observations and (ii) standardized visible intensity only among pigmented observations.

### Broad environmental and spatial template

Climate predictors came from CHELSA v2.1, soils from SoilGrids 2.0 and elevation from WorldClim 2.1; terrain derivatives were calculated from elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Response-blind principal components summarized correlated environmental structure (Appendix S3).

We fitted separate observation-level INLA-SPDE models for pigmentation state and conditional intensity. Pigmentation state used a Bernoulli likelihood with logit link and conditional intensity a Gaussian likelihood. Environmental variables entered as fixed effects and a Matérn SPDE field represented continuous residual geography (Lindgren et al., 2011), with penalised-complexity priors on spatial range and variance (Simpson et al., 2017). A separate cell-level predictive reference was fitted for later local analyses. Five geographical folds based on approximately 100-km blocks assessed transfer to withheld geography and generated cross-fitted predictive maps (Roberts et al., 2017; Valavi et al., 2019).

### Bumblebee SDMs and local sharp-transition test

Fresh SDMs were built for five focal Japanese Bombus species over a common Honshu–Shikoku–Kyushu domain using shared predictors, `maxnet`, spatial block partitions and finite-AICc model selection. These surfaces represent predicted habitat support rather than abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). Because raw values are not directly comparable among species, each flower-cell prediction was mapped to its empirical rank relative to predictions at that species' observed occurrence cells. The primary exposure was the maximum occurrence-referenced support of *B. ardens* and *B. diversus*, the two broadly distributed focal pollinators with direct support in the *C. punctata* system. Full SDM calibration and all-five alternatives are in Appendices S4–S5.

The main pollinator test intentionally changed scale. At the 1-km-cell level, we connected up to the five nearest neighbours within 5 km and identified pure white–pigmented transitions, defined as an absolute difference of one in observed cell pigment share. Pair selection was blind to Bombus values and to transition direction; non-overlapping pairs were selected before being oriented from white to pigmented. For each pair, we calculated pigmented-minus-white focal-pollinator support. The primary statistic was the mean contrast across pairs, tested with 100,000 sign flips. Environment did not select or orient pairs; as a diagnostic, environmental-PC distances of selected transitions were compared with all local graph edges.

Predefined sensitivities repeated the design at 10 and 25 km, relaxed the transition threshold and substituted raw SDM support. A supplementary guardrail tested whether support from the three montane/alpine Bombus species remained associated with pigmentation when transition endpoints differed by no more than 50 or 100 m elevation. Five-species community turnover and full spatial matching are retained in Appendix S5 because turnover is unsigned and does not predict which side of a boundary should be pigmented.

### Event-based local departures and human context

Rather than thresholding fitted residuals, we defined an ecological event: a pigmented cell with at least three neighbours within 10 km that were environmentally similar under the predefined multiscale environmental summary and contained no pigmented neighbours. Human variables did not enter this definition. The identical event detector was replayed on 10,000 held-out cross-fitted predictive maps and, as a sensitivity, 200,000 joint posterior-predictive maps. These references ask how often the fitted natural geography itself generates the same locally discordant configuration under the observed sampling geometry.

Only after candidate identities were fixed did we characterize human context using population, densely inhabited district (DID) metrics, land use and roads. Multiple comparisons within human-feature families used maxT familywise correction. Full event-definition and human-context sensitivities are in Appendix S6. All manuscript-facing analyses, seeds and output provenance are versioned; an anonymized peer-review data/code package will accompany submission.

## Results

### Broad geography separates pigmentation state from conditional intensity

The 1,922 photographs were nearly evenly divided between white-like (966) and pigmented (956) flowers. Pigmentation state declined along the temperature axis (posterior mean -0.542; 95% CrI -1.033 to -0.049). Conditional visible intensity also declined along that axis (mean -0.319; CrI -0.484 to -0.156) and along the topographic axis (mean -0.138; CrI -0.231 to -0.045). Other fixed effects were more uncertain after conditioning on the spatial field.

Substantial residual geography remained. Posterior mean SPDE range was 132.7 km (95% CrI 88.7–195.6) for pigmentation state and 60.9 km (31.0–115.8) for conditional intensity. Geographically blocked prediction gave image-level AUC=0.863 for pigmentation state (cell any-pigmented AUC=0.858; majority-pigmented AUC=0.871; Brier=0.150). Conditional intensity had RMSE=0.919, MAE=0.715 and 95% predictive coverage=0.944 across 674 pigmented-support cells. Thus the broad models formed a strong geographical reference while leaving substantial spatial structure not uniquely attributable to measured environment.

### Sharp local colour boundaries show weak alignment with focal bumblebee availability

The strict 5-km graph yielded 67 non-overlapping pure white–pigmented transitions with median separation 2.0 km. Their median environmental-PC distance was 0.180, compared with 0.343 among all local graph edges, indicating that the focal colour boundaries were not unusually divergent in the measured environmental summary.

Occurrence-referenced focal-bumblebee support was, on average, 0.0359 higher on the pigmented side (one-sided sign-flip P=0.0272). The pattern was not a majority-of-pairs effect: median contrast was -0.0028 and only 49.3% of pairs were positive. Across the 5-, 10- and 25-km pure-transition family, q=0.0815; mean contrasts attenuated to +0.0084 at 10 km (P=0.325) and +0.0029 at 25 km (P=0.436), and raw-cloglog support did not reproduce the 5-km result (P=0.267). We therefore regard the result as weak, highly local consistency with the directional hypothesis rather than evidence of pollinator-mediated selection.

The apparent high-elevation association between montane/alpine Bombus support and pigmented flowers did not survive the elevation guardrail: among pure transitions with <=50 m endpoint elevation difference, mean pigmented-minus-white montane support was near zero or negative at 5, 10 and 25 km (all one-sided P>=0.755; Appendix S5). No corresponding Bombus signal was supported for conditional intensity among pigmented flowers.

### Local departures are field targets, not demonstrated anthropogenic anomalies

The primary ecological-event definition identified 17 pigmented cells embedded among environmentally similar white neighbourhoods. On 10,000 held-out natural maps, the null mean candidate count was 13.61 (upper-tail P=0.200), and candidate fraction was also compatible with the reference (observed 0.0474; null mean 0.0343; P=0.0874). The joint spatial posterior-predictive sensitivity likewise showed no robust excess (count P=0.314; fraction P=0.196).

Human context was suggestive only after these locations had been fixed. The strongest contrasts involved 5-km population rank (directional P=0.0270; maxT familywise P=0.0899) and population-DID alignment (P=0.0230; maxT P=0.0759). We therefore found no robust evidence that the local-departure set requires an additional anthropogenic process or that the candidates have horticultural provenance.

## Discussion

### Broad flower-colour geography is a biological template, not a single environmental mechanism

The first answer to the flower-colour puzzle is that the polymorphism is strongly geographical. Both whether pigmentation was expressed and how intense already-pigmented flowers became covaried with broad environmental structure, while continuous residual spatial fields remained on scales of tens to more than 100 km. Previous work has shown that floral pigmentation and reflectance can track climatic gradients and colonization history (Koski & Ashman, 2015; Koski & Galloway, 2020); our results extend that logic by showing that the white–pigmented transition and within-pigmented intensity should not automatically be treated as one ecological axis. Temperature covaried with both components, whereas the additional topographic association was confined to intensity. The data therefore suggest at least two layers of geographical regulation: processes controlling whether a visible pigment state is maintained, and processes modulating colour after pigmentation is present.

We do not assign the temperature or topographic associations to a single physiological mechanism. The photographs are not pigment assays, environmental axes combine correlated variables and the spatial field can absorb unmeasured climate, dispersal history and population structure. Ecologically, the important result is that broad geography provides a strong baseline against which local hypotheses must be judged. That baseline also explains why simply adding a national pollinator SDM to the same regression would be unsatisfactory: a pollinator surface can inherit much of the same geography.

### If bumblebees matter for colour, the signal is more consistent with local maintenance of a pigment state than with regional darkening

The local transition analysis asks a narrower biological question than a national distribution overlap. In *C. punctata*, geographical changes in realized Bombus fauna can change pollinator–flower size matching and pollen removal, and island populations respond strongly to the loss of bumblebee pollination in their reproductive ecology (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). These natural-history results make pollinator opportunity biologically relevant even though they do not establish colour selection.

Against that background, the 5-km result has a coherent interpretation. If visible pigmentation contributes to attraction or detection by important bumblebee visitors, its reproductive benefit should be greatest where those visitors are available; where focal-pollinator opportunity declines, selection maintaining the pigmented state could relax. The fact that the signal appeared only at the strictest local scale is compatible with a process acting among neighbouring populations rather than across entire regions. However, the near-zero median, the 49% positive-pair fraction, the failure of raw SDM support and the null broader scales make the evidence weak. The result identifies a plausible local correspondence and a field-testable mechanism, not a causal effect.

The two-part phenotype sharpens that interpretation. We found no persuasive Bombus association with how dark already-pigmented flowers became. If the local signal reflects pollination at all, it is therefore more naturally framed as maintenance or loss of a visible pigment state than as selection for progressively darker flowers. This is the biological payoff of separating state from intensity at the start of the study.

The high-elevation Bombus result provides a second, equally important payoff. Montane and alpine species overlapped pigmented highland flowers on the national map, but that correspondence disappeared when nearby white and pigmented endpoints were constrained to nearly equal elevations. The visually striking association was therefore adequately explained by shared elevational biogeography in the present data. This negative guardrail is why the main availability metric uses the broadly distributed, directly documented focal pollinators rather than treating all five species as interchangeable. It also illustrates a general lesson for trait biogeography: map overlap between two environmentally structured biological variables can be ecologically compelling and still fail as evidence for a direct interaction.

### Locally discordant colour states narrow the provenance question without turning residuals into causes

The broad template does not make every local mismatch biologically exceptional. Seventeen pigmented cells met a reproducible ecological definition of being embedded in white, environmentally similar neighbourhoods, but comparable configurations arose often enough in natural predictive maps that their total number was not anomalous. This changes the ecological interpretation of visually surprising populations. A local departure from the dominant geographical pattern need not imply planting, introgression or another unmodelled process; spatially structured natural variation can generate some of the same configurations.

The ornamental use of *C. punctata* nevertheless gives the 17 sites a clear follow-up value. Human-context contrasts were in the expected direction for population-related metrics but remained familywise inconclusive, so the appropriate inference is not anthropogenic origin but prioritization. Vouchers, local planting histories and population-genetic comparisons among candidates, neighbouring wild populations and horticultural material could directly test provenance.

Here the YAMAP sampling frame returns as both strength and limitation. Concentration on hiking routes and mountains enriches the natural and semi-natural habitats relevant to broad wild-population geography, which is advantageous for the first two questions. The same concentration narrows the urban–rural gradient available to the final human-context test and may reduce its power, while roads and trailheads can create countervailing access bias. A single property of the data source therefore has different consequences at different inferential stages; making those consequences explicit is preferable to describing opportunistic imagery as either simply biased or simply abundant.

### A layered design can extract mechanism-oriented questions from opportunistic trait geography

The broader contribution is not a claim that environment, pollinators and humans have been partitioned into independent effects. It is a workflow for asking increasingly specific ecological questions without forcing them into one geographical regression. Recreational digital traces first expanded the measurable trait geography; a two-part phenotype then preserved biologically distinct aspects of colour; broad environment and space established the geographical template; a local transition design tested the pollinator hypothesis where it was most interpretable; and repeated ecological events, rather than residual tails, defined the final provenance targets.

For *C. punctata*, this sequence narrows the flower-colour puzzle substantially. Broad environment and spatial history dominate the national pattern; pigmentation state and intensity are not ecologically interchangeable; a bumblebee contribution, if present, is more likely to be local and associated with maintaining a pigment state; high-elevation pollinator overlap does not provide an independent mechanism; and locally unusual pigmented populations remain testable provenance targets rather than evidence of human origin. The unanswered mechanisms are now concrete: direct species-resolved visitation and fitness measurements can test the pollinator pathway, while spectroscopy, pigment assays and population genetics can connect image phenotypes to physiology and provenance. In that sense, the main value of the layered design is not to close the mystery with one coefficient, but to turn a national colour mosaic into a sequence of falsifiable ecological hypotheses.

## Acknowledgements

[Omitted from the anonymized manuscript for double-anonymous review.]

## References

Araújo, M. B., & Rozenfeld, A. (2014). The geographic scaling of biotic interactions. *Ecography*, 37, 406-415. https://doi.org/10.1111/j.1600-0587.2013.00643.x

Arista, M., Talavera, M., Berjano, R., & Ortiz, P. L. (2013). Abiotic factors may explain the geographical distribution of flower colour morphs and the maintenance of colour polymorphism in the scarlet pimpernel. *Journal of Ecology*, 101, 1613-1622. https://doi.org/10.1111/1365-2745.12151

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N. (2022). Global climate-related predictors at kilometer resolution for the past and future. *Earth System Science Data*, 14, 5573-5603. https://doi.org/10.5194/essd-14-5573-2022

Dick, C. A., Buenrostro, J., Butler, T., Carlson, M. L., Kliebenstein, D. J., & Whittall, J. B. (2011). Arctic mustard flower color polymorphism controlled by petal-specific downregulation at the threshold of the anthocyanin biosynthetic pathway. *PLoS ONE*, 6, e18230. https://doi.org/10.1371/journal.pone.0018230

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology*, 37, 4302-4358. https://doi.org/10.1002/joc.5086

Guillera-Arroita, G. (2015). Is my species distribution model fit for purpose? Matching data and models to applications. *Global Ecology and Biogeography*, 24, 276-292. https://doi.org/10.1111/geb.12268

Hanks, E. M., Schliep, E. M., Hooten, M. B., & Hoeting, J. A. (2015). Restricted spatial regression in practice: geostatistical models, confounding, and robustness under model misspecification. *Environmetrics*, 26, 243-254. https://doi.org/10.1002/env.2331

Inoue, K. (1988). Pattern of breeding-system change in the Izu Islands in *Campanula punctata*: Bumblebee-absence hypothesis. *Plant Species Biology*, 3, 125-128. https://doi.org/10.1111/j.1442-1984.1988.tb00178.x

Inoue, K., & Amano, M. (1986). Evolution of *Campanula punctata* in the Izu Islands: Changes of pollinators and evolution of breeding systems. *Plant Species Biology*, 1, 89-97. https://doi.org/10.1111/j.1442-1984.1986.tb00018.x

Jarić, I., Correia, R. A., Brook, B. W., Buettel, J. C., Courchamp, F., Di Minin, E., Firth, J. A., Gaston, K. J., Jepson, P., Kalinkat, G., Ladle, R., Soriano-Redondo, A., Souza, A. T., & Roll, U. (2020). iEcology: Harnessing large online resources to generate ecological insights. *Trends in Ecology & Evolution*, 35, 630-639. https://doi.org/10.1016/j.tree.2020.03.003

Karger, D. N., Conrad, O., Böhner, J., Kawohl, J., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

Koski, M. H., & Ashman, T.-L. (2015). Floral pigmentation patterns provide an example of Gloger's rule in plants. *Nature Plants*, 1, 14007. https://doi.org/10.1038/nplants.2014.7

Koski, M. H., & Galloway, L. F. (2020). Geographic variation in floral color and reflectance correlates with temperature and colonization history. *Frontiers in Plant Science*, 11, 991. https://doi.org/10.3389/fpls.2020.00991

Laitly, A. C., Callaghan, C. T., Delhey, K., & Cornwell, W. K. (2021). Is color data from citizen science photographs reliable for biodiversity research? *Ecology and Evolution*, 11, 4071-4083. https://doi.org/10.1002/ece3.7307

Lindgren, F., Rue, H., & Lindström, J. (2011). An explicit link between Gaussian fields and Gaussian Markov random fields: The stochastic partial differential equation approach. *Journal of the Royal Statistical Society: Series B*, 73, 423-498. https://doi.org/10.1111/j.1467-9868.2011.00777.x

Luong, Y., Gasca-Herrera, A., Misiewicz, T. M., & Carter, B. E. (2023). A pipeline for the rapid collection of color data from photographs. *Applications in Plant Sciences*, 11, e11546. https://doi.org/10.1002/aps3.11546

McKenzie, P. F., Church, S. H., & Hopkins, R. (2026). High-throughput iNaturalist image analysis reveals flower color divergence in *Monarda fistulosa*. *The American Naturalist*, 208, 101-109. https://doi.org/10.1086/739413

Nagano, Y., Abe, K., Kitazawa, T., Hattori, M., Hirao, A. S., & Itino, T. (2014). Changes in pollinator fauna affect altitudinal variation of floral size in a bumblebee-pollinated herb. *Ecology and Evolution*, 4, 3395-3407. https://doi.org/10.1002/ece3.1191

Paciorek, C. J. (2010). The importance of scale for spatial-confounding bias and precision of spatial regression estimators. *Statistical Science*, 25, 107-125. https://doi.org/10.1214/10-STS326

Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., & Rossiter, D. (2021). SoilGrids 2.0: Producing soil information for the globe with quantified spatial uncertainty. *SOIL*, 7, 217-240. https://doi.org/10.5194/soil-7-217-2021

Rausher, M. D. (2008). Evolutionary transitions in floral color. *International Journal of Plant Sciences*, 169, 7-21. https://doi.org/10.1086/523358

Renner, I. W., & Warton, D. I. (2013). Equivalence of MAXENT and Poisson point process models for species distribution modeling. *Biometrics*, 69, 274-281. https://doi.org/10.1111/j.1540-0420.2012.01824.x

Roberts, D. R., et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40, 913-929. https://doi.org/10.1111/ecog.02881

Simpson, D., Rue, H., Riebler, A., Martins, T. G., & Sørbye, S. H. (2017). Penalising model component complexity: A principled, practical approach to constructing priors. *Statistical Science*, 32, 1-28. https://doi.org/10.1214/16-STS576

Tasaki, K., et al. (2022). Identification of candidate genes responsible for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*, 13, 906879. https://doi.org/10.3389/fpls.2022.906879

Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower color evolution and the evidence of pollinator-mediated selection. *Frontiers in Plant Science*, 12, 617851. https://doi.org/10.3389/fpls.2021.617851

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation. *Methods in Ecology and Evolution*, 10, 225-232. https://doi.org/10.1111/2041-210X.13107

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait variation in plants: a renewed focus on its role in ecological processes. *Annals of Botany*, 127, 397-410. https://doi.org/10.1093/aob/mcab011

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016). How to colour a flower: On the optical principles of flower coloration. *Proceedings of the Royal Society B*, 283, 20160429. https://doi.org/10.1098/rspb.2016.0429

## Data Accessibility Statement

For double-anonymous review, an anonymized private repository will contain the derived flower-colour tables, environmental source registry, Bombus SDM configuration and occurrence-referenced support, analysis code, seeds, local-transition specifications, anomaly-event definitions and workflow provenance. Original YAMAP photographs are third-party content and cannot be redistributed; the derived trait table retains the provenance and quantitative measurements needed to reproduce manuscript analyses. A permanent public repository and DOI will replace the private review link upon acceptance.

## Conflict of Interest

[Statement withheld from the anonymized manuscript and supplied separately at submission.]

## Author Contributions

[Author contributions withheld from the anonymized manuscript and supplied on the separate title page/submission form.]
