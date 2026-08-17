# From broad geography to local boundaries: biogeography of flower-colour polymorphism from hiking photographs

**Running title:** Flower-colour polymorphism

## Abstract

**Aim:** Environment, population history, biotic interactions and human movement can produce similar trait maps. We asked how these overlapping processes structure flower-colour polymorphism in *Campanula punctata* across Japan.

**Location:** Japan.

**Taxon:** *Campanula punctata* (Campanulaceae).

**Methods:** We first built a national flower-colour dataset from author-screened YAMAP hiking photographs and separated pigmentation state from colour intensity among pigmented flowers. We then fitted broad-scale INLA-SPDE models of environment and continuous spatial structure. Because bumblebee SDMs share environmental geography with flower colour, we changed comparison scale and tested 67 sharp white-pigmented transitions against predicted habitat support for *Bombus ardens* and *B. diversus*. Finally, we defined local pigmented departures without human variables, replayed the same event detector on 10,000 natural predictive maps, and examined human context only after candidate sites were fixed.

**Results:** The first reveal was that flower colour did not form one ecological gradient. Pigmentation was less likely in warmer climates, whereas intensity among pigmented flowers depended on Temperature PC1 × temperature seasonality and was lower in wetter and more rugged environments. The broad map then gave way to only weak local bumblebee correspondence: mean support was slightly higher on pigmented sides, but the effect was driven by a few large contrasts and faded at broader scales. Sixteen local departures looked striking but were not more frequent than expected under the natural model. Population exposure within 5 km was the strongest human-context feature, but global maxT FWER P=0.0548.

**Main conclusions:** Each layer changed the next question. National images exposed two components of flower-colour geography; broad environment left unresolved spatial structure; pollinator overlap narrowed to a weak local maintenance hypothesis; and apparent exceptions remained plausible under natural variation. The result is a scale-dependent explanation of how physiological context, history, local pollinator opportunity and possible human influence can jointly maintain an intraspecific polymorphism.

**Keywords:** digital phenotyping, flower colour, hiking photographs, iEcology, intraspecific variation, pollination, spatial confounding, trait biogeography

## 1. Introduction

### 1.1 The geographical mystery of flower-colour polymorphism

Why does one species remain white in some parts of its range and pigmented in others? This is not only a descriptive question. Intraspecific variation can change performance, persistence and species interactions (Westerband et al., 2021), and different explanations predict different responses to environmental change. Yet the map alone can mislead. Climate, population history, biotic interactions and human movement are all spatially structured, so several processes can draw similar geographical patterns.

Flower colour makes this problem especially revealing because anthocyanin pigmentation can do two ecological jobs at once. Pigments alter how petals absorb and reflect light and can be involved in responses to temperature, water stress and floral heat balance. The same visible colour is also a signal to pollinators and may alter reproductive success. A pigmented morph can therefore gain a physiological benefit in one setting and a reproductive benefit in another (Warren & Mackenzie, 2001; Kellenberger et al., 2019; Trunschke et al., 2021). Spatial variation in those benefits offers a plausible route by which white and pigmented flowers persist within one species.

### 1.2 The first hidden layer: state is not intensity

The apparent white-to-dark continuum may itself contain two biological questions. A white-pigmented transition can reflect whether the anthocyanin pathway is visibly expressed at all. Variation among flowers that are already pigmented can instead reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). A process that changes whether pigmentation is switched on need not control how dark the flower becomes afterwards. We therefore separated pigmentation state from visible intensity before asking what geography explains either response.

This split also gives environmental hypotheses more precise targets. Low temperature can increase chalcone-synthase expression and anthocyanin accumulation in corollas (Shvarts et al., 1997), floral pigmentation has been linked to aridity (Sullivan & Koski, 2021), and darker petals can absorb more radiation and alter flower temperature (Li et al., 2026). In *C. punctata*, UV-B exclusion changes flavonoid accumulation in leaves (Hashiba et al., 2006), showing environmental responsiveness of the flavonoid system in the focal species. These precedents predict a broad thermal signal, with moisture, climatic variability, radiation and terrain modifying either the presence or the expression of pigmentation.

### 1.3 Solving measurement reveals the attribution problem

Testing those ideas across Japan first required a phenotype dense enough to reveal the pattern. We built that phenotype ourselves from YAMAP, a hiking-navigation and activity-diary platform. Every retrieved candidate was screened for taxon identity, focal flower and usable petal region; duplicate images were audited; and colour was extracted with a fixed image pipeline. In the matched 2023-2025 period, YAMAP yielded 1,964 georeferenced focal-species records, compared with 516 iNaturalist observations with photographs. YAMAP is not unbiased, but it provided a complementary mountain-route image stream that could be curated into a quantitative trait dataset. This is an iEcology use of digital material created for another purpose (Jarić et al., 2020; Appendices S1-S2).

Making the national pattern visible did not solve the causal problem; it exposed it. Climate and topography are spatially structured, population history is spatially structured, and bumblebee species-distribution models (SDMs) are built from environmental predictors. A national model could therefore assign a precise coefficient to Bombus support even when flower colour and bumblebees merely follow the same climate (Soberón, 2007). Biotic interactions are also scale dependent (Araújo & Rozenfeld, 2014). We therefore used the national analysis to establish the broad environmental and spatial template, then changed comparison scale for the pollinator hypothesis.

### 1.4 From broad geography to local boundaries and local exceptions

The local pollinator test has a strong natural-history basis in *C. punctata*. Bumblebees are effective pollinators of its tubular flowers, and geographical changes in *Bombus* fauna are linked to pollen-removal ecology and breeding-system change (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). However, rather than asking whether national maps overlap, we asked whether predicted habitat opportunity for focal bumblebees increases specifically from the white side to the pigmented side of abrupt nearby boundaries. This scale shift makes the comparison more local without pretending that an SDM measures visits or selection.

The broad natural template creates one final question: what should count as exceptional? A point with a large residual is not automatically a new biological process. We instead defined a relational event—a pigmented cell surrounded by nearby environmentally similar white cells—and asked how often the fitted natural geography generated the same configuration. Only after candidate sites were fixed did we inspect human context. This order is important because *C. punctata* is cultivated, so planting, escape or introgression are plausible clues, but they should not define the sites used to support them.

### 1.5 Predictions

The analysis follows one dependent sequence. First, if flower colour contains two ecological layers, pigmentation state and pigmented-only intensity should show different broad geography. Second, if focal bumblebees help maintain visible pigmentation, predicted habitat support should be higher on the pigmented side of nearby white-pigmented boundaries, even though national overlap alone is ambiguous. Third, if local pigmented departures require an additional process, the observed ecological events should be more frequent than events generated by the natural model. Human context is a post-selection follow-up to that third prediction. Each answer determines the comparison needed for the next layer.

## 2. Materials and Methods

### 2.1 Study system and YAMAP sampling

The sampling frame covered the 2023-2025 flowering seasons. We screened all YAMAP records returned by the fixed Japanese focal-name search frame rather than subsampling candidates. Taxonomic errors and non-focal flowers were removed, and the focal flower and usable petal region were confirmed. Duplicate-image and raster-completeness checks left 1,922 observations for environmental analysis. Appendices S1-S2 give the complete record flow, matched public-database benchmark and sampling limitations.

### 2.2 From photographs to a two-part phenotype

The image pipeline retained source provenance, date, coordinates and image hashes. Display-referred sRGB pixels within the confirmed petal region were summarized and median RGB was converted to CIELAB under D65. CIELAB a* is used here as a reproducible human-visible red-green phenotype, not as calibrated spectral reflectance, UV contrast or anthocyanin concentration.

The white-pigmented boundary was estimated from a* alone before geography, environment, Bombus predictions or human data were used. Gaussian-mixture classification gave an operational boundary of a*=4.9688. The final dataset contained 966 white-like and 956 pigmented observations. Pigmentation state was analysed across all 1,922 observations, and standardized visible intensity was analysed only among pigmented flowers. Appendix S2 reports the mixture model, ambiguity checks and phenotype sensitivities.

### 2.3 Broad environmental and continuous spatial geography

Climate predictors came from CHELSA v2.1, soils from SoilGrids 2.0 and terrain from derivatives of WorldClim 2.1 elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Eight response-blind abiotic axes represented warm-season temperature, climatic moisture, temperature seasonality, precipitation seasonality, terrain, two soil axes and shortwave radiation. Elevation was not added as another fixed effect because it overlaps several of these gradients.

Separate INLA-SPDE models were fitted for pigmentation state and conditional intensity. The state model used a Bernoulli likelihood and the intensity model a Gaussian likelihood. A continuous Matérn field represented geographical structure remaining after measured environment (Lindgren et al., 2011; Simpson et al., 2017). The field is not interpreted as population history or dispersal distance; it may combine unmeasured environment, population structure, dispersal and sampling geography. Model extensions were retained only when ecologically motivated and supported by prediction to held-out geographical blocks. Collinearity, hydroclimate alternatives and spatial specifications were checked in Appendix S3. The later departure analysis used the same eight abiotic axes and five approximately 100-km geographical folds (Roberts et al., 2017; Valavi et al., 2019).

### 2.4 Zooming to local focal-Bombus boundaries

We built new SDMs for five Japanese *Bombus* species over a common mainland domain using shared predictor screening and spatial blocks. The surfaces represent predicted habitat support, not abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). To make species comparable, each prediction was ranked against predictions at that species' retained occurrence sites. The primary exposure combined occurrence-referenced support for *B. ardens* and *B. diversus*, the two broad focal pollinators documented in the system. Appendix S4 gives the complete SDM workflow.

The main comparison deliberately moved from the national map to 1-km flower cells. We identified pure white-pigmented transitions within 5 km and selected non-overlapping pairs without Bombus values, environmental values or final contrast direction. Only after pair identities were fixed were they oriented from white to pigmented. We calculated pigmented-minus-white focal-bumblebee support and tested the mean with 100,000 sign flips. Environment did not select, orient or weight the pairs; after selection, environmental distance in the final eight axes was used only as a balance diagnostic. Appendix S5 reports 5-, 10- and 25-km analyses, raw-SDM and all-five-species alternatives, community turnover and the elevation-controlled guardrail.

### 2.5 Defining local departures before reading human context

We did not select unusual sites from large fitted residuals. A local ecological event required a pigmented focal cell with at least three neighbours within 10 km, root-mean-square environmental distance <=1 across the eight standardized abiotic axes, and only observed white flowers among eligible neighbours. Human variables were absent from the natural model, event rule and candidate selection.

The identical detector was applied to 10,000 cross-fitted predictive maps from the final pigmentation-state model while cell geometry and observed trial counts remained fixed. This provided natural reference distributions for event count and candidate fraction. Only then were eleven predefined settlement, land-use, access and natural-context features tested in one global maxT family. Observation-effort measures were evaluated separately. Appendix S6 gives the full feature definitions, null distributions and sensitivities.

### 2.6 Reproducibility and inferential order

All manuscript-facing analyses, seeds, evidence identities and validation rules are versioned. The order is fixed: phenotype construction precedes ecological predictors; the broad natural template precedes the local Bombus test; Bombus pairs are fixed before Bombus values are compared; and departure sites are fixed before human variables are read. This order prevents a later hypothesis from defining the observations then used to support it.

## 3. Results

### 3.1 A national polymorphism becomes measurable

The environmental analysis contained 1,922 georeferenced flowers in 1,305 1-km cells: 966 white-like and 956 pigmented. The curated YAMAP stream was substantially denser than the matched focal-species iNaturalist photo record, allowing the national polymorphism to be measured rather than represented by a few population samples. Because the a* boundary was estimated before ecological predictors were read, the phenotype was not defined by the geography later used to explain it.

### 3.2 One apparent colour gradient separates into two geographies

The first ecological surprise was that pigmentation state and colour intensity did not tell the same geographical story. Pigmentation became less likely toward warmer Temperature PC1. The posterior mean log-odds was -0.542 (95% CrI -1.033 to -0.049), equivalent to an odds ratio of about 0.58 per SD, and no interaction met the full promotion criteria for state.

Among flowers that were already pigmented, intensity followed a different pattern. The final model retained Temperature PC1 × temperature seasonality (mean -0.204; 95% CrI -0.302 to -0.107): the decline in intensity toward warmer climates was stronger where annual temperature variability was greater. Intensity was also lower toward wetter/moister climates (-0.174; -0.323 to -0.024) and toward steeper, more rugged terrain (-0.134; -0.224 to -0.043).

Measured environment did not exhaust the geography. Residual correlation range was 132.8 km (95% CrI 88.8-195.7) for pigmentation state and 65.7 km (31.0-132.6) for conditional intensity. These are scales of remaining correlation, not seed, pollen or colonization distances. The national map therefore resolved one part of the mystery while exposing another: broad environmental association was clear, but coherent spatial structure remained.

### 3.3 Broad pollinator overlap narrows to weak local correspondence

Because broad environment and residual geography were strong, the Bombus question was tested only at local colour boundaries. The 5-km design produced 67 sharp transitions between pure white and pigmented cells, with median separation 2.0 km. The fixed pairs were environmentally closer than ordinary local edges: median eight-axis distance was 0.244 versus 0.318.

Mean focal-bumblebee support was 0.0359 higher on the pigmented side (one-sided sign-flip P=0.0272), but the apparent answer weakened on inspection. Median contrast was -0.0028, only 49.3% of pairs were positive, and the 5-, 10- and 25-km family gave q=0.0815. Mean contrast fell to +0.0084 at 10 km (P=0.325) and +0.0029 at 25 km (P=0.436), and raw SDM support did not reproduce the 5-km result (P=0.267).

A visually stronger national clue also dissolved under local control. Pigmented highland flowers overlapped montane/alpine *Bombus* support, but that pattern disappeared when nearby white and pigmented endpoints were constrained to similar elevation (all one-sided P>=0.755 for the <=50 m test). No persuasive Bombus relationship was found for intensity among pigmented flowers. The pollinator layer therefore yielded not a national mechanism, but a weak and scale-dependent local correspondence.

### 3.4 Visually striking departures remain plausible under nature

Sixteen pigmented cells met the predefined local-departure rule. On the observed map these sites appeared exceptional, yet the same detector applied to 10,000 natural predictive maps produced a mean of 13.59 candidates and a 95% interval of 7-21. The observed count was not unusually high (P=0.27897). Candidate fraction was also compatible with the natural reference (observed 0.04071; null mean 0.03107; upper-tail P=0.12609). The broad natural model therefore changed the interpretation of the map: local discordance did not by itself require an additional cause.

### 3.5 Human context leaves a clue, not a provenance answer

Human variables were read only after the 16 sites were fixed. Population exposure within 5 km was the strongest feature: candidates exceeded local white comparators by 0.06744 rank units (directional P=0.00800), but global maxT FWER P=0.05479. Other settlement scales and DID proximity pointed in the same direction without surviving the global correction, and observation-effort alternatives were null after correction. The human layer therefore left a short-range provenance clue, not evidence that the departures were anthropogenic.

## 4. Discussion

### 4.1 The national map revealed rather than solved the mystery

The study began with an apparently simple phenotype—white, pale or dark flowers—but the national dataset revealed that this was not one ecological axis. Whether visible pigmentation was present followed broad thermal geography, whereas intensity after pigmentation was expressed followed thermal variability, moisture and terrain. The measurement step therefore did more than increase sample size. It changed the biological problem from explaining one colour gradient to explaining at least two linked components of a polymorphism.

That distinction matters because state and intensity can arise from different levels of anthocyanin regulation. Switching visible pigmentation on or off can reflect pathway-level control, while variation among pigmented flowers can reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). A factor that helps retain a visible signal need not make that signal progressively darker. This first reveal becomes important later: the only possible Bombus correspondence emerged at white-pigmented boundaries, not along the intensity axis.

### 4.2 The physiological value of pigmentation can change across space

The broad environmental results identify a candidate landscape of changing costs and benefits. Moderate low temperature can induce chalcone-synthase expression and anthocyanin accumulation in flowers (Shvarts et al., 1997), consistent with pigmentation being less common in warmer climates. At the warm end, darker petals absorb more radiation and can reduce thermal safety margins (Li et al., 2026), suggesting that pigmentation may also carry a heat cost. Together, these mechanisms predict neither universal benefit nor universal cost, but a thermal balance that changes geographically.

Moisture adds a second possible trade-off. Among pigmented flowers, colour was stronger toward the drier end of the climate gradient. Experiments in other anthocyanin polymorphisms show that relative morph performance can depend on water supply (Warren & Mackenzie, 2001), and long-term geographical data link stronger floral pigmentation with aridity and weaker pigmentation with warming (Sullivan & Koski, 2021). These precedents do not show that dark *C. punctata* petals cause drought tolerance. Petal anthocyanin can be regulated separately from pigmentation in photosynthetic tissues (Del Valle et al., 2019), so direct tests must measure petal and vegetative pigments, water relations, flower temperature and fitness together.

Terrain and the SPDE field mark the limit of the macroecological view. Rugged terrain may stand for wind, drainage, slope aspect, canopy openness or fine-scale temperature and moisture, none of which is resolved directly by 1-km layers. The remaining spatial field may include population history, but it is not a genetic map; it can also contain unmeasured environment and sampling geography. The unresolved geography is therefore not an inconvenient leftover. It identifies where field microclimate and population genomics should look for the next layer of explanation.

### 4.3 The tempting pollinator map was not the mechanism

A national overlap between pigmented flowers and bumblebee habitat would have offered an attractive answer. The analyses show why that answer would have been premature. Montane bumblebees and pigmented flowers appeared to share highland geography, yet the relationship vanished when local endpoints were constrained to similar elevation. Shared mountain habitat can draw a convincing map without providing an independent pollinator mechanism.

The focal species gave a subtler result. Pigmented sides of 5-km boundaries had slightly higher mean *B. ardens*/*B. diversus* support, but the median was near zero, fewer than half the pairs were positive, the signal attenuated with distance and raw SDM values did not reproduce it. The evidence is therefore not that bumblebees created the national colour pattern. If the weak local association is biological, it is more consistent with bumblebees slightly modifying the local maintenance or loss of a visible pigmented state.

This interpretation also explains why no comparable signal appeared for intensity. Crossing from white to visibly pigmented may alter floral contrast or recognition by effective visitors, whereas extra darkness may add little once colour is already visible. That remains a hypothesis because the study did not measure bee visual space, UV reflectance, learning, visitation or fitness. The boundaries identified here now provide explicit sites for species-resolved visits, stigma contact, pollen deposition, seed set and colour-choice experiments.

### 4.4 Apparent exceptions first belonged to the natural geography

The final apparent clue was the set of pigmented cells embedded among environmentally similar white neighbours. Without calibration, these locations could easily be narrated as horticultural introductions, unknown selective environments or model failures. Replaying the same event on natural predictive maps changed that interpretation. Sixteen observed sites were visually striking but statistically compatible with the fitted natural geography.

This null result is ecologically informative. Spatially structured natural variation can create local phenotypic exceptions without requiring an extra process, so an unusual-looking point should not be treated as a cause. Human context became relevant only after that natural explanation was allowed to operate. The near-threshold 5-km population signal justifies provenance work, but it does not identify human origin. Repeated field sampling, vouchers, planting histories, local microenvironment and genomic comparison with nearby white populations and horticultural material are needed to separate natural local variation from planting, escape or introgression.

### 4.5 A scale-dependent balance can maintain flower-colour polymorphism

The layers converge on one working model rather than four separate explanations. Climate can change the physiological expression and cost-benefit balance of anthocyanin pigmentation. Population history can preserve or redistribute colour variants across regions. Local bumblebee opportunity may weakly modify the reproductive value of maintaining a visible pigmented state. Human movement may occasionally add a new local source, but is not needed to explain the national pattern or the observed number of departures.

Under this model, neither white nor pigmented flowers must be favoured everywhere. The same pigment can be physiologically useful under one thermal or moisture context, costly under another, and reproductively valuable only where effective pollinators are locally available. Spatially changing benefits, combined with historical movement and persistence of variants, offer a coherent adaptive hypothesis for why the polymorphism remains geographically structured rather than fixing one colour across Japan. The current data identify that hypothesis; they do not yet demonstrate selection.

### 4.6 From the map to direct tests

The macroecological analysis ends by making the hidden mechanisms experimentally addressable. Thermal and moisture gradients define populations for common-garden and reciprocal-transplant tests of pigmentation, physiology and fitness. Residual spatial geography defines regions for ancestry, isolation-by-distance and admixture analyses, followed by genotype-environment tests after neutral structure is accounted for. Local Bombus boundaries define sites for direct pollination and selection measurements. The 16 departure sites define a targeted provenance study.

The broader biogeographic lesson lies in the sequence. Each answer exposed the next confounding layer: measurement revealed two phenotypes; broad environmental models exposed unresolved geography; broad pollinator overlap narrowed to a weak local signal; and local exceptions became natural once their event frequency was calibrated. Matching the comparison scale to the biological process did not produce one simple cause. It produced something more useful—a set of mechanisms that can now be tested separately, in the populations where each is most likely to matter.

## Acknowledgements

[Omitted from the anonymized manuscript for double-anonymous review.]

## References

Araújo, M. B., & Rozenfeld, A. (2014). The geographic scaling of biotic interactions. *Ecography*, 37, 406-415. https://doi.org/10.1111/j.1600-0587.2013.00643.x

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N. (2022). Global climate-related predictors at kilometer resolution for the past and future. *Earth System Science Data*, 14, 5573-5603. https://doi.org/10.5194/essd-14-5573-2022

Del Valle, J. C., Alcalde-Eon, C., Escribano-Bailón, M. T., Buide, M. L., Whittall, J. B., & Narbona, E. (2019). Stability of petal color polymorphism: the significance of anthocyanin accumulation in photosynthetic tissues. *BMC Plant Biology*, 19, 496. https://doi.org/10.1186/s12870-019-2082-6

Dick, C. A., Buenrostro, J., Butler, T., Carlson, M. L., Kliebenstein, D. J., & Whittall, J. B. (2011). Arctic mustard flower color polymorphism controlled by petal-specific downregulation at the threshold of the anthocyanin biosynthetic pathway. *PLoS ONE*, 6, e18230. https://doi.org/10.1371/journal.pone.0018230

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology*, 37, 4302-4358. https://doi.org/10.1002/joc.5086

Guillera-Arroita, G. (2015). Is my species distribution model fit for purpose? Matching data and models to applications. *Global Ecology and Biogeography*, 24, 276-292. https://doi.org/10.1111/geb.12268

Hashiba, K., Iwashina, T., & Matsumoto, S. (2006). Variation in the quality and quantity of flavonoids in the leaves of coastal and inland *Campanula punctata*. *Biochemical Systematics and Ecology*, 34, 854-861. https://doi.org/10.1016/j.bse.2006.04.012

Inoue, K. (1988). Pattern of breeding-system change in the Izu Islands in *Campanula punctata*: Bumblebee-absence hypothesis. *Plant Species Biology*, 3, 125-128. https://doi.org/10.1111/j.1442-1984.1988.tb00178.x

Inoue, K., & Amano, M. (1986). Evolution of *Campanula punctata* in the Izu Islands: Changes of pollinators and evolution of breeding systems. *Plant Species Biology*, 1, 89-97. https://doi.org/10.1111/j.1442-1984.1986.tb00018.x

Inoue, K., & Kawahara, T. (1990). Allozyme differentiation and genetic structure in island and mainland Japanese populations of *Campanula punctata* (Campanulaceae). *American Journal of Botany*, 77, 1440-1448. https://doi.org/10.1002/j.1537-2197.1990.tb12554.x

Jarić, I., Correia, R. A., Brook, B. W., Buettel, J. C., Courchamp, F., Di Minin, E., Firth, J. A., Gaston, K. J., Jepson, P., Kalinkat, G., Ladle, R., Soriano-Redondo, A., Souza, A. T., & Roll, U. (2020). iEcology: Harnessing large online resources to generate ecological insights. *Trends in Ecology & Evolution*, 35, 630-639. https://doi.org/10.1016/j.tree.2020.03.003

Kellenberger, R. T., Byers, K. J. R. P., De Brito Francisco, R. M., Staedler, Y. M., LaFountain, A. M., Schönenberger, J., Schiestl, F. P., & Schlüter, P. M. (2019). Emergence of a floral colour polymorphism by pollinator-mediated overdominance. *Nature Communications*, 10, 63. https://doi.org/10.1038/s41467-018-07936-x

Karger, D. N., Conrad, O., Böhner, J., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

Li, Y., Grossiord, C., Ke, X., & Bachofen, C. (2026). Thermal regulation of flowers: color-driven differences in radiation absorption, cooling, and heat tolerance. *New Phytologist*, 250, 3661-3674. https://doi.org/10.1111/nph.71163

Lindgren, F., Rue, H., & Lindström, J. (2011). An explicit link between Gaussian fields and Gaussian Markov random fields: the stochastic partial differential equation approach. *Journal of the Royal Statistical Society: Series B*, 73, 423-498. https://doi.org/10.1111/j.1467-9868.2011.00777.x

Nagano, Y., Abe, K., Kitazawa, T., Hattori, M., Hirao, A. S., & Itino, T. (2014). Changes in pollinator fauna affect altitudinal variation of floral size in a bumblebee-pollinated herb. *Ecology and Evolution*, 4, 3395-3407. https://doi.org/10.1002/ece3.1191

Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., & Rossiter, D. (2021). SoilGrids 2.0: Producing soil information for the globe with quantified spatial uncertainty. *SOIL*, 7, 217-240. https://doi.org/10.5194/soil-7-217-2021

Renner, I. W., & Warton, D. I. (2013). Equivalence of MAXENT and Poisson point process models for species distribution modeling. *Biometrics*, 69, 274-281. https://doi.org/10.1111/j.1540-0420.2012.01824.x

Roberts, D. R., et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40, 913-929. https://doi.org/10.1111/ecog.02881

Shvarts, M., Borochov, A., & Weiss, D. (1997). Low temperature enhances petunia flower pigmentation and induces chalcone synthase gene expression. *Physiologia Plantarum*, 99, 67-72. https://doi.org/10.1111/j.1399-3054.1997.tb03432.x

Simpson, D., Rue, H., Riebler, A., Martins, T. G., & Sørbye, S. H. (2017). Penalising model component complexity: A principled, practical approach to constructing priors. *Statistical Science*, 32, 1-28. https://doi.org/10.1214/16-STS576

Soberón, J. (2007). Grinnellian and Eltonian niches and geographic distributions of species. *Ecology Letters*, 10, 1115-1123. https://doi.org/10.1111/j.1461-0248.2007.01107.x

Sullivan, C. N., & Koski, M. H. (2021). The effects of climate change on floral anthocyanin polymorphisms. *Proceedings of the Royal Society B*, 288, 20202693. https://doi.org/10.1098/rspb.2020.2693

Tasaki, K., et al. (2022). Identification of candidate genes responsible for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*, 13, 906879. https://doi.org/10.3389/fpls.2022.906879

Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower color evolution and the evidence of pollinator-mediated selection. *Frontiers in Plant Science*, 12, 617851. https://doi.org/10.3389/fpls.2021.617851

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation. *Methods in Ecology and Evolution*, 10, 225-232. https://doi.org/10.1111/2041-210X.13107

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016). How to colour a flower: On the optical principles of flower coloration. *Proceedings of the Royal Society B*, 283, 20160429. https://doi.org/10.1098/rspb.2016.0429

Warren, J., & Mackenzie, S. (2001). Why are all colour combinations not equally represented as flower-colour polymorphisms? *New Phytologist*, 151, 237-241. https://doi.org/10.1046/j.1469-8137.2001.00159.x

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait variation in plants: a renewed focus on its role in ecological processes. *Annals of Botany*, 127, 397-410. https://doi.org/10.1093/aob/mcab011

## Data Accessibility Statement

For double-anonymous review, an anonymized private repository will contain the derived flower-colour tables, environmental source registry, *Bombus* SDM configuration and occurrence-referenced support, analysis code, seeds, local-transition specifications, environmental-balance audit, local-departure definitions and workflow provenance. Original YAMAP photographs are third-party content and cannot be redistributed. The derived trait table retains the provenance and quantitative measurements needed to reproduce manuscript analyses. A permanent public repository and DOI will replace the private review link upon acceptance.

## Conflict of Interest

[Statement withheld from the anonymized manuscript and supplied separately at submission.]

## Author Contributions

[Author contributions withheld from the anonymized manuscript and supplied on the separate title page/submission form.]