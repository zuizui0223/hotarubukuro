# From broad geography to local boundaries: flower-colour biogeography from hiking photographs

**Running title:** Layered flower-colour biogeography

## Abstract

**Aim:** Flower colour can affect both plant physiology and pollinator attraction. We asked why white and pigmented flowers of *Campanula punctata* vary across Japan, and whether different processes act at different spatial scales.

**Location:** Japan.

**Taxon:** *Campanula punctata* (Campanulaceae).

**Methods:** We built a national flower-colour dataset from author-screened YAMAP hiking photographs. We analysed two responses: whether a flower was white-like or pigmented, and colour intensity among pigmented flowers. Broad-scale INLA-SPDE models tested environment while accounting for continuous spatial structure. We then tested 67 sharp transitions between white and pigmented cells against predicted habitat support for *Bombus ardens* and *B. diversus*. Finally, local pigmented departures were defined without using human variables, replayed on 10,000 natural predictive maps, and only then examined for human context.

**Results:** Pigmentation was less likely in warmer climates. Among pigmented flowers, intensity depended on Temperature PC1 × temperature seasonality and was lower in wetter and more rugged environments. Focal-bumblebee support was slightly higher on the pigmented side of local boundaries, but the effect was weak, driven by a few large contrasts, and faded at broader scales. Sixteen local departures were not more frequent than expected under the natural model. Population exposure within 5 km was the strongest human-context feature, but the global maxT familywise correction gave P=0.0548.

**Main conclusions:** Flower colour did not behave as one simple white-to-dark trait. Climate was linked to whether pigmentation was expressed, whereas colour intensity followed a different environmental pattern. Bumblebees may contribute to local maintenance of a pigmented state, but the evidence is weak and does not show pollinator-mediated selection. The broad-scale analysis also identifies clear next tests: experiments for physiological function, field observations for pollination, genomics for population history, and provenance work at local departures.

**Keywords:** digital phenotyping, flower colour, hiking photographs, iEcology, intraspecific variation, pollination, spatial confounding, trait biogeography

## Introduction

Variation within a species can matter as much as differences among species. It can change performance, persistence and species interactions across a range (Westerband et al., 2021). The hard part is explaining where that variation comes from. Climate, population history, biotic interactions and human movement can all leave similar geographical patterns.

Flower colour is a good trait for asking this question because it can do two jobs at once. Anthocyanin pigments change how petals absorb and reflect light. They can also be involved in responses to temperature and water stress. At the same time, flower colour is a visual signal to pollinators. These two functions mean that the best colour may differ from place to place. A colour morph can gain a physiological benefit in one environment but a reproductive benefit in another (Warren & Mackenzie, 2001; Kellenberger et al., 2019; Trunschke et al., 2021).

White and pigmented flowers may also differ in more than pigment amount. A white-pigmented transition can reflect whether the anthocyanin pathway is visibly expressed at all. Variation among pigmented flowers can then reflect how much pigment is produced, pigment chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). We therefore separated two questions: is the flower pigmented, and how strong is the visible colour once pigmentation is present?

There are clear physiological reasons to expect geography in both traits. Low temperature can increase chalcone-synthase expression and anthocyanin accumulation in corollas (Shvarts et al., 1997). Floral pigmentation has also been linked to aridity across space and time (Sullivan & Koski, 2021). Darker petals absorb more radiation, which can change flower temperature and heat risk (Li et al., 2026). In *C. punctata*, UV-B exclusion changes flavonoid accumulation in leaves, showing that the flavonoid system responds to environment in this species (Hashiba et al., 2006). These findings led us to expect a strong thermal signal, with moisture, climatic variability, radiation and terrain modifying the pattern.

Testing these ideas across Japan requires many georeferenced flowers. We built that dataset ourselves from YAMAP, a hiking-navigation and activity-diary platform. We screened every retrieved candidate, checked the taxon and focal flower, confirmed the petal region, audited duplicates, and then extracted colour with a fixed image pipeline. For the matched 2023-2025 period, YAMAP yielded 1,964 georeferenced focal-species records, compared with 516 iNaturalist observations with photographs. The aim was not to claim that YAMAP is unbiased. Its value was a large, complementary image stream from mountain routes that could be curated into a quantitative trait dataset. This is an iEcology use of digital material that was created for another purpose (Jarić et al., 2020; Appendices S1-S2).

A dense national map creates a second problem: several mechanisms share the same geography. Climate and topography are spatially structured. Population history is spatially structured. Bumblebee species-distribution models (SDMs) are also built from environmental predictors. A single national regression could therefore give a precise Bombus coefficient even when flower colour and Bombus support simply follow the same climate (Soberón, 2007). Biotic interactions are also strongly scale dependent (Araújo & Rozenfeld, 2014). We dealt with both problems by changing scale. We first estimated broad environmental and spatial structure. We then tested the Bombus hypothesis only at nearby white-pigmented boundaries.

This local test has a biological basis in *C. punctata*. Bumblebees are effective pollinators of its large tubular flowers, and geographical changes in *Bombus* fauna are linked to pollen-removal ecology and breeding-system change (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). We therefore asked a narrow question. If visible pigmentation gives a reproductive signalling benefit, should the pigmented side of a local colour boundary occur where focal bumblebees have greater habitat opportunity? This predicts a difference between white and pigmented states more directly than a steady increase in benefit with darker colour.

The broad natural model also lets us ask what counts as a local exception. A large model residual is not automatically a biological anomaly. We instead defined a simple local event: a pigmented cell surrounded by environmentally similar white cells. We first asked how often the natural model generated the same event. Only after candidate sites were fixed did we examine human context. This order matters because *C. punctata* is cultivated, so planting, escape or introgression are possible but should not be assumed from an unusual-looking map.

We tested three linked predictions. First, pigmentation state should be more common in cooler environments, while colour intensity may respond to a different environmental context. Second, if focal bumblebees help maintain visible pigmentation, local white-pigmented boundaries should have higher predicted focal-bumblebee support on the pigmented side. Third, if local pigmented departures need an extra explanation, those events should be more frequent than expected under the natural model. Human context was a follow-up to the third test, not part of event selection.

## Materials and Methods

### Digital sampling and two-part flower-colour phenotype

The sampling frame covered the 2023-2025 flowering seasons. We screened all YAMAP records returned by the fixed Japanese focal-name search frame rather than taking a subsample. We removed taxonomic errors and non-focal flowers, then confirmed the focal flower and usable petal area. After duplicate and raster-completeness checks, 1,922 observations entered the environmental analyses. Appendices S1-S2 give the full record flow and image audit.

The image pipeline kept source provenance, date, coordinates and image hashes. It summarized display-referred sRGB pixels in the confirmed petal area and converted median RGB to CIELAB under D65. We use CIELAB a* as a reproducible human-visible red-green trait. It is not a measurement of spectral reflectance, UV contrast or anthocyanin concentration.

We estimated the white-pigmented boundary from a* alone, before using geography, environment, Bombus predictions or human data. Gaussian-mixture classification gave a boundary of a*=4.9688. The final dataset contained 966 white-like and 956 pigmented observations. We analysed pigmentation state in all 1,922 observations and standardized visible intensity only among pigmented flowers. Appendix S2 gives the mixture and sensitivity analyses.

### Broad environmental and spatial model

We described climate with CHELSA v2.1, soils with SoilGrids 2.0 and terrain with derivatives from WorldClim 2.1 elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Eight response-blind abiotic axes represented warm-season temperature, climatic moisture, temperature seasonality, precipitation seasonality, terrain, two soil axes and shortwave radiation. We did not add elevation as another fixed effect because it overlaps several of these environmental gradients.

We fitted separate INLA-SPDE models for pigmentation state and conditional intensity. The state model used a Bernoulli likelihood and the intensity model used a Gaussian likelihood. A continuous Matérn spatial field captured remaining geographical structure after measured environment (Lindgren et al., 2011; Simpson et al., 2017). We do not interpret this field as population history or as a dispersal distance. It can contain unmeasured environment, population structure, dispersal and sampling geography.

We kept a model extension only when it had ecological support and improved prediction to held-out geographical blocks. We also checked collinearity, hydroclimate alternatives and different spatial specifications. Appendix S3 contains these model-selection and sensitivity analyses. The later local-departure analysis used the same eight abiotic axes and five approximately 100-km geographical folds (Roberts et al., 2017; Valavi et al., 2019).

### Bumblebee SDMs and local boundary test

We built fresh SDMs for five Japanese *Bombus* species over one common mainland domain. All species used the same predictor-screening and spatial-block framework. These maps represent predicted habitat support, not bee abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). To make species comparable, we ranked each SDM prediction against predictions at that species' retained occurrence sites. The main exposure combined the ranks for *B. ardens* and *B. diversus*, the two broad focal pollinators documented in this system. Appendix S4 gives the full SDM workflow.

For the main test, we moved from the national map to local boundaries. At the 1-km-cell level, we found pure white-pigmented transitions within 5 km. We selected non-overlapping pairs without using Bombus values, environmental values or the final contrast direction. Only after the pairs were fixed did we orient them from white to pigmented. We then calculated focal-bumblebee support on the pigmented side minus support on the white side and tested the mean contrast with 100,000 sign flips.

Environment did not select or weight the pairs. After pair selection, we checked whether the fixed pairs were environmentally local in the same eight abiotic axes. Appendix S5 reports this check and the full 5-, 10- and 25-km, raw-SDM, all-five-species and elevation-controlled sensitivities.

### Local departures and human context

We did not define unusual sites from large residuals. We defined a local ecological event instead. A focal cell had to be pigmented and have at least three neighbours within 10 km. Those neighbours had to be similar across the eight standardized abiotic axes (root-mean-square distance <=1), and every eligible observed neighbour had to be white. No human variable entered this definition.

We applied the same detector to 10,000 cross-fitted predictive maps from the final pigmentation-state model. Cell geometry and observed trial counts stayed fixed. This gave a natural reference for the number and fraction of local departures.

Only then did we examine human context. Eleven predefined features covered settlement, land use, access and natural alternatives. We tested them together with a global maxT correction. Observation-effort measures were checked separately. Appendix S6 gives the full feature set, null distributions and sensitivity analyses.

## Results

### Broad environment separates pigmentation state from colour intensity

The 1,922 photographs were split almost evenly between white-like (966) and pigmented (956) flowers. Pigmentation became less likely toward warmer Temperature PC1. The posterior mean log-odds was -0.542 (95% CrI -1.033 to -0.049), equivalent to an odds ratio of about 0.58 per SD. No interaction met the full criteria for the state model.

Among pigmented flowers, colour intensity followed a different pattern. The final model retained Temperature PC1 × temperature seasonality (mean -0.204; 95% CrI -0.302 to -0.107). In plain terms, the decline in colour intensity toward warmer climates was stronger where annual temperature variability was greater. Intensity was also lower toward wetter/moister climates (-0.174; -0.323 to -0.024) and toward steeper, more rugged terrain (-0.134; -0.224 to -0.043).

A strong spatial pattern remained after measured environment. The estimated residual correlation range was 132.8 km (95% CrI 88.8-195.7) for pigmentation state and 65.7 km (31.0-132.6) for conditional intensity. These values describe remaining spatial correlation. They are not seed, pollen or colonization distances.

### 67 sharp transitions show only weak local Bombus correspondence

The 5-km design produced 67 sharp transitions between pure white and pigmented cells. Their median separation was 2.0 km. The fixed pairs were also environmentally closer than ordinary local edges: median eight-axis distance was 0.244 versus 0.318.

Mean focal-bumblebee support was 0.0359 higher on the pigmented side (one-sided sign-flip P=0.0272). But the median contrast was -0.0028, and only 49.3% of pairs were positive. The 5-, 10- and 25-km family gave q=0.0815. The mean contrast fell to +0.0084 at 10 km (P=0.325) and +0.0029 at 25 km (P=0.436). Raw SDM support did not reproduce the 5-km result (P=0.267). We therefore treat this as a weak and very local correspondence, not evidence of pollinator-mediated selection.

A second negative control was useful. Pigmented highland flowers overlapped montane/alpine *Bombus* support on the national map, but that pattern disappeared when nearby white and pigmented endpoints were forced to have similar elevation (all one-sided P>=0.755 for the <=50 m test). We also found no persuasive *Bombus* relationship with colour intensity among pigmented flowers. Appendix S5 contains these tests.

### Sixteen local departures do not exceed the natural model

Sixteen local departures met the predefined event rule. Across 10,000 natural predictive maps, the mean candidate count was 13.59, with a 95% interval of 7-21. The observed count was not unusually high (P=0.279). Candidate fraction was also compatible with the natural reference (observed 0.0407; null mean 0.0311; upper-tail P=0.126).

Human variables were tested only after these 16 sites were fixed. Population exposure within 5 km was the strongest feature. Candidates were higher than their local white comparators by 0.0674 rank units (directional P=0.0080), but the global maxT familywise correction gave P=0.0548. Other settlement scales and DID proximity pointed in the same direction but did not survive the global correction. Observation-effort measures were null after correction. These sites are therefore useful provenance targets, but they are not demonstrated anthropogenic anomalies.

## Discussion

### Flower colour can link plant stress physiology with pollinator signalling

The clearest result is that white-versus-pigmented state and colour intensity are not the same ecological trait. Whether pigmentation was present followed broad thermal geography. Once pigmentation was present, its intensity depended on thermal variability, moisture and terrain. A single white-to-dark axis would have hidden this difference.

This split fits what is known about anthocyanin biology. Switching visible pigmentation on or off can reflect pathway-level regulation, whereas variation within pigmented flowers can reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). The two responses can therefore have different ecological drivers.

This matters for adaptation because floral anthocyanins can affect both plant performance and reproduction. Their value is unlikely to be constant across Japan. A pigmented flower may gain under one combination of temperature, water stress and pollinator service but lose under another. Spatially varying costs and benefits could therefore help maintain white and pigmented flowers within one species. Our data do not prove that mechanism, but they identify where each part of it is plausible.

### Temperature and moisture give specific physiological hypotheses

The temperature result has a direct mechanistic precedent. In experimental flowers, moderate low temperature can induce chalcone-synthase expression and anthocyanin accumulation in the corolla (Shvarts et al., 1997). Our finding that pigmentation is less common in warmer climates is consistent with that response.

The opposite end of the temperature gradient may also matter. Darker petals absorb more radiation. Recent experiments show that this can raise flower temperature and reduce thermal safety margins, even when darker morphs have somewhat greater heat tolerance (Li et al., 2026). Pigmentation may therefore have both a cold-side benefit and a warm-side cost. That is a testable explanation for why one colour does not dominate everywhere.

Moisture points to another possible trade-off. Among pigmented flowers, colour was stronger toward the drier end of the climate gradient. Experiments on anthocyanin polymorphisms show that morph performance can depend on water supply, with pigmented plants doing relatively better under drought in some systems (Warren & Mackenzie, 2001). Long-term geographical data also link stronger floral pigmentation with aridity and weaker pigmentation with warming (Sullivan & Koski, 2021).

We should not turn those precedents into a claim that dark *C. punctata* petals cause drought tolerance. Petal anthocyanin can be regulated separately from pigmentation in photosynthetic tissues (Del Valle et al., 2019). The next experiment should therefore measure petal and vegetative pigments, water relations, flower temperature and fitness together.

Terrain prevents an overly simple mountain story. Colour intensity was weaker, not stronger, toward more rugged terrain after climate and space were included. Ruggedness can stand for drainage, wind, slope aspect, canopy openness and fine-scale temperature or moisture. These processes are not resolved by 1-km climate layers. The terrain result therefore tells us where field microclimate measurements are needed; it does not show a direct topographic effect on anthocyanin.

A large spatial pattern also remained after measured environment. Historical work makes population structure a plausible part of that pattern (Inoue & Kawahara, 1990), but the spatial field is not genetics. It may also contain unmeasured environment and sampling geography. Population genomics can now test whether the remaining colour geography follows ancestry, isolation by distance or admixture.

### Bumblebees may add a local reproductive benefit to visible pigmentation

The Bombus analysis asks a different question from the broad environmental model. Here the possible benefit is reproductive signalling. Anthocyanin-based colour morphs can differ in pollinator choice and reproductive success in other systems (Kellenberger et al., 2019; Trunschke et al., 2021). In our study, however, the SDMs measure habitat opportunity for bumblebees, not actual visits.

The local result is therefore suggestive rather than decisive. Pigmented sides of 5-km boundaries had slightly higher focal-bumblebee support on average. But the median was near zero, fewer than half the pairs were positive, the result weakened with scale, and it failed with raw SDM values. The national overlap with montane bumblebees also disappeared after controlling elevation.

If the weak local pattern is biological, it fits a maintenance hypothesis better than a darkening hypothesis. Crossing from white to visibly pigmented may change contrast or recognition by effective visitors. Once a flower is already pigmented, extra darkness may add little. We did not measure bee colour space, UV reflectance or learning, so this remains a hypothesis. The next step is direct: measure species-specific visitation, colour choice, stigma contact, pollen deposition and seed set at the local boundaries identified here.

### Human context is a follow-up, not the main explanation

The 16 local departures looked striking on the observed map, but the natural model produced similar configurations often enough that their total number was not unusual. This is an important null result. Spatially structured natural variation can create local colour exceptions without any extra process.

Still, *C. punctata* is cultivated, so these sites are useful for asking about provenance. The strongest human-context signal was population exposure within 5 km, but the global familywise P value was 0.0548. That is close enough to motivate field work, not close enough to claim human origin.

The next test is straightforward. Revisit the 16 sites, collect vouchers, document planting and management history, and compare their genomes with nearby white populations and horticultural material. That work can separate natural local variation from planting, escape or introgression.

### A changing balance of physiological and reproductive benefits can maintain the polymorphism

The results support one simple working model. Climate changes the physiological context of pigmentation. Local bumblebee opportunity may change its reproductive value. Population history can move or preserve colour variants across space. Human movement may occasionally add another local source.

This model does not require one factor to explain the whole map. It also does not require pigmentation to be beneficial everywhere. White and pigmented flowers can persist together if the balance of costs and benefits changes across the species range.

The study cannot prove adaptation from photographs and maps alone. What it can do is make the next tests much more specific. Temperature and moisture gradients define populations for common-garden and reciprocal-transplant experiments. The remaining spatial pattern defines regions for ancestry and admixture tests. Local Bombus boundaries define sites for direct pollination work. The 16 departures define sites for provenance work.

That is also the main value of the approach. We built a national quantitative flower-colour dataset from hiking photographs, separated two biologically different parts of the phenotype, matched each ecological question to an appropriate spatial scale, and kept the causal claims below the resolution of the data. The result is not only a map of flower colour. It is a set of testable hypotheses about why the polymorphism persists and where to test them next.

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

Karger, D. N., Conrad, O., Böhner, J., Kawohl, J., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

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