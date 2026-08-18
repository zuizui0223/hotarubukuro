# From broad geography to local boundaries: biogeography of flower-colour polymorphism from hiking photographs

**Running title:** Flower-colour polymorphism

## Abstract

**Aim:** Geographical trait variation can reflect environment, population history, biotic interactions and human activity at different scales. We tested how these processes assemble flower-colour geography while separating pigment presence from pigmented-flower intensity.

**Location:** Japan.

**Taxon:** spotted bellflower (*Campanula punctata*).

**Methods:** We converted 1,922 author-screened YAMAP photographs into pigmentation state and pigmented-only intensity. INLA-SPDE models separated environmental associations from residual geography, and 67 independently fixed white-pigmented boundaries supplied the local bumblebee comparison. For human context, we measured distance to the nearest occurrence of the same colour in all 1,305 cells, scaled it by local flower-cell spacing and replayed the geometry on 10,000 natural predictive maps. This continuous analysis was post hoc; a threshold event detector was retained only for calibration and field targeting.

**Results:** Pigmentation state, but not pigmented-only intensity, exceeded a cross-fitted spatial expectation along environmental difference. Focal-bumblebee support was higher on pigmented sides of local boundaries, whereas stronger-looking highland overlap disappeared after elevation matching. Among 674 pigmented cells, isolation correlated with 5-km population exposure (rho=0.252) more strongly than natural maps expected (mean 0.133; P=0.0002). The relationship remained after local flower-cell spacing was removed (rho=0.285; natural mean 0.154; P=0.0009). The raw white relationship did not retain its opposite sign after density correction. Sixteen event-defined populations were not excessive under natural maps.

**Main conclusions:** Flower-colour geography contains distinct environmental, spatial, local biotic and contemporary human-context layers. Human context was expressed most clearly as an excess positive isolation-population relationship within pigmented occurrences, not as reciprocal colour displacement or proof of horticultural origin. Integrative trait biogeography gains resolution by connecting, rather than collapsing, processes operating at different scales and comparison units.

**Keywords:** digital phenotyping, flower colour, hiking photographs, iEcology, intraspecific variation, pollination, spatial confounding, trait biogeography

## 1. Introduction

### 1.1 The geographical mystery of flower-colour polymorphism

Why does one species remain white in some parts of its range and pigmented in others? This is not only a descriptive question. Intraspecific variation can change performance, persistence and species interactions (Westerband et al., 2021), and different explanations predict different responses to environmental change. Yet the map alone can mislead. Climate, population history, biotic interactions and human movement are all spatially structured, so several processes can draw similar geographical patterns.

Flower colour makes this problem especially revealing because anthocyanin pigmentation can do two ecological jobs at once. Pigments alter how petals absorb and reflect light and can contribute to temperature responses, water-stress responses and floral heat balance. The same visible colour is also a pollinator signal that can alter reproductive success. A pigmented morph can therefore gain a physiological benefit in one setting and a reproductive benefit in another (Warren & Mackenzie, 2001; Kellenberger et al., 2019; Trunschke et al., 2021). Spatial variation in those benefits offers a plausible route by which white and pigmented flowers persist within one species. The spotted bellflower (*Campanula punctata* Lam.; Campanulaceae) is a strong system for testing this idea because its large tubular flowers vary conspicuously from white to pigmented across Japan and are visited effectively by bumblebees.

### 1.2 The first hidden layer: state is not intensity

The apparent white-to-dark continuum may itself contain two biological questions. A white-pigmented transition can reflect whether the anthocyanin pathway is visibly expressed at all. Variation among flowers that are already pigmented can instead reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). A process that changes whether pigmentation is switched on need not control how dark the flower becomes afterwards. We therefore separated pigmentation state from visible intensity before asking what geography explains either response.

This split gives environmental hypotheses more precise targets. Low temperature can increase chalcone-synthase expression and anthocyanin accumulation in corollas (Shvarts et al., 1997), floral pigmentation has been linked to aridity (Sullivan & Koski, 2021), and darker petals can absorb more radiation and alter flower temperature (Li et al., 2026). In *C. punctata*, UV-B exclusion changes flavonoid accumulation in leaves (Hashiba et al., 2006), showing environmental responsiveness of the flavonoid system in the focal species. These precedents predict a broad thermal signal, with moisture, climatic variability, radiation and terrain modifying either the presence or expression of pigmentation.

### 1.3 A new observation stream makes the national phenotype visible

Testing those ideas across Japan first required a phenotype dense enough to reveal the pattern. We built that phenotype ourselves from YAMAP, a hiking-navigation and activity-diary platform. Every retrieved candidate was screened for taxon identity, focal flower and usable petal region; duplicate images were audited; and colour was extracted with a fixed image pipeline. In the matched 2023–2025 period, YAMAP yielded 1,964 georeferenced focal-species records, compared with 516 iNaturalist observations with photographs—a 3.81-fold difference. The three YAMAP years were also closely balanced (642, 687 and 635 records). YAMAP is not spatially random, but it provided a complementary mountain-route image stream that could be curated into a recent quantitative trait dataset rather than a simple occurrence export. This is an iEcology use of digital material created for another purpose (Jarić et al., 2020; see Appendices S1–S2 in Supporting Information).

Making the national pattern visible exposed the next problem. Climate and topography are spatially structured, population history is spatially structured, and bumblebee species-distribution models (SDMs) are built from environmental predictors. A national model could therefore assign a precise coefficient to Bombus support even when flower colour and bumblebees merely follow the same climate (Soberón, 2007). Biotic interactions are also scale dependent (Araújo & Rozenfeld, 2014). We therefore used the national analysis to partition measured environmental associations from coherent residual geography, then changed comparison scale for the pollinator hypothesis.

### 1.4 From broad geography to local boundaries and human overlays

The local pollinator test has a strong natural-history and scale basis in *C. punctata*. Bumblebees are effective pollinators of its tubular flowers, and geographical changes in *Bombus* fauna are linked to pollen-removal ecology and breeding-system change (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). Pollinator-mediated selection is realized through local visitation, pollen transfer and reproductive success, and divergent selection on floral traits can arise among populations separated by less than 10 km when local pollinator assemblages differ (Torres-Vanegas et al., 2024). We therefore made nearby white-pigmented boundaries the primary comparison and asked whether focal-bumblebee habitat opportunity increased specifically from the white side to the pigmented side.

The 5-km radius is not assumed to be an exact bumblebee foraging or selection distance. It was the finest predeclared scale that retained enough replicated colour transitions and it targets the population-neighbourhood scale at which pollinator assemblages and floral selection can vary. National overlap and equal-elevation comparisons were then used as a guardrail: they ask whether a stronger-looking broad pattern can be explained by shared mountain environment. In this design, the local comparison is the biological test and the broad comparison shows why that localization is necessary.

The broad natural template also makes the spatial geometry of human context testable without first declaring a small set of anomalies. For every 1-km flower cell, we measured distance to the nearest other cell with the same colour state. This continuous isolation measure is defined for all pigmented and white cells and requires no event radius, environmental caliper, minimum-neighbour rule or residual cutoff. We then asked whether isolation covaried with population exposure within each colour, whether that relationship changed after scaling by the local spacing of all flower cells, and whether the fitted natural model generated the same pattern across 10,000 predictive maps.

A separate relational event—a pigmented cell surrounded by nearby environmentally similar white cells—was retained for a different job. Replaying that event on natural maps calibrates visual surprise and provides reproducibly selected populations for field history, vouchers and genetic assignment. Because *C. punctata* is cultivated, planting, escape or introgression remain plausible provenance hypotheses, but neither continuous geometry nor event membership is treated as ancestry evidence.

### 1.5 Predictions and exploratory human-context question

The analysis follows one dependent sequence. First, if flower colour contains two ecological layers, pigmentation state and pigmented-only intensity should show different broad geography. Second, if focal bumblebees help maintain visible pigmentation, predicted habitat support should be higher on the pigmented side of nearby white-pigmented boundaries; the signal should be strongest at the local scale where plant-pollinator neighbourhoods differ, whereas broad highland overlap may reflect shared environment.

The all-cell isolation analysis was developed after inspection of the earlier event-based human-context results and is therefore explicitly post hoc rather than a preregistered third prediction. Its frozen guardrail question is narrower: are pigmented occurrences that lie farther from other pigmented occurrences more population-exposed than expected from the fitted natural flower-colour geography, and does that result survive local sampling-density correction? The event analysis remains a supplementary calibration and field-target procedure; an excess event count is not required for the continuous human-context question to be answerable.

## 2. Materials and Methods

### 2.1 Study system and YAMAP sampling

The sampling frame covered the 2023–2025 flowering seasons. We screened all YAMAP records returned by the fixed Japanese focal-name search frame rather than subsampling candidates. Taxonomic errors and non-focal flowers were removed, and the focal flower and usable petal region were confirmed. Duplicate-image and raster-completeness checks left 1,922 observations for environmental analysis. Appendices S1–S2 give the complete record flow, matched public-database benchmark and sampling limitations.

### 2.2 From photographs to a two-part phenotype

The image pipeline retained source provenance, date, coordinates and image hashes. Display-referred sRGB pixels within the confirmed petal region were summarized and median RGB was converted to CIELAB under D65. CIELAB a* is used here as a reproducible human-visible red-green phenotype, not as calibrated spectral reflectance, UV contrast or anthocyanin concentration.

The white-pigmented boundary was estimated from a* alone before geography, environment, Bombus predictions or human data were used. Gaussian-mixture classification gave an operational boundary of a*=4.9688. The final dataset contained 966 white-like and 956 pigmented observations. Pigmentation state was analysed across all 1,922 observations, and standardized visible intensity was analysed only among pigmented flowers. Appendix S2 reports the mixture model, ambiguity checks and phenotype sensitivities.

### 2.3 Broad environmental and continuous spatial geography

Climate predictors came from CHELSA v2.1, soils from SoilGrids 2.0 and terrain from derivatives of WorldClim 2.1 elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Eight response-blind abiotic axes represented warm-season temperature, climatic moisture, temperature seasonality, precipitation seasonality, terrain, two soil axes and shortwave radiation. Elevation was not added as another fixed effect because it overlaps several of these gradients.

Analyses were run in R 4.5.3 (R Core Team, 2026); latent Gaussian and SPDE models used the 'INLA' package version 25.10.19 (Rue et al., 2009). Separate models were fitted for pigmentation state and conditional intensity. The state model used a Bernoulli likelihood and the intensity model a Gaussian likelihood. A continuous Matérn field represented geographical structure remaining after measured environment (Lindgren et al., 2011; Simpson et al., 2017). This field estimates coherent residual geography rather than serving only as a nuisance correction. It may combine unmeasured environment, population structure, dispersal and sampling geography, and is not interpreted as a direct genetic map or dispersal distance. Model extensions were retained only when ecologically motivated and supported by prediction to held-out geographical blocks. Collinearity, hydroclimate alternatives and spatial specifications were checked in Appendix S3.

As a supporting sensitivity, we asked a complementary question at the 1-km-cell scale: whether phenotype divergence between environmentally dissimilar held-out locations exceeded an intercept + Matérn SPDE expectation at comparable geographical separation. Environmental distance used a separate frozen, response-blind six-score representation of broad and within-neighbourhood climate, aridity and topography. For each of five held-out geographical folds, pairs were divided into five geographical-distance strata, upper and lower environmental-distance quartiles were contrasted, and the observed mean high-minus-low divergence was compared with 500 space-only posterior-predictive realizations (seed 20260725). This FST/PST-inspired comparison is explicitly non-genetic: the spatial field is an empirical continuity expectation rather than drift, and any excess is environmental alignment rather than proof of selection or local adaptation (Appendix S3).

The later departure analysis used the same eight abiotic axes and five approximately 100-km geographical folds (Roberts et al., 2017; Valavi et al., 2019).

### 2.4 Zooming to local focal-Bombus boundaries

We built new SDMs for five Japanese *Bombus* species over a common mainland domain using shared predictor screening and spatial blocks. The surfaces represent predicted habitat support, not abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). To make species comparable, each prediction was ranked against predictions at that species' retained occurrence sites. The primary exposure combined occurrence-referenced support for *B. ardens* and *B. diversus*, the two broad focal pollinators documented in the system. Appendix S4 gives the complete SDM workflow.

The main comparison deliberately moved from the national map to 1-km flower cells. We identified pure white-pigmented transitions within 5 km and selected non-overlapping pairs without Bombus values, environmental values or final contrast direction. The 5-km radius was the finest predeclared scale with sufficient replicated transitions; it represents a local population-neighbourhood comparison, not an estimate of bumblebee foraging distance. Only after pair identities were fixed were they oriented from white to pigmented. We calculated pigmented-minus-white focal-bumblebee support and tested the mean with 100,000 sign flips. Environment did not select, orient or weight the pairs; after selection, environmental distance in the final eight axes was used as a balance diagnostic. Appendix S5 reports 5-, 10- and 25-km analyses, raw-SDM and all-five-species alternatives, community turnover and the elevation-controlled guardrail.

### 2.5 Continuous colour isolation and event-based field targets

The human-context analysis used the 1,305 environment-complete 1-km cells. A cell was pigmented when it contained at least one pigmented observation and white otherwise, giving 674 pigmented and 631 white cells. For every cell, raw colour isolation was the Euclidean distance to the nearest other cell with the same colour state. The primary human feature was population-exposure rank within 5 km. We calculated Spearman correlations separately for pigmented and white cells and their direct difference, rho(pigmented) - rho(white). Focal, 10-, 25- and 50-km population ranks, DID proximity, land-cover variables, mountainness and observation-process measures were retained as labelled secondary features.

Raw nearest-neighbour distance can increase where the full flower-cell frame is sparse. We therefore calculated nearest and fifth-nearest distances to any flower cell and repeated the primary relationship with relative isolation, log(same-colour nearest distance / any-colour nearest distance). Five leave-one-geographical-fold-out estimates assessed regional stability. Within-colour feature values were permuted within geographical folds for descriptive restricted-randomisation checks.

The decisive natural-geography guardrail replayed the complete isolation geometry on 10,000 checksum-locked final-eight-axis predictive maps. Each simulated map used the same `count > 0` state rule, cell geometry and observed binomial trial counts. For every map we recomputed pigmented and white correlations and their difference. The primary 5-km comparison was reported for all nondegenerate maps and for a fixed count-conditioned sensitivity. Only 19 maps contained exactly the observed 674 pigmented cells, so the pre-specified fallback retained the 1,000 maps closest to the observed count. Population scales were evaluated as one maxT family. The motivating correlations were inspected before this analysis was specified; the analysis is reported as a post hoc exploratory generalisation, while the density and natural-map guardrails were frozen before the validated execution.

For continuity with the earlier workflow, we also retained the threshold-defined ecological event. A candidate required a pigmented focal cell with at least three neighbours within 10 km, root-mean-square environmental distance <=1 across the eight standardized abiotic axes, and only observed white flowers among eligible neighbours. Human variables were absent from the event rule. The identical detector was applied to 10,000 cross-fitted predictive maps to calibrate event count and fraction, after which predefined human contrasts were calculated. This event family now serves as supplementary calibration and as a reproducible selector of extreme field/provenance targets rather than the primary human-context estimand. Appendix S6 gives complete definitions and results.

### 2.6 Reproducibility and inferential order

The study provides more than a fitted model: it exposes the full route from source images to manuscript claims. Derived data, source-construction code, analysis code, fixed seeds, software manifests, evidence identities and validation rules are versioned, and manuscript values are checked against checksum-locked artifacts in continuous integration. Phenotype construction precedes ecological predictors; the broad natural template precedes the local Bombus test; and Bombus pairs are fixed before Bombus values are compared. Event identities are fixed before their human variables are read.

The continuous-isolation analysis has a different evidential status and is labelled accordingly. Its motivating pattern was observed before formal specification, so it is not presented as preregistered confirmation. Reproducibility instead locks the exact all-cell geometry, sampling-density correction, frozen human feature registry, 2,000 restricted permutations and 10,000-map natural guardrail. This separates transparent exploration from outcome-dependent redefinition while preserving the paper's sequence of broad template, local mechanism and contemporary overlay.

## 3. Results

### 3.1 A new image stream revealed a national quantitative polymorphism

The environmental analysis contained 1,922 georeferenced flowers in 1,305 1-km cells: 966 white-like and 956 pigmented. The matched YAMAP retrieval contained 3.81 times as many georeferenced focal-species records as iNaturalist in the same three-year period, and nearly equal annual counts supplied dense contemporary replication. Author screening, image hashing and deterministic phenotyping converted that alternative observation stream into a traceable quantitative trait dataset. Because the a* boundary was estimated before ecological predictors were read, the phenotype was independent of the geography later used to explain it.

This data layer was itself a biological result: it made national variation measurable at two levels rather than representing flower colour by a few categorical population samples. It also supplied the local replication needed for boundary and neighbourhood analyses later in the study.

### 3.2 Broad models separated two environmental responses and coherent residual geography

Pigmentation state and colour intensity did not tell the same geographical story. Pigmentation became less likely toward warmer Temperature PC1. The posterior mean log-odds was -0.542 (95% CrI -1.033 to -0.049), equivalent to an odds ratio of about 0.58 per SD, and no interaction met the full promotion criteria for state.

Among flowers that were already pigmented, intensity followed a different pattern. The final model retained Temperature PC1 × temperature seasonality (mean -0.204; 95% CrI -0.302 to -0.107): the decline in intensity toward warmer climates was stronger where annual temperature variability was greater. Intensity was also lower toward wetter/moister climates (-0.174; -0.323 to -0.024) and toward steeper, more rugged terrain (-0.134; -0.224 to -0.043).

The spatial models also quantified a second, coherent layer of geography after those measured associations. Residual correlation range was 132.8 km (95% CrI 88.8-195.7) for pigmentation state and 65.7 km (31.0-132.6) for conditional intensity. Thus the broad stage delivered two outputs: a response-specific abiotic landscape and an independently testable map of unresolved regional structure. The ranges describe remaining correlation rather than seed, pollen or colonization distances.

The complementary cross-fitted spatial-null test sharpened the state–intensity difference. For pigmentation state, the mean high-minus-low environmental-distance contrast in held-out phenotype divergence was 0.1068, compared with a space-only posterior-predictive median of 0.0582 (excess +0.0486; one-sided P=0.0339). Conditional intensity showed no positive excess (observed -0.0472; null median -0.0013; P=0.8723). At comparable geographical separation, environmental difference therefore aligned with white–pigmented state divergence beyond fitted spatial continuity, whereas variation within already pigmented flowers did not.

### 3.3 Local boundaries revealed a short-range Bombus signal

The 5-km design produced 67 sharp transitions between pure white and pigmented cells, with median separation 2.0 km. The fixed pairs were environmentally closer than ordinary local edges: median eight-axis distance was 0.244 versus 0.318. The design therefore compared colour states within substantially tighter environmental neighbourhoods than a national overlay and at the finest scale with enough replicated boundaries.

At this local scale, mean focal-bumblebee support was 0.0359 higher on the pigmented side (one-sided sign-flip P=0.0272). The signal was concentrated rather than uniform: median contrast was -0.0028, 49.3% of pairs were positive and the 5-, 10- and 25-km family gave q=0.0815. Mean contrast declined to +0.0084 at 10 km (P=0.325) and +0.0029 at 25 km (P=0.436), and no persuasive Bombus relationship was found for intensity among pigmented flowers. The scale pattern therefore localized the plausible biotic contribution to short-range white-pigmented state boundaries rather than a national gradient in darkness. Raw SDM support did not reproduce the 5-km result (P=0.267), showing that the directional inference depended on the species-calibrated habitat-support scale.

A separate highland guardrail reinforced the value of the local comparison. Pigmented highland flowers and montane/alpine *Bombus* support overlapped strongly on national maps, but the contrast vanished when nearby white and pigmented endpoints were constrained to similar elevation (all one-sided P>=0.755 for the <=50 m test). The stronger-looking broad pattern was therefore consistent with shared mountain geography. By starting with local focal-species boundaries, the analysis retained a different, state-specific signal that was not simply inherited from national highland overlap.

### 3.4 Continuous isolation revealed a pigmented human-context overlay

The 674 pigmented cells were strongly clustered: median distance to the nearest other pigmented cell was 3.61 km. Pigmented isolation increased with population exposure at the focal cell (rho=0.271), 5 km (rho=0.252) and 10 km (rho=0.172), but the raw correlation weakened at 25 km (rho=0.026) and was negative at 50 km (rho=-0.058). The corresponding raw white correlations were 0.003, -0.072, -0.141, -0.180 and -0.148. At 5 km, the direct raw colour contrast was therefore +0.324, and all five leave-one-fold-out estimates remained positive (0.267-0.391), although fold-specific strength varied.

The raw white sign reversal was not robust to sampling-density correction. When same-colour distance was scaled by nearest distance to any flower cell, the 5-km correlations were +0.285 for pigmented and +0.079 for white cells, giving a smaller direct difference of +0.207. Observation effort was unrelated to raw pigmented isolation (rho=-0.032; within-fold permutation P=0.429) and did not explain relative isolation.

Natural-map replay separated the focal pigmented relationship from the less stable colour contrast. For raw 5-km isolation, pigmented rho was +0.252 compared with a natural-map mean of +0.133 and a 95% interval of 0.071-0.196 (upper-tail P=0.0002). Relative isolation gave rho=+0.285 versus a natural mean of +0.154 and interval 0.068-0.236 (P=0.0009). The same conclusions held in the 1,000 nearest-count maps (raw P=0.0020; relative P=0.0040).

The direct raw colour difference lay just above its natural interval (observed +0.324; null mean +0.205; interval 0.088-0.316; P=0.0194; five-scale maxT P=0.0465), but the relative-isolation difference did not (observed +0.207; null mean +0.152; interval 0.044-0.260; P=0.1586; maxT P=0.1190). Thus the robust result is an excess positive isolation-population relationship within pigmented occurrences, not a density-independent reciprocal displacement of pigmented and white states. The observed raw correlation attenuated at broad radii, but observed-minus-natural displacement was not confined cleanly to focal-10-km scales.

### 3.5 Predictive replay retained extreme populations as field targets

Sixteen pigmented cells met the predefined local-departure rule. Applying the identical detector to 10,000 natural predictive maps produced a mean of 13.59 candidates and a 95% interval of 7-21. The observed count was compatible with that reference (P=0.27897), as was candidate fraction (observed 0.04071; null mean 0.03107; upper-tail P=0.12609).

The event-based human comparison remained directionally consistent with the continuous result: 5-km population exposure was +0.06744 rank units higher at candidates than at local white comparators (directional P=0.00800; global maxT FWER P=0.05479). These results do not make the 16 sites anthropogenic anomalies. They define extreme, reproducibly selected populations for planting-history, voucher, microenvironment and genomic tests, while the all-cell continuous analysis supplies the main descriptive human-context geometry.

## 4. Discussion

### 4.1 The national trait dataset changed the biological question

The first major contribution is empirical: a non-biodiversity hiking-image stream was transformed into a dense, recent and auditable national flower-colour dataset. The matched benchmark shows why this mattered for the focal species—YAMAP yielded 3.81 times as many georeferenced records as iNaturalist during the same three years—and exhaustive author screening turned that quantity into study-specific trait quality. This is not a claim that route-based photographs are unbiased. It is evidence that an alternative observation process, combined with explicit curation, can reveal intraspecific trait geography that was previously too sparse for national, boundary and neighbourhood analyses.

The new coverage changed the biological question rather than simply increasing sample size. The apparent white-to-dark gradient separated into pigmentation state and pigmented-only intensity, and those responses showed different environmental and spatial organization. A single continuous colour index would have averaged over that distinction. The dataset therefore revealed that the polymorphism has at least two ecological layers before any particular mechanism was invoked.

### 4.2 Broad modelling resolved environmental structure and exposed a second spatial layer

The broad models explain a meaningful part of flower-colour geography. Pigmentation state showed a clear cool-climate association, while intensity among pigmented flowers responded to thermal seasonality, moisture and terrain. These are not generic correlations without biological content. Moderate low temperature can induce chalcone-synthase expression and anthocyanin accumulation in flowers (Shvarts et al., 1997), darker petals can absorb more radiation and reduce thermal safety margins (Li et al., 2026), and relative performance of anthocyanin morphs can depend on water supply (Warren & Mackenzie, 2001). Together with the aridity signal reported across historical floral records (Sullivan & Koski, 2021), the present geography defines a specific candidate landscape in which the physiological value and expression of pigmentation may change.

The result is also more informative than a single temperature rule. State and intensity differed, the temperature effect on intensity depended on seasonality, and rugged terrain retained an independent association after climate and space. These patterns point to distinct developmental or selective contexts rather than one universal “darker in the cold” response. They directly motivate factorial common-garden tests of mean temperature, thermal variability and water availability with petal chemistry, flower temperature, water relations and fitness measured together.

The cross-fitted spatial-null comparison reached the same state–intensity distinction from a different direction. At comparable geographical separation, pigmentation-state divergence increased with environmental difference beyond what the fitted continuous field reproduced, whereas conditional intensity did not. This does not convert the spatial field into neutral genetic divergence or demonstrate adaptation. It shows that geographical proximity alone is insufficient to explain the environmental ordering of the white–pigmented switch, and it prioritizes common-garden and genomic tests of environment-aligned state differentiation.

INLA-SPDE added a second achievement: it quantified coherent geography that remained after measured environment instead of hiding it in overconfident coefficients or arbitrary regional groups. The different residual ranges for state and intensity show that unresolved processes themselves operate at different spatial scales. That spatial layer is now a positive sampling guide for field microclimate, ancestry, isolation-by-distance and admixture analyses. It is not labelled as genetics in advance, but neither is it dismissed as noise.

### 4.3 Local boundaries reveal where pollinator opportunity may matter

Pollinator-mediated selection is generated through visits, pollen transfer and reproductive success within local plant-pollinator neighbourhoods. Distinct local pollinator assemblages can produce divergent selection on floral traits even among populations separated by less than 10 km (Torres-Vanegas et al., 2024). The 5-km analysis was designed at this population-neighbourhood scale, not because 5 km was assumed to be an exact bumblebee foraging distance. Its main ecological result is that focal-Bombus habitat support was higher on the pigmented side on average within environmentally tighter white-pigmented boundaries.

The heterogeneity tells us how to interpret that result rather than cancelling it. The mean direction was concentrated in a subset of boundaries, weakened as the comparison radius expanded and did not extend to pigmented-only intensity. This pattern is consistent with a geographic mosaic in which local Bombus opportunity may modify the maintenance or loss of a visible pigmented state in some neighbourhoods, rather than a uniform force that progressively darkens flowers across Japan. The result therefore identifies both the likely phenotype component and the spatial scale of a possible pollinator contribution.

The highland analysis supports this interpretation by showing what the local design avoids. A visually stronger national overlap between pigmented flowers and montane bumblebees disappeared after elevation was matched. That finding does not replace or diminish the focal local result; it demonstrates that broad environment and space can manufacture an appealing pollinator pattern, whereas the boundary design asks a more discriminating question within local environmental neighbourhoods.

The SDMs still measure habitat opportunity rather than realized selection, but the analysis has made that next causal layer tractable. The 67 boundaries are explicit sites for species-resolved visitation, bee visual contrast, stigma contact, pollen deposition, seed set and selection gradients. The macro analysis has thus converted a confounded national hypothesis into a localized ecological signal and a concrete field programme.

### 4.4 Human landscapes overlay the spatial geometry of pigmentation

The continuous analysis changes the human-context conclusion from a near-threshold result in 16 selected sites to an all-cell property of the pigmented distribution. Pigmented occurrences that were farther from other pigmented occurrences were more population-exposed than the fitted natural geography predicted. This was not only a consequence of sparse flower sampling: the pigmented relationship remained when same-colour distance was scaled by the local spacing of all flower cells, under all 10,000 natural maps and in the fixed near-count sensitivity.

The guardrails also prevent the clearest-looking descriptive contrast from being overstated. White cells showed a negative raw isolation-population relationship, but that sign disappeared after local flower-cell density was removed, and the raw white rho was almost exactly the natural expectation. The defensible inference is therefore not that human landscapes move the two colour states in perfectly opposite directions. It is that the pigmented state carries an additional positive human-context association beyond the broad environmental, spatial and sampling geometry already represented by the natural maps.

The scale profile does not uniquely identify mechanism. Raw pigmented correlations were concentrated from the focal cell through 10 km, but the excess over natural expectation extended more broadly after density correction. Horticultural planting or escape, human-modified establishment conditions, fine-scale environmental plasticity and access-linked observation remain distinguishable hypotheses rather than interchangeable explanations. The YAMAP mountain-route frame may both compress the full urban-rural gradient and overrepresent accessible edges. Repeated field history, voucher-level morphology, local environmental measurement and genomic comparison are required before the overlay can be assigned to provenance or causation.

### 4.5 Event calibration defines targets without turning them into causes

The earlier event analysis retains a useful but narrower role. A residual map can always make a few sites look extraordinary. By defining a relational ecological configuration and replaying it on 10,000 natural maps, we showed that 16 observed pigmented-among-white events are compatible with natural spatial variation. The event count is therefore a calibration guardrail, not a prerequisite for asking whether human context changes the geometry of all pigmented occurrences.

At the same time, the detector supplies 16 reproducible populations selected without human variables. The continuous analysis places them in a broader distribution rather than making them the sole evidence. They are extreme local configurations that can be revisited for planting and management histories, vouchers, repeated colour sampling, pigment chemistry and ancestry comparison. Their status is stronger than an anecdotal point and more honest than an anthropogenic anomaly: they are calibrated provenance targets.

### 4.6 A spatially varying balance can maintain flower-colour polymorphism

The full study converges on one ecological model. Climate can change the physiological expression, benefit and heat cost of anthocyanin pigmentation. Population history can preserve or redistribute colour variants across regions. Local bumblebee opportunity may modify the reproductive value of retaining a visible pigmented state. Human landscapes can overlay that natural geography by changing where isolated pigmented occurrences persist or are observed. These are not four interchangeable predictors; they leave distinguishable signatures on different components of colour and at different spatial scales.

Under this model, neither white nor pigmented flowers must be favoured everywhere. The same pigment can be physiologically useful under one thermal or moisture context, costly under another, and reproductively valuable only where effective visitors are locally available. Spatially changing benefits, combined with historical movement and persistence of variants, provide a coherent adaptive hypothesis for why one colour has not fixed across Japan. The current data do not directly estimate fitness or provenance, but they identify the phenotype component, geographical scale, candidate process and population set for each causal test.

The broader biogeographic contribution lies in this resolution. Repurposed images revealed the phenotype; broad models separated measured environment from coherent residual geography, while cross-fitted spatial-null replay detected environment-aligned divergence specifically for pigmentation state; local boundaries identified a short-range state-specific Bombus signal; an equal-elevation guardrail showed why broad highland overlap was not an adequate biotic test; continuous all-cell geometry identified a pigmented isolation-population relationship beyond natural geography and local sampling density; and event replay retained calibrated field targets without turning visual exceptions into causes. The study gains clarity by letting strong, heterogeneous and null results perform different inferential jobs within one connected sequence.

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

R Core Team. (2026). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing. https://www.R-project.org/

Renner, I. W., & Warton, D. I. (2013). Equivalence of MAXENT and Poisson point process models for species distribution modeling. *Biometrics*, 69, 274-281. https://doi.org/10.1111/j.1540-0420.2012.01824.x

Roberts, D. R., et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40, 913-929. https://doi.org/10.1111/ecog.02881

Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference for latent Gaussian models by using integrated nested Laplace approximations. *Journal of the Royal Statistical Society: Series B*, 71, 319-392. https://doi.org/10.1111/j.1467-9868.2008.00700.x

Shvarts, M., Borochov, A., & Weiss, D. (1997). Low temperature enhances petunia flower pigmentation and induces chalcone synthase gene expression. *Physiologia Plantarum*, 99, 67-72. https://doi.org/10.1111/j.1399-3054.1997.tb03432.x

Simpson, D., Rue, H., Riebler, A., Martins, T. G., & Sørbye, S. H. (2017). Penalising model component complexity: A principled, practical approach to constructing priors. *Statistical Science*, 32, 1-28. https://doi.org/10.1214/16-STS576

Soberón, J. (2007). Grinnellian and Eltonian niches and geographic distributions of species. *Ecology Letters*, 10, 1115-1123. https://doi.org/10.1111/j.1461-0248.2007.01107.x

Sullivan, C. N., & Koski, M. H. (2021). The effects of climate change on floral anthocyanin polymorphisms. *Proceedings of the Royal Society B*, 288, 20202693. https://doi.org/10.1098/rspb.2020.2693

Tasaki, K., et al. (2022). Identification of candidate genes responsible for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*, 13, 906879. https://doi.org/10.3389/fpls.2022.906879

Torres-Vanegas, F., Temesvári, V., Hildesheim, L. S., Rodríguez-Otero, C., Müller, V., Aukema, E., Friberg, M., & Opedal, Ø. H. (2024). Linking divergence in phenotypic selection on floral traits to divergence in local pollinator assemblages in a pollination-generalized plant. *Journal of Evolutionary Biology*, 37, 1312-1328. https://doi.org/10.1093/jeb/voae115

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
