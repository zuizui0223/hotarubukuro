# From broad geography to local boundaries: biogeography of flower-colour polymorphism from hiking photographs

**Running title:** Flower-colour polymorphism

## Abstract

**Aim:** Environment, history, pollinators and human movement can produce similar trait maps. We asked how these processes structure flower-colour polymorphism in the spotted bellflower (*Campanula punctata*) across Japan when tested at biologically matched scales.

**Location:** Japan.

**Taxon:** *Campanula punctata* (Campanulaceae).

**Methods:** We converted author-screened YAMAP hiking photographs into a national two-part phenotype: pigmentation state and pigmented-only intensity. INLA-SPDE models separated measured environmental associations from continuous residual geography. We characterized 67 independently fixed white-pigmented boundaries within 5 km, separately selected locally isolated pigmented cells, replayed that event detector on 10,000 natural predictive maps, and then examined human context.

**Results:** The dataset contained 1,922 observations, 3.81 times the matched iNaturalist records. Pigmentation state and conditional intensity differed geographically; only state exceeded a cross-fitted spatial expectation along environmental difference. Pigmentation was less likely in warmer climates, whereas intensity depended on temperature seasonality and was lower in wetter and more rugged environments. At local boundaries, a positive mean focal-bumblebee contrast coexisted with a near-zero median and 49.3% positive pairs, indicating subset-driven heterogeneity that attenuated at broader scales. Equal-elevation comparisons removed stronger-looking highland overlap. The observed event rule selected 16 local isolates, but natural maps reproduced their frequency. Five-kilometre population exposure was the leading post-selection human feature (global maxT FWER P=0.0548).

**Main conclusions:** Quantitative phenotyping revealed an environmentally ordered white-pigmented switch distinct from variation within the pigmented state. Scale change exposed heterogeneous local Bombus correspondence while preventing shared mountain geography from being mistaken for mechanism. Natural-map replay separated reproducible field targets from evidence that an additional process was required. The study converts one trait map into bounded physiological, spatial, biotic and provenance hypotheses for direct testing.

**Keywords:** digital phenotyping, flower colour, hiking photographs, iEcology, intraspecific variation, pollination, spatial confounding, trait biogeography

## 1. Introduction

### 1.1 The geographical mystery of flower-colour polymorphism

Why does one species remain white in some parts of its range and pigmented in others? This is not only a descriptive question. Intraspecific variation can change performance, persistence and species interactions (Westerband et al., 2021), and different explanations predict different responses to environmental change. Yet the map alone can mislead. Climate, population history, biotic interactions and human movement are all spatially structured, so several processes can draw similar geographical patterns.

Flower colour makes this problem especially revealing because anthocyanin pigmentation can do two ecological jobs at once. Pigments alter how petals absorb and reflect light and can contribute to temperature responses, water-stress responses and floral heat balance. The same visible colour is also a pollinator signal that can alter reproductive success. A pigmented morph can therefore gain a physiological or reproductive benefit in one setting while paying a thermal, hydraulic or biosynthetic cost in another (Warren & Mackenzie, 2001; Kellenberger et al., 2019; Trunschke et al., 2021; Li et al., 2026). Spatial variation in those benefits and costs offers a plausible route by which white and pigmented flowers persist within one species. The spotted bellflower (*Campanula punctata* Lam.; Campanulaceae) is a strong system for testing this idea because its large tubular flowers vary conspicuously from white to pigmented across Japan and are visited effectively by bumblebees.

### 1.2 The first hidden layer: state is not intensity

The apparent white-to-dark continuum may itself contain two biological questions. A white-pigmented transition can reflect whether the anthocyanin pathway is visibly expressed at all. Variation among flowers that are already pigmented can instead reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). A process that changes whether pigmentation is switched on need not control how dark the flower becomes afterwards. We therefore separated pigmentation state from visible intensity before asking what geography explains either response.

This split gives environmental hypotheses more precise targets, but it does not imply one universal stress-darkening rule. Moderate low temperature can activate chalcone-synthase expression and anthocyanin accumulation in corollas, and broad comparative data associate pigmentation with cooler and more arid climates (Shvarts et al., 1997; Sullivan & Koski, 2021). Darker petals can also absorb more shortwave radiation. That extra absorption may be useful in cool conditions yet impose heat and water costs in warm conditions because floral cooling can require transpiration (Li et al., 2026). Water limitation can favour anthocyanin morphs in some systems, but the direction is taxon and context dependent (Warren & Mackenzie, 2001). In *C. punctata*, UV-B exclusion changes flavonoid accumulation in leaves (Hashiba et al., 2006), showing environmental responsiveness of the flavonoid system without establishing the regulatory mechanism in petals. We therefore treated temperature as the strongest directional expectation, moisture as a possible stress-benefit and hydraulic-cost axis, and radiation and terrain as contextual modifiers whose signs could not be assumed in advance.

### 1.3 A new observation stream makes the national phenotype visible

For this species, occurrence coordinates alone could not answer either colour question, and matched public image streams were substantially sparser than the coverage required for dense national, boundary and neighbourhood analyses. Testing the hypotheses across Japan therefore first required a phenotype dense enough to resolve both the white-pigmented transition and variation among flowers that were already pigmented. We built that phenotype ourselves from YAMAP, a hiking-navigation and activity-diary platform. Every retrieved candidate was screened for taxon identity, focal flower and usable petal region; duplicate images were audited; and colour was extracted with a fixed image pipeline. In the matched 2023–2025 period, YAMAP yielded 1,964 georeferenced focal-species records, compared with 516 iNaturalist observations with photographs—a 3.81-fold difference. The three YAMAP years were also closely balanced (642, 687 and 635 records).

YAMAP is not spatially random. Its mountain-route frame enriches natural and semi-natural settings in which self-sustaining populations are plausible, but it does not establish wild provenance for any individual record. Nor can route sampling be assumed to weaken human associations in only one direction. It can compress the represented urban-rural gradient, while trailheads, roads and mountain margins can simultaneously increase observation opportunity, disturbance and opportunities for human-mediated movement. Here its primary value was a complementary image stream that could be curated into a recent quantitative trait dataset rather than a simple occurrence export. This is an iEcology use of digital material created for another purpose (Jarić et al., 2020; see Appendices S1–S2 in Supporting Information).

Making the national pattern visible exposed the next problem. Climate and topography are spatially structured, population history is spatially structured, and bumblebee species-distribution models (SDMs) are built from environmental predictors. A national model could therefore assign a precise coefficient to Bombus support even when flower colour and bumblebees merely follow the same climate (Soberón, 2007). Biotic interactions are also scale dependent (Araújo & Rozenfeld, 2014). We therefore used the national analysis to partition measured environmental associations from coherent residual geography, then changed comparison scale for the pollinator hypothesis.

### 1.4 From broad geography to local boundaries and calibrated departures

The local pollinator test has a strong natural-history and scale basis in *C. punctata*. Its large tubular flowers narrow the plausible set of effective visitors, and bumblebees are effective pollinators in the system. Geographical changes in *Bombus* fauna are linked to pollen-removal ecology and breeding-system change, including the bumblebee-absence hypothesis developed from the Izu Islands (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). This island evidence motivates a species-specific pollinator hypothesis; it does not by itself show that bumblebee absence caused white flowers. Pollinator-mediated selection is realized through local visitation, pollen transfer and reproductive success, and divergent selection on floral traits can arise among populations separated by less than 10 km when local pollinator assemblages differ (Torres-Vanegas et al., 2024). We therefore made nearby white-pigmented boundaries the local comparison and asked whether focal-bumblebee habitat opportunity increased specifically from the white side to the pigmented side.

The 5-km radius is not assumed to be an exact bumblebee foraging or selection distance. It was the finest predeclared scale that retained enough replicated colour transitions and it targets the population-neighbourhood scale at which pollinator assemblages and floral selection can vary. The 67 selected boundaries are geographically repeated local contrasts rather than one continuous transect. The mean, median and proportion of positive contrasts were all required because a minority of large local differences could create a positive mean. National overlap and equal-elevation comparisons were then used as a guardrail: they ask whether a stronger-looking broad pattern can be explained by shared mountain environment.

The broad natural template also makes local exceptions testable. This is a second and distinct local question. The boundary analysis is edge based: it asks about direction across a sharp transition. The departure analysis is node and neighbourhood based: it asks whether a pigmented cell isolated among environmentally similar white neighbours is an event that the natural model can reproduce. A point with a large residual is not automatically a new biological process. We therefore defined the relational event without human variables, identified its observed instances, and replayed the same detector on natural predictive maps. Only after the event definition was fixed did we inspect human context. That order is important because *C. punctata* is cultivated, so planting, escape or introgression are plausible provenance hypotheses but should not define the sites used to support them.

### 1.5 Predictions

The analysis follows one dependent sequence. First, if flower colour contains two ecological layers, pigmentation state and pigmented-only intensity should show different broad geography. Second, if focal bumblebees help maintain visible pigmentation, at least some nearby white-pigmented boundaries should show greater predicted habitat support on the pigmented side; a geographically uniform or intensity-wide effect was not required. Third, if isolated pigmented configurations require an additional process, their count or fraction should exceed the distribution generated by the natural model. Human context is a post-selection follow-up to that third prediction. Each answer determines the comparison needed for the next layer.

## 2. Materials and Methods

### 2.1 Study system and YAMAP sampling

The sampling frame covered the 2023–2025 flowering seasons. We screened all YAMAP records returned by the fixed Japanese focal-name search frame rather than subsampling candidates. Taxonomic errors and non-focal flowers were removed, and the focal flower and usable petal region were confirmed. Duplicate-image and raster-completeness checks left 1,922 observations for environmental analysis. Appendices S1–S2 give the complete record flow, matched public-database benchmark and sampling limitations.

### 2.2 From photographs to a two-part phenotype

The image pipeline retained source provenance, date, coordinates and image hashes. Display-referred sRGB pixels within the confirmed petal region were summarized and median RGB was converted to CIELAB under D65. CIELAB a* is used here as a reproducible human-visible red-green phenotype, not as calibrated spectral reflectance, UV contrast or anthocyanin concentration.

The white-pigmented boundary was estimated from a* alone before geography, environment, Bombus predictions or human data were used. Gaussian-mixture classification gave an operational boundary of a*=4.9688. The final dataset contained 966 white-like and 956 pigmented observations. Pigmentation state was analysed across all 1,922 observations, and standardized visible intensity was analysed only among pigmented flowers. The intensity coefficients are therefore conditional associations within flowers that crossed the observed pigmentation boundary. If measured or unmeasured factors affect both entry into the pigmented subset and intensity within it, conditioning on pigmentation can alter the composition of that subset; the two model parts should not be read as causally independent pathways. Appendix S2 reports the mixture model, ambiguity checks and phenotype sensitivities.

### 2.3 Broad environmental and continuous spatial geography

Climate predictors came from CHELSA v2.1, soils from SoilGrids 2.0 and terrain from derivatives of WorldClim 2.1 elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Eight response-blind abiotic axes represented warm-season temperature, climatic moisture, temperature seasonality, precipitation seasonality, terrain, two soil axes and shortwave radiation. Temperature represented a directional regulatory and thermal-balance hypothesis. Moisture represented both possible stress-linked pigment benefit and the water context needed to dissipate additional radiative heat. RSDS represented broad shortwave exposure rather than UV-B, while ruggedness represented a mixture of aspect, shading, drainage and microclimatic heterogeneity rather than a monotonic stress axis. Elevation was not added as another fixed effect because it overlaps several of these gradients.

Analyses were run in R 4.5.3 (R Core Team, 2026); latent Gaussian and SPDE models used the 'INLA' package version 25.10.19 (Rue et al., 2009). Separate models were fitted for pigmentation state and conditional intensity. The state model used a Bernoulli likelihood and the intensity model a Gaussian likelihood. A continuous Matérn field represented geographical structure remaining after measured environment (Lindgren et al., 2011; Simpson et al., 2017). This field estimates coherent residual geography rather than serving only as a nuisance correction. It may combine unmeasured environment, population structure, dispersal and sampling geography, and is not interpreted as a direct genetic map or dispersal distance. Model extensions were retained only when ecologically motivated and supported by prediction to held-out geographical blocks. Collinearity, hydroclimate alternatives and spatial specifications were checked in Appendix S3.

As a supporting sensitivity, we asked a complementary question at the 1-km-cell scale: whether phenotype divergence between environmentally dissimilar held-out locations exceeded an intercept + Matérn SPDE expectation at comparable geographical separation. Environmental distance used a separate frozen, response-blind six-score representation of broad and within-neighbourhood climate, aridity and topography. For each of five held-out geographical folds, pairs were divided into five geographical-distance strata, upper and lower environmental-distance quartiles were contrasted, and the observed mean high-minus-low divergence was compared with 500 space-only posterior-predictive realizations (seed 20260725). This FST/PST-inspired comparison is explicitly non-genetic: the spatial field is an empirical continuity expectation rather than drift, and any excess is environmental alignment rather than proof of selection or local adaptation (Appendix S3).

The later departure analysis used the same eight abiotic axes and five approximately 100-km geographical folds (Roberts et al., 2017; Valavi et al., 2019).

### 2.4 Zooming to local focal-Bombus boundaries

We built new SDMs for five Japanese *Bombus* species over a common mainland domain using shared predictor screening and spatial blocks. The surfaces represent predicted habitat support, not abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). To make species comparable, each prediction was ranked against predictions at that species' retained occurrence sites. The primary exposure combined occurrence-referenced support for *B. ardens* and *B. diversus*, the two broad focal pollinators documented in the system. The three montane or alpine species were retained as a supporting guardrail because their national overlap with pigmented highland flowers could reflect shared mountain geography. Appendix S4 gives the complete SDM workflow.

The main comparison deliberately moved from the national map to 1-km flower cells. We identified pure white-pigmented transitions within 5 km and selected non-overlapping pairs without Bombus values, environmental values or final contrast direction. The 5-km radius was the finest predeclared scale with sufficient replicated transitions; it represents a local population-neighbourhood comparison, not an estimate of bumblebee foraging distance. Only after pair identities were fixed were they oriented from white to pigmented. We calculated pigmented-minus-white focal-bumblebee support and tested the mean with 100,000 sign flips. Median contrast and the proportion of positive pairs were mandatory heterogeneity diagnostics. Environment did not select, orient or weight the pairs; after selection, environmental distance in the final eight axes was used as a balance diagnostic. Appendix S5 reports 5-, 10- and 25-km analyses, raw-SDM and all-five-species alternatives, community turnover and the elevation-controlled guardrail.

### 2.5 Defining local departures before reading human context

We did not select unusual sites from large fitted residuals. A local ecological event required a pigmented focal cell with at least three neighbours within 10 km, root-mean-square environmental distance <=1 across the eight standardized abiotic axes, and only observed white flowers among eligible neighbours. Human variables were absent from the natural model, event rule and candidate selection. Applying this fixed rule to the observed map selected the 16 observed sites.

The identical detector was then applied to 10,000 cross-fitted predictive maps from the final pigmentation-state model while cell geometry and observed trial counts remained fixed. The first use of replay was calibration: it asked whether the observed event count and fraction were unusual under the natural geography. The second use was post-selection inference: on every predictive map, event cells were selected anew and the same human-context contrasts were recalculated, so the null distribution included uncertainty in which cells the event rule would select. Eleven predefined settlement, land-use, access and natural-context features were tested in one global maxT family. Observation-effort measures were evaluated separately. Appendix S6 gives the full feature definitions, null distributions and sensitivities.

### 2.6 Reproducibility and inferential order

The study provides more than a fitted model: it exposes the full route from source images to manuscript claims. Derived data, source-construction code, analysis code, fixed seeds, software manifests, evidence identities and validation rules are versioned, and manuscript values are checked against checksum-locked artifacts in continuous integration. The inferential order is also fixed: phenotype construction precedes ecological predictors; the broad natural template precedes the local Bombus test; Bombus pairs are fixed before Bombus values are compared; the isolate-event rule is fixed before its natural frequency is calibrated; and human variables are read only after the observed event set has been defined. This makes the full investigation reproducible while preventing a later hypothesis from defining the observations then used to support it.

## 3. Results

### 3.1 A new image stream revealed a national quantitative polymorphism

The environmental analysis contained 1,922 georeferenced flowers in 1,305 1-km cells: 966 white-like and 956 pigmented. The matched YAMAP retrieval contained 3.81 times as many georeferenced focal-species records as iNaturalist in the same three-year period, and nearly equal annual counts supplied dense contemporary replication. Author screening, image hashing and deterministic phenotyping converted that alternative observation stream into a traceable quantitative trait dataset. Because the a* boundary was estimated before ecological predictors were read, the phenotype was independent of the geography later used to explain it.

This data layer was itself a biological result: it made national variation measurable at two levels rather than representing flower colour by a few categorical population samples. It also supplied the local replication needed for boundary and neighbourhood analyses later in the study.

### 3.2 Broad models separated two environmental responses and coherent residual geography

Pigmentation state and colour intensity did not tell the same geographical story. Pigmentation became less likely toward warmer Temperature PC1. The posterior mean log-odds was -0.542 (95% CrI -1.033 to -0.049), equivalent to an odds ratio of about 0.58 per SD, and no interaction met the full promotion criteria for state.

Among flowers that were already pigmented, intensity followed a different pattern. The final model retained Temperature PC1 × temperature seasonality (mean -0.204; 95% CrI -0.302 to -0.107). At mean seasonality, the temperature main effect was weak, but the warm-climate slope became increasingly negative as annual temperature variability increased. Intensity was also lower toward wetter/moister climates (-0.174; -0.323 to -0.024) and toward steeper, more rugged terrain (-0.134; -0.224 to -0.043). Soil axes, precipitation seasonality and RSDS did not show independently resolved effects in the final intensity model.

The spatial models also quantified a second, coherent layer of geography after those measured associations. Residual correlation range was 132.8 km (95% CrI 88.8-195.7) for pigmentation state and 65.7 km (31.0-132.6) for conditional intensity. Thus the broad stage delivered two outputs: a response-specific abiotic landscape and an independently testable map of unresolved regional structure. The ranges describe remaining correlation rather than seed, pollen or colonization distances.

The complementary cross-fitted spatial-null test sharpened the state–intensity difference. For pigmentation state, the mean high-minus-low environmental-distance contrast in held-out phenotype divergence was 0.1068, compared with a space-only posterior-predictive median of 0.0582 (excess +0.0486; one-sided P=0.0339). Conditional intensity showed no positive excess (observed -0.0472; null median -0.0013; P=0.8723). At comparable geographical separation, environmental difference therefore aligned with white–pigmented state divergence beyond fitted spatial continuity, whereas variation within already pigmented flowers did not.

### 3.3 Local boundaries exposed a subset-driven Bombus correspondence

The 5-km design produced 67 sharp transitions between pure white and pigmented cells, with median separation 2.0 km. The fixed pairs were environmentally closer than ordinary local edges: median eight-axis distance was 0.244 versus 0.318. The design therefore compared colour states within substantially tighter environmental neighbourhoods than a national overlay and at the finest scale with enough replicated boundaries.

Mean focal-bumblebee support was 0.0359 higher on the pigmented side (one-sided sign-flip P=0.0272), but the distribution did not show a pervasive shift: median contrast was -0.0028, only 49.3% of pairs were positive and the 5-, 10- and 25-km family gave q=0.0815. Mean contrast declined to +0.0084 at 10 km (P=0.325) and +0.0029 at 25 km (P=0.436), and no persuasive Bombus relationship was found for intensity among pigmented flowers. Raw SDM support also did not reproduce the 5-km result (P=0.267). The result is therefore heterogeneity itself: a minority of strong local boundaries raised the mean, whereas most boundaries showed little directional correspondence.

A separate highland guardrail reinforced the value of the local comparison. Pigmented highland flowers and montane/alpine *Bombus* support overlapped strongly on national maps, but the contrast vanished when nearby white and pigmented endpoints were constrained to similar elevation (all one-sided P>=0.755 for the <=50 m test). The stronger-looking broad pattern was therefore consistent with shared mountain geography. The local result and highland guardrail perform different roles: the first identifies boundaries for direct study of why correspondence appears in some neighbourhoods, and the second prevents national co-distribution from being promoted to mechanism.

### 3.4 Predictive replay calibrated a different local configuration

The isolate detector was not the Figure 3 boundary-pair rule. Applied to the observed map, it selected 16 pigmented cells that had at least three environmentally similar neighbours within 10 km and no observed pigmented flower in those eligible neighbouring cells. These were node-centred local configurations, not edges across which a Bombus contrast was calculated.

Replaying the identical detector on 10,000 natural predictive maps produced a mean of 13.59 events and a 95% interval of 7-21. The observed count was compatible with that reference (P=0.27897), as was candidate fraction (observed 0.04071; null mean 0.03107; upper-tail P=0.12609). The replay therefore did not select “natural-process-resistant” sites. It tested whether the frequency of the already defined event type exceeded the fitted natural geography, and the answer was no.

This calibration did not erase the 16 sites. It changed their inferential status from apparent anomalies to reproducibly selected field targets whose configuration can be revisited, measured and genetically assigned. Natural spatial variation was sufficient to generate the observed number of configurations, while the site identities remained useful for provenance and microenvironmental work.

### 3.5 Post-selection analysis identified a short-range human-context clue

Human variables were read only after the observed 16 sites were fixed. In the null calculation, however, the event detector was reapplied to every natural predictive map and human contrasts were recalculated for each map-selected event set. Population exposure within 5 km was the leading observed feature: candidates exceeded local white comparators by 0.06744 rank units (directional P=0.00800), with global maxT FWER P=0.05479. Other settlement scales and DID proximity pointed in the same direction, whereas observation-effort alternatives were null after correction.

This result is doubly conditional. The event count itself did not exceed the natural reference, and the strongest human feature remained just outside familywise support. The result therefore identifies a short-range provenance hypothesis and comparison scale, not a demonstrated human process or horticultural origin.

## 4. Discussion

### 4.1 The national trait dataset changed the biological question

The first major contribution is empirical: a non-biodiversity hiking-image stream was transformed into a dense, recent and auditable national flower-colour dataset. The matched benchmark shows why this mattered for the focal species—YAMAP yielded 3.81 times as many georeferenced records as iNaturalist during the same three years—and exhaustive author screening turned that quantity into study-specific trait quality. The missing resource was not occurrence coordinates alone, but enough images to distinguish a white-pigmented switch from variation within the pigmented state. This is not a claim that route-based photographs are unbiased. It is evidence that an alternative observation process, combined with explicit curation, can reveal intraspecific trait geography that was previously too sparse for national, boundary and neighbourhood analyses.

The mountain-route frame also had a specific ecological advantage and a bidirectional ceiling. It enriched settings in which natural or self-sustaining populations were plausible, which helped establish the broad natural template, but it did not guarantee wild provenance for every record. The same concentration on mountains and trails can narrow the human-context gradient and attenuate contrasts, while accessible routes can also concentrate disturbance, observations and human-mediated movement. Treating this sampling frame in both directions is more defensible than assuming it only removes or only creates human influence.

The new coverage changed the biological question rather than simply increasing sample size. The apparent white-to-dark gradient separated into pigmentation state and pigmented-only intensity, and those responses showed different environmental and spatial organization. A single continuous colour index would have averaged over that distinction. The dataset therefore revealed that the polymorphism has at least two observational layers before any particular mechanism was invoked.

### 4.2 Broad geography defines physiological hypotheses, not one stress rule

The cool-climate association of pigmentation state is biologically coherent with experimental evidence that moderate low temperature can activate chalcone-synthase expression and anthocyanin accumulation in corollas and with comparative evidence of lower pigmentation in warming climates (Shvarts et al., 1997; Sullivan & Koski, 2021). Two mechanisms remain open. The geography may reflect plastic regulation during floral development, or it may reflect population differentiation in the probability of entering a visibly pigmented state. The photographs cannot distinguish them, but the state-specific environmental excess beyond spatial continuity identifies the populations and environmental contrasts needed for common-garden and genomic tests.

Conditional intensity does not support a simple statement that warmer sites always have paler pigmented flowers. Its temperature slope depends on temperature seasonality: the warm-climate decline becomes stronger in more seasonal regions. One ecological possibility is that greater absorption by darker petals is advantageous or less costly under cool conditions but increasingly costly where warm-season heat is embedded in a large annual thermal range. Experimental work shows that dark flowers can absorb more shortwave radiation yet require greater latent cooling and still retain narrower thermal safety margins (Li et al., 2026). That mechanism is plausible, not demonstrated here, because the national layers do not measure flower temperature, transpiration, short developmental weather windows or spectral absorptance.

The negative precipitation coefficient for intensity places darker pigmented flowers toward the drier end of the national gradient. This direction agrees with studies in which anthocyanin morphs perform relatively better under drought and with broad associations between floral pigmentation and aridity (Warren & Mackenzie, 2001; Sullivan & Koski, 2021). Yet it cannot be read as proof that darker flowers are drought adapted. Dark petals can also incur higher radiative and hydraulic costs, precipitation PC1 is not atmospheric demand or flower water status, and VPD did not supply stable independent predictive information in the present geography. The result defines a water-balance hypothesis in which stress protection and cooling cost must be measured together.

Rugged terrain was associated with lower, not higher, intensity. This result resists a generic “more environmental stress means darker flowers” narrative. Topography PC1 measures relief rather than elevation and can combine aspect, shading, cold-air drainage, soil moisture, exposure and population isolation. Likewise, RSDS had no independent final effect and the dryness × RSDS state sensitivity did not pass the full predictive rule. The present data therefore support thermal and moisture hypotheses, but not a resolved national radiation mechanism or a universal terrain-stress effect.

The two-part phenotype also has a statistical ceiling. Intensity is observed only after a flower crosses the pigmentation boundary. If an unmeasured factor affects both state and intensity, restricting analysis to pigmented flowers can induce selection or collider-like distortion. The response split remains biologically useful because it avoids treating white flowers as merely low-intensity pigmented flowers, but coefficients from the two models are conditional descriptions rather than proof of independent regulatory pathways. Joint pigment chemistry, spectra and experimental induction are needed to resolve that dependence.

INLA-SPDE added a second achievement: it quantified coherent geography that remained after measured environment instead of hiding it in overconfident coefficients or arbitrary regional groups. The broader residual range for state than intensity is compatible with more regionally persistent organization of the on/off state and more local modulation after pigmentation is expressed, but it is not evidence that one component is more genetic. The field may contain ancestry, isolation by distance, dispersal, unmeasured microclimate and sampling geometry. It is a sampling guide, not a mechanism label.

### 4.3 Boundary heterogeneity is the pollinator result

Pollinator-mediated selection is generated through visits, pollen transfer and reproductive success within local plant-pollinator neighbourhoods. The tubular floral system and the Izu-island pollinator and breeding-system contrast make bumblebees a biologically focused hypothesis rather than one more arbitrary SDM layer. The island comparison remains motivation, not evidence that flower colour itself evolved through bumblebee absence. The 5-km design asked a predeclared local directional question across 67 geographically repeated boundaries.

The answer is not a uniform pigmented-side advantage. A positive mean coexisted with a near-zero negative median and fewer than half positive pairs, weakened with distance, depended on occurrence-reference scaling and did not extend to intensity. The ecological result is therefore a mosaic: a subset of boundaries may contain conditions under which focal-Bombus opportunity contributes to maintaining or losing a visible pigmented state, while most boundaries do not show the predicted direction. Once pigmentation is visible, further darkness may add little to recognition or learnt discrimination, but bee colour space, ultraviolet contrast and behaviour were not measured.

This framing makes the 67-site output more useful. Field work should not only estimate one national mean; it should compare strongly positive boundaries with near-zero and negative boundaries. Species-resolved abundance and phenology, nest and floral-resource context, visitation, stigma contact, pollen deposition, seed set, bee visual contrast, breeding system, gene flow and microenvironment can then explain why correspondence appears in some neighbourhoods and not others.

The highland analysis shows what this local design avoids. A visually stronger national overlap between pigmented flowers and montane bumblebees disappeared after elevation was matched. The three highland species therefore perform a negative-guardrail role rather than entering the broad-ranging focal index. Broad environment and space can manufacture an appealing pollinator pattern; only direct local observation can establish whether the subset-driven boundary correspondence reflects realized reproductive selection.

### 4.4 Natural-map replay distinguishes event definition from event excess

The departure analysis asks a different local question from the Bombus boundary analysis. Figure 3 uses non-overlapping edges and tests a signed Bombus difference from white to pigmented. Figure 4 uses focal cells and asks whether a pigmented cell is locally isolated among environmentally similar observed white neighbours. A cell can satisfy one design without satisfying the other, and neither set was selected to maximize the result of the other analysis.

The 16 sites were selected by applying the fixed relational rule to the observed map. The 10,000-map replay was then used twice. First, it calibrated event frequency: the natural model produced similar counts and fractions often enough that no additional process was required to explain how many such configurations occurred. Second, it supplied the post-selection null for human context by reselecting event cells and recalculating human contrasts on every predictive map. Thus replay did not identify 16 places that natural processes could not reproduce. It propagated the fitted natural geography through both the ecological event and the later comparison.

This distinction is the methodological gain. A residual threshold would have labelled extreme prediction errors after fitting one map. The relational detector instead defines a biologically legible configuration, and predictive replay asks whether that configuration is exceptional under the model that generated the natural template. The null result prevents visual surprise from becoming a causal claim while preserving deliberately chosen populations for field microclimate, repeated colour sampling and genomic provenance.

### 4.5 Human context remains a doubly conditional provenance hypothesis

Population exposure within 5 km was the leading feature in a predefined eleven-variable family, and population at 10 km plus DID proximity pointed in the same direction. However, global maxT FWER P=0.05479 does not establish a human mechanism. More fundamentally, the 16-event frequency was already compatible with natural geography. The supported statement is therefore narrow: among sites selected by a human-blind isolate rule, short-range settlement exposure was the strongest tested contextual feature, but neither an excess of isolates nor familywise evidence of human origin was obtained.

Horticultural movement is biologically plausible because *C. punctata* and coloured material are cultivated. Human influence could also act through planting, escape, introgression, soil movement, local irrigation, shade, mowing or disturbance rather than ancestry alone. Hiking routes cannot be treated as human-free controls: clothing and footwear can transport seeds in protected landscapes, and trails can operate as disturbed establishment or spread corridors in other plant systems (Mount & Pickering, 2009; Zani et al., 2025). These studies establish general pathways, not dispersal of *C. punctata* in the present data.

The YAMAP frame can move the observed human contrast in either direction. Conditioning both candidates and comparators on mountain-route access may compress settlement variation and weaken a real difference. Conversely, population, roads and trailheads may proxy both human exposure and the probability that a site was photographed. Null within-dataset effort diagnostics argue against the simplest explanation that the 16 cells merely contain more records or independent activities, but they do not remove entry into the sampling frame as a broader source of bias.

The appropriate next step is therefore focused rather than rhetorical. Repeated field sampling, vouchers, planting and management histories, local environmental measurements, trail and access context, and genomic comparison with neighbouring white populations and horticultural material can distinguish natural local variation, environmentally induced pigmentation, planting, escape and introgression. The current analysis has supplied the sites, comparators and spatial scale for that work without deciding the answer in advance.

### 4.6 A spatially varying balance can maintain flower-colour polymorphism

The full study converges on one candidate eco-evolutionary model. Climate can change the regulation, physiological benefit and thermal or hydraulic cost of anthocyanin pigmentation. Unresolved historical and dispersal processes can preserve or redistribute colour variants across regions. Local bumblebee opportunity may modify the reproductive value of retaining a visible pigmented state in some neighbourhoods. Human movement or managed microenvironment may occasionally add a local layer. These are not interchangeable predictors; they leave different signatures on different components of colour and at different spatial scales.

Under this model, neither white nor pigmented flowers must be favoured everywhere. A pigment can be useful under one thermal or moisture context, costly under another, and reproductively valuable only where effective visitors and local community conditions make the signal consequential. Spatially changing benefits and costs, combined with movement and persistence of variants, offer a testable explanation for why one colour has not fixed across Japan. The current data do not estimate fitness, plasticity or ancestry directly; they identify the phenotype component, geographical scale, competing mechanism and population set for each causal test.

The broader biogeographic contribution lies in this resolution. Repurposed images revealed the phenotype; broad models separated measured environment from coherent residual geography; cross-fitted space-only replay detected environment-aligned divergence specifically for pigmentation state; boundary analysis exposed heterogeneous rather than uniform Bombus correspondence; an equal-elevation guardrail showed why highland overlap was not an adequate biotic test; predictive replay showed that locally isolated configurations occur at a natural-model-compatible frequency; and post-selection analysis identified the leading human-context clue without establishing human origin. The paper gains clarity by letting positive, heterogeneous and null results perform different inferential jobs. The payoff is a reproducible map-to-mechanism programme rather than a single overfitted causal map.

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

Mount, A., & Pickering, C. M. (2009). Testing the capacity of clothing to act as a vector for non-native seed in protected areas. *Journal of Environmental Management*, 91, 168-179. https://doi.org/10.1016/j.jenvman.2009.08.002

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

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation of species distribution models. *Methods in Ecology and Evolution*, 10, 225-232. https://doi.org/10.1111/2041-210X.13107

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016). How to colour a flower: On the optical principles of flower coloration. *Proceedings of the Royal Society B*, 283, 20160429. https://doi.org/10.1098/rspb.2016.0429

Warren, J., & Mackenzie, S. (2001). Why are all colour combinations not equally represented as flower-colour polymorphisms? *New Phytologist*, 151, 237-241. https://doi.org/10.1046/j.1469-8137.2001.00159.x

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait variation in plants: a renewed focus on its role in ecological processes. *Annals of Botany*, 127, 397-410. https://doi.org/10.1093/aob/mcab011

Zani, D., Lischke, H., Åkerman, J., & Lehsten, V. (2025). Hiking trails facilitate the spread of a native high-Arctic species. *Ecology and Evolution*, 15, e70809. https://doi.org/10.1002/ece3.70809

## Data Accessibility Statement

For double-anonymous review, an anonymized private repository will contain the derived flower-colour tables, environmental source registry, *Bombus* SDM configuration and occurrence-referenced support, analysis code, seeds, local-transition specifications, environmental-balance audit, local-departure definitions and workflow provenance. Original YAMAP photographs are third-party content and cannot be redistributed. The derived trait table retains the provenance and quantitative measurements needed to reproduce manuscript analyses. A permanent public repository and DOI will replace the private review link upon acceptance.

## Conflict of Interest

[Statement withheld from the anonymized manuscript and supplied separately at submission.]

## Author Contributions

[Author contributions withheld from the anonymized manuscript and supplied on the separate title page/submission form.]
