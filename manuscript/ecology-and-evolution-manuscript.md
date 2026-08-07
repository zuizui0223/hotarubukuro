# Layered flower-colour geography in *Campanula punctata*: abiotic structure, local bumblebee limitation and anthropogenic context

## Abstract

Flower-colour geography can reflect several layers of process, from broad abiotic gradients to local biotic interactions and, finally, departures associated with human-modified landscapes. These layers are difficult to interpret when colour is treated as a single continuous response or when every predictor is placed in one national model. We assembled an author-reviewed, route-linked photographic dataset of *Campanula punctata* from YAMAP and analysed 1,909 observations in a staged framework. A response-blind Gaussian mixture separated 955 white-like and 954 pigmented flowers at CIELAB a*=5.0202; 125 low-confidence classifications were flagged but retained. We therefore analysed two responses: pigmentation presence and visible intensity conditional on pigmentation. First, national INLA-SPDE models established the broad natural geography. Temperature was negatively associated with both components, conditional intensity also varied with topography, and residual spatial ranges were approximately 131 km for pigmentation presence and 61 km for conditional intensity. Five geographically blocked folds yielded AUC=0.863 for pigmentation presence and RMSE=0.912 for conditional intensity. We then zoomed to a local biotic hypothesis rather than treating bumblebee SDMs as national causal predictors. Among one-to-one pairs within 25 km and matched on environmental context, 22 lower-third pairs had 0.223 higher pigmented share where at least one focal *Bombus* species had moderate predicted availability than where all five species had low predicted availability (upper-tail predictive p=0.017). Because the lower-third gate was adopted after exploratory design development, the complete threshold grid was retained and the across-grid BH q-value was 0.102; this is directionally coherent exploratory support, not a confirmatory test. Conditional intensity did not increase across the same gate (difference approximately -0.615; p=0.904), suggesting that predicted bumblebee availability may be more relevant to whether pigmentation is maintained than to how dark already pigmented flowers become. Finally, we asked what remained after the natural reference. Eighteen local pigmented isolates were identified, but their excess was not robust across held-out and joint posterior-predictive references; population and densely inhabited district context were suggestive but did not survive familywise correction. The resulting picture is layered rather than additive: abiotic environment and spatial structure define the national background, local bumblebee limitation may modify the maintenance of pigmentation, and residual local departures can be screened for anthropogenic context without being labelled anthropogenic in origin. Predicted *Bombus* support remains an availability proxy rather than visitation or selection pressure, and direct field measurements are required to test the proposed attraction mechanism.

**Keywords:** anthropogenic context; bumblebee limitation; digital phenotyping; flower colour; INLA-SPDE; pollinator availability; *Campanula punctata*

## 1 | Introduction

Intraspecific trait variation contributes to population responses, eco-evolutionary dynamics and community assembly, yet its geographical distribution remains poorly measured for many plant traits (Westerband et al., 2021). Floral colour is especially informative because it can mediate animal attraction while also reflecting physiological, developmental and historical processes (Rausher, 2008; Koski & Ashman, 2015; Trunschke et al., 2021). Broad-scale studies frequently reduce this variation to a few colour categories. Such categories can describe morph distributions, but they cannot distinguish the transition between little visible pigmentation and pigmentation expression from quantitative variation among flowers that are already pigmented.

Large collections of community photographs can partly close this geographical trait-data gap (Laitly et al., 2021; Luong et al., 2023; McKenzie et al., 2026). Their value is accompanied by camera, illumination, access and observer-selection heterogeneity. YAMAP is a hiking and outdoor-activity platform in which photographs are associated with GPS-linked activity records. The platform was not designed as a biodiversity survey, but its route-linked structure can provide geographically explicit trait observations when taxon identity and measurement regions are manually reviewed. At the same time, the sampling frame is necessarily concentrated along accessible hiking routes. We therefore treat the data as an author-reviewed platform census within explicit dates and screening rules, not as an areally random sample of Japanese populations.

A second problem is the biological interpretation of continuous colour. A threshold-like transition in anthocyanin-pathway activity can separate visibly white and pigmented flowers, whereas colour variation among pigmented flowers can arise from pigment amount, cell structure and optical context (Dick et al., 2011; Tasaki et al., 2022; van der Kooi et al., 2016). A single white-to-pink numerical response risks analysing small optical differences among white flowers as if they were pigment concentration. We therefore use a two-part response: pigmentation presence for all flowers and visible intensity only after a flower is assigned to the pigmented regime. CIELAB a* remains an uncalibrated human-visible optical measure; it is not anthocyanin concentration, ultraviolet reflectance or receptor-based bumblebee colour contrast.

A third problem is the scale at which pollinator hypotheses are formulated. Climate, topography, population history and dispersal can create broad geographical clines in both plants and pollinators. Species-distribution models of pollinators are themselves constructed from environmental geography. Adding environment, space and predicted pollinators to one national regression therefore does not identify an independent pollinator effect (Soberón, 2007; Paciorek, 2010; Hanks et al., 2015). A stronger observational strategy is to use the national environment-plus-space model as a natural reference, then ask a local directional question among nearby sites that have already been matched on measured environmental context.

*Campanula punctata* Lam. provides a biologically motivated system for such a test. Its large tubular flowers are visited and effectively pollinated by bumblebees, and previous work showed that changes in pollinator fauna accompany geographical variation in floral size and that floral–pollinator size matching affects pollen removal (Nagano et al., 2014). In the Izu Islands, loss of bumblebees has long been linked to shifts in pollinator assemblages and breeding systems, forming the “bumblebee-absence hypothesis” for this species complex (Inoue & Amano, 1986; Inoue, 1988). These studies do not demonstrate that bumblebee absence caused white flower colour, but they provide independent natural-history motivation for asking whether a conspicuous floral signal loses benefit when effective bumblebee pollinators are poorly available.

We therefore formulate a **pigmentation-benefit relaxation hypothesis**. If pigmentation increases the attractiveness or detectability of flowers to important bumblebee pollinators, then maintaining pigmentation can provide a reproductive benefit where bumblebees are locally available. Where all focal bumblebee taxa are poorly available, that attraction benefit should weaken and selection maintaining the pigmented state may relax, making white flowers relatively more common. A physiological cost of pigment production could reinforce such loss, but pigment cost is not measured here and is not required for the primary prediction. A stronger dose-response prediction—that greater bumblebee availability should produce darker flowers among already pigmented individuals—is treated as secondary because occurrence-based SDMs are not measurements of visitation rate.

This hypothesis creates a direct link to the two-part phenotype. The primary prediction concerns **pigmentation state**: locally bumblebee-limited sites should have lower pigmented share than environmentally similar bumblebee-available sites. Conditional intensity is a separate secondary response and need not follow the same pattern if it is more strongly governed by temperature, light, physiology or genetic background.

Yet neither a broad natural baseline nor one focal biotic hypothesis should be expected to explain every local observation. The remainder is itself informative. A pigmented cell embedded in an otherwise white, environmentally similar neighbourhood marks a local departure from the broad natural expectation. If such departures are defined without using human variables, their landscape context can then be examined without defining the anomaly by the same anthropogenic predictors later used to characterize it. We therefore treat human influence as a third, bounded layer: not as a competing national driver, but as a way to characterize local departures left after the natural geography has been established.

The analysis consequently narrows the question step by step. First, we define the two-part flower-colour phenotype. Second, we establish the national abiotic and continuous spatial background and generate geographically cross-fitted natural predictive maps. Third, we zoom to environmentally similar nearby sites and ask whether low predicted *Bombus* availability corresponds to relaxation of pigmentation. Finally, we identify local pigmented departures from the natural reference and ask whether those pre-defined departures occupy distinctive anthropogenic contexts. The hierarchy is deliberate: abiotic environment and space set the broad stage; pollinator availability tests a local biotic mechanism; and human context is examined only after the remaining local departures have been identified independently of human variables.

## 2 | Materials and Methods

### 2.1 | Sampling frame and author review

The source dataset comprised eligible *C. punctata* records identified by the author in YAMAP activity records during the flowering-season sampling windows in 2023–2025. Before analytical tables were constructed, the author inspected the focal subject, confirmed the flower and petal region used for colour measurement, removed repeated photographs of the same individual and removed taxonomic errors. Records were not selected on the basis of flower colour, fitted model residuals, *Bombus* predictions or human-landscape context.

YAMAP observations are route- and access-biased. Users can hide locations, trails differ in activity and flowers differ in conspicuousness. The analytical population is therefore the retained route-linked photographic sample, not a random plot survey. The active reproducible complete-support population contains 1,909 observations.

### 2.2 | Visible-colour extraction and quality control

The deterministic image pipeline measured the author-confirmed petal region using display-referred sRGB pixels and used the channel-wise median RGB value as the primary colour statistic. The resulting visible colour was converted to CIELAB under D65 and a 2° standard-observer convention. CIELAB a* was used as the red–green optical phenotype.

No image-specific white balance was possible because neutral references and standardized camera metadata were unavailable. Mask coverage, exposure, shadows, multimodality and disagreement among alternative pixel summaries generated warning flags. Warning flags did not automatically remove dark or otherwise extreme observations. The resulting response should be interpreted as an aggregate human-visible image phenotype rather than calibrated reflectance or pigment chemistry.

### 2.3 | Response-blind mixture classification and the two-part response

We estimated the pigmentation boundary from CIELAB a* without geography, date, environment, *Bombus*, human context or fitted-model information. The implementation used `mclust` 6.1.2 and fitted univariate Gaussian mixtures with one to eight components under both equal-variance (`E`) and variable-variance (`V`) parameterizations. `Mclust` selected the model by BIC. In the active 1,909 analysis, the selected model was a four-component variable-variance (`V`) mixture.

Because the statistical mixture contained more than two optical components, components were ordered by their fitted means and the largest adjacent mean gap was used to collapse them into two measurement regimes. The lower-mean components defined the white-like regime and the upper-mean components the pigmented regime. For each observation, posterior membership probabilities of the upper components were summed to obtain P(pigmented). Flowers with P(pigmented) >=0.5 were assigned to the pigmented regime; otherwise they were assigned to the white-like regime. Classification confidence was defined from this posterior regime probability: P(pigmented) >=0.8 or <=0.2 was high confidence, whereas intermediate values were flagged as ambiguous and retained. This yielded an operational boundary of a*=5.020161, 955 white-like observations, 954 pigmented observations and 125 ambiguous observations.

We then defined two responses:

1. **pigmentation presence**, a binary response for all 1,909 observations; and
2. **conditional visible intensity**, standardized a* among the 954 observations assigned to the pigmented regime.

White-flower a* variation was not interpreted as pigment intensity.

### 2.4 | Environmental predictors and multiscale context

Climate variables were obtained from CHELSA v2.1 climatologies, soil variables from ISRIC SoilGrids 2.0 and elevation from WorldClim 2.1, with slope and terrain heterogeneity derived from elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Continuous layers were aligned to a common grid and extracted to observations. Response-blind principal components summarized temperature, precipitation, soil and topography for the observation-level models.

For the geographically cross-fitted natural reference and local matching, elevation, temperature, precipitation and radiation were summarized at two spatial scales. We calculated 50-km neighbourhood means and cell-minus-neighbourhood deviations and reduced these to two broad-scale and two within-neighbourhood principal-component axes. These four axes define the environmental context used by the local matched-pair analyses.

### 2.5 | National environment-plus-space models

We used separate observation-level INLA-SPDE models for the two response parts. Pigmentation presence used a Bernoulli likelihood with logit link; conditional intensity used a Gaussian likelihood. Standardized environmental axes were included as fixed effects and a Matérn SPDE field represented continuous residual geography (Lindgren et al., 2011).

The mesh used maximum inner and outer edge lengths of 20 and 100 km with a 5-km cutoff. The 20-km inner resolution was chosen to resolve subregional spatial structure while remaining substantially finer than the approximately 100-km-scale field anticipated for national residual geography; the 100-km outer edge limited unnecessary mesh density away from observations, and the 5-km cutoff avoided extremely small triangles around tightly clustered records. Penalised-complexity priors specified P(spatial range <100 km)=0.05 and P(spatial SD >1)=0.05 (Simpson et al., 2017). These priors weakly regularize the field away from very short-range or very high-amplitude structure rather than asserting that the true range is 100 km. Fixed-effect estimates are conditional on this mesh and prior specification.

We separately built a cell-level predictive natural baseline. Pigmented counts were modelled as binomial trials and conditional intensity as Gaussian among cells with pigmented observations. Five response-blind geographical folds based on 100-km blocks evaluated transfer to withheld regions (Roberts et al., 2017; Valavi et al., 2019). Environmental standardization occurred inside training folds. For each held-out cell we generated 1,000 predictive replicates while preserving observation counts. These cross-fitted predictive maps provide the common natural reference for later local questions.

### 2.6 | Predicted *Bombus* availability

We considered five Japanese bumblebee taxa used in the established analysis: *Bombus ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus* and *B. honshuensis*. The active 1,909 pipeline restores checksum-locked prediction surfaces generated from the earlier ENMeval/maxnet source-build workflow. The historical ENMeval candidate/tuning objects required to replay the complete model-selection path are not all retained; therefore the active downstream analysis is reproducible **conditional on these frozen prediction surfaces**, not from GBIF occurrence records through model selection to final inference.

Presence-background SDM outputs are not abundance, visit counts or pollination service (Renner & Warton, 2013; Guillera-Arroita, 2015). We consequently avoid summing raw suitability values and calling the result “Bombus pressure.” Each species surface was converted to a within-species support rank. This monotone transformation retains relative local support within a species without assuming quantitative comparability among species.

For each flower cell, we defined **best focal-Bombus support** as the maximum of the five within-species support ranks. A low maximum means that all five focal taxa have low predicted support; a high maximum means that at least one focal taxon is moderately or strongly supported. This gate-type exposure is intended to represent potential bumblebee availability rather than visitation intensity.


### 2.7 | Local *Bombus*-limitation gate

The local analysis tested the directional pigmentation-benefit relaxation hypothesis. It deliberately did **not** fit a second environment-plus-space model. Instead, potential comparisons were defined before flower colour was read.

We first constructed response-blind local neighbour candidates within 25 km, restricted to endpoints in the same held-out flower-model fold and to the five-species common-support region. We then retained only candidate pairs with environmental RMS distance <=0.75 across the four broad/within-50-km environmental axes. One-to-one pairs were selected greedily using environmental similarity and geographical distance only, preventing repeated reuse of the same cell.

A pair was oriented from **Bombus-limited** to **Bombus-available** using the SDM ranks alone. The biologically interpretable active gate defined a limited endpoint as a cell where the maximum within-species rank across all five focal taxa was <=0.33—equivalently, all five taxa were in their lower third of predicted support. The available endpoint required at least one focal taxon with rank >=0.50. Flower colour was not used to define or orient pairs.

The active lower-third gate was adopted after exploratory design development because it provided a clear biological interpretation and a useful local matched sample. To avoid disguising this history, the full fixed development grid of lower thresholds 0.10, 0.20, 0.25 and 0.33 is retained in every run. We report both the within-threshold correction across the two response parts and the more conservative BH correction across all gate × response tests. The stage is therefore a mechanistically motivated local sensitivity rather than a preregistered confirmatory test.

The primary statistic was the mean directed difference in pigmented share,

`pigmented share_available - pigmented share_limited`.

The secondary statistic used the same orientation but compared conditional visible intensity only when both endpoints contained pigmented observations.

To ask whether an observed difference exceeded what the broad natural geography would generate, the fixed matched pairs and their *Bombus*-defined orientation were replayed on each of the 1,000 cross-fitted environment-plus-SPDE flower maps. One-sided upper-tail predictive probabilities were computed from the resulting reference distribution. Thus environment and spatial structure are not added again as local regression covariates; they enter first through pre-outcome environmental matching and second as a conservative predictive reference.

This design reduces broad environmental and spatial confounding but cannot remove it completely. The *Bombus* surfaces are themselves predictions from environmental geography, and unmeasured environment, distribution history and sampling structure can remain. The result is therefore a directional correspondence with predicted bumblebee availability, not an estimate of bumblebee abundance, visitation, attraction strength or pollinator-mediated selection.

The previous unsigned local community-turnover analysis is retained as a sensitivity only and is not read by the final result or claim registries.

### 2.8 | Local pigmented isolates and post-selection human context

A separate local event asked whether pigmented cells occurred unusually often among environmentally similar white neighbours. Candidate selection used flower state, local distance, environmental similarity and sampling support but no population, land use, roads, DID proximity, flowering date or intensity. The same event extractor was replayed on natural predictive maps.

The active analysis identified 18 pre-fixed candidate cells. To evaluate the stability of their apparent excess, we used a higher-precision held-out cross-fitted reference and a separate full-data joint spatial posterior-predictive sensitivity. Because the candidate fraction was near the upper tail under the cross-fitted reference but compatible with the joint posterior-predictive reference, candidates are treated as follow-up sites rather than evidence of excess occurrence.

Only after candidate identities were fixed did we characterize local human context using WorldPop population, MLIT land use, road context and DID proximity. Familywise maxT correction was used within feature families. These analyses characterize the locations of selected candidates; they do not identify planting, escape, introgression or horticultural origin.

Flowering date was retained as a model-free Supplementary description after candidate selection and did not enter any main claim.

### 2.9 | Reproducibility and claim control

The active numerical population, stage graph, package environment, frozen inputs, result tables and validators are versioned in the repository. Final results are generated from fresh pipeline outputs. The stage-03 metadata explicitly records the lower-third gate history, the full threshold grid, environmental matching, absence of a second local environment/SPDE model and the fixed-SDM uncertainty ceiling. The previous 1,923-observation analysis is archived under `legacy/published-1923/` and is not treated as the active paper.

## 3 | Results

### 3.1 | Two-part optical phenotype

The active ecological analysis contained 1,909 observations. The response-blind four-component variable-variance mixture separated 955 white-like and 954 pigmented observations at a*=5.0202. One hundred twenty-five observations had intermediate regime-membership probability and were retained with an ambiguity flag (Figure 1). Conditional intensity was calculated only among the 954 pigmented observations.

### 3.2 | National environmental clines and residual geography

Temperature was negatively associated with both response components. For pigmentation presence, the temperature-axis posterior mean was -0.579 (95% CrI -1.077 to -0.079). For conditional intensity, the temperature coefficient was -0.322 (-0.487 to -0.160). Conditional intensity also declined along the pre-oriented topographic axis (mean -0.144, CrI -0.238 to -0.051). Other coefficients should be interpreted as conditional on the spatial field; notably, the earlier soil association was not retained as a robust 95% interval result in the active 1,909 analysis.

Residual spatial structure remained substantial. The posterior SPDE range was 130.5 km (95% CrI 87.7–191.5) for pigmentation presence and 60.8 km (31.2–113.9) for conditional intensity. The difference is descriptive because the two response models differ in likelihood and spatial support; it is not a formal test that the two biological processes operate at different ranges.

The five-fold cross-fitted national presence model had AUC=0.863. Conditional intensity had RMSE=0.912. These metrics quantify prediction to withheld parts of the sampled geography rather than causal adequacy (Figure 2).

### 3.3 | After environmental matching, local *Bombus* limitation tracked pigmentation state


The local limitation analysis asked a different, directional question. Under the active lower-third gate, 22 one-to-one environmentally matched pairs contrasted a cell where all five focal species were in their lower third of predicted support with a nearby cell where at least one species was at or above median support. Pigmented share was **0.223 higher** on the *Bombus*-available endpoint. The mean natural-map contrast was approximately 0.001 and the upper-tail predictive p-value was 0.017. Within the lower-third gate, BH correction across pigmentation state and conditional intensity gave q=0.034. Because the lower-third gate was adopted after exploratory design development, the complete threshold grid was retained; across all gate × response tests the BH q-value for the pigmentation contrast was 0.102. We therefore interpret this pattern as directionally coherent exploratory support rather than a confirmatory significance result (Figure 3).

The direction was not confined to a single spatial fold. Lower-third pairs occurred in all five folds; fold-level mean contrasts were positive in four folds and zero in the fifth, with none negative. The tighter gates had much smaller matched samples: the 0.10 gate produced no eligible pairs, the 0.20 gate produced two pairs and the 0.25 gate six pairs. Their observed pigmentation differences were positive, but the predictive probabilities were correspondingly imprecise.

Conditional intensity did not follow the proposed dose-like direction. Only six lower-third matched pairs had pigmented observations at both endpoints, and the mean available-minus-limited intensity contrast was approximately -0.615 (upper-tail p=0.904). Thus the active local evidence concerns the probability of being pigmented, not stronger visible pigmentation once pigmentation is present. This local biotic pattern did not, however, exhaust the geography left by the natural reference. We next asked where pigmented cells remained locally discordant with otherwise similar white neighbourhoods.

### 3.4 | The natural reference left 18 local pigmented departures, but their excess was not robust

Eighteen pre-fixed local pigmented isolates were identified, corresponding to candidate fraction 0.0501. Under the higher-precision held-out cross-fitted reference, the candidate-count upper-tail probability was 0.136 and the candidate-fraction probability was 0.049 (Monte Carlo 95% interval 0.045–0.053). The same-fold sensitivity gave fraction p=0.048. However, a full-data joint spatial posterior-predictive check gave count p=0.222 and fraction p=0.123 (Monte Carlo 95% interval 0.121–0.125). Because the apparent fraction excess did not persist across predictive references, the candidate set is retained for follow-up rather than interpreted as evidence that such isolates occur more often than expected naturally. These 18 cells therefore serve as pre-defined local departures for a final question: do they occupy distinctive human-modified contexts?

### 3.5 | The remaining local departures showed suggestive anthropogenic context

The focal-minus-neighbour 5-km population contrast was positive (estimate 0.055; raw p=0.031), but its corrected p-value was 0.102. Population–DID alignment was also positive (estimate 0.058; raw p=0.017) with corrected p=0.060. The observed fraction of candidates in the DID-proximate high-population context was 0.50; its corrected p-value was 0.200. These results are compatible with modest human-context tendencies but do not pass their familywise thresholds and do not establish horticultural provenance.

## 4 | Discussion

### 4.1 | A layered explanation of flower-colour geography

The clearest way to read the analysis is from broad structure to increasingly local questions. The two-part phenotype first establishes what varies. National environment-plus-space models then define the broad natural geography. Against that background, the *Bombus* analysis asks whether a biologically motivated local contrast appears among environmentally similar nearby sites. Only after those natural layers are established do we ask where local pigmented departures remain and whether those departures occupy distinctive anthropogenic contexts.

This order matters because the three explanatory layers are not interchangeable covariates in a single regression. Abiotic environment and continuous space describe the national background. Predicted bumblebee availability addresses a local biotic hypothesis at a scale where broad environmental differences are reduced. Anthropogenic variables are used last, to characterize pre-defined local departures rather than to create them. The result is therefore a sequence of bounded questions, each opening the next, rather than four coequal objectives competing for a single causal interpretation.

The route-linked photographic sample makes this layered design possible because it provides broad geographical coverage, but it remains an opportunistic sample rather than a random survey. The inferential aim is consequently bounded hypothesis generation with explicit failure conditions, not population-wide causal estimation.

### 4.2 | The broad layer: abiotic and spatial structure differs between colour components

The two-part response was biologically useful. Temperature was associated with both pigmentation state and intensity, but conditional intensity also showed a topographic association and a shorter-centred residual spatial range. Most importantly for the pollinator hypothesis, the local *Bombus*-limitation signal occurred for pigmentation state but not for intensity.

This pattern is consistent with a division between the benefit of expressing a visible signal and the processes governing how strong that signal becomes once expressed. It does not prove separate developmental pathways. Reflectance spectroscopy, anthocyanin assays and expression data would be required to connect the statistical hurdle to pigment biochemistry.

### 4.3 | The biotic layer must be tested locally

We do not estimate an independent national *Bombus* effect. The bumblebee SDMs are themselves derived from environmental geography and share broad climatic and spatial structure with the plant phenotype, so a national environment-plus-space-plus-*Bombus* regression would be difficult to interpret mechanistically. The national models therefore serve only as the natural flower-colour reference; the pollinator hypothesis begins at the local matched comparison.

For this reason we did not attempt to “solve” confounding by inserting the same environmental predictors and another spatial field into the local *Bombus* regression. Such a model would ask a different question and could remove precisely the environmentally mediated variation through which predicted bumblebee availability is defined. Instead, the local design used geographic restriction and environmental matching to compare sites with similar measured context, and the broad natural maps were retained as a separate predictive reference. This reduces the most obvious large-scale confounding without claiming that environment and pollinators have been fully separated.

### 4.4 | Low predicted bumblebee availability may relax the benefit of pigmentation

The lower-third gate produced the pattern predicted by the pigmentation-benefit relaxation hypothesis: environmentally similar nearby cells had substantially lower pigmented share when all five focal *Bombus* taxa had low predicted support than when at least one taxon had moderate predicted support. The direction was also geographically consistent across the five folds. This is more directly related to the proposed ecology than an unsigned turnover correlation because the pair direction is set by the pollinator hypothesis before flower colour is read.

The result should nevertheless be kept at its appropriate evidence tier. The 0.33 gate was selected after exploratory design development, and the conservative across-grid q-value was 0.102. The finding is therefore not a clean confirmatory rejection of a null hypothesis. Rather, it identifies a coherent pattern that merits direct field testing and that is not contradicted by the tighter low-support gates, which simply contain few local comparisons.

The proposed mechanism is biologically plausible in *C. punctata*. Bumblebees are effective pollinators of its large tubular flowers, and earlier work showed that geographical changes in pollinator fauna are associated with floral-size variation and pollen-removal performance (Nagano et al., 2014). Inoue's Izu-island work further established that bumblebee absence can alter the reproductive ecology of the species complex (Inoue & Amano, 1986; Inoue, 1988). We extend that natural-history framework to a new hypothesis: where effective bumblebees are poorly available, the attraction benefit maintaining a pigmented floral signal may relax. A production cost of pigmentation could strengthen the transition toward white flowers, but our data do not measure such costs and the hypothesis does not require them.

The absence of a positive conditional-intensity response is informative. If SDM support were a simple proxy for increasing visitation pressure and visitation selected progressively darker flowers, intensity should rise from limited to available endpoints. It did not. The current data instead fit a threshold-like interpretation better: pollinator availability may matter to whether pigmentation is worth expressing, while the visible intensity of already pigmented flowers may be governed more strongly by abiotic physiology, genetic background or other selective agents.

There are two important inferential limits. First, predicted habitat support is not actual bumblebee abundance or use of *C. punctata*. A site can be environmentally suitable for a bumblebee yet receive few visits to this plant. Second, the SDM surfaces are themselves environmentally generated. Environmental matching reduces measured differences, but unmeasured microclimate, historical distribution structure and observation processes could produce residual correspondence. We therefore use terms such as **predicted *Bombus* availability** and **bumblebee-limitation contrast**, not visitation pressure or selection pressure.

A decisive test should now target transition zones selected by this analysis. Species-resolved visits to white and pigmented flowers, first approaches, pollen removal and deposition, seed production and standardized flower reflectance would distinguish simple availability from actual attraction-mediated fitness differences. Comparative sampling on islands with known differences in bumblebee fauna would provide an especially strong natural experiment, but island flower colour should itself be measured rather than assumed from the present national analysis.

### 4.5 | Residual local departures point toward anthropogenic context, not provenance

After the broad natural geography and the local bumblebee hypothesis were considered, a small set of locally discordant pigmented cells remained worth examining. These cells were defined without human variables. That ordering is essential: population density, densely inhabited districts, roads or land use did not decide which observations counted as departures; they were inspected only after the candidate set had been fixed.

The evidence for an excess of these departures was itself reference-dependent. Candidate fraction lay near the upper tail of the held-out cross-fitted reference but was compatible with the full-data joint posterior-predictive reference. The 18 cells should therefore not be read as proof that an additional process is required. They are better treated as locations where the natural model gives us a concrete reason to look more closely.

That final look produced a suggestive but incomplete anthropogenic signal. Population and DID contrasts pointed in the expected direction, yet neither survived the relevant familywise correction. Human-modified landscapes can also correlate with hiking access and observation opportunity, so the remaining association cannot establish horticultural introduction, escape or introgression. Its value is narrower but still useful: it characterizes where some local departures occur and converts an otherwise vague horticultural story into a finite set of testable field targets. Demonstrating provenance would require planting histories, vouchers and population-genetic comparison with surrounding wild populations and horticultural material.

### 4.6 | Limitations

The principal limitations are explicit. First, YAMAP records are opportunistic and trail-biased. Second, visible image colour is not calibrated reflectance or pigment chemistry. Third, the mixture boundary is operational and includes classification uncertainty. Fourth, national environmental coefficients are conditional on the INLA mesh, priors and spatial field. Fifth, the stage-03 lower-third gate was adopted after exploratory design development, so the full threshold grid must accompany the result. Sixth, the active bumblebee surfaces are frozen predictions whose historical model-selection uncertainty is not propagated. Seventh, within the current common-support design, truly bumblebee-absent or structurally out-of-range regions may be underrepresented; the present test is therefore about **low predicted availability within analyzable support**, not literal absence. Eighth, local matching cannot eliminate unmeasured environmental confounding. Ninth, no visitation, pollen transfer, fitness or pigment-production cost is measured. Tenth, the human-context extension cannot establish provenance.

These limits define a productive next step rather than making the analysis uninterpretable. The broad photographic dataset identifies national colour structure and local contrasts that can be targeted with much smaller, mechanistically explicit field studies.

## 5 | Conclusions

The geography of *C. punctata* flower colour is best understood layer by layer. First, the phenotype itself contains two separable components: whether visible pigmentation is expressed and how intense that pigmentation becomes once expressed. Second, at the national scale, abiotic environment and continuous spatial structure provide the dominant background for both components. Third, when the analysis zooms to environmentally similar nearby sites, predicted bumblebee limitation shows a large and directionally coherent association with pigmentation state, but not with conditional intensity. This pattern is consistent with the possibility that bumblebee availability affects the benefit of maintaining a pigmented floral signal rather than simply selecting for progressively darker flowers.

The story does not end there. A small set of local pigmented departures remained after comparison with the natural predictive reference. Their apparent excess was not robust across predictive references, and their associations with population and DID context were suggestive rather than familywise significant. These observations therefore do not demonstrate anthropogenic origin. Instead, they define the final layer of the framework: natural structure first, a local biotic hypothesis second, and anthropogenic characterization only for the residual departures that remain worth investigating.

Taken together, the study does not support a single-driver explanation of flower-colour geography. It provides a staged map of where different questions become informative. Broad abiotic and spatial structure sets the background; local pollinator context may modify whether pigmentation is maintained; and residual departures identify where direct field, provenance and genetic work can most efficiently test human influence. Predicted *Bombus* support is still only an availability proxy, so species-resolved visitation, pollen transfer, reproductive fitness and standardized flower reflectance are required for a causal test of the proposed attraction mechanism.

## Data Accessibility Statement

The active 1,909 analysis code, derived flower-colour tables, immutable input descriptor, environmental source registry, frozen *Bombus* prediction surfaces, seeds, stage outputs, validation files and claim locks are versioned in the project repository. Original YAMAP photographs are third-party content and are not redistributed. The archived 1,923 analysis is retained as provenance under `legacy/published-1923/` and is not the active manuscript baseline.

## References

Araújo, M. B., & Rozenfeld, A. (2014). The geographic scaling of biotic interactions. *Ecography*, 37, 406–415. https://doi.org/10.1111/j.1600-0587.2013.00643.x

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N. (2022). Global climate-related predictors at kilometer resolution for the past and future. *Earth System Science Data*, 14, 5573–5603. https://doi.org/10.5194/essd-14-5573-2022

Dick, C. A., Buenrostro, J., Butler, T., Carlson, M. L., Kliebenstein, D. J., & Whittall, J. B. (2011). Arctic mustard flower color polymorphism controlled by petal-specific downregulation at the threshold of the anthocyanin biosynthetic pathway. *PLoS ONE*, 6, e18230. https://doi.org/10.1371/journal.pone.0018230

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology*, 37, 4302–4358. https://doi.org/10.1002/joc.5086

Guillera-Arroita, G. (2015). Is my species distribution model fit for purpose? Matching data and models to applications. *Global Ecology and Biogeography*, 24, 276–292. https://doi.org/10.1111/geb.12268

Hanks, E. M., Schliep, E. M., Hooten, M. B., & Hoeting, J. A. (2015). Restricted spatial regression in practice: geostatistical models, confounding, and robustness under model misspecification. *Environmetrics*, 26, 243–254. https://doi.org/10.1002/env.2331

Inoue, K. (1988). Pattern of breeding-system change in the Izu Islands in *Campanula punctata*: Bumblebee-absence hypothesis. *Plant Species Biology*, 3, 125–128. https://doi.org/10.1111/j.1442-1984.1988.tb00178.x

Inoue, K., & Amano, M. (1986). Evolution of *Campanula punctata* in the Izu Islands: Changes of pollinators and evolution of breeding systems. *Plant Species Biology*, 1, 89–97. https://doi.org/10.1111/j.1442-1984.1986.tb00018.x

Karger, D. N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

Kass, J. M., et al. (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species' niches and distributions. *Methods in Ecology and Evolution*, 12, 1602–1608. https://doi.org/10.1111/2041-210X.13628

Koski, M. H., & Ashman, T.-L. (2015). Floral pigmentation patterns provide an example of Gloger's rule in plants. *Nature Plants*, 1, 14007. https://doi.org/10.1038/nplants.2014.7

Laitly, A., Callaghan, C. T., Delhey, K., & Cornwell, W. K. (2021). Is color data from citizen science photographs reliable for biodiversity research? *Ecology and Evolution*, 11, 4071–4083. https://doi.org/10.1002/ece3.7307

Lindgren, F., Rue, H., & Lindström, J. (2011). An explicit link between Gaussian fields and Gaussian Markov random fields: The stochastic partial differential equation approach. *Journal of the Royal Statistical Society: Series B*, 73, 423–498. https://doi.org/10.1111/j.1467-9868.2011.00777.x

Luong, Y., Gasca-Herrera, A., Misiewicz, T. M., & Carter, B. E. (2023). A pipeline for the rapid collection of color data from photographs. *Applications in Plant Sciences*, 11, e11546. https://doi.org/10.1002/aps3.11546

McKenzie, P. F., Church, S. H., & Hopkins, R. (2026). High-throughput iNaturalist image analysis reveals flower color divergence in *Monarda fistulosa*. *The American Naturalist*, 208, 101–109. https://doi.org/10.1086/739413

Nagano, Y., Abe, K., Kitazawa, T., Hattori, M., Hirao, A. S., & Itino, T. (2014). Changes in pollinator fauna affect altitudinal variation of floral size in a bumblebee-pollinated herb. *Ecology and Evolution*, 4, 3395–3407. https://doi.org/10.1002/ece3.1191

Paciorek, C. J. (2010). The importance of scale for spatial-confounding bias and precision of spatial regression estimators. *Statistical Science*, 25, 107–125. https://doi.org/10.1214/10-STS326

Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., & Rossiter, D. (2021). SoilGrids 2.0: Producing soil information for the globe with quantified spatial uncertainty. *SOIL*, 7, 217–240. https://doi.org/10.5194/soil-7-217-2021

Rausher, M. D. (2008). Evolutionary transitions in floral color. *International Journal of Plant Sciences*, 169, 7–21. https://doi.org/10.1086/523358

Renner, I. W., & Warton, D. I. (2013). Equivalence of MAXENT and Poisson point process models for species distribution modeling. *Biometrics*, 69, 274–281. https://doi.org/10.1111/j.1540-0420.2012.01824.x

Roberts, D. R., et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography*, 40, 913–929. https://doi.org/10.1111/ecog.02881

Simpson, D., Rue, H., Riebler, A., Martins, T. G., & Sørbye, S. H. (2017). Penalising model component complexity: A principled, practical approach to constructing priors. *Statistical Science*, 32, 1–28. https://doi.org/10.1214/16-STS576

Soberón, J. (2007). Grinnellian and Eltonian niches and geographic distributions of species. *Ecology Letters*, 10, 1115–1123. https://doi.org/10.1111/j.1461-0248.2007.01107.x

Tasaki, K., et al. (2022). Identification of candidate genes responsible for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*, 13, 906879. https://doi.org/10.3389/fpls.2022.906879

Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower color evolution and the evidence of pollinator-mediated selection. *Frontiers in Plant Science*, 12, 617851. https://doi.org/10.3389/fpls.2021.617851

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation. *Methods in Ecology and Evolution*, 10, 225–232. https://doi.org/10.1111/2041-210X.13107

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016). How to colour a flower: On the optical principles of flower coloration. *Proceedings of the Royal Society B*, 283, 20160429. https://doi.org/10.1098/rspb.2016.0429

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait variation in plants: A renewed focus on its role in ecological processes. *Annals of Botany*, 127, 397–415. https://doi.org/10.1093/aob/mcab011

# Table 1. Locked active results and claim ceilings

| Analysis | Active estimate | Reference / uncertainty | Interpretation ceiling |
|---|---:|---|---|
| Pigmentation state | 955 white-like; 954 pigmented | a* boundary=5.0202; 125 ambiguous | optical regime, not chemical pigment |
| National presence prediction | AUC=0.863 | five geographical folds | withheld-geography discrimination |
| Conditional intensity prediction | RMSE=0.912 | pigmented-only response | optical intensity among pigmented flowers |
| Lower-third *Bombus*-limitation gate | +0.223 pigmented share | 22 pairs; p=0.017; within-gate q=0.034; across-grid q=0.102 | exploratory directional support; predicted availability, not selection |
| Gate conditional intensity | approximately -0.615 | 6 pairs; p=0.904 | no support for darker flowers with availability |
| Local-isolate count | 18 | crossfit p=0.136; joint PPC p=0.222 | follow-up candidates |
| Local-isolate fraction | 0.0501 | crossfit p=0.049; joint PPC p=0.123 | upper-tail pattern not robust across references |
| 5-km population contrast | 0.055 | raw p=0.031; corrected p=0.102 | suggestive human context only |
| Population–DID alignment | 0.058 | raw p=0.017; corrected p=0.060 | suggestive human context only |

# Figure legends

**Figure 1. Observation-level extracted colours and the two-part flower-colour response.** National distribution of the active 1,909 observations, response-blind mixture classification, pigmentation state and conditional visible intensity. Displayed colours are uncalibrated human-visible sRGB and CIELAB values, not anthocyanin concentration or bumblebee receptor contrast.

**Figure 2. National environmental and continuous spatial structure of the two flower-colour components.** Posterior environmental coefficients and SPDE ranges for pigmentation presence and pigmented-only intensity, together with geographically cross-fitted predictions. The spatial fields quantify coherent residual geography rather than a uniquely identified mechanism.

**Figure 3. Local bumblebee-limitation hypothesis.** (a) Pigmented share in the 22 one-to-one lower-third limitation pairs, oriented from *Bombus*-limited to *Bombus*-available before flower colour was read. (b) Observed available-minus-limited pigmentation contrasts across the retained low-support gate grid; open points show natural-map means. (c) The lower-third observed contrast relative to 1,000 cross-fitted natural maps. The 0.33 gate was adopted after exploratory design development; the full threshold grid and across-grid multiplicity are retained. Predicted *Bombus* support is an availability proxy, not abundance or visitation.

**Figure 4. Residual local departures and their anthropogenic context.** Locations of pre-fixed pigmented isolates, predictive-reference checks of candidate count and fraction, and post-selection population/DID context. Candidate locations are follow-up priorities and do not establish horticultural origin.
