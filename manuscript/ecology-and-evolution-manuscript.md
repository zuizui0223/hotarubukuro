# Layered flower-colour geography in *Campanula punctata*: abiotic structure, local bumblebee limitation and anthropogenic context

## Abstract

Flower-colour geography can emerge from several layers of process: broad abiotic gradients, local biotic interactions and, in some places, human-modified landscapes. These layers are difficult to separate if flower colour is treated as one continuous response or if every predictor is placed in a single national model. We analysed 1,909 author-reviewed YAMAP photographs of *Campanula punctata* in a staged framework. A response-blind Gaussian mixture separated 955 white-like and 954 pigmented flowers at CIELAB a*=5.0202; 125 low-confidence classifications were flagged but retained. We therefore analysed two responses: pigmentation presence and visible intensity conditional on pigmentation.

First, national INLA-SPDE models established the broad natural geography. Temperature was negatively associated with both responses, conditional intensity also varied with topography, and residual spatial ranges were approximately 131 km for pigmentation presence and 61 km for conditional intensity. Five geographically blocked folds yielded AUC=0.863 for pigmentation presence and RMSE=0.912 for conditional intensity.

We then zoomed to a local biotic hypothesis. Rather than treating bumblebee SDMs as national causal predictors, we compared one-to-one site pairs within 25 km that were already matched on environmental context. In 22 lower-third pairs, pigmented share was 0.223 higher where at least one focal *Bombus* species had moderate predicted availability than where all five species had low predicted availability (upper-tail predictive p=0.017). Because the lower-third gate was adopted after exploratory design development, the full threshold grid was retained; the across-grid BH q-value was 0.102. The result is therefore directionally coherent exploratory support, not a confirmatory test. Conditional intensity did not increase across the same gate (difference approximately -0.615; p=0.904).

Finally, we asked what remained after the natural reference. Eighteen local pigmented departures were identified, but their excess was not robust across held-out and joint posterior-predictive references. Population and densely inhabited district context were suggestive but did not survive familywise correction. The resulting picture is layered rather than additive: abiotic environment and spatial structure define the national background, local bumblebee limitation may modify whether pigmentation is maintained, and residual departures can be screened for anthropogenic context without being labelled anthropogenic in origin. Predicted *Bombus* support is an availability proxy rather than visitation or selection pressure; direct field measurements are required to test the proposed attraction mechanism.

**Keywords:** anthropogenic context; bumblebee limitation; digital phenotyping; flower colour; INLA-SPDE; pollinator availability; *Campanula punctata*

## 1 | Introduction

Intraspecific trait variation can shape population responses, eco-evolutionary dynamics and community assembly, yet its geography remains poorly measured for many plant traits (Westerband et al., 2021). Flower colour is especially interesting because it can influence animal attraction while also reflecting physiology, development and history (Rausher, 2008; Koski & Ashman, 2015; Trunschke et al., 2021). The challenge is that broad-scale studies often compress this variation into a few colour categories, potentially mixing biologically different transitions.

Community photographs offer a way to recover geographical trait variation at much larger scales (Laitly et al., 2021; Luong et al., 2023; McKenzie et al., 2026). YAMAP is useful in this respect because photographs are linked to GPS-tracked hiking activities. But the platform is not a biodiversity survey: photographs are affected by access, user behaviour, camera properties and flower conspicuousness. We therefore treat the dataset as an author-reviewed, route-linked photographic sample, not as an areally random sample of Japanese populations.

A first biological distinction concerns the phenotype itself. A threshold-like change in anthocyanin-pathway activity can separate visibly white and pigmented flowers, whereas variation among already pigmented flowers can reflect pigment amount, cell structure and optical context (Dick et al., 2011; Tasaki et al., 2022; van der Kooi et al., 2016). A single white-to-pink numerical axis risks treating small optical variation among white flowers as if it were pigment intensity. We instead analyse two responses: **pigmentation presence** for all flowers and **conditional visible intensity** only among flowers assigned to the pigmented regime. CIELAB a* is an uncalibrated human-visible optical measure; it is not anthocyanin concentration, ultraviolet reflectance or bumblebee receptor contrast.

A second distinction concerns scale. Climate, topography, population history and dispersal can produce broad geographical structure in both plants and pollinators. Pollinator SDMs are themselves built from environmental geography. A national regression containing environment, space and predicted pollinators would therefore be difficult to interpret as an independent pollinator effect (Soberón, 2007; Paciorek, 2010; Hanks et al., 2015). We instead use the national environment-plus-space model to establish a natural reference, then test the pollinator hypothesis locally among nearby sites with similar measured environments.

*Campanula punctata* provides a strong biological reason to ask that local question. Its large tubular flowers are effectively pollinated by bumblebees, and geographical shifts in pollinator fauna are associated with floral-size variation and pollen-removal performance (Nagano et al., 2014). In the Izu Islands, bumblebee loss has also been linked to shifts in pollinator assemblages and breeding systems, forming the “bumblebee-absence hypothesis” for this species complex (Inoue & Amano, 1986; Inoue, 1988). These studies do not show that bumblebee absence caused white flower colour. They do, however, make it reasonable to ask whether a conspicuous colour signal becomes less beneficial where effective bumblebees are poorly available.

This leads to our **pigmentation-benefit relaxation hypothesis**. If pigmentation improves attractiveness or detectability to important bumblebee pollinators, then maintaining a pigmented state can carry a reproductive benefit where bumblebees are available. Where all focal bumblebee taxa are poorly available, that benefit should weaken and selection maintaining pigmentation may relax, making white flowers relatively more common. Pigment-production cost could reinforce such a transition, but cost is not measured here and is not required for the primary prediction. A stronger dose-response prediction—greater bumblebee availability producing darker flowers among already pigmented individuals—is secondary because occurrence-based SDMs do not measure visitation rate.

Even a broad natural baseline and a focal biotic hypothesis need not explain every local observation. Those remaining departures can be informative. A pigmented cell embedded in an otherwise white, environmentally similar neighbourhood marks a local mismatch with the natural expectation. If such departures are defined without using human variables, their later association with population, roads or land use can be examined without circularly defining the anomaly by the same predictors used to characterize it. We therefore treat human influence as a third, bounded layer: not as a competing national driver, but as context for local departures that remain after the natural geography has been established.

The analysis narrows the question step by step. We first define the two-part flower-colour phenotype. We then establish the national abiotic and spatial background and generate geographically cross-fitted natural predictive maps. Next, we zoom to environmentally similar nearby sites and ask whether low predicted *Bombus* availability corresponds to reduced pigmentation. Finally, we identify local pigmented departures from the natural reference and ask whether those pre-defined departures occupy distinctive anthropogenic contexts. The hierarchy is deliberate: abiotic environment and space set the broad stage; pollinator availability tests a local biotic mechanism; and human context is examined only after the remaining local departures have been identified independently of human variables.

## 2 | Materials and Methods

### 2.1 | Sampling frame and author review

The source dataset comprised eligible *C. punctata* records identified by the author in YAMAP activity records during the flowering seasons of 2023–2025. Before any analytical table was constructed, the author inspected the focal subject, confirmed the flower and petal region used for colour measurement, removed repeated photographs of the same individual and removed taxonomic errors. Records were not selected using flower colour, fitted residuals, *Bombus* predictions or human-landscape context.

YAMAP observations are route- and access-biased. Users can hide locations, trails differ in activity and flowers differ in conspicuousness. The analytical population is therefore the retained route-linked photographic sample rather than a random plot survey. The active complete-support population contains 1,909 observations.

### 2.2 | Visible-colour extraction and quality control

The deterministic image pipeline measured the author-confirmed petal region from display-referred sRGB pixels. The channel-wise median RGB value was used as the primary colour statistic and converted to CIELAB under D65 and a 2° standard-observer convention. CIELAB a* was used as the red–green optical phenotype.

Image-specific white balance was not possible because neutral references and standardized camera metadata were unavailable. Mask coverage, exposure, shadows, multimodality and disagreement among alternative pixel summaries generated warning flags, but these flags did not automatically remove dark or otherwise extreme observations. The response is therefore an aggregate human-visible image phenotype, not calibrated reflectance or pigment chemistry.

### 2.3 | Response-blind mixture classification and the two-part response

We estimated the pigmentation boundary from CIELAB a* without using geography, date, environment, *Bombus*, human context or fitted-model information. The implementation used `mclust` 6.1.2 and fitted univariate Gaussian mixtures with one to eight components under equal-variance (`E`) and variable-variance (`V`) parameterizations. `Mclust` selected the model by BIC. The active 1,909 analysis selected a four-component variable-variance (`V`) mixture.

The statistical mixture contained more than two optical components, so we ordered components by fitted mean and used the largest adjacent mean gap to collapse them into two measurement regimes. Lower-mean components defined the white-like regime; upper-mean components defined the pigmented regime. Posterior probabilities of the upper components were summed to obtain P(pigmented) for each observation.

Flowers with P(pigmented) >=0.5 were assigned to the pigmented regime. Classification confidence was based on the same posterior probability: P(pigmented) >=0.8 or <=0.2 was considered high confidence, while intermediate values were flagged as ambiguous and retained. The operational boundary was a*=5.020161, yielding 955 white-like observations, 954 pigmented observations and 125 ambiguous observations.

We then defined two responses:

1. **pigmentation presence**, a binary response for all 1,909 observations; and
2. **conditional visible intensity**, standardized a* among the 954 observations assigned to the pigmented regime.

Variation in a* among white-like flowers was not interpreted as pigment intensity.

### 2.4 | Environmental predictors and multiscale context

Climate variables came from CHELSA v2.1 climatologies, soil variables from ISRIC SoilGrids 2.0 and elevation from WorldClim 2.1; slope and terrain heterogeneity were derived from elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Continuous layers were aligned to a common grid and extracted to observations. Response-blind principal components summarized temperature, precipitation, soil and topography for the observation-level models.

The predictive natural reference and local matching used a second multiscale summary. Elevation, temperature, precipitation and radiation were represented by 50-km neighbourhood means and cell-minus-neighbourhood deviations, then reduced to two broad-scale and two within-neighbourhood principal-component axes. These four axes defined the environmental context used in local matched comparisons.

### 2.5 | National environment-plus-space models

We fitted separate observation-level INLA-SPDE models for the two response parts. Pigmentation presence used a Bernoulli likelihood with logit link; conditional intensity used a Gaussian likelihood. Standardized environmental axes entered as fixed effects, and a Matérn SPDE field represented continuous residual geography (Lindgren et al., 2011).

The mesh used maximum inner and outer edge lengths of 20 and 100 km with a 5-km cutoff. The 20-km inner resolution was chosen to resolve subregional structure while remaining finer than the approximately 100-km-scale residual field expected at the national level. The 100-km outer edge limited unnecessary mesh density away from observations, and the 5-km cutoff avoided extremely small triangles around tightly clustered records.

Penalised-complexity priors specified P(spatial range <100 km)=0.05 and P(spatial SD >1)=0.05 (Simpson et al., 2017). These priors regularize the field away from very short-range or very high-amplitude structure; they do not assert that the true range is 100 km. Fixed-effect estimates are conditional on this mesh and prior specification.

We also built a cell-level predictive natural baseline. Pigmented counts were modelled as binomial trials, and conditional intensity was modelled as Gaussian among cells with pigmented observations. Five response-blind geographical folds based on 100-km blocks evaluated transfer to withheld regions (Roberts et al., 2017; Valavi et al., 2019). Environmental standardization was performed within each training fold. For every held-out cell we generated 1,000 predictive replicates while preserving observation counts. These cross-fitted maps provide the common natural reference for later local questions.

### 2.6 | Predicted *Bombus* availability

We considered five focal Japanese bumblebee taxa: *Bombus ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus* and *B. honshuensis*. The active 1,909 pipeline restores checksum-locked prediction surfaces generated by the earlier ENMeval/maxnet source-build workflow. Historical ENMeval candidate and tuning objects required to replay the complete model-selection path are not all retained. The downstream analysis is therefore reproducible **conditional on the frozen prediction surfaces**, not from GBIF occurrences through model selection to final inference.

Presence-background SDM outputs do not measure abundance, visit counts or pollination service (Renner & Warton, 2013; Guillera-Arroita, 2015). We therefore do not sum raw suitability values and call the result “Bombus pressure.” Instead, each species surface was converted to a within-species support rank. This preserves relative support within a species without assuming that raw SDM values are quantitatively comparable among species.

For each flower cell, **best focal-Bombus support** was defined as the maximum of the five within-species ranks. A low maximum means that all five focal taxa have low predicted support. A high maximum means that at least one focal taxon has moderate or high predicted support. We interpret this gate as potential bumblebee availability, not visitation intensity.

### 2.7 | Local *Bombus*-limitation gate

The local analysis tested the directional pigmentation-benefit relaxation hypothesis. It did **not** fit a second environment-plus-space model. Instead, comparison pairs were defined before flower colour was read.

We first identified candidate neighbours within 25 km. Pair endpoints had to occur in the same held-out flower-model fold and within the five-species common-support region. We then retained only pairs with environmental RMS distance <=0.75 across the four broad/within-50-km environmental axes. One-to-one pairs were selected greedily using environmental similarity and geographical distance only, so no cell was repeatedly reused.

Pair direction was determined from the SDM ranks alone. A **Bombus-limited** endpoint had a maximum within-species rank <=0.33, meaning that all five focal taxa were in their lower third of predicted support. A **Bombus-available** endpoint required at least one focal taxon with rank >=0.50. Flower colour did not define or orient pairs.

The 0.33 lower-third gate was adopted after exploratory design development because it gave a clear biological interpretation and a usable matched sample. We therefore retain the full development grid of low thresholds 0.10, 0.20, 0.25 and 0.33 in every run. We report both correction across the two responses within the lower-third gate and the more conservative BH correction across all gate × response tests. This stage is a mechanistically motivated local sensitivity, not a preregistered confirmatory test.

The primary statistic was the mean directed difference in pigmented share:

`pigmented share_available - pigmented share_limited`.

The secondary statistic used the same pair orientation but compared conditional visible intensity only when both endpoints contained pigmented observations.

We then asked whether the observed contrasts were larger than expected from the broad natural geography. The fixed pairs and their *Bombus*-defined directions were replayed on each of the 1,000 cross-fitted environment-plus-SPDE flower maps, producing a predictive reference distribution. Environment and spatial structure were therefore not added again as local regression covariates: they entered through pre-outcome environmental matching and through the natural-map reference.

This design reduces broad environmental and spatial confounding but cannot remove it completely. The *Bombus* surfaces are themselves predictions from environmental geography, and unmeasured environment, distribution history and sampling structure can remain. The result is therefore a directional correspondence with predicted bumblebee availability, not an estimate of bumblebee abundance, visitation, attraction strength or pollinator-mediated selection.

The previous unsigned local community-turnover analysis is retained only as a sensitivity and is not read by the final result or claim registries.

### 2.8 | Local pigmented departures and post-selection human context

A separate analysis asked where pigmented cells remained locally discordant with the natural background. Candidate selection used flower state, local distance, environmental similarity and sampling support, but not population, land use, roads, DID proximity, flowering date or intensity. The same event extractor was replayed on natural predictive maps.

The active analysis identified 18 pre-fixed candidate cells. We evaluated whether such departures were unusually common using a higher-precision held-out cross-fitted reference and a separate full-data joint spatial posterior-predictive sensitivity. Candidate fraction lay near the upper tail of the cross-fitted reference but was compatible with the joint posterior-predictive reference. We therefore treat the 18 cells as follow-up sites rather than evidence that local departures are more frequent than expected naturally.

Human context was examined only after candidate identities were fixed. We characterized candidates using WorldPop population, MLIT land use, road context and DID proximity, with familywise maxT correction within feature families. These analyses describe where the candidates occur; they do not identify planting, escape, introgression or horticultural origin.

Flowering date was retained as a model-free Supplementary description after candidate selection and did not enter any main claim.

### 2.9 | Reproducibility and claim control

The active numerical population, stage graph, package environment, frozen inputs, result tables and validators are versioned in the repository. Final results are generated from fresh pipeline outputs. Stage-03 metadata records the lower-third gate history, the full threshold grid, environmental matching, the absence of a second local environment/SPDE model and the fixed-SDM uncertainty ceiling. The previous 1,923-observation analysis is archived under `legacy/published-1923/` and is not treated as the active paper.

## 3 | Results

### 3.1 | Two-part optical phenotype

The 1,909 observations separated almost evenly into the two optical regimes: 955 white-like and 954 pigmented. The response-blind four-component variable-variance mixture placed the boundary at a*=5.0202. One hundred twenty-five observations had intermediate regime-membership probability and were retained with an ambiguity flag (Figure 1). Conditional intensity was calculated only for the 954 pigmented observations.

### 3.2 | National environmental clines and residual geography

The broad geographical signal was clearest for temperature. Pigmentation presence declined along the temperature axis (posterior mean -0.579; 95% CrI -1.077 to -0.079), and conditional intensity showed the same direction (mean -0.322; CrI -0.487 to -0.160). Conditional intensity also declined along the pre-oriented topographic axis (mean -0.144; CrI -0.238 to -0.051). Other coefficients are conditional on the spatial field; notably, the earlier soil association was not retained as a robust 95% interval result in the active 1,909 analysis.

Substantial residual geography remained after the measured environment was included. The posterior SPDE range was 130.5 km (95% CrI 87.7–191.5) for pigmentation presence and 60.8 km (31.2–113.9) for conditional intensity. Because the response models differ in likelihood and spatial support, this range difference is descriptive rather than a formal test of different biological scales.

The five-fold cross-fitted national presence model had AUC=0.863, while conditional intensity had RMSE=0.912. These metrics describe prediction to withheld parts of the sampled geography rather than causal adequacy (Figure 2).

### 3.3 | After environmental matching, local *Bombus* limitation tracked pigmentation state

The local comparison produced a clear directional pattern. Under the lower-third gate, 22 environmentally matched one-to-one pairs contrasted a cell where all five focal species were in their lower third of predicted support with a nearby cell where at least one species was at or above median support. Pigmented share was **0.223 higher** at the *Bombus*-available endpoint. The mean natural-map contrast was approximately 0.001 and the upper-tail predictive p-value was 0.017 (Figure 3).

The result remains exploratory. Within the lower-third gate, BH correction across pigmentation state and conditional intensity gave q=0.034. But the lower-third gate was adopted after exploratory design development; retaining the full gate × response grid gave q=0.102 for the pigmentation contrast. We therefore treat the effect as directionally coherent exploratory support rather than a confirmatory significance result.

The direction was geographically consistent. Lower-third pairs occurred in all five folds; mean contrasts were positive in four folds and zero in the fifth, with none negative. Tighter gates contained little information: the 0.10 gate produced no pairs, the 0.20 gate two pairs and the 0.25 gate six pairs. Their observed pigmentation differences were positive but imprecise.

Conditional intensity did not show the predicted dose-like increase. Only six lower-third pairs had pigmented observations at both endpoints, and the available-minus-limited intensity contrast was approximately -0.615 (upper-tail p=0.904). The local signal therefore concerns whether flowers are pigmented, not how dark they become once pigmented.

That biotic pattern did not exhaust the geography left by the natural reference. We next asked where pigmented cells remained locally discordant with otherwise similar white neighbourhoods.

### 3.4 | The natural reference left 18 local pigmented departures

The analysis identified 18 pre-fixed local pigmented departures, corresponding to a candidate fraction of 0.0501. Their apparent excess depended on the predictive reference. Under the higher-precision held-out cross-fitted reference, the candidate-count upper-tail probability was 0.136 and the candidate-fraction probability was 0.049 (Monte Carlo 95% interval 0.045–0.053); the same-fold sensitivity gave fraction p=0.048. Under the full-data joint spatial posterior-predictive check, however, count p=0.222 and fraction p=0.123 (Monte Carlo 95% interval 0.121–0.125).

Because the excess did not persist across references, the 18 cells are not evidence that an additional process is required. They are instead a pre-defined set of places where the natural model gives a reason to look more closely. That leads to the final question: what kind of human-modified contexts do these departures occupy?

### 3.5 | Anthropogenic context was suggestive, not decisive

Some human-context contrasts pointed in the expected direction. The focal-minus-neighbour 5-km population contrast was positive (estimate 0.055; raw p=0.031), but the corrected p-value was 0.102. Population–DID alignment was also positive (estimate 0.058; raw p=0.017), with corrected p=0.060. Half of the candidates occurred in the DID-proximate high-population context, but that contrast also failed familywise correction (corrected p=0.200).

Thus the anthropogenic signal is suggestive rather than decisive. The results describe the context of the selected departures but do not establish horticultural provenance.

## 4 | Discussion

### 4.1 | A layered explanation of flower-colour geography

The analysis is easiest to read from broad structure to increasingly local questions. First, the two-part phenotype establishes what varies. Second, national environment-plus-space models define the broad natural geography. Third, the *Bombus* analysis asks whether a biologically motivated local contrast appears once nearby sites are matched on environment. Finally, the remaining local departures are examined for anthropogenic context.

These layers are not interchangeable covariates in one regression. Abiotic environment and continuous space describe the national background. Predicted bumblebee availability addresses a local biotic hypothesis at a scale where broad environmental differences are reduced. Anthropogenic variables are used last, to characterize pre-defined departures rather than to create them. Each layer therefore opens the next question instead of competing for a single causal interpretation.

The route-linked photographic sample makes this progression possible because it provides broad geographical coverage, but it remains opportunistic rather than random. The aim is bounded hypothesis generation with explicit failure conditions, not population-wide causal estimation.

### 4.2 | The broad layer: pigmentation state and intensity have different geography

The two-part response was biologically useful. Temperature was associated with both pigmentation state and conditional intensity, while topography was additionally associated with intensity. Residual spatial structure also appeared more extensive for pigmentation presence than for intensity, although the ranges were not formally compared.

The distinction becomes more interesting when the pollinator result is added: *Bombus* limitation tracked pigmentation state but not conditional intensity. This is consistent with a division between the benefit of expressing a visible signal and the processes governing how strong that signal becomes once it is expressed. It does not prove separate developmental pathways. Reflectance spectroscopy, anthocyanin assays and expression data would be needed to connect the statistical two-part response to pigment biochemistry.

### 4.3 | The biotic layer must be tested locally

We do not estimate an independent national *Bombus* effect. Bumblebee SDMs are themselves derived from environmental geography and share broad climatic and spatial structure with the plant phenotype. A national environment-plus-space-plus-*Bombus* regression would therefore be difficult to interpret mechanistically. The national models instead provide the natural flower-colour reference; the pollinator hypothesis begins with the local matched comparison.

For the same reason, we did not add the same environmental predictors and another spatial field to a second local *Bombus* regression. That would answer a different question and could remove environmentally mediated variation through which predicted bumblebee availability is defined. We instead restricted comparisons geographically, matched sites on measured environment and retained the broad natural maps as a separate predictive reference. This reduces obvious large-scale confounding without claiming that environment and pollinators have been fully separated.

### 4.4 | Low predicted bumblebee availability may relax the benefit of pigmentation

The lower-third gate produced the pattern predicted by the pigmentation-benefit relaxation hypothesis. Nearby, environmentally similar cells had less pigmentation when all five focal *Bombus* taxa had low predicted support than when at least one taxon had moderate support. The direction was also consistent across geographical folds. Because pair direction was defined by the pollinator hypothesis before flower colour was read, this result is more directly connected to the proposed mechanism than an unsigned turnover correlation.

The evidence tier still matters. The 0.33 gate was selected after exploratory design development, and the conservative across-grid q-value was 0.102. The result is therefore not a confirmatory rejection of a null hypothesis. It is a coherent directional pattern that now deserves direct field testing.

The mechanism is biologically plausible in *C. punctata*. Bumblebees are effective pollinators of its large tubular flowers, and previous work linked geographical changes in pollinator fauna to floral-size variation and pollen-removal performance (Nagano et al., 2014). Inoue's Izu-island studies further showed that bumblebee absence can alter the reproductive ecology of this species complex (Inoue & Amano, 1986; Inoue, 1988). We extend that natural-history framework to a new possibility: where effective bumblebees are poorly available, the attraction benefit that helps maintain a pigmented floral signal may weaken. Pigment-production cost could strengthen a shift toward white flowers, but we did not measure such costs and do not require them for the hypothesis.

The negative result for conditional intensity sharpens this interpretation. If SDM support simply indexed increasing visitation pressure and stronger visitation selected progressively darker flowers, intensity should increase from limited to available endpoints. It did not. The present data fit a threshold-like interpretation better: pollinator availability may matter to whether pigmentation is worth expressing, while the intensity of already pigmented flowers may depend more on abiotic physiology, genetic background or other selective agents.

Two limits are crucial. First, predicted habitat support is not actual bumblebee abundance or use of *C. punctata*. A suitable site can still receive few visits to this plant. Second, the SDM surfaces are themselves environmentally generated. Matching reduces measured environmental differences, but unmeasured microclimate, distribution history and observation processes can still generate residual correspondence. We therefore use **predicted *Bombus* availability** and **bumblebee-limitation contrast**, not visitation pressure or selection pressure.

A decisive test now requires field data. Species-resolved visits to white and pigmented flowers, first approaches, pollen removal and deposition, seed production and standardized flower reflectance would separate simple availability from attraction-mediated fitness differences. Islands with known contrasts in bumblebee fauna offer an especially strong natural experiment, but island flower colour should be measured rather than assumed from the present analysis.

### 4.5 | Residual departures point toward anthropogenic context, not provenance

After the broad natural geography and the local bumblebee hypothesis were considered, 18 locally discordant pigmented cells remained worth examining. Crucially, these cells were defined without human variables. Population density, DID, roads and land use therefore did not decide which observations counted as departures; they were inspected only after the candidate set was fixed.

The evidence that these departures were unusually common was reference-dependent. Candidate fraction lay near the upper tail of the held-out cross-fitted reference but was compatible with the full-data joint posterior-predictive reference. The 18 cells should therefore not be read as proof that an additional process is required. Their value is more practical: they identify concrete locations where the natural model says, “look closer.”

That closer look produced a suggestive but incomplete anthropogenic signal. Population and DID contrasts pointed in the expected direction, yet neither survived familywise correction. Human-modified landscapes can also correlate with hiking access and observation opportunity. The pattern therefore cannot establish horticultural introduction, escape or introgression.

Still, the human-context layer is useful. It converts a vague horticultural story into a finite set of testable field targets. Demonstrating provenance would require planting histories, vouchers and population-genetic comparison with surrounding wild populations and horticultural material.

### 4.6 | Limitations

The first set of limitations concerns the data and phenotype. YAMAP records are opportunistic and trail-biased, and visible image colour is not calibrated reflectance or pigment chemistry. The mixture boundary is operational, and 125 observations retain intermediate classification confidence.

The second set concerns inference from the models. National coefficients are conditional on the INLA mesh, priors and spatial field. The lower-third *Bombus* gate was adopted after exploratory design development, so the full threshold grid must accompany the result. The bumblebee surfaces are frozen predictions whose historical model-selection uncertainty is not propagated. Truly bumblebee-absent or structurally out-of-range regions may also be underrepresented because the current test requires common support. The result therefore concerns **low predicted availability within analyzable support**, not literal bumblebee absence. Local matching cannot remove all unmeasured environmental confounding, and no visitation, pollen transfer, fitness or pigment-production cost was measured.

The final limitation concerns the anthropogenic layer. Human-context associations can reflect both genuine landscape processes and observation opportunity, and they cannot establish provenance. These limits do not make the analysis uninformative; they define the next measurements needed. The broad photographic dataset identifies national structure, local pollinator contrasts and a small set of locations where targeted field and genetic work can be most informative.

## 5 | Conclusions

The geography of *C. punctata* flower colour is best understood layer by layer. The phenotype first separates into two components: whether visible pigmentation is expressed and how intense it becomes once expressed. At the national scale, abiotic environment and continuous spatial structure define the main geographical background. At the local scale, low predicted bumblebee availability shows a large, directionally coherent association with reduced pigmentation state, but not with conditional intensity. This pattern is consistent with bumblebee availability affecting the benefit of maintaining a pigmented signal rather than simply selecting for progressively darker flowers.

The story then narrows further. Eighteen local pigmented departures remained worth examining after comparison with the natural predictive reference. Their apparent excess was not robust across predictive references, and their population and DID associations were suggestive rather than familywise significant. They therefore do not demonstrate anthropogenic origin. Instead, they define the final layer of the framework: after broad abiotic structure and a local biotic hypothesis, residual departures identify where human influence can be tested most efficiently.

Taken together, the study supports a staged rather than single-driver view of flower-colour geography. Broad abiotic and spatial structure sets the background; local pollinator context may modify whether pigmentation is maintained; and residual departures provide focused targets for field, provenance and genetic tests of human influence. Predicted *Bombus* support remains an availability proxy, so species-resolved visitation, pollen transfer, reproductive fitness and standardized flower reflectance are still required for a causal test of the proposed attraction mechanism.

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

Karger, D. N., Conrad, O., Böhner, J., Kawohl, J., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

Kass, J. M., et al. (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species' niches and distributions. *Methods in Ecology and Evolution*, 12, 1602–1608. https://doi.org/10.1111/2041-210X.13628

Koski, M. H., & Ashman, T.-L. (2015). Floral pigmentation patterns provide an example of Gloger's rule in plants. *Nature Plants*, 1, 14007. https://doi.org/10.1038/nplants.2014.7

Laitly, A. C., Callaghan, C. T., Delhey, K., & Cornwell, W. K. (2021). Is color data from citizen science photographs reliable for biodiversity research? *Ecology and Evolution*, 11, 4071–4083. https://doi.org/10.1002/ece3.7307

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