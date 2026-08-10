# From broad geography to local departures: environmental structure, bumblebee availability and human context in *Campanula punctata* flower colour

## Abstract

Geographical flower-colour variation can reflect processes operating at different spatial scales, but broad environmental gradients, spatial history and local biotic interactions are difficult to separate when they are entered together in a single national model. We used 1,922 author-reviewed YAMAP photographs of *Campanula punctata* to analyse flower-colour geography in three stages: a broad natural template, a local pollinator hypothesis and local departures from that template. A response-blind Gaussian mixture of CIELAB a* separated 966 white-like and 956 pigmented flowers at a*=4.9688; 124 observations with intermediate classification probability were retained but flagged. We analysed pigmentation state and, only among pigmented flowers, conditional visible intensity.

First, national environment-plus-INLA-SPDE models described the broad natural geography. Pigmentation state and conditional intensity both declined along the temperature axis; intensity also varied with topography. Residual spatial ranges were approximately 133 km for pigmentation state and 61 km for conditional intensity. Five geographically blocked folds gave AUC=0.863 for pigmentation state and RMSE=0.919 for conditional intensity.

Second, we changed scale rather than treating environmentally derived bumblebee SDMs as independent national causal predictors. The primary pollinator exposure was the occurrence-referenced predicted availability of *Bombus ardens* and *B. diversus*, the two broadly distributed focal taxa with direct evidence as predominant pollinators in the *C. punctata* system. We identified non-overlapping, Bombus-blind, pure white-versus-pigmented transitions among the five nearest neighbours within 5 km and oriented each pair only after selection from the whiter to the more pigmented side. Across 67 pairs, the mean pigmented-minus-white availability contrast was +0.0359 (100,000 sign-flip one-sided P=0.027), but the median was near zero, the proportion positive was 0.493, the across-scale BH q-value was 0.081, and the pattern was not reproduced by raw SDM support or at 10-25 km. We therefore interpret this as weak local-scale consistency with a pigmentation-benefit relaxation hypothesis, not evidence of pollinator-mediated selection.

Finally, rather than ranking cells by a raw fitted residual, we defined a biologically interpretable local event: a pigmented cell embedded among geographically close, environmentally similar white neighbours. The same event extractor was replayed on repeated natural predictive maps. Seventeen observed departures were not more frequent than expected robustly across cross-fitted and joint posterior-predictive references. Population density at 5 km and population-DID alignment were nominally elevated around candidates, but neither survived maxT familywise correction. The resulting framework separates a conventional broad-scale description from two species-specific local challenges: a pollinator hypothesis tested at the scale where selection should act, and a post-selection screen of recurrently defined local departures for human context.

**Keywords:** bumblebee availability; citizen-science photographs; flower colour; INLA-SPDE; local anomaly; multiscale ecology; pollinator limitation; *Campanula punctata*

## 1 | Introduction

Intraspecific trait variation can influence population responses, eco-evolutionary dynamics and species interactions, yet its geography remains incompletely characterized for many plant traits (Westerband et al., 2021). Flower colour is especially useful because it can simultaneously reflect physiology, developmental regulation, population history and interactions with animal pollinators (Rausher, 2008; Koski & Ashman, 2015; Trunschke et al., 2021). The same multiplicity of processes, however, makes geographical associations difficult to interpret. A national correlation between flower colour and a pollinator distribution may arise because both respond to the same climate or spatial history rather than because the pollinator generated the floral pattern.

Community photographs now make it possible to measure visible traits across much broader geographical extents than conventional population sampling (Laitly et al., 2021; Luong et al., 2023; McKenzie et al., 2026). YAMAP is useful for this purpose because photographs are linked to GPS-tracked hiking activities. It is not an areally random biodiversity survey: records are influenced by access, user activity, camera properties and flower conspicuousness. We therefore use the photographs as an author-reviewed, route-linked sample and treat inference as applying to the sampled geographical network rather than to every population in Japan.

A first distinction concerns the phenotype. A threshold-like change in anthocyanin-pathway activity can separate visibly white and pigmented flowers, whereas colour differences among already pigmented flowers may reflect pigment amount, cellular optics or additional biochemical pathways (Dick et al., 2011; Tasaki et al., 2022; van der Kooi et al., 2016). Treating the entire white-to-pink gradient as a single continuous response can therefore mix two biologically different questions: whether pigmentation is expressed and how strong the visible colour becomes once it is expressed. We analyse those two components separately. CIELAB a* is used as a reproducible human-visible image phenotype, not as anthocyanin concentration, ultraviolet reflectance or bumblebee receptor contrast.

A second distinction concerns spatial scale. Abiotic environment, dispersal and population history can structure plant traits over tens to hundreds of kilometres. Pollinator species distributions are also environmentally and spatially structured, and species-distribution models (SDMs) are explicitly predictions from that geography. Consequently, a single model containing national environment, space and predicted pollinator suitability cannot cleanly identify an independent pollinator effect (Soberón, 2007; Paciorek, 2010; Hanks et al., 2015). Biotic interactions are also expected to become more informative at local than continental or national scales (Araújo & Rozenfeld, 2014). We therefore use broad and fine scales for different questions rather than forcing all predictors into one regression.

*Campanula punctata* provides unusually strong natural-history motivation for this design. Its large tubular flowers are effectively pollinated by bumblebees. Nagano et al. (2014) showed that geographical changes in the realized bumblebee fauna of *C. punctata* are associated with floral-size variation and pollinator-flower size matching, with consequences for pollen removal. In the Izu Islands, transitions from bumblebee-dominated to bumblebee-poor pollinator assemblages are associated with major changes in reproductive ecology and breeding system (Inoue & Amano, 1986; Inoue, 1988). These studies do not show that bumblebees determine white versus pink flower colour. They do, however, justify a species-specific hypothesis that would be much less defensible in a plant without this pollination history.

Our primary local hypothesis is therefore a **pigmentation-benefit relaxation hypothesis**. If a pigmented floral signal improves attraction or detectability to important bumblebee pollinators, then the benefit maintaining a pigmented state should be greater where those pollinators are available. Where their availability is low, that benefit can relax and white flowers may be more easily maintained. This prediction concerns the presence of pigmentation, not necessarily progressively darker colour among already pigmented flowers. Pigment-production costs could strengthen such a transition, but costs are not measured here and are not required by the hypothesis.

The relevant pollinator quantity also requires care. Five Japanese *Bombus* taxa overlap the study region, but they do not represent one interchangeable national pool. *B. ardens* and *B. diversus* are broadly distributed and have direct evidence as predominant pollinators in the focal *C. punctata* system, whereas *B. beaticola*, *B. consobrinus* and *B. honshuensis* are increasingly associated with montane or alpine geography. Combining all five species as a single maximum-availability index therefore risks converting an availability question into a description of lowland-to-montane community replacement. We use *B. ardens* and *B. diversus* for the primary directional availability hypothesis and retain all-five community turnover and montane/alpine taxa as supplementary biogeographic sensitivities.

A third question begins where the broad natural model does not provide a simple local description. A raw residual is not, by itself, a compelling biological anomaly: its magnitude depends on model leverage, uncertainty and sampling effort. We instead define a local ecological event without using human variables—a pigmented cell embedded among nearby, environmentally similar white neighbours—and ask how often the same event occurs in repeated maps generated from the natural model. Human landscape variables are examined only after candidate identities are fixed. This creates a conservative final layer: not a claim of horticultural origin, but a way to ask whether locally discordant colour states provide unusually informative targets for field and provenance work.

The paper therefore follows a **broad -> fine -> anomaly** hierarchy. First, we establish the national environmental and spatial template of the two-part flower-colour phenotype. Second, we change scale and test whether the sharpest nearby white-pigmented transitions align directionally with predicted availability of the documented broad focal bumblebee pollinators. Third, we define local departures as repeatable ecological events relative to the natural predictive model and ask whether their human-landscape context is distinctive. The purpose of this hierarchy is not to partition all causal variance. It is to let each scale answer the question for which it is ecologically interpretable while making the remaining inferential ceilings explicit.

## 2 | Materials and Methods

### 2.1 | Sampling frame and author review

The source table contained 1,965 eligible *C. punctata* records recovered from YAMAP activity records during the flowering seasons of 2023-2025. Before model fitting, the author reviewed the focal flower and petal region, removed taxonomic errors and audited duplicate photographs. Exact image-hash duplicates were resolved using stable source-row order, retaining the first copy. Analysis-specific data availability produced 1,922 observations in the final two-part flower-colour analysis and 1,305 1-km cells in the cell-level analyses.

Records were not selected using flower colour, fitted residuals, *Bombus* predictions or human-landscape context. Because YAMAP sampling follows hiking routes and user behaviour, the analytical population is the retained route-linked photographic sample rather than a random plot survey.

### 2.2 | Visible-colour extraction and quality control

The deterministic image pipeline measured the author-confirmed petal region from display-referred sRGB pixels. The channel-wise median RGB value was used as the primary colour statistic and converted to CIELAB under D65 and a 2-degree standard-observer convention. CIELAB a* was used as the red-green optical phenotype.

Image-specific radiometric or white-balance calibration was impossible because neutral references and standardized camera metadata were unavailable. Mask coverage, exposure, shadows and disagreement among alternative pixel summaries were retained as quality-control information rather than used to remove extreme colours automatically. The phenotype is therefore a standardized image-derived visible-colour measure, not calibrated floral reflectance.

### 2.3 | Response-blind mixture classification and two-part response

The white-pigmented boundary was estimated from a* alone, without geography, date, environment, *Bombus*, human context or fitted-model information. We fitted univariate Gaussian mixtures under equal- and variable-variance parameterizations, selected the model by BIC and ordered components by fitted mean. The largest adjacent gap in component means was used to collapse the statistical components into lower-mean white-like and upper-mean pigmented optical regimes.

The final analysis selected a four-component variable-variance mixture. The operational boundary was a*=4.968780, yielding 966 white-like and 956 pigmented observations. A total of 124 observations had intermediate regime-membership probability and were retained with an ambiguity flag.

We then analysed:

1. **pigmentation state**, a binary response for all 1,922 observations; and
2. **conditional visible intensity**, standardized a* among observations assigned to the pigmented regime.

Variation within the white-like regime was not interpreted as pigment intensity.

### 2.4 | Environmental predictors and broad spatial context

Climate variables came from CHELSA v2.1 climatologies, soil variables from SoilGrids 2.0 and elevation from WorldClim 2.1; terrain derivatives were calculated from elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Response-blind principal components summarized temperature, precipitation, soil and topographic structure, with additional seasonality and radiation terms retained where defined by the pipeline.

For local diagnostics and anomaly construction, broad and within-neighbourhood environmental context was represented by 50-km neighbourhood summaries and cell-minus-neighbourhood deviations, reduced to four standardized environmental axes. These local environmental axes were used only to define or diagnose comparable local contexts; they were not used to create the *Bombus* directional contrast.

### 2.5 | National environment-plus-space models

We fitted separate observation-level INLA-SPDE models for pigmentation state and conditional intensity. Pigmentation state used a Bernoulli likelihood with logit link and conditional intensity used a Gaussian likelihood. Environmental variables entered as fixed effects, while a Matern SPDE field represented continuous residual geography (Lindgren et al., 2011). Penalised-complexity priors regularized spatial range and marginal variance (Simpson et al., 2017).

A cell-level natural predictive reference was fitted separately for later local analyses. Pigmented counts were modelled as binomial trials, and conditional intensity was modelled as Gaussian among cells with pigmented observations. Five response-blind geographical folds based on approximately 100-km blocks evaluated prediction to withheld geography (Roberts et al., 2017; Valavi et al., 2019). For every held-out cell, the pipeline generated predictive maps while retaining observed trial counts. These maps are a natural reference for observed sampling cells, not a complete map of unsampled Japan.

### 2.6 | *Bombus* SDMs and the primary availability exposure

Fresh SDMs were built for *B. ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus* and *B. honshuensis* using a common Honshu-Shikoku-Kyushu domain, shared environmental predictors, `maxnet`, spatial block partitions in ENMeval and the finite-AICc-selected model for each species. Predictions were expressed on the cloglog scale. Their cross-validation AUCs were 0.756, 0.601, 0.913, 0.897 and 0.870, respectively. These surfaces are predictions of habitat support, not abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015).

Raw SDM values are not directly comparable among species. For the main analysis, each species surface was therefore mapped to an **occurrence-referenced support scale**. For species k, the fitted cloglog support at the exact occurrence cells used in its selected SDM was converted to an empirical cumulative distribution function, F_occ,k. Flower-cell support s_k was then transformed as A_k=F_occ,k(s_k). This quantity indicates how a flower-cell prediction ranks relative to predictions at that species' observed occurrence cells; it is not occurrence probability or a calibrated abundance estimate.

The primary directional exposure was

`effective_occmax = max(A_ardens, A_diversus)`.

We restricted the primary exposure to *B. ardens* and *B. diversus* because they are the broad focal taxa with direct evidence as predominant pollinators in the *C. punctata* system. An initial five-species maximum-rank availability metric was structurally uninformative in the fresh data: the minimum five-species maximum rank was 0.489, so no cell represented low support for all five species. More importantly, including montane/alpine taxa changes the ecological estimand from availability of the documented broad focal pollinators to geographical replacement among *Bombus* niches. All-five and montane/alpine metrics were therefore retained as supplementary sensitivities rather than promoted to the main mechanism test.

### 2.7 | Fine-scale sharp-transition test of the pigmentation-benefit relaxation hypothesis

The main pollinator analysis intentionally changed scale. We did not fit a national environment + space + *Bombus* model and did not fit a second local environment-SPDE model. Instead, we asked whether the direction of the sharpest observed local flower-colour transitions aligned with the direction of the primary *Bombus* availability contrast.

At the 1-km cell level, we constructed a geographical graph using up to the five nearest neighbours within 5 km. The primary transition set contained only **pure white-versus-pigmented transitions**, defined as an absolute difference in observed cell pigment share of 1. Pair selection did not use *Bombus* values or the sign of the flower-colour difference. To reduce endpoint reuse, pairs were greedily selected without overlap, prioritizing transition magnitude and then geographical distance. Only after this fixed selection were pairs oriented from the whiter to the more pigmented endpoint.

For each pair we calculated

`Delta A = effective_occmax_pigmented - effective_occmax_white`.

The primary statistic was the mean Delta A across non-overlapping pairs. A 100,000-replicate sign-flip randomization conditioned on the observed absolute pair contrasts and tested whether the direction of the *Bombus* contrast aligned with the flower-colour transition more than expected under exchangeable orientation. Environment did not select, orient or regress the pairs. It was reported as a diagnostic: the median environmental-PC distance of selected transitions was compared with that of all edges in the same local graph.

Predefined sensitivity summaries repeated the graph at 10 and 25 km, relaxed the flower-colour transition threshold to 0.75 and 0.50, and substituted raw-cloglog effective support and broader all-five support. Because this local refinement followed earlier exploratory analyses, inferential language is explicitly exploratory and multiplicity summaries are retained.

### 2.8 | Supplementary community-turnover and montane/alpine guardrails

Five-species community turnover was not treated as a flower-colour mechanism in the main paper. In Supplementary analyses we asked a narrower biogeographic question: are sharp flower-colour boundaries also unusually strong boundaries in predicted *Bombus* assemblage composition? Species supports were converted to relative composition and Hellinger distance. Each sharp transition was compared with nearby non-transition edges matched without *Bombus* values on edge length, midpoint elevation and absolute elevation difference, using fixed background windows and matched-control counts. Spatial-block and leave-one-block-out summaries tested whether any correspondence was carried by a single region.

We also treated *B. beaticola*, *B. consobrinus* and *B. honshuensis* as a montane/alpine sensitivity group. Their apparent flower-colour associations were re-evaluated among pure transitions with <=50 m or <=100 m endpoint elevation difference. This analysis is a guardrail against interpreting shared high-elevation geography as an additional pollinator effect.

### 2.9 | Event-based local departures from the natural reference

The anomaly analysis did not rank observations by the magnitude of a fitted residual. Instead, it defined a local event that has a direct geographical interpretation. For the primary configuration, a candidate cell had to contain pigmentation, have at least three other sampled cells within 10 km and environmental distance <=1, and have **no pigmented neighbour** among those geographically and environmentally eligible cells. Human variables, flowering date and colour intensity did not enter candidate selection.

The same graph construction and event extractor were then applied to repeated maps generated by the natural flower-colour model. We compared the observed candidate count and the fraction of supported pigmented cells meeting the event definition with 10,000 held-out cross-fitted natural maps. A separate full-data joint spatial posterior-predictive sensitivity used 10,000 latent maps with 20 observation replicates each (200,000 event maps in total). Thus the inferential object was the frequency of the **same local event** under the fitted natural geography, rather than an arbitrary residual cutoff.

### 2.10 | Post-selection human context

Candidate identities were fixed before any human-context variable was inspected. We then compared candidates with their white local neighbourhoods using WorldPop population at several spatial scales, MLIT land-use and densely inhabited district (DID) context. Population-DID alignment was included as a targeted indicator of densely settled human context. Feature-family inference used matched/randomization references and maxT familywise correction. These analyses characterize candidate context only; they cannot establish planting, escape, introgression or horticultural provenance.

### 2.11 | Reproducibility and analysis hierarchy

The final manuscript-facing hierarchy is locked as:

1. **Main 1 - Broad natural template:** two-part phenotype + national environment and continuous space;
2. **Main 2 - Fine-scale pollinator hypothesis:** local sharp-transition test using occurrence-referenced *B. ardens* + *B. diversus* availability;
3. **Main 3 - Anomaly screen:** event-based local departures + post-selection human context;
4. **Supplement:** five-species community turnover, montane/alpine guardrails, alternative *Bombus* exposures/scales, and full human/anomaly sensitivities.

The final integration workflow restores checksum-locked current-input and *Bombus* artifacts and reruns the manuscript-facing local tests. Historical 1,909 and earlier five-species gate analyses are retained for provenance but are not manuscript-facing estimands.

## 3 | Results

### 3.1 | The flower-colour phenotype separates into state and conditional intensity

The 1,922 final observations separated nearly evenly into 966 white-like and 956 pigmented flowers. The response-blind four-component variable-variance mixture placed the operational boundary at a*=4.9688, and 124 observations had intermediate classification confidence. Conditional visible intensity was analysed only within the pigmented regime (Figure 1).

### 3.2 | Broad environment and spatial structure define a strong national template

Pigmentation state declined along the temperature axis (posterior mean -0.542; 95% CrI -1.033 to -0.049). Conditional visible intensity showed a clearer negative temperature association (mean -0.319; CrI -0.484 to -0.156) and also declined along the topographic axis (mean -0.138; CrI -0.231 to -0.045). Other fixed effects were more uncertain after conditioning on the spatial field.

Substantial residual geography remained. The posterior mean SPDE range was 132.7 km (95% CrI 88.7-195.6) for pigmentation state and 60.9 km (31.0-115.8) for conditional intensity. These ranges are descriptive because the two response models have different likelihoods and support.

The five-fold cell-level natural model discriminated pigmentation state well across withheld geography (image-level AUC=0.863; cell any-pigmented AUC=0.858; cell majority-pigmented AUC=0.871; Brier score=0.150). The conditional-intensity model had RMSE=0.919, MAE=0.715 and 95% predictive coverage=0.944 across 674 pigmented-support cells (Figure 2). These results establish a predictive natural template; they do not imply that environment alone explains most phenotypic variance.

### 3.3 | The sharpest local white-pigmented transitions show weak alignment with focal *Bombus* availability

The strict 5-km graph yielded 67 non-overlapping pure white-versus-pigmented transitions. Their median geographical separation was 2.0 km. The median environmental-PC distance among selected transitions was 0.180, compared with 0.343 among all edges in the same 5-km graph. Thus the focal transitions were not unusually environmentally divergent; they were, if anything, more similar in the measured environmental summary than generic nearby pairs.

Using occurrence-referenced *B. ardens* + *B. diversus* support, the mean predicted-availability difference from the white to the pigmented endpoint was +0.0359. The one-sided sign-flip P-value was 0.0272. However, the median contrast was -0.0028 and only 49.3% of pairs had higher availability on the pigmented side, showing that the positive mean was driven by the magnitude of a subset of contrasts rather than by a majority of transitions. Correction across the 5, 10 and 25-km pure-transition scale family gave q=0.0815.

The signal attenuated rapidly with scale: mean contrasts were +0.0084 at 10 km (P=0.325; n=109) and +0.0029 at 25 km (P=0.436; n=171). Raw-cloglog effective support did not reproduce the strict 5-km result (mean +0.0044; P=0.267). The 5-km occurrence-referenced result was similar when the transition threshold was relaxed to 0.75 but disappeared when the threshold was relaxed to 0.50. We therefore treat the result as **weak and highly local consistency** with the pigmentation-benefit relaxation hypothesis rather than as a robust general effect (Figure 3).

The pollinator result concerns the white-versus-pigmented state. Previous local community analyses and the present exposure sensitivities provided no persuasive evidence that *Bombus* availability or community turnover explains how dark already pigmented flowers become.

Supplementary analyses clarified why montane/alpine species were not added to the primary availability index. Their apparent positive association with pigmented transitions disappeared when endpoints were constrained to near-equal elevations. For pure transitions with <=50 m elevation difference, mean pigmented-minus-white montane support was -0.0033, -0.0020 and -0.0059 at 5, 10 and 25 km, respectively (all one-sided P>=0.755). Thus the high-elevation overlap of montane *Bombus* and pigmented flowers provides no evidence here for an additional montane pollinator effect.

### 3.4 | Five-species community turnover is a supplementary biogeographic correspondence, not the main mechanism

Although community turnover was not used as the main mechanism test, sharp flower-colour boundaries also tended to coincide with larger predicted five-species *Bombus* compositional changes than nearby spatial/elevationally matched non-transition edges. Under the fixed occurrence-referenced Hellinger comparison, mean matched excess turnover was +0.0330 at 5 km (P=0.0628), +0.0327 at 10 km (P=0.0142) and +0.0411 at 25 km (P=0.0001). The direction was positive across alternative matching windows and was not carried by one 100-km geographical block.

We interpret this only as evidence that flower-colour transition zones can also be boundaries in predicted pollinator-community geography. Because the community surfaces remain environmentally derived, and because montane/alpine associations disappear under equal-elevation comparisons, the turnover result is retained in the Supplement rather than used to claim species-specific colour selection.

### 3.5 | Event-based local departures were real locations but not a robust excess beyond the natural model

The primary 10-km local-event definition identified 17 pigmented cells embedded among environmentally similar white neighbourhoods. Replaying the identical event on 10,000 held-out natural maps gave a null mean candidate count of 13.61 and an upper-tail P=0.200. The observed candidate fraction was 0.0474 versus a null mean of 0.0343 (P=0.0874). Under the joint spatial posterior-predictive sensitivity, the corresponding P-values were 0.314 for candidate count and 0.196 for candidate fraction.

Thus the final data do not show that such local departures occur more often than the fitted natural geography can generate. The 17 cells are nevertheless a reproducibly defined set of local mismatches and are useful as post-selection field targets. The important methodological distinction is that they were defined by a local ecological event and evaluated by repeated natural maps, not declared anomalous because a single residual exceeded an arbitrary cutoff.

### 3.6 | Human context around the departures was suggestive but familywise-inconclusive

The clearest population contrast occurred at the 5-km scale. Candidates had a focal-minus-white-neighbour population-rank contrast of +0.0531 (directional P=0.0270), but the maxT familywise P-value was 0.0899. Population-DID alignment was also positive (+0.0516; directional P=0.0230; maxT P=0.0759). Other population scales and DID features were weaker.

Accordingly, the candidate set shows a suggestive tendency toward densely populated human context, but no human-context result crossed the familywise threshold. The analysis does not establish horticultural origin or human causation (Figure 4).

## 4 | Discussion

### 4.1 | A broad -> fine -> anomaly framework separates questions that should not share one scale

The strongest contribution of the analysis is not a claim that three independent drivers have been partitioned. It is the separation of three different ecological questions. The first is conventional but necessary: what broad environmental and spatial structure organizes flower colour across Japan? The second is more species-specific: within that broad template, do the sharpest nearby white-pigmented transitions align with a pollinator quantity that has a direct natural-history interpretation? The third asks where the broad natural model still produces locally discordant observed states and whether those pre-defined locations occupy distinctive human landscapes.

This hierarchy avoids a common interpretive trap. If environment, spatial fields, five *Bombus* SDMs and human variables were placed in one national model, the coefficients would compete across strongly shared geography and would be difficult to translate into mechanisms. Instead, the broad model is allowed to do what broad models do well: describe and predict the national template. The pollinator question then changes scale, and the human question changes the object of inference from a coefficient to a pre-defined local event.

The latter two stages are possible precisely because *C. punctata* has unusually informative natural history and a visibly discrete colour polymorphism. Its tubular flowers, documented bumblebee effectiveness and island history motivate a local relaxation hypothesis that would otherwise be speculative. Its white-pigmented mosaic also allows a sharp transition to be defined independently of the pollinator surface. The design therefore uses species biology to determine the scale and contrast rather than allowing the statistical model alone to define the question.

### 4.2 | The broad natural template is strong, but it is a template rather than a causal partition

The national results confirm that flower colour has strong environmental and spatial geography. Temperature was associated with both whether pigmentation was expressed and how intense the visible colour became among pigmented flowers, while topography additionally contributed to conditional intensity. The remaining SPDE structure extended over roughly 100 km for pigmentation state and a shorter range for conditional intensity.

The two-part response is important here. A white-to-pigmented switch and variation among pigmented flowers need not share one ecological control. The current results are consistent with that distinction: broad abiotic geography is evident for both components, whereas the local pollinator signal, weak as it is, concerns pigmentation state rather than intensity. This does not identify the underlying developmental pathway, but it prevents optical variation among white flowers from being treated as a dose of pigmentation.

The spatial field should not be interpreted as a single historical mechanism. It can absorb unmeasured environment, population history, dispersal structure and sampling. Likewise, high predictive AUC does not imply that measured environment explains most biological variance. The broad analysis is best viewed as a well-performing geographical reference against which more local questions can be posed.

### 4.3 | Why the main pollinator analysis uses *B. ardens* and *B. diversus*, not all five species

The primary pollinator hypothesis is about **availability of documented broad focal pollinators**, not about total habitat suitability of every *Bombus* taxon. This distinction became critical in the fresh SDMs. Taking the maximum rank across all five species made nearly every flower cell appear highly supported because species replace one another geographically; the minimum five-species maximum rank was already 0.489. Such an index cannot represent a meaningful bumblebee-poor end of the gradient.

More importantly, the five-species maximum mixes two biological quantities. A decrease in *B. ardens* or *B. diversus* can coincide with an increase in a montane species. Treating that replacement as unchanged "availability" assumes that every species contributes equivalently to the focal pollination mechanism. The present data do not justify that assumption. The high-elevation taxa also overlap strongly with the broad geography of pigmented flowers, and their apparent local association disappears when elevation is matched closely. Including them in the primary availability metric would therefore re-import the very broad elevational niche structure that the local design is intended to avoid.

Restricting the primary exposure to *B. ardens* and *B. diversus* is consequently not a claim that no other bumblebee ever visits *C. punctata*. It is a claim about the estimand: these are the broadly distributed, directly documented focal pollinators for which a directional local availability hypothesis can be interpreted. All-five community turnover answers a different question and belongs in the Supplement.

### 4.4 | Fine-scale availability provides a weak but biologically coherent local test

At the strictest scale, pigmented sides of sharp transitions had a modestly higher mean occurrence-referenced support for *B. ardens* or *B. diversus*. The selected transitions were separated by a median of only 2 km and were more similar in the measured environmental summary than generic 5-km edges. This is exactly the scale change we intended: rather than asking whether pink regions of Japan overlap bumblebee habitat, we ask whether abrupt colour boundaries within otherwise local contexts tend to point in the same direction as focal-pollinator availability.

The evidence remains weak. The median *Bombus* contrast was essentially zero, fewer than half the pairs were positive, the mean result was not robust to raw SDM values, and it attenuated at 10-25 km. The occurrence-referenced 5-km result should therefore be read as a magnitude-driven local pattern that is compatible with the relaxation hypothesis, not as a demonstration that bumblebees maintain pigmentation.

The biological interpretation nevertheless has a clear direction. If conspicuous pigmentation increases attraction or detection by important bumblebee pollinators, then the reproductive benefit of maintaining that signal can be greater where those pollinators are available. When focal pollinator availability declines, that benefit can relax, allowing white flowers to persist or increase without requiring an assumed metabolic cost of pigment production. The absence of a corresponding intensity signal suggests that this mechanism, if real, is more naturally framed as maintenance or loss of a pigment state than as selection for progressively darker flowers.

The SDM ceiling is unavoidable. Predicted habitat support is generated from environment and is not realized visitation, pollen deposition or selection. Changing scale and choosing sharp nearby transitions reduces broad geographical confounding by design, but it does not make environment disappear. Direct visitation and fitness data are the next required test.

### 4.5 | Community turnover belongs in the Supplement because it describes a boundary, not a colour mechanism

The supplementary community result is useful, but its ecological role is narrower than the availability hypothesis. Flower-colour boundaries also coincide with relatively large changes in predicted *Bombus* assemblage composition, even compared with nearby edges matched on geographical distance and elevation structure. This correspondence repeats across regions and therefore is not simply one national boundary.

However, an unsigned Hellinger turnover has no inherent prediction about which side should be white or pigmented. Different *Bombus* species need not have opposite colour preferences, and the present data do not show that they do. The turnover analysis therefore describes a **biogeographic correspondence between flower-colour boundaries and pollinator-community boundaries**, not a causal route from species replacement to flower colour. The equal-elevation montane guardrail reinforces that restraint: the visually striking overlap between high-elevation *Bombus* taxa and pigmented flowers is adequately explained by shared elevational geography in the present data.

This distinction also clarifies why turnover is supplementary while the weaker availability result remains the main pollinator test. Statistical strength alone does not determine hierarchy. The primary analysis is the one with the clearest a priori biological direction and the closest connection to the focal species' pollination natural history; turnover is a useful contextual pattern whose mechanism for colour remains unresolved.

### 4.6 | Event-based anomalies are more interpretable than raw residual tails

The final stage deliberately avoids calling a large fitted residual an anomaly. A residual can be large because of model uncertainty, sampling effort, leverage or local spatial interpolation. By contrast, the event used here has an ecological meaning that can be applied identically to observed and simulated maps: a pigmented cell occurs inside an otherwise white, geographically close and environmentally comparable neighbourhood.

Replaying the event on thousands of cross-fitted and joint posterior-predictive maps asks a cleaner question: how often would the fitted natural geography itself generate this kind of local discordance? The answer in the present data is conservative. Seventeen cells meet the observed definition, but their count and fraction are not robustly excessive under both predictive references. We therefore do not claim an unexplained additional process from the anomaly count itself.

This negative result is a strength of the workflow rather than a reason to abandon the candidate set. It prevents the human-context stage from being built on an uncalibrated residual tail. The 17 locations are simply reproducibly defined places where local colour state is discordant with its natural neighbourhood, and human variables were not used to choose them.

### 4.7 | Human context is a follow-up hypothesis, not provenance

Population at 5 km and population-DID alignment were the strongest human-context contrasts, but neither survived familywise correction. The appropriate conclusion is therefore not that urbanization or horticulture generated pigmented flowers. Human-modified landscapes correlate with access, observation density, planting history and many unmeasured ecological variables, all of which can create the same association.

The value of this stage is instead logistical and hypothesis-generating. It converts a broad statement such as "some unusual pigmented populations may be human associated" into a finite set of pre-selected locations and explicit field predictions. Provenance would require vouchers, planting histories and population-genetic comparison among candidate sites, neighbouring wild populations and horticultural material.

### 4.8 | Limitations and next tests

The photographic phenotype is route-biased and not calibrated reflectance. The mixture boundary is operational, and classification uncertainty remains for 124 observations. National coefficients are conditional on the environmental representation, INLA mesh, priors and spatial field.

For the pollinator layer, occurrence-referenced SDM support is an availability proxy, not abundance, floral visitation or selection pressure. The local analysis was refined after exploratory work; the strict 5-km pattern must therefore be presented with its scale, exposure and multiplicity sensitivities. The apparent high-elevation association of montane/alpine *Bombus* should not be given a mechanistic interpretation because it disappears under near-equal-elevation comparison.

For the anomaly layer, the candidate set is not demonstrably more frequent than the natural predictive model expects, and human-context contrasts remain familywise-inconclusive. These ceilings define the next empirical work. Species-resolved visits to white and pigmented flowers, first approaches, stigma contact, pollen removal and deposition, reproductive success, standardized visible/UV reflectance and population genetics would directly test the mechanisms that the photographic geography can only prioritize.

## 5 | Conclusions

The flower-colour geography of *C. punctata* is best analysed by changing the question with scale. At the broad scale, environmental gradients and continuous spatial structure provide a strong and predictable national template for both pigmentation state and conditional visible intensity. This is the conventional geographical component of the study and the reference against which the more species-specific questions are posed.

At the fine scale, the sharpest white-pigmented transitions show weak directional consistency with predicted availability of the documented broad focal bumblebee pollinators, *B. ardens* and *B. diversus*. The signal is local and not robust enough to establish pollinator-mediated selection, but its direction is biologically interpretable in a species with a documented history of bumblebee-dependent pollination. High-elevation *Bombus* taxa are not required for this conclusion; their apparent association with pigmented flowers is explained by shared elevational geography in the present data.

Finally, local departures are treated as ecological events rather than residual magnitudes. Seventeen pigmented-in-white neighbourhoods were reproducibly defined, but their frequency was compatible with repeated natural predictive maps and their human-context associations remained suggestive after familywise correction. They are therefore targets for provenance and field testing, not evidence of anthropogenic origin.

Together, the study supports a staged **broad -> fine -> anomaly** framework. Broad models describe the geographical template; species-specific natural history motivates a local mechanistic contrast at the scale where biotic selection is most plausible; and event-based departures identify where additional processes can be tested without defining anomalies from the same human variables later used to explain them.

## Data Accessibility Statement

The source population audit, derived flower-colour tables, environmental source registry, fresh *Bombus* SDM configuration, occurrence-referenced support transformation, seeds, local-transition specifications, anomaly-event definitions, result tables and workflow provenance are versioned in the project repository. Original YAMAP photographs are third-party content and are not redistributed. Manuscript-facing numerical claims are linked to checksum-locked GitHub Actions artifacts in `reproducibility/final_paper_pipeline_2026-08-09.md`.

## References

Araújo, M. B., & Rozenfeld, A. (2014). The geographic scaling of biotic interactions. *Ecography*, 37, 406-415. https://doi.org/10.1111/j.1600-0587.2013.00643.x

Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., & Karger, D. N. (2022). Global climate-related predictors at kilometer resolution for the past and future. *Earth System Science Data*, 14, 5573-5603. https://doi.org/10.5194/essd-14-5573-2022

Dick, C. A., Buenrostro, J., Butler, T., Carlson, M. L., Kliebenstein, D. J., & Whittall, J. B. (2011). Arctic mustard flower color polymorphism controlled by petal-specific downregulation at the threshold of the anthocyanin biosynthetic pathway. *PLoS ONE*, 6, e18230. https://doi.org/10.1371/journal.pone.0018230

Fick, S. E., & Hijmans, R. J. (2017). WorldClim 2: New 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology*, 37, 4302-4358. https://doi.org/10.1002/joc.5086

Guillera-Arroita, G. (2015). Is my species distribution model fit for purpose? Matching data and models to applications. *Global Ecology and Biogeography*, 24, 276-292. https://doi.org/10.1111/geb.12268

Hanks, E. M., Schliep, E. M., Hooten, M. B., & Hoeting, J. A. (2015). Restricted spatial regression in practice: geostatistical models, confounding, and robustness under model misspecification. *Environmetrics*, 26, 243-254. https://doi.org/10.1002/env.2331

Inoue, K. (1988). Pattern of breeding-system change in the Izu Islands in *Campanula punctata*: Bumblebee-absence hypothesis. *Plant Species Biology*, 3, 125-128. https://doi.org/10.1111/j.1442-1984.1988.tb00178.x

Inoue, K., & Amano, M. (1986). Evolution of *Campanula punctata* in the Izu Islands: Changes of pollinators and evolution of breeding systems. *Plant Species Biology*, 1, 89-97. https://doi.org/10.1111/j.1442-1984.1986.tb00018.x

Karger, D. N., Conrad, O., Böhner, J., Kawohl, J., Kreft, H., Soria-Auza, R. W., Zimmermann, N. E., Linder, H. P., & Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data*, 4, 170122. https://doi.org/10.1038/sdata.2017.122

Kass, J. M., et al. (2021). ENMeval 2.0: Redesigned for customizable and reproducible modeling of species' niches and distributions. *Methods in Ecology and Evolution*, 12, 1602-1608. https://doi.org/10.1111/2041-210X.13628

Koski, M. H., & Ashman, T.-L. (2015). Floral pigmentation patterns provide an example of Gloger's rule in plants. *Nature Plants*, 1, 14007. https://doi.org/10.1038/nplants.2014.7

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

Soberón, J. (2007). Grinnellian and Eltonian niches and geographic distributions of species. *Ecology Letters*, 10, 1115-1123. https://doi.org/10.1111/j.1461-0248.2007.01107.x

Tasaki, K., et al. (2022). Identification of candidate genes responsible for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*, 13, 906879. https://doi.org/10.3389/fpls.2022.906879

Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower color evolution and the evidence of pollinator-mediated selection. *Frontiers in Plant Science*, 12, 617851. https://doi.org/10.3389/fpls.2021.617851

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation. *Methods in Ecology and Evolution*, 10, 225-232. https://doi.org/10.1111/2041-210X.13107

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016). How to colour a flower: On the optical principles of flower coloration. *Proceedings of the Royal Society B*, 283, 20160429. https://doi.org/10.1098/rspb.2016.0429

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait variation in plants: A renewed focus on its role in ecological processes. *Annals of Botany*, 127, 397-415. https://doi.org/10.1093/aob/mcab011

# Table 1. Final manuscript-facing results and claim ceilings

| Layer | Active estimate | Interpretation ceiling |
|---|---|---|
| Phenotype | n=1,922; 966 white-like; 956 pigmented; a*=4.9688 | image-derived optical regimes, not pigment chemistry |
| Broad presence | AUC=0.863; spatial range mean 132.7 km | strong geographical natural template, not variance partition |
| Broad intensity | RMSE=0.919; spatial range mean 60.9 km | pigmented-only optical intensity |
| Fine *Bombus* availability | 5 km n=67; mean Delta=+0.0359; P=0.027; across-scale q=0.081 | weak/local exploratory consistency; predicted availability, not selection |
| Fine robustness | median Delta=-0.0028; positive=49.3%; raw support P=0.267; 10/25 km null | no robust general directional effect |
| Local departures | 17 candidates; count P=0.200 cross-fit / 0.314 joint PPC | reproducible follow-up locations, not evidence for an extra process |
| Human context | population 5 km maxT P=0.090; DID alignment maxT P=0.076 | suggestive, familywise-inconclusive; no provenance claim |
| Supplementary turnover | matched excess positive at 5/10/25 km; P=0.063/0.014/<0.001 | pollinator-community boundary correspondence, not colour mechanism |
| Montane/alpine guardrail | equal-elevation contrasts approximately zero/negative | shared elevational geography; no additional montane effect |

# Figure legends

**Figure 1. Observation-level visible colours and the two-part flower-colour response.** National distribution of 1,922 author-reviewed observations, response-blind mixture classification, pigmentation state and conditional visible intensity. Displayed sRGB/CIELAB colours are image-derived human-visible phenotypes rather than anthocyanin concentration or bumblebee receptor contrast.

**Figure 2. Broad environmental and spatial template of flower-colour geography.** Posterior environmental associations, SPDE spatial ranges and geographically cross-fitted predictions for pigmentation state and conditional visible intensity. The spatial field represents coherent residual geography rather than a uniquely identified historical mechanism.

**Figure 3. Fine-scale sharp-transition test of focal bumblebee availability.** Pure white-versus-pigmented transition pairs were selected without *Bombus* information among the five nearest neighbours within 5 km and oriented only after selection from white to pigmented. Panels show pair geography/environmental similarity, the occurrence-referenced *B. ardens* + *B. diversus* availability contrast and its sign-flip reference. Sensitivity to 10/25-km scales and raw SDM support is shown in the Supplement. The 5-km mean signal is weak and magnitude-driven rather than a majority-of-pairs pattern.

**Figure 4. Event-based local departures and post-selection human context.** Locations of the 17 pigmented cells embedded in environmentally similar white neighbourhoods, repeated-natural-map references for candidate count/fraction, and post-selection population/DID contrasts. The candidate set is not a robust excess over the natural predictive model, and human-context contrasts do not survive maxT familywise correction.
