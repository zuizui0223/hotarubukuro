# Layered flower-colour geography from repurposed hiking photographs: broad environment, local bumblebee availability and human-context departures in *Campanula punctata*

## Abstract

Range-wide studies of flower-colour variation face two linked problems. Quantitative colour phenotypes are difficult to obtain across large geographical extents, and environmental, spatial and biotic predictors often share the same geography. We repurposed GPS-linked hiking photographs from YAMAP, a recreation platform rather than a biodiversity-recording database, as an iEcology data source and recovered 1,922 author-reviewed photographs of *Campanula punctata*. A response-blind Gaussian mixture of CIELAB a* separated 966 white-like and 956 pigmented flowers at a*=4.9688; 124 intermediate assignments were retained but flagged. We analysed two phenotypic components: pigmentation state and, only among pigmented flowers, conditional visible intensity.

We then changed the inferential question with spatial scale. First, national environment-plus-INLA-SPDE models defined a broad natural template. Pigmentation state and conditional intensity both declined along a temperature axis, intensity also varied with topography, and substantial residual spatial structure remained. Geographically blocked prediction gave AUC=0.863 for pigmentation state and RMSE=0.919 for conditional intensity. Second, rather than placing environmentally derived bumblebee SDMs into the same national regression, we tested a species-specific local hypothesis at abrupt white-pigmented boundaries. Across 67 non-overlapping pure transitions within 5 km, the pigmented side had a modestly higher mean occurrence-referenced predicted availability of *Bombus ardens* or *B. diversus* (+0.0359; one-sided sign-flip P=0.027), but the median was near zero, only 49.3% of pairs were positive, the across-scale q-value was 0.081, and raw-SDM and 10-25-km sensitivities were null. We therefore interpret this as weak local consistency with a pigmentation-benefit relaxation hypothesis, not evidence of pollinator-mediated selection. Finally, instead of defining anomalies by raw residual magnitude, we defined a local ecological event—a pigmented cell embedded among geographically close, environmentally similar white neighbours—and replayed the identical event on repeated natural predictive maps. Seventeen departures were not robustly more frequent than expected; population and population-DID context were suggestive but familywise-inconclusive.

The study therefore contributes both a biological result and a design framework. Recreational GPS-linked photographs can extend quantitative trait geography beyond purpose-built biodiversity portals, while broad environmental structure, local biotic hypotheses and exceptional local events can be investigated with different comparison units rather than forced into one omnibus model.

**Keywords:** bumblebee availability; digital phenotyping; flower colour; hiking photographs; iEcology; INLA-SPDE; local anomaly; multiscale ecology; *Campanula punctata*

## 1 | Introduction

Geographical trait variation records the combined influence of physiology, selection, dispersal and population history, but many intraspecific traits remain poorly measured across entire species ranges. Flower colour is a particularly informative example because it can reflect pigment physiology and developmental regulation while also mediating interactions with animal pollinators (Rausher, 2008; Koski & Ashman, 2015; Trunschke et al., 2021). The difficulty is not merely to find correlates of colour. It is to ask ecological questions at the scales at which their mechanisms are interpretable.

### 1.1 | A data gap between occurrence maps and quantitative trait geography

Purpose-built biodiversity platforms and aggregators have transformed species-occurrence research, and their photographs increasingly support phenotypic measurements (Laitly et al., 2021; Luong et al., 2023; McKenzie et al., 2026). A complementary opportunity is **iEcology**: extracting ecological information from digital material accumulated online for purposes other than ecological recording (Jarić et al., 2020). Such sources can contain observations that were never submitted as formal species records and can therefore expand the pool of usable ecological imagery.

YAMAP provides one such source for mountain plants. It is primarily a hiking-navigation and activity-diary platform. During a recorded hike, photographs can retain positions along a GPS activity track and can be displayed at those photographed locations when an activity is shared. The unit of organization is therefore not a biodiversity checklist but a mountain activity and route. For a conspicuous flowering plant, this creates a potentially valuable stream of incidental, route-linked photographs across many mountain systems.

This does not make YAMAP unbiased, nor do we regard it as universally superior to GBIF or iNaturalist. Users choose routes and subjects, trails differ in use, photograph locations can be hidden, GPS is imperfect and conspicuous flowers may be photographed preferentially. Instead, YAMAP changes the observation process: the original user goal is hiking documentation rather than taxon reporting. We therefore combine this source with manual ecological validation. Author review can reduce taxonomic errors, repeated-image problems and mistakes in defining the focal petal region, although it cannot remove route-access or subject-selection bias. The resulting data are best understood as an author-reviewed, GPS-linked hiking-photograph sample.

### 1.2 | A phenotype-resolution gap in geographical flower-colour studies

A second limitation is how colour itself is represented. Many range-wide studies necessarily reduce flower colour to discrete morphs or coarse colour classes and then compare morph frequencies among populations or environments (e.g. Arista et al., 2013; Surmacz, 2023). Such analyses are powerful for detecting morph geography and niche differences, but by construction they treat individuals within a morph as equivalent. Quantitative reflectance studies demonstrate that continuous variation within a colour class can also track climate or population history (Koski & Galloway, 2020), yet standardized spectroscopy is difficult to obtain for thousands of historical or geographically dispersed individuals.

Digital photographs offer an intermediate solution. They cannot replace calibrated reflectance, ultraviolet measurements or pigment chemistry, but they can provide a reproducible human-visible quantitative phenotype at much larger scale. In *Campanula punctata*, this is especially useful because white versus pigmented flowers and variation among already pigmented flowers need not represent the same biological transition. Threshold-like changes in anthocyanin-pathway activity can separate visible pigment states, whereas colour intensity among pigmented flowers can additionally reflect pigment amount, cellular optics and other biochemical processes (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). We therefore separate **whether pigmentation is expressed** from **how intense visible colour is once pigmentation is present**, rather than compressing the entire white-to-pink axis into one response.

### 1.3 | Spatial scale is part of the ecological hypothesis

A third difficulty is confounding across geography. Climate, topography, dispersal and population history can generate broad spatial structure in plant traits. Pollinator distributions are themselves geographically structured, and species-distribution models (SDMs) predict distributions from environmental geography. A national model containing environmental predictors, a spatial field and predicted pollinator suitability can therefore produce coefficients that are statistically estimable but mechanistically difficult to interpret (Soberón, 2007; Paciorek, 2010; Hanks et al., 2015). The problem is particularly acute when the proposed biotic process is expected to act locally while both plant and pollinator show broad range structure.

We address this by changing scale rather than insisting that every process enter one model. The broad analysis asks where flower-colour states and intensities occur across Japan after explicitly representing continuous residual spatial structure. The pollinator analysis then changes the comparison unit to abrupt nearby flower-colour transitions, where broad geographical differences are reduced by design. This does not remove environmental confounding, but it prevents a national overlap between two environmentally structured maps from being treated automatically as evidence of a biotic effect.

### 1.4 | Why *Campanula punctata* permits a species-specific local pollinator test

The local hypothesis is motivated by unusually rich natural history. *C. punctata* has large tubular flowers and is effectively pollinated by bumblebees. Nagano et al. (2014) showed that geographical changes in realized bumblebee fauna are associated with floral-size variation and flower-pollinator size matching, with consequences for pollen removal. In the Izu Islands, changes from bumblebee-dominated to bumblebee-poor pollinator assemblages are associated with major changes in reproductive ecology and breeding system (Inoue & Amano, 1986; Inoue, 1988). These studies do not demonstrate that bumblebees cause white versus pink flowers, but they make a local pollinator hypothesis biologically meaningful in this species rather than an arbitrary correlation chosen from a large predictor set.

Our directional hypothesis is a **pigmentation-benefit relaxation hypothesis**. Bumblebees generally possess visual systems capable of discriminating chromatic floral signals, and a pigmented signal may improve attraction or detection. If so, the reproductive benefit maintaining pigmentation should be greater where important bumblebee pollinators are available. Where their availability declines, that benefit may relax and white flowers may be more easily maintained. The hypothesis does not require us to assume a measurable metabolic cost of pigment production, and it predicts a change in the probability of being pigmented more directly than a progressive darkening among already pigmented flowers.

Five focal Japanese *Bombus* taxa overlap the study region, but they do not represent one interchangeable pollinator pool. *B. ardens* and *B. diversus* are broadly distributed and have direct evidence as predominant pollinators in the focal *C. punctata* system. *B. beaticola*, *B. consobrinus* and *B. honshuensis* increasingly characterize montane or alpine geography. Combining all five into a single maximum-availability index would therefore mix two quantities: local availability of documented broad focal pollinators and broad geographical replacement among bumblebee niches. This distinction is important because pigmented *C. punctata* is itself geographically associated with high-elevation regions. We therefore use *B. ardens* and *B. diversus* for the primary directional availability test and use all-five turnover and montane/alpine species only as supplementary biogeographic guardrails.

### 1.5 | A final layer: local departures and human context

A final species-specific motivation comes from horticulture. *C. punctata* is cultivated as an ornamental and named cultivars are commercially recognized. This makes human-associated establishment, escape, planting history or introgression plausible hypotheses for some geographically unusual populations, but none can be inferred from flower colour alone.

We therefore avoid defining an anthropogenic anomaly from human variables or from a large model residual. Instead, we define a biologically interpretable local event before human context is examined: a pigmented cell embedded among geographically close, environmentally similar white neighbours. The same event extractor can then be replayed on repeated maps from the natural model. Only after candidate locations are fixed do we ask whether population density, densely inhabited districts, roads or land use distinguish them. This converts a vague horticultural story into a bounded, falsifiable follow-up question.

The paper thus follows a **broad -> fine -> anomaly** hierarchy. It is not a sequence of increasingly complicated models. It is a sequence of changing ecological questions. First, quantitative colour data are used to establish the broad environmental and spatial template. Second, the observation scale is narrowed to test a local pollinator hypothesis without reinterpreting a national SDM overlap as causation. Third, the object of inference changes from a model coefficient to a repeatable ecological event, which is then characterized for human context. In this way, each layer removes one ambiguity before the next layer is opened.

## 2 | Materials and Methods

### 2.1 | YAMAP sampling frame and author review

The source table contained 1,965 eligible *C. punctata* records recovered from publicly accessible YAMAP activity records during the flowering seasons of 2023-2025. YAMAP activities are GPS-linked hiking records in which photographs can be associated with positions along the recorded activity. Records without usable public coordinates could not enter the geographical analyses. We retained the activity/route provenance rather than treating each photograph as an areally random presence record.

Before model fitting, the author reviewed the focal organism and flower, confirmed the petal region used for image phenotyping, removed taxonomic errors and audited repeated photographs. Exact image-hash duplicates were resolved using stable source-row order. These steps reduce identification and image-processing errors but do not remove hiking-route, access or subject-selection bias. After analysis-specific data-availability filtering, the final two-part phenotype contained 1,922 observations in 1,305 one-kilometre cells.

Records were not selected using flower colour, fitted residuals, *Bombus* predictions or human-landscape variables. The analytical population is therefore the retained route-linked photographic sample rather than a random sample of all Japanese *C. punctata* populations.

### 2.2 | Visible-colour extraction and two-part phenotype

The deterministic image pipeline measured the author-confirmed petal region from display-referred sRGB pixels. The channel-wise median RGB value was converted to CIELAB under D65 and a 2-degree standard-observer convention, and a* was used as the primary red-green optical phenotype. Image-specific radiometric calibration and white balance were impossible because neutral standards and standardized camera metadata were unavailable. The phenotype is therefore a reproducible image-derived visible-colour measure, not calibrated reflectance, ultraviolet contrast or anthocyanin concentration.

The white-pigmented boundary was estimated from a* alone without geography, date, environment, *Bombus*, human context or fitted-model information. We fitted univariate Gaussian mixtures under equal- and variable-variance parameterizations, selected the model by BIC, ordered components by fitted mean and collapsed the mixture at the largest adjacent component-mean gap. The final analysis selected a four-component variable-variance mixture. The operational boundary was a*=4.968780, yielding 966 white-like and 956 pigmented observations. A total of 124 observations had intermediate regime-membership probability and were retained with an ambiguity flag.

We then analysed (1) **pigmentation state**, a binary response for all 1,922 observations, and (2) **conditional visible intensity**, standardized a* only among observations assigned to the pigmented regime. Variation within the white-like regime was not interpreted as pigment intensity.

### 2.3 | Environmental predictors and national environment-plus-space models

Climate variables came from CHELSA v2.1, soil variables from SoilGrids 2.0 and elevation from WorldClim 2.1; terrain derivatives were calculated from elevation (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Response-blind principal components summarized correlated environmental structure, with additional seasonality and radiation terms retained where defined by the frozen pipeline.

Separate observation-level INLA-SPDE models were fitted for pigmentation state and conditional visible intensity. Pigmentation state used a Bernoulli likelihood with logit link; conditional intensity used a Gaussian likelihood. Environmental variables entered as fixed effects and a Matern SPDE field represented continuous residual geography (Lindgren et al., 2011). Penalised-complexity priors regularized spatial range and marginal variance (Simpson et al., 2017).

A cell-level predictive natural reference was fitted separately for later local questions. Pigmented counts were modelled as binomial trials and conditional intensity as Gaussian among cells with pigmented observations. Five response-blind geographical folds based on approximately 100-km blocks evaluated transfer to withheld geography (Roberts et al., 2017; Valavi et al., 2019). Repeated predictive maps retained each cell's observed trial count and were used as the natural reference for the anomaly analysis.

### 2.4 | Fresh *Bombus* SDMs and occurrence-referenced support

Fresh SDMs were built for *B. ardens*, *B. diversus*, *B. beaticola*, *B. consobrinus* and *B. honshuensis* over a common Honshu-Shikoku-Kyushu domain using shared environmental predictors, `maxnet`, spatial block partitions in ENMeval and the finite-AICc-selected model for each species. Predictions were expressed on the cloglog scale. Cross-validation AUCs were 0.756, 0.601, 0.913, 0.897 and 0.870, respectively. These surfaces estimate habitat support, not abundance, visitation, pollen transfer or pollination effectiveness (Renner & Warton, 2013; Guillera-Arroita, 2015).

Because raw SDM values are not directly comparable among species, each species surface was mapped to an occurrence-referenced support scale. For species k, fitted cloglog support at the occurrence cells used in its selected SDM defined an empirical cumulative distribution function F_occ,k. Flower-cell support s_k was transformed to A_k=F_occ,k(s_k). This quantity reports how a flower-cell prediction ranks relative to predictions at that species' observed occurrence cells; it is not occurrence probability.

The primary directional exposure was

`effective_occmax = max(A_ardens, A_diversus)`.

An initial maximum across all five within-species ranks proved structurally uninformative: its minimum was already 0.489, because different species replace one another geographically. All-five metrics were therefore retained as sensitivities rather than treated as a national measure of pollinator availability.

### 2.5 | Fine-scale sharp-transition test

The main pollinator analysis intentionally changed scale. We did not fit a national environment + space + *Bombus* model and did not fit a second local SPDE regression. Instead, at the one-kilometre-cell level we constructed a graph using up to the five nearest neighbours within 5 km and identified **pure white-versus-pigmented transitions**, defined by an absolute difference in observed cell pigment share of one.

Pair selection did not use *Bombus* values or the sign of the flower-colour difference. To reduce endpoint reuse, pairs were selected greedily without overlap, prioritizing transition magnitude and then geographical distance. Only after the transition set was fixed were pairs oriented from the whiter to the more pigmented endpoint. For each pair we calculated

`Delta A = effective_occmax_pigmented - effective_occmax_white`.

The primary statistic was mean Delta A across non-overlapping pairs. A 100,000-replicate sign-flip randomization conditioned on observed pair magnitudes and asked whether *Bombus* contrasts aligned with flower-colour orientation more than expected under exchangeable direction. Environment did not select or orient the pairs. It was used only as a diagnostic by comparing environmental-PC distances of selected transitions with all local graph edges.

Sensitivity analyses repeated the design at 10 and 25 km, relaxed the transition threshold to 0.75 and 0.50 and substituted raw-cloglog or broader all-five support. Because this refinement followed exploratory development, multiplicity summaries and all null sensitivities are retained.

### 2.6 | Supplementary community turnover and montane/alpine guardrails

Five-species community turnover was treated as a supplementary biogeographic question rather than a colour mechanism. For each local edge, species support was normalized to relative composition and Hellinger turnover was calculated. Pure flower-colour transition edges were compared with nearby non-transition controls selected without *Bombus* information and matched on edge length, midpoint elevation and absolute elevation difference. Matching windows of 25, 50 and 100 km and several control-set sizes were retained as sensitivities. Fixed 100-km spatial blocks and shifted grids assessed whether the correspondence was carried by one region.

The high-elevation group (*B. beaticola*, *B. consobrinus*, *B. honshuensis*) was analysed only as a guardrail. If their apparent association with pigmentation simply reflects shared high-elevation geography, it should disappear when white and pigmented endpoints occur at nearly equal elevations. We therefore repeated pure-transition contrasts under <=50-m and <=100-m elevation-difference restrictions. No mechanistic claim about montane substitution was allowed unless an additional signal remained after this guardrail.

### 2.7 | Event-based local departures and human context

The anomaly analysis used an ecological event rather than a residual threshold. The primary event was a pigmented cell with at least three neighbours within 10 km, environmental distance <=1 in the predefined multiscale environmental summary and zero pigmented neighbours. Human variables did not enter this definition.

The identical event extractor was replayed on 10,000 held-out cross-fitted natural predictive maps and, as a sensitivity, 200,000 joint posterior-predictive maps. This asks how often the fitted natural geography itself generates a locally discordant pigmented-in-white configuration under the same sampling geometry.

After candidate identities were fixed, human context was characterized using WorldPop population, densely inhabited district (DID) proximity/alignment, land use and roads. Multiple comparisons within feature families were evaluated with maxT familywise correction. These analyses characterize candidate context only and cannot establish planting, escape, introgression or horticultural provenance.

### 2.8 | Reproducibility and analysis hierarchy

The final manuscript hierarchy was locked before final integration reporting:

1. **Main 1 - Broad natural template:** quantitative two-part phenotype + national environment and continuous space;
2. **Main 2 - Fine-scale pollinator hypothesis:** sharp local transitions versus occurrence-referenced *B. ardens* + *B. diversus* availability;
3. **Main 3 - Event-based anomaly screen:** repeated-natural-map departures + post-selection human context;
4. **Supplement:** five-species community turnover, montane/alpine guardrails and full availability/anomaly/human sensitivities.

All manuscript-facing numerical claims are linked to checksum-locked GitHub Actions artifacts and fixed seeds. Historical five-species limitation gates and earlier environment+SPDE *Bombus* analyses are retained as provenance rather than current estimands.

## 3 | Results

### 3.1 | Quantitative image phenotyping separates colour state from intensity

The final 1,922 photographs separated nearly evenly into 966 white-like and 956 pigmented flowers. The response-blind four-component variable-variance mixture placed the operational boundary at a*=4.9688, and 124 photographs had intermediate classification confidence. Conditional visible intensity was analysed only within the pigmented regime.

### 3.2 | Broad environment and space define a strong national template

Pigmentation state declined along the temperature axis (posterior mean -0.542; 95% CrI -1.033 to -0.049). Conditional visible intensity also declined along temperature (mean -0.319; CrI -0.484 to -0.156) and along the topographic axis (mean -0.138; CrI -0.231 to -0.045). Other fixed effects were more uncertain after conditioning on the spatial field.

Substantial residual geography remained. Posterior mean SPDE range was 132.7 km (95% CrI 88.7-195.6) for pigmentation state and 60.9 km (31.0-115.8) for conditional intensity. Five-fold prediction discriminated pigmentation state well across withheld geography (image-level AUC=0.863; cell any-pigmented AUC=0.858; cell majority-pigmented AUC=0.871; Brier=0.150). Conditional intensity had RMSE=0.919, MAE=0.715 and 95% predictive coverage=0.944 across 674 pigmented-support cells. These metrics establish a predictive natural template; they do not imply that measured environment explains most biological variance.

### 3.3 | The sharpest local transitions show weak directional alignment with focal *Bombus* availability

The strict 5-km graph yielded 67 non-overlapping pure white-versus-pigmented transitions with median geographical separation 2.0 km. Median environmental-PC distance was 0.180 among selected transitions compared with 0.343 among all local 5-km edges. Thus the transition set was not environmentally more divergent than generic nearby pairs; in the measured summary it was more similar.

Using occurrence-referenced *B. ardens* + *B. diversus* support, mean predicted availability was 0.0359 higher on the pigmented side (one-sided sign-flip P=0.0272). However, the median contrast was -0.0028 and only 49.3% of pairs had higher availability on the pigmented side. Correction across the 5-, 10- and 25-km pure-transition family gave q=0.0815. The signal attenuated to +0.0084 at 10 km (P=0.325; n=109) and +0.0029 at 25 km (P=0.436; n=171), and raw-cloglog effective support did not reproduce the 5-km result (mean +0.0044; P=0.267). The occurrence-referenced 5-km result persisted under a 0.75 transition threshold but disappeared at 0.50.

We therefore interpret the main pollinator result as **weak and highly local consistency** with the pigmentation-benefit relaxation hypothesis. It is magnitude-driven by a subset of transitions rather than a majority-of-pairs pattern and does not establish pollinator-mediated selection. No persuasive evidence linked *Bombus* availability or community turnover to conditional visible intensity among already pigmented flowers.

### 3.4 | High-elevation *Bombus* overlap does not provide an additional pollinator effect

The apparent geographical association between montane/alpine *Bombus* support and pigmented flowers disappeared when endpoints were constrained to nearly equal elevations. For pure transitions with <=50 m elevation difference, mean pigmented-minus-white montane support was -0.0033, -0.0020 and -0.0059 at 5, 10 and 25 km, respectively; all one-sided P-values were >=0.755. Results under <=100 m were likewise near zero or negative.

This negative guardrail is biologically important. It indicates that the visually striking overlap between high-elevation bumblebee niches and high-elevation pigmented flowers is adequately explained by shared elevational geography in the current data. It therefore should not be recycled as evidence that montane species maintain pink colour.

### 3.5 | Five-species turnover marks a supplementary biogeographic boundary correspondence

Pure flower-colour boundaries also tended to show larger predicted five-species *Bombus* compositional change than nearby spatial/elevationally matched non-transition edges. Under the fixed occurrence-referenced Hellinger comparison, mean matched excess turnover was +0.0330 at 5 km (P=0.0628), +0.0327 at 10 km (P=0.0142) and +0.0411 at 25 km (P=0.0001). The direction remained positive across alternative matching windows and was not carried by one 100-km geographical block.

Because Hellinger turnover is unsigned, it does not predict which community should favour white or pigmented flowers. We therefore treat this as a supplementary **biogeographic correspondence between flower-colour boundaries and predicted pollinator-community boundaries**, not a directional flower-colour mechanism.

### 3.6 | Local departures are reproducible locations but not a robust excess beyond the natural model

The primary 10-km event definition identified 17 pigmented cells embedded among environmentally similar white neighbourhoods. Replaying the same event on 10,000 held-out natural maps gave a null mean candidate count of 13.61 and upper-tail P=0.200. Observed candidate fraction was 0.0474 versus null mean 0.0343 (P=0.0874). Under the joint spatial posterior-predictive sensitivity, count and fraction P-values were 0.314 and 0.196.

Thus the observed event is not demonstrably more frequent than the fitted natural geography can generate. The 17 cells nevertheless remain a reproducibly defined set of local mismatches and concrete field targets. Their scientific value is not that each has a large fitted residual, but that all were selected by the same biologically interpretable local rule and evaluated against the same rule on repeated natural maps.

### 3.7 | Human context is suggestive but familywise-inconclusive

The strongest human-context contrasts involved population. The focal-minus-white-neighbour population-rank contrast at 5 km was +0.0531 (directional P=0.0270; maxT familywise P=0.0899). Population-DID alignment was +0.0516 (directional P=0.0230; maxT P=0.0759). Other population scales and DID features were weaker.

Accordingly, human-modified context is suggestive rather than decisive. The analysis does not establish horticultural origin, planting or introgression.

## 4 | Discussion

### 4.1 | The main contribution is a sequence of ecological contrasts, not a list of predictors

The flower-colour geography of *C. punctata* becomes clearer when the question changes with scale. At the broadest layer, environment and continuous spatial structure describe where pigmentation states and intensities occur across Japan. At the second layer, species-specific natural history motivates a local directional pollinator test at abrupt boundaries rather than a national map correlation. At the final layer, the object of inference changes again: instead of a coefficient or residual, a repeatable local colour-state event is identified and then characterized for human context.

This hierarchy avoids an attractive but misleading omnibus analysis. If climate, topography, a spatial field, five environmentally generated *Bombus* SDMs and human variables were placed in the same national model, the resulting coefficients would compete over strongly shared geography. A statistically significant coefficient would not necessarily identify a biologically independent mechanism. Our approach instead asks each question where its comparison is most interpretable. The design is therefore an ecological argument expressed through statistical choices, rather than a succession of models chosen because they are available.

### 4.2 | YAMAP is not merely a source of more points: it extends the observation process available for trait ecology

Repurposing YAMAP is a methodological contribution in its own right. Most digital biodiversity analyses begin with records that users intentionally submitted as biodiversity observations. YAMAP begins with a different behaviour: people document hikes. Photographs are organized by activities and routes and can retain photographed positions along GPS tracks. This allows incidental plant photographs to be recovered from a digital stream generated for recreation rather than ecological recording, fitting the broader logic of iEcology (Jarić et al., 2020).

The value is not that YAMAP is intrinsically less biased than a biodiversity portal. Its biases are different and unusually transparent for mountain ecology. Sampling is concentrated on accessible hiking routes and mountains, users can suppress location information, and subject choice remains opportunistic. Conversely, route structure supplies spatial provenance that is useful for clustering, duplicate audits and local neighbourhood analyses. Human review can verify the focal taxon and petal region before image values are treated as phenotypes. Together, these features make YAMAP especially useful for geographically structured mountain traits when its sampling frame is stated explicitly.

This distinction matters for reproducibility. We do not infer a species range from anonymous images or use photographs without coordinates. The analysis is tied to retained public coordinates, documented image screening, deterministic colour extraction and versioned source-row provenance. The resulting framework could be adapted to other hiking, outdoor recreation or route-tracking platforms wherever photographs and spatial provenance can be recovered ethically and reproducibly.

### 4.3 | The broad analysis closes a measurement gap even before the more exploratory layers

The national environment-plus-space analysis is intentionally conventional, but the data object is not. Previous range-wide studies using discrete colour morphs can reveal strong geographical clines, yet cannot ask whether the intensity of already pigmented flowers follows the same environmental gradients. Conversely, spectroscopy can resolve continuous colour variation but is difficult to scale across a national range. The two-part image phenotype links these approaches: it preserves the biologically conspicuous white-pigmented state transition while retaining quantitative information within the pigmented state.

The results justify that distinction. Temperature covaried with both pigmentation state and conditional intensity, whereas topography was additionally associated with intensity, and residual spatial ranges differed descriptively between the two responses. A single white-to-pink numeric axis would have hidden this structure. Explicit SPDE modelling also prevents spatially clustered records from being treated as independent replicates and makes the unresolved geographical component visible rather than silently assigning it to measured environment.

Thus Main 1 is not merely a baseline inserted before the more novel analyses. It fills two methodological gaps at once: large-scale quantitative flower-colour measurement from photographs and explicit representation of spatial structure in a range-wide environmental analysis.

### 4.4 | The local *Bombus* analysis uses scale to reduce a circular geographical comparison

The most tempting pollinator analysis would be to overlay national flower colour and national *Bombus* suitability. It would also be the hardest to interpret. The same broad climate that predicts bumblebee distributions also structures flower colour, and high-elevation bumblebee niches overlap high-elevation pigmented flowers. We therefore deliberately refuse to treat broad map overlap as a pollinator effect.

Instead, we first let the broad model describe national geography and then zoom to the scale at which pollinator-mediated selection should plausibly act. The transition pairs were geographically close, had a median separation of only 2 km and, diagnostically, were more environmentally similar than generic 5-km edges. Pair selection was blind to *Bombus*. Only after sharp colour boundaries were fixed did we ask whether the availability contrast pointed from white toward pigmented.

This is not equivalent to proving an environment-independent bumblebee effect. SDM support remains generated from environmental predictors, and unmeasured microenvironment or shared distribution history can remain. The advantage is narrower: the analysis no longer asks whether two broad geographical surfaces overlap. It asks whether local biological boundaries exhibit a directional correspondence that is predicted in advance by the focal natural-history hypothesis.

The result is appropriately modest. The mean 5-km contrast was positive, but the median, sign proportion, raw-SDM sensitivity and broader scales prevent us from calling it robust. That weakness should remain visible in the paper. What makes the analysis informative is not a threshold-crossing P-value but the match between ecological hypothesis, comparison scale and explicit failure conditions.

### 4.5 | Why *B. ardens* and *B. diversus* are the main exposure and montane taxa are a guardrail

Restricting the primary exposure to *B. ardens* and *B. diversus* is an estimand decision, not a claim that other bumblebees never visit *C. punctata*. These two broadly distributed taxa have direct evidence as predominant pollinators in the focal system and therefore support a directional availability hypothesis. Adding *B. beaticola*, *B. consobrinus* and *B. honshuensis* changes the question from availability of documented broad focal pollinators to replacement among lowland and high-elevation bumblebee niches.

The data show why that distinction matters. A five-species maximum is almost never low because species replace one another geographically. Moreover, the apparent positive association of montane/alpine support with pigmented flowers disappears when white and pigmented endpoints are constrained to nearly equal elevations. The high-elevation association is therefore best read as shared biogeography, not an additional pollinator effect.

This negative result also clarifies the role of the five-species turnover analysis. Turnover is useful as a map of where flower-colour and pollinator-community boundaries coincide, and its spatially matched signal is stronger than the directional availability result at some scales. But an unsigned community distance cannot explain why one side should be white and the other pigmented. Statistical strength therefore does not determine narrative priority. The main analysis is the one with the clearest directional ecological meaning; turnover remains a supplementary description of biogeographic boundary correspondence.

### 4.6 | Event-based departures provide a stricter bridge to the human-context question

The final layer follows a similar logic of changing the inferential object. Large model residuals are not automatically biological anomalies: they can reflect uncertainty, leverage, sampling density or local interpolation. Our event definition instead describes something ecologically tangible—a pigmented locality embedded in a nearby, environmentally comparable white neighbourhood—and can be applied identically to observed and simulated maps.

The natural model can generate such configurations with non-negligible frequency, so the 17 observed candidates do not demonstrate an unexplained process. This is an important negative result. It prevents the human-context analysis from beginning with a set of locations declared anomalous simply because their residuals were large.

Because *C. punctata* is cultivated ornamentally, human-associated origin remains a plausible follow-up hypothesis for individual sites. Population and DID associations point weakly in that direction but do not survive familywise correction. The candidate set is therefore best viewed as a field-prioritization product: places where vouchers, planting history and population genetics would be especially informative.

The YAMAP sampling frame also helps interpret this weak human signal. Sampling is concentrated on mountain and hiking-route geography rather than the full urban-rural continuum, which can compress the range of human modification represented in the dataset and reduce power to detect a broad anthropogenic gradient. At the same time, trailheads, roads and accessible mountain edges can create the opposite observation-opportunity bias. These two forces mean that a null or weak human-context signal should not be interpreted as evidence that human influence is absent. It is evidence that the present mountain-route sample does not support a stronger claim.

### 4.7 | Limits and next tests

The first limitation is measurement. CIELAB a* from uncalibrated photographs is not floral reflectance, ultraviolet contrast or pigment concentration. The mixture boundary is operational and 124 observations remain ambiguous. Spectroscopy, anthocyanin assays and standardized imaging are required to connect the image phenotype to biochemical mechanisms.

The second limitation is sampling. YAMAP is route-biased, GPS and public-location availability vary, and author review does not remove observer choice. These constraints should be treated as properties of the sampling frame rather than hidden as noise.

The third limitation is pollinator inference. Occurrence-referenced SDM support is not abundance, floral visitation or selection pressure. The 5-km result was refined after exploratory work and is not robust to every exposure or scale. Direct species-resolved visitation, first approaches, stigma contact, pollen removal and deposition and reproductive success are required to test whether pigmentation actually changes fitness through bumblebee attraction.

Finally, the anomaly layer does not establish horticultural provenance. The appropriate next evidence is population genetics among candidate sites, neighbouring wild populations and horticultural material, combined with vouchers and local planting histories.

### 4.8 | Broader methodological implication

The broader contribution is a way to extract more ecological meaning from opportunistic digital imagery without pretending that one model can solve every source of confounding. First, repurposed digital traces can expand quantitative trait sampling. Second, a phenotype can be represented in biologically distinct components rather than inherited categories. Third, spatial structure can be modelled at the broad scale where it is strongest. Fourth, a biotic hypothesis can be tested by changing scale and comparison unit rather than by adding another geographically structured predictor. Fifth, local departures can be defined as repeatable ecological events and calibrated against repeated natural maps before candidate explanatory variables are inspected.

These steps are portable beyond *C. punctata*. What is species-specific is the ecological logic that connects them: bumblebee-dependent pollination makes the local availability hypothesis meaningful, the white-pigmented mosaic supplies sharp boundaries, and ornamental cultivation motivates the final human-context screen. Generality therefore comes from the design principle, not from pretending that every species should use the same predictors.

## 5 | Conclusions

The flower-colour geography of *C. punctata* can be read as a sequence of layers rather than a competition among coefficients. Repurposed YAMAP hiking photographs supplied a national, GPS-linked image sample from which a two-part quantitative colour phenotype could be recovered. Broad environmental and spatial models then established the main geographical template for pigmentation state and conditional visible intensity.

Within that template, the sharpest local white-pigmented transitions showed weak directional consistency with predicted availability of the documented broad focal bumblebee pollinators *B. ardens* and *B. diversus*. The result is too fragile to establish pollinator-mediated selection, but its ecological direction is explicit and its scale avoids treating national niche overlap as mechanism. High-elevation bumblebee taxa add no effect beyond shared elevational geography in the present data and are therefore retained only as a guardrail.

Finally, local departures were treated as repeatable ecological events rather than residual tails. Seventeen pigmented-in-white neighbourhoods were reproducibly identified, but their frequency was compatible with natural predictive maps and their human-context associations remained familywise-inconclusive. They are field targets rather than evidence of anthropogenic origin.

The main novelty is therefore not that environment, pollinators and humans were all analysed. It is that **data source, phenotype representation, spatial scale and comparison unit were changed deliberately as each ecological question became more specific**. This layered design turns a large opportunistic photographic dataset into a sequence of testable ecological hypotheses and provides a reproducible route for studying geographical trait variation from digital records originally created for other purposes.

## Data Accessibility Statement

The source-population audit, derived colour tables, environmental source registry, fresh *Bombus* SDM configuration, occurrence-referenced support transformation, seeds, local-transition specifications, anomaly-event definitions, result tables and workflow provenance are versioned in the project repository. Original YAMAP photographs are third-party content and are not redistributed. Manuscript-facing numerical claims are linked to checksum-locked GitHub Actions artifacts in `reproducibility/final_paper_pipeline_2026-08-09.md`.

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

Royal Horticultural Society. (2026). *Campanula punctata* plant profile. RHS Plants. Accessed 9 August 2026.

Simpson, D., Rue, Riebler, A., Martins, T. G., & Sørbye, S. H. (2017). Penalising model component complexity: A principled, practical approach to constructing priors. *Statistical Science*, 32, 1-28. https://doi.org/10.1214/16-STS576

Surmacz, B. (2023). Using citizen-science photographs and machine learning to map flower-colour morph geography. *Plant Biology*, 25, 681-686. https://doi.org/10.1111/plb.13537

Tasaki, K., et al. (2022). Identification of candidate genes responsible for flower colour intensity in *Gentiana triflora*. *Frontiers in Plant Science*, 13, 906879. https://doi.org/10.3389/fpls.2022.906879

Trunschke, J., Lunau, K., Pyke, G. H., Ren, Z.-X., & Wang, H. (2021). Flower color evolution and the evidence of pollinator-mediated selection. *Frontiers in Plant Science*, 12, 617851. https://doi.org/10.3389/fpls.2021.617851

Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019). blockCV: An R package for generating spatially or environmentally separated folds for k-fold cross-validation. *Methods in Ecology and Evolution*, 10, 225-232. https://doi.org/10.1111/2041-210X.13107

van der Kooi, C. J., Elzenga, J. T. M., Staal, M., & Stavenga, D. G. (2016). How to colour a flower: On the optical principles of flower coloration. *Proceedings of the Royal Society B*, 283, 20160429. https://doi.org/10.1098/rspb.2016.0429

Westerband, A. C., Funk, J. L., & Barton, K. E. (2021). Intraspecific trait variation in plants: A renewed focus on its role in ecological processes. *Annals of Botany*, 127, 397-415. https://doi.org/10.1093/aob/mcab011

YAMAP Magazine Editorial Team. (2022). How to use YAMAP during a hike: GPS activities and photographs on the route map. YAMAP Magazine; updated 22 May 2024.
