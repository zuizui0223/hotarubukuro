# From broad geography to local boundaries: biogeography of flower-colour polymorphism from hiking photographs

**Running title:** Flower-colour polymorphism

## Abstract

**Aim:** Geographical trait variation can reflect abiotic environment, unresolved history, biotic interactions and human context at different scales. We asked how these processes assemble flower-colour polymorphism while separating pigment presence from intensity after pigmentation is present.

**Location:** Japan.

**Taxon:** spotted bellflower (*Campanula punctata*).

**Methods:** We converted 1,922 author-screened YAMAP photographs into pigmentation state and pigmented-only intensity. Final environment + INLA-SPDE models estimated directional environmental associations and residual geography. Using already fixed held-out pairs and 500 cached space-only predictive maps, we then asked whether distance along the environmental terms supported by each final model ordered phenotype divergence beyond spatial continuity. Sixty-seven independently fixed local boundaries supplied the Bombus comparison. Human context was tested with same-colour isolation in all 1,305 cells and 10,000 natural predictive maps.

**Results:** Pigmentation was less likely toward warmer Temperature PC1. Temperature-separated locations were also more different in pigmentation state than fixed spatial continuity predicted (excess=0.0521; P=0.010), whereas the combined supported-term distance for conditional intensity did not exceed spatial expectation (P=0.263). At local boundaries, a positive mean focal-Bombus contrast coexisted with a near-zero median and 49.3% positive pairs. Among 674 pigmented cells, isolation correlated with 5-km population exposure more strongly than natural maps expected in raw (rho=0.252; P=0.0002) and density-corrected form (rho=0.285; P=0.0009).

**Main conclusions:** Quantitative phenotyping resolved a temperature-aligned white-pigmented switch from conditional variation within pigmented flowers. Local Bombus correspondence was heterogeneous rather than pervasive, and human context appeared as an excess positive isolation-population relationship within pigmented occurrences rather than proof of horticultural origin. Integrative trait biogeography gains resolution by connecting, rather than collapsing, processes operating at different scales and comparison units.

**Keywords:** digital phenotyping, flower colour, hiking photographs, iEcology, intraspecific variation, pollination, spatial confounding, trait biogeography

## 1. Introduction

### 1.1 The geographical mystery of flower-colour polymorphism

Why does one species remain white in some parts of its range and pigmented in others? This is not only a descriptive question. Intraspecific trait variation can alter individual performance, population persistence and species interactions, and different causes predict different responses to environmental change (Westerband et al., 2021). Yet a geographical pattern is easy to overinterpret. Climate, population history, pollinators, dispersal and human activity are all spatially structured, so several processes can draw similar trait maps.

Flower colour makes this problem especially revealing because anthocyanin pigmentation can affect both physiology and reproduction. Pigments change optical absorption and can contribute to thermal and water-stress responses, while the visible flower also acts as a signal to pollinators. A pigmented morph may therefore gain physiological or reproductive benefits in one setting while paying thermal, hydraulic or biosynthetic costs in another (Warren & Mackenzie, 2001; Kellenberger et al., 2019; Trunschke et al., 2021; Li et al., 2026). Spatial variation in that balance offers a plausible route by which white and pigmented flowers persist within one species.

The spotted bellflower (*Campanula punctata* Lam.; Campanulaceae) is unusually tractable for separating these processes. Its large tubular flowers vary conspicuously from white to pigmented across Japan, bumblebees are effective visitors, and the species is also cultivated. The ecological need is therefore not simply to add more predictors to one national regression, but to connect evidence while preserving the scale and comparison unit at which each process has a defensible biological meaning.

### 1.2 The first hidden layer: state is not intensity

The apparent white-to-dark continuum contains at least two observational questions. A white-pigmented transition can reflect whether the anthocyanin pathway is visibly expressed at all. Variation among flowers that are already pigmented can instead reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). A process that changes whether pigmentation is switched on need not control how dark the flower becomes afterwards. We therefore separated pigmentation state from visible intensity before asking what geography explains either response.

This split gives environmental hypotheses more precise targets, but it does not imply a universal stress-darkening rule. Moderate low temperature can activate chalcone-synthase expression and anthocyanin accumulation in corollas, and broad comparative data associate floral pigmentation with cool or arid settings (Shvarts et al., 1997; Sullivan & Koski, 2021). Darker petals can also absorb more radiation. That absorption may be useful in cool conditions yet impose heat and water costs in warm conditions because floral cooling can require transpiration (Li et al., 2026). Water limitation can favour anthocyanin morphs in some systems, but its direction is taxon and context dependent (Warren & Mackenzie, 2001). In *C. punctata*, UV-B exclusion alters flavonoid accumulation in leaves (Hashiba et al., 2006), showing environmental responsiveness without establishing the petal mechanism. Temperature therefore supplied the strongest directional expectation, while moisture, seasonality, radiation and terrain were treated as interacting contexts rather than interchangeable stress indices.

### 1.3 A new observation stream makes the national phenotype visible

For this species, occurrence coordinates alone could not answer either colour question, and matched public image streams were too sparse for dense national, boundary and neighbourhood comparisons. We therefore built the phenotype from YAMAP, a hiking-navigation and activity-diary platform. Every candidate was screened for taxon identity, focal flower and usable petal region; duplicates and coordinate-image correspondence were audited; and colour was extracted with one deterministic pipeline. During the matched 2023–2025 window, YAMAP yielded 1,964 georeferenced focal-species records, compared with 516 iNaturalist observations with photographs—a 3.81-fold difference. The three YAMAP years were also closely balanced.

YAMAP is not spatially random. Its mountain-route frame enriches natural and semi-natural settings in which self-sustaining populations are plausible, but it does not prove wild provenance for any record. Nor can route conditioning be assumed to weaken human associations in only one direction. It can compress the represented urban-rural gradient, while trailheads, roads and accessible mountain margins can increase observation opportunity, disturbance and opportunities for human-mediated movement. Its contribution here is therefore an alternative observation process that could be curated into a recent quantitative-trait dataset, not a claim of globally superior or unbiased citizen-science data (Jarić et al., 2020; Appendices S1–S2).

Making the national pattern visible did not solve causal attribution; it exposed it. Climate and terrain are spatially structured, unresolved history is spatially structured, and Bombus SDMs are built from environmental predictors. We therefore first estimated the broad environmental and continuous spatial template. We then asked whether the environmental terms supported by that final model organized held-out phenotype divergence beyond a fixed space-only expectation. Only after those broad layers were established did we change comparison unit for local pollinator and human-context questions.

### 1.4 From broad geography to local boundaries and human overlays

The local pollinator test has a species-specific natural-history basis. The large tubular flowers restrict the plausible set of effective visitors, and geographical changes in *Bombus* fauna have been linked to pollen-removal ecology and breeding-system change in *C. punctata*, including the bumblebee-absence hypothesis developed from the Izu Islands (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). The Izu evidence motivates why bumblebees are focal; it does not show that bumblebee absence caused white flowers in the present data.

A national flower-colour–Bombus regression would remain ambiguous because both maps inherit climate and mountain geography. We therefore fixed 67 non-overlapping white-pigmented boundaries within 5 km before reading Bombus values, environmental values or contrast direction. The 5-km radius was the finest predeclared scale with enough replicated transitions, not an exact foraging or selection distance. Mean, median and sign proportion were all required because a minority of large local contrasts could raise the mean. Montane and alpine species were retained as a guardrail: if their stronger-looking national overlap disappeared after elevation matching, shared mountain geography—not a second pollinator mechanism—would be the defensible interpretation.

The broad natural template also makes contemporary human context testable without first declaring a small set of anomalies. For every 1-km flower cell, we measured distance to the nearest other cell with the same colour state and scaled that distance by local spacing of all flower cells. We then replayed the full geometry on 10,000 predictive maps. This all-cell analysis was developed after inspection of earlier human-context patterns and is explicitly post hoc. The earlier 16-event detector is retained in Appendix S6 to calibrate extreme local configurations and preserve field/provenance targets, not as the statistical foundation of the Main human-context claim.

### 1.5 Predictions and exploratory human-context question

The confirmatory sequence contained two dependent predictions. First, pigmentation state and pigmented-only intensity should show different broad environmental and spatial geography. Second, if focal bumblebees contribute locally to maintaining visible pigmentation, at least some nearby white-pigmented boundaries should show greater habitat support on the pigmented side; a uniform national or intensity-wide effect was not required.

Two supporting questions then tested how much stronger those interpretations could become. A model-informed distance analysis asked whether separation along the environmental terms supported by each final model ordered held-out phenotype divergence beyond fixed spatial continuity. The all-cell isolation analysis asked whether pigmented occurrences farther from other pigmented occurrences were more population-exposed than fitted natural geography predicted, after correcting for local flower-cell spacing. The latter is reported as an exploratory generalisation, not as preregistered confirmation.

## 2. Materials and Methods

### 2.1 Study system and YAMAP sampling

The sampling frame covered the 2023–2025 flowering seasons. We screened all YAMAP records returned by the fixed Japanese focal-name search frame rather than subsampling candidates. Taxonomic errors and non-focal flowers were removed, and the focal flower and usable petal region were confirmed. Duplicate-image and raster-completeness checks left 1,922 observations for environmental analysis. Appendices S1–S2 give the complete record flow, matched public-database benchmark and sampling limitations.

### 2.2 From photographs to a two-part phenotype

The image pipeline retained source provenance, date, coordinates and image hashes. Display-referred sRGB pixels within the confirmed petal region were summarized and median RGB was converted to CIELAB under D65. CIELAB a* is used here as a reproducible human-visible red-green phenotype, not as calibrated spectral reflectance, UV contrast or anthocyanin concentration.

The white-pigmented boundary was estimated from a* alone before geography, environment, Bombus predictions or human data were used. Gaussian-mixture classification gave an operational boundary of a*=4.9688. The final dataset contained 966 white-like and 956 pigmented observations. Pigmentation state was analysed across all 1,922 observations, and standardized visible intensity was analysed only among pigmented flowers.

Intensity is therefore a conditional response. If measured or unmeasured factors influence both entry into the pigmented subset and intensity within it, conditioning on pigmentation can alter the composition of the analysed subset. Separating state from intensity remains necessary to avoid treating white flowers as merely very pale pigmented flowers, but the two fitted parts are not interpreted as causally independent pathways. Appendix S2 reports mixture, ambiguity and phenotype sensitivities.

### 2.3 Broad environmental and continuous spatial geography

Climate predictors came from CHELSA v2.1, soils from SoilGrids 2.0 and terrain from WorldClim-derived elevation products (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Eight response-blind axes represented warm-season temperature, climatic moisture, temperature seasonality, precipitation seasonality, terrain, two soil axes and shortwave radiation. Temperature represented regulatory and thermal-balance hypotheses; moisture represented both possible pigment benefit and hydraulic context; RSDS represented broad shortwave exposure rather than UV-B; and ruggedness represented relief, aspect, drainage and microclimatic heterogeneity rather than one monotonic stress axis.

Analyses were run in R 4.5.3 (R Core Team, 2026); latent Gaussian and SPDE models used the 'INLA' package version 25.10.19 (Rue et al., 2009). Pigmentation state used a Bernoulli likelihood and conditional intensity a Gaussian likelihood. Both final observation-level models retained all eight environmental axes, an East/West structural adjustment and a stationary Matérn SPDE field (Lindgren et al., 2011; Simpson et al., 2017). The intensity model additionally retained Temperature PC1 × temperature seasonality. The field represents coherent geography remaining after measured environment and may combine unmeasured environment, population structure, dispersal and sampling geometry; its range is not a dispersal distance or genetic boundary. Model extensions required ecological motivation, acceptable collinearity and improved prediction to held-out geographical blocks.

As one model-informed corroboration, we reused the already fixed held-out cell pairs, five geographical-distance strata and 500 posterior-predictive maps from an intercept + Matérn space-only model. No environment or spatial model was refitted. For pigmentation state, environmental distance was the absolute difference in training-fold-standardized Temperature PC1, the only measured environmental term whose final 95% credible interval excluded zero. For conditional intensity, distance was Euclidean across training-fold-standardized precipitation PC1, temperature seasonality, topography PC1 and the Temperature PC1 × temperature-seasonality product. Coefficients were not used as weights; ecological direction came from the final full-model coefficients. Within each fold-by-distance stratum, phenotype divergence in the upper environmental-distance quartile was contrasted with the lower quartile and compared with the identical statistic from the fixed space-only maps. Appendix S3 gives full selection, spatial and reproducibility details.

### 2.4 Zooming to local focal-Bombus boundaries

We built SDMs for five Japanese *Bombus* species over a common mainland domain using shared predictor screening and spatial blocks. The surfaces represent habitat support rather than abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). Species predictions were ranked against predictions at retained occurrence sites. The primary exposure combined occurrence-referenced support for *B. ardens* and *B. diversus*, the two broad-ranging focal pollinators documented in the system. The three montane/alpine species were retained as a confounding guardrail rather than folded into the primary index.

We identified pure white-pigmented transitions among 1-km cells within 5 km and selected non-overlapping pairs without Bombus values, environmental values or final contrast direction. Only after pair identities were fixed were they oriented white-to-pigmented. We calculated pigmented-minus-white focal support and tested the mean with 100,000 sign flips. The median and proportion of positive pairs were mandatory distributional diagnostics, and 5-, 10- and 25-km tests formed one family. Environment did not select, orient or weight the pairs; final-eight-axis distance was inspected only after selection as a balance diagnostic. Appendix S5 reports raw-support alternatives, intensity tests, community turnover and equal-elevation highland comparisons.

### 2.5 Continuous colour isolation and natural-geography guardrails

The human-context analysis used the 1,305 environment-complete 1-km cells. A cell was pigmented when it contained at least one pigmented observation and white otherwise, giving 674 pigmented and 631 white cells. For every cell, raw colour isolation was the Euclidean distance to the nearest other cell with the same colour state. The primary human feature was population-exposure rank within 5 km. We calculated Spearman correlations separately for pigmented and white cells and their direct difference, rho(pigmented) - rho(white). Focal, 10-, 25- and 50-km population ranks, DID proximity, land-cover variables, mountainness and observation-process measures were retained as labelled secondary features.

Raw nearest-neighbour distance can increase where the full flower-cell frame is sparse. We therefore calculated nearest and fifth-nearest distances to any flower cell and repeated the primary relationship with relative isolation, log(same-colour nearest distance / any-colour nearest distance). Five leave-one-geographical-fold-out estimates assessed regional stability (Roberts et al., 2017; Valavi et al., 2019). Within-colour feature values were permuted within geographical folds for descriptive restricted-randomisation checks.

The decisive natural-geography guardrail replayed the complete isolation geometry on 10,000 checksum-locked final-eight-axis predictive maps. Each simulated map used the same `count > 0` state rule, cell geometry and observed binomial trial counts. For every map we recomputed pigmented and white correlations and their difference. The primary 5-km comparison was reported for all nondegenerate maps and for a fixed count-conditioned sensitivity. Only 19 maps contained exactly the observed 674 pigmented cells, so the pre-specified fallback retained the 1,000 maps closest to the observed count. Population scales were evaluated as one maxT family. The motivating correlations were inspected before this analysis was specified; the analysis is reported as a post hoc exploratory generalisation, while the density and natural-map guardrails were frozen before the validated execution.

An earlier threshold-event analysis is retained only in Appendix S6 as a supplementary sensitivity and does not contribute to the Main human-context inference.

### 2.6 Reproducibility and inferential order

The study provides more than a fitted model: it exposes the full route from source images to manuscript claims. Derived data, source-construction code, analysis code, fixed seeds, software manifests, evidence identities and validation rules are versioned, and manuscript values are checked against checksum-locked artifacts in continuous integration. Phenotype construction precedes ecological predictors; the broad natural template precedes the local Bombus test; and Bombus pairs are fixed before Bombus values are compared.

The continuous-isolation analysis has a different evidential status and is labelled accordingly. Its motivating pattern was observed before formal specification, so it is not presented as preregistered confirmation. Reproducibility instead locks the exact all-cell geometry, sampling-density correction, frozen human feature registry, 2,000 restricted permutations and 10,000-map natural guardrail. This separates transparent exploration from outcome-dependent redefinition while preserving the paper's sequence of broad template, local mechanism and contemporary overlay.

## 3. Results

### 3.1 A new image stream revealed a national quantitative polymorphism

The environmental analysis contained 1,922 georeferenced flowers in 1,305 1-km cells: 966 white-like and 956 pigmented. The matched YAMAP retrieval contained 3.81 times as many georeferenced focal-species records as iNaturalist in the same three-year period, and nearly equal annual counts supplied dense contemporary replication. Author screening, image hashing and deterministic phenotyping converted that alternative observation stream into a traceable quantitative trait dataset. Because the a* boundary was estimated before ecological predictors were read, the phenotype was independent of the geography later used to explain it.

This data layer was itself a biological result: it made national variation measurable at two levels rather than representing flower colour by a few categorical population samples. It also supplied the local replication needed for boundary and neighbourhood analyses later in the study.

### 3.2 Broad models separated two environmental responses and coherent residual geography

Pigmentation state and conditional intensity did not tell the same geographical story. Pigmentation became less likely toward warmer Temperature PC1. The posterior mean log-odds was -0.542 (95% CrI -1.033 to -0.049), equivalent to an odds ratio of about 0.58 per SD. No state interaction met the full promotion criteria.

Among already-pigmented flowers, intensity retained Temperature PC1 × temperature seasonality (mean -0.204; 95% CrI -0.302 to -0.107): the warm-climate decline strengthened where annual temperature variability was greater. Intensity was also lower toward wetter/moister climate (-0.174; -0.323 to -0.024) and steeper, more rugged terrain (-0.134; -0.224 to -0.043), while temperature seasonality at mean Temperature PC1 was positive (+0.207; +0.044 to +0.369). RSDS did not retain an independent national effect.

Coherent geography remained after these measured associations. Residual correlation range was 132.8 km (95% CrI 88.8–195.7) for pigmentation state and 65.7 km (31.0–132.6) for conditional intensity. These are correlation scales, not seed, pollen or colonization distances.

The fixed-null environmental-distance check connected the coefficients to a stronger pairwise question. For pigmentation state, the observed high-minus-low Temperature PC1 distance contrast was 0.100608, compared with a space-only median of 0.048475 (excess +0.052133; one-sided P=0.00998). Excess was positive in all five folds and 19 of 25 fold-by-distance strata. For conditional intensity, the combined distance across precipitation, temperature seasonality, terrain and the thermal interaction yielded observed contrast 0.047416 versus space-only median 0.020897 (excess +0.026519; P=0.26347). Thus the cool-climate state association was corroborated by held-out state divergence beyond spatial continuity, whereas the supported conditional-intensity gradients did not produce that stronger signature.

### 3.3 Local boundaries revealed heterogeneous Bombus correspondence

The 5-km design produced 67 sharp transitions between pure white and pigmented cells with median separation 2.0 km. The fixed pairs occupied tighter environmental neighbourhoods than ordinary local edges: median final-eight-axis distance was 0.244 versus 0.318.

Mean focal-Bombus support was 0.03590 higher on the pigmented side (one-sided P=0.02716), but the distribution was not a pervasive shift. The median was -0.00277, only 49.3% of pairs were positive and the 5/10/25-km family gave q=0.0815. Mean contrast declined to +0.0084 at 10 km and +0.0029 at 25 km, raw SDM support did not reproduce the 5-km result (P=0.267), and no persuasive relation appeared for conditional intensity. A subset of strong boundaries therefore raised the mean.

A separate highland guardrail showed why the local comparison was necessary. Strong national overlap between pigmented highland flowers and montane/alpine *Bombus* disappeared when nearby white and pigmented endpoints were matched on elevation (all one-sided P>=0.755 at <=50 m). The broad highland pattern was consistent with shared mountain geography, not an independent pollinator mechanism.

### 3.4 Continuous isolation revealed a pigmented human-context overlay

The 674 pigmented cells were strongly clustered: median distance to the nearest other pigmented cell was 3.61 km. Pigmented isolation increased with population exposure at the focal cell (rho=0.271), 5 km (rho=0.252) and 10 km (rho=0.172), but the raw correlation weakened at 25 km (rho=0.026) and was negative at 50 km (rho=-0.058). The corresponding raw white correlations were 0.003, -0.072, -0.141, -0.180 and -0.148. At 5 km, the direct raw colour contrast was therefore +0.324, and all five leave-one-fold-out estimates remained positive (0.267-0.391), although fold-specific strength varied.

The raw white sign reversal was not robust to sampling-density correction. When same-colour distance was scaled by nearest distance to any flower cell, the 5-km correlations were +0.285 for pigmented and +0.079 for white cells, giving a smaller direct difference of +0.207. Observation effort was unrelated to raw pigmented isolation (rho=-0.032; within-fold permutation P=0.429) and did not explain relative isolation.

Natural-map replay separated the focal pigmented relationship from the less stable colour contrast. For raw 5-km isolation, pigmented rho was +0.252 compared with a natural-map mean of +0.133 and a 95% interval of 0.071-0.196 (upper-tail P=0.0002). Relative isolation gave rho=+0.285 versus a natural mean of +0.154 and interval 0.068-0.236 (P=0.0009). The same conclusions held in the 1,000 nearest-count maps (raw P=0.0020; relative P=0.0040).

The direct raw colour difference lay just above its natural interval (observed +0.324; null mean +0.205; interval 0.088-0.316; P=0.0194; five-scale maxT P=0.0465), but the relative-isolation difference did not (observed +0.207; null mean +0.152; interval 0.044-0.260; P=0.1586; maxT P=0.1190). Thus the robust result is an excess positive isolation-population relationship within pigmented occurrences, not a density-independent reciprocal displacement of pigmented and white states. The observed raw correlation attenuated at broad radii, but observed-minus-natural displacement was not confined cleanly to focal-10-km scales.

## 4. Discussion

### 4.1 The national trait dataset changed the biological question

The first contribution is empirical: a non-biodiversity hiking-image stream was converted into a dense, recent and auditable national flower-colour dataset. YAMAP supplied 3.81 times the matched iNaturalist records during the same three years, and exhaustive screening, image hashing and deterministic phenotyping turned that quantity into study-specific trait quality. The missing resource was not occurrence coordinates alone, but enough images to separate a white-pigmented switch from variation within the pigmented state.

The mountain-route frame has both value and cost. It enriches settings in which natural or self-sustaining populations are plausible, but it does not guarantee wild provenance. In the human-context analysis, the same frame can narrow the urban-rural gradient, while accessible routes and mountain margins can increase both observation and human exposure. Treating these directions explicitly is more defensible than assigning YAMAP one global bias score.

The coverage changed the biological question rather than merely increasing sample size. A single white-to-dark index would have averaged over two responses that differed in environmental coefficients, residual spatial range and divergence beyond spatial continuity.

### 4.2 Temperature organizes the state transition more strongly than conditional intensity

The broad result is not simply that several coefficients exclude zero. For pigmentation state, the full model identifies a cool-climate direction and the fixed-null check shows that Temperature PC1 separation also orders held-out state divergence beyond the same spatial-continuity expectation. These are non-equivalent lines of evidence: one estimates a partial directional association, while the other asks whether environmentally separated locations differ phenotypically more than fitted space alone predicts.

This combined result is consistent with environmentally responsive anthocyanin regulation and with longer-term environment-aligned population differentiation. Low temperature can induce floral anthocyanin synthesis, while petal pigmentation changes radiative absorption (Shvarts et al., 1997; Li et al., 2026). The present data do not distinguish plasticity from inherited differentiation and do not estimate fitness.The comparison asks whether environmental separation orders phenotype divergence beyond fitted spatial continuity; it does not identify the underlying mechanism or demonstrate selection or local adaptation.

Conditional intensity carries a different ecological signature. Moisture, temperature seasonality, terrain and the Temperature × seasonality interaction describe directional conditional geography, but their joint distance did not create pairwise divergence beyond spatial continuity. This can arise when responses vary smoothly, when an interaction surface maps environmentally distant locations to similar intensities, or when conditioning on the pigmented subset changes the analysed composition. The result therefore rejects a universal stress-darkening account without implying that the intensity coefficients are false.

The remaining spatial fields add a second positive output. Their different ranges show that unresolved geography is response specific and provide a sampling guide for microclimate, ancestry, isolation-by-distance, isolation-by-environment and admixture tests. They are not labelled as genetics in advance.

### 4.3 Boundary heterogeneity is the pollinator result

The tubular floral system and Izu pollinator/breeding-system history make bumblebees a focused natural-history hypothesis rather than one arbitrary SDM layer. The Izu evidence remains motivation, not proof that bumblebee absence caused white flowers. The primary achievement of the 5-km design is that it asks the pollinator question inside repeated local colour boundaries rather than through national map overlap.

The distribution of contrasts governs interpretation. A positive mean coexisted with a near-zero median, fewer than half the pairs in the predicted direction, family q>0.05, broader-scale attenuation and failure of raw support to reproduce the result. This is not a uniform pigmented-side advantage. It is a geographic mosaic in which a minority of boundaries may have strong Bombus correspondence while many do not.

That heterogeneity is scientifically useful. The 67 boundaries can be stratified into strongly positive, near-zero and negative groups for direct tests of species-resolved visitation, bee visual contrast, stigma contact, pollen deposition, seed set, selfing and selection gradients. The highland guardrail further shows that a visually stronger national pattern can disappear when shared elevation is controlled. SDM support remains habitat opportunity, not realized visitation or selection. This correspondence is not evidence of pollinator-mediated selection.

### 4.4 Human landscapes overlay the spatial geometry of pigmentation

The continuous analysis changes the human-context question from a comparison among a small set of selected events to an all-cell property of the pigmented distribution. Pigmented occurrences farther from other pigmented occurrences were more population-exposed than the fitted natural geography predicted. The relation remained when same-colour distance was scaled by local spacing of all flower cells and when predictive maps were restricted toward the observed pigmented-cell count.

The guardrails prevent the most eye-catching contrast from being overstated. White cells showed a negative raw isolation-population relation, but that sign disappeared after density correction, and raw white rho was almost exactly the natural expectation. The defensible conclusion is therefore an additional positive human-context association within pigmented occurrences, not perfectly reciprocal displacement of the two colour states.

The scale profile does not identify mechanism. Horticultural planting or escape, human-modified establishment conditions, fine-scale plasticity and access-linked observation remain alternatives. The analysis is explicitly post hoc, and even small Monte Carlo P values do not convert the pattern into provenance evidence. Repeated field history, vouchers, standardized spectra and pigment chemistry, local environmental measurements and genomic comparison with neighbouring white and horticultural material are required.

The threshold event family remains useful in Appendix S6. Its 16 local configurations were not more frequent than natural maps expected, so they are not evidence that nature fails to reproduce them. They remain reproducible extreme targets rather than the basis of the Main human-context inference.

This analysis does not establish horticultural origin.

### 4.5 A spatially varying balance can maintain flower-colour polymorphism

The study converges on one cross-scale model. Temperature can alter the regulation, physiological value and thermal cost of pigmentation. Unresolved historical and population processes can preserve or redistribute variants. Local bumblebee opportunity may modify the reproductive value of maintaining a visible pigmented state in some neighbourhoods. Human landscapes can overlay that natural geography by changing where isolated pigmented occurrences persist or are observed.

These processes are not interchangeable predictors. They leave different signatures on different phenotype components and at different comparison scales. Pigmentation state carries both a directional cool-climate coefficient and temperature-aligned divergence beyond space. Conditional intensity carries conditional moisture, seasonality and terrain gradients without the same divergence signature. Bombus correspondence is concentrated in a subset of local boundaries, while human context appears in the geometry of isolated pigmented occurrences.

The broader biogeographic advance is therefore scale-matched integration. Repurposed images recover the phenotype; broad models separate named environmental gradients from coherent geography; fixed spatial-null maps ask whether supported gradients organize extra divergence; local boundaries prevent national biotic overlap from becoming mechanism; and all-cell geometry asks how contemporary human context overlays the natural distribution. Positive, heterogeneous and null results perform different inferential jobs while defining direct physiological, pollination, field-history and genomic tests.

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
