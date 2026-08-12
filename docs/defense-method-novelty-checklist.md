# Defense checklist — method novelty, rationale and claim ceiling

Purpose: prepare concise, defensible answers to questions about what is genuinely new in the study, what builds on established methods, and why the adopted design is biologically necessary rather than merely technically elaborate.

## Core defense position

The strongest claim is **not** that every statistical component was invented here. The study is strong because three forms of novelty coincide:

1. **new empirical information** — a national, author-curated quantitative flower-colour dataset derived from a non-biodiversity hiking-image stream;
2. **new biological conclusions** — response-specific broad flower-colour geography, a weak/local focal-Bombus correspondence, and calibrated local departures that are not excessive under the natural model;
3. **new analytical design** — a dependent cross-scale workflow that changes data representation and comparison unit as the ecological question changes.

Safe one-sentence defense:

> The contribution is a new quantitative trait dataset and new biological inference, combined with a scale-aware analytical design; the individual statistical building blocks are established, but their deployment and connection are tailored to a problem that a single national regression cannot answer cleanly.

Do **not** claim that INLA, SPDE, SDMs, posterior predictive checks, internet photographs, or residual analysis were invented in this study.

---

## 1. Hiking-app photographs as ecological trait data

### What is already established

Internet photographs have already been used to recover geographic variation in visible organismal traits. Leighton et al. (2016) validated Google Images as a source for geographic morph and visible-trait information. The broader iEcology framework explicitly treats digital resources accumulated for non-research purposes as ecological data sources (Jarić et al. 2020). Geotagged visitor photographs have also been used in recreation/landscape research, including hiking-trail applications in Japan.

Relevant precedent:

- Leighton, G. R. M. et al. 2016. *Just Google it: assessing the use of Google Images to describe geographical variation in visible traits of organisms*. Methods in Ecology and Evolution. DOI: 10.1111/2041-210X.12562.
- Jarić, I. et al. 2020. *iEcology: Harnessing Large Online Resources to Generate Ecological Insights*. Trends in Ecology & Evolution 35:630–639. DOI: 10.1016/j.tree.2020.03.003.
- Mizuuchi, Y. 2023. *Landscape assessment of forest trail using geotagged visitor employed photography*. Journal of Forest Research. DOI: 10.1080/13416979.2022.2117091.

### What is new here

The novelty is narrower and stronger than “first use of internet photos.” The adopted workflow uses a **hiking/recreation platform whose primary data-generating purpose is not biodiversity reporting**, then converts incidental photographs into a study-curated quantitative trait dataset through exhaustive author review, flower/ROI validation, coordinate/date provenance, duplicate-image hashing and deterministic colour extraction.

The defensible novelty statement is:

> We are not claiming that online photographs are new ecological data. The unusual step is using a recreation-platform image stream, independent of a formal biodiversity-reporting workflow, to construct a nationally distributed, author-validated quantitative floral-trait dataset with auditable image and spatial provenance.

### Likely defense question: “Isn’t this just citizen science?”

Answer:

> It is related to citizen/community-sourced biodiversity data but has a different observation process. The user records a hike and photographs subjects encountered along it rather than necessarily creating a focal-species observation. That does not remove route choice, subject choice or conspicuousness bias. The methodological value is access to a complementary incidental-image stream, followed by explicit research-grade curation.

### Likely defense question: “Can you trust uncalibrated photographs for colour?”

Answer:

> Only for the phenotype actually defined. We use display-referred visible CIELAB measurements under deterministic QC, not pigment chemistry, spectral reflectance, UV contrast or Bombus visual-space coordinates. The state/intensity split and explicit claim ceiling are part of the method.

### Defense evidence to know

- 1,922 environment-complete observations.
- white/pigmented and pigmented-only intensity are separate responses.
- original YAMAP photographs are third-party content and are not redistributed.
- `Data_S1.csv` is the public derived observation/trait table.

---

## 2. Broad environmental geography with INLA-SPDE

### What is already established

INLA and the SPDE representation are established statistical methods. Rue et al. (2009) developed INLA for latent Gaussian models; Lindgren et al. (2011) established the SPDE link between continuously indexed Gaussian fields and sparse Gaussian Markov random fields. INLA-SPDE has already been applied to ecological and environmental spatial modelling, including species distributions.

Core precedent:

- Rue, H., Martino, S. & Chopin, N. 2009. *Approximate Bayesian inference for latent Gaussian models by using integrated nested Laplace approximations*. JRSS B 71:319–392. DOI: 10.1111/j.1467-9868.2008.00700.x.
- Lindgren, F., Rue, H. & Lindström, J. 2011. *An explicit link between Gaussian fields and Gaussian Markov random fields: the stochastic partial differential equation approach*. JRSS B 73:423–498. DOI: 10.1111/j.1467-9868.2011.00777.x.
- Example ecological application: spatial modelling of grassland species with INLA-SPDE has shown stable inference when spatial autocorrelation is explicitly represented.

### What is new here

Do not call INLA-SPDE itself a new method. The novelty is the **role assigned to continuous spatial structure in a national intraspecific trait model and how that broad model becomes the ecological template for later analyses**.

Defensible statement:

> INLA-SPDE is established. What is distinctive here is using a continuous Matérn spatial field to separate measured broad environmental associations from unresolved spatial geography in a quantitative intraspecific floral trait, then carrying the finalized environmental state space forward to define local comparisons rather than treating the spatial term as a nuisance correction that disappears after one regression.

### Why not a simple random effect?

A conventional random intercept is appropriate for discrete groups such as sites, populations or regions. It does not naturally represent **continuous distance-dependent covariance among irregularly distributed observations**. Here the unresolved geography is not known a priori to belong to discrete biological groups. A continuous spatial field therefore better matches the scientific uncertainty.

Defense answer:

> The question was not only whether observations from the same named region resemble one another. It was whether residual similarity decays continuously across geographic distance after measured environment is included. A Matérn SPDE field directly represents that structure, estimates its spatial scale, propagates uncertainty, and works with the Bernoulli and Gaussian response layers in one latent-Gaussian framework.

### Why is that better than ignoring space and fitting a linear environment response?

Ignoring spatial dependence can destabilize environmental coefficients and produce false confidence when predictors and responses share geography. Ecological applications of spatial modelling have repeatedly shown that explicitly representing spatial autocorrelation can improve stability and reduce false-positive inference.

But do **not** say the spatial field “solves confounding.” It can also absorb unmeasured environment, population history, dispersal or sampling geography.

### Why not just use a GAM or Gaussian process?

Safe answer:

> Other spatial frameworks could be defensible. The advantage of INLA-SPDE here is a continuous Matérn field, sparse computation, posterior uncertainty, interpretable spatial hyperparameters, and compatibility with the two response distributions. The defense is therefore biological fit and computational/inferential coherence, not uniqueness.

### Key novelty phrase

> The methodological contribution is **scale-aware spatial attribution**, not invention of SPDE.

---

## 3. Linking flower-colour transitions to Bombus SDMs

### What is already established

There is substantial precedent for relating flower colour to pollinator fauna.

- Müller (2017) found regional abundance of blue-purple flowers correlated with Bombus species richness in Arctic floras.
- Kudo (2019) compared flower-colour composition and pollinator fauna between Japanese and New Zealand alpine communities.
- Koski & Galloway (2020) tested geographic variation in *Campanula americana* petal colour/reflectance against climate, pollinator visitation and colonization history.
- Japan-wide bee distributions have been estimated with SDMs; recent work mapped distributions and functional ranges of 13 bumblebee species and one honey bee in Japan.

Therefore the novelty is **not** “first study to connect flower colour and bumblebee geography,” and not “first use of a bee SDM.”

### What appears unusual in the present literature search

The search did not identify a direct precedent combining all of the following:

1. within-species quantitative flower-colour state geography;
2. independently constructed species-specific Bombus SDMs;
3. occurrence-referenced calibration of the SDM surfaces;
4. response/Bombus/environment-blind selection of nearby sharp white–pigmented boundaries;
5. a signed pigmented-minus-white habitat-opportunity contrast tested only after pairs are fixed;
6. explicit separation of this local directional test from broader five-species community turnover.

This should be phrased as **“we found no direct precedent in our targeted literature search”**, not “this has never been done.”

### Why use an SDM at all if it is not visitation?

Answer:

> The SDM is used as a spatial hypothesis about habitat opportunity for the focal pollinators, not as a substitute for visitation. Direct national visitation data do not exist at the same coverage. The SDM therefore supplies a reproducible, species-specific availability surface, while the manuscript explicitly stops short of calling it abundance, visitation, pollen transfer or selection pressure.

### Why not put Bombus SDM in the national Broad model?

This is one of the strongest defense points.

> Because the Bombus SDM is itself generated from environmental geography. A national regression containing climate, spatial field and environment-derived Bombus suitability risks assigning shared geography a mechanistic meaning. The analysis therefore changes comparison unit: Broad environment/space is estimated nationally, whereas Bombus is examined at independently selected local colour boundaries.

### What makes the pair design rational?

- pair selection is blind to Bombus values;
- pair selection is blind to environmental values;
- pair orientation is assigned only after pairs are fixed;
- non-overlap reduces endpoint reuse;
- final-eight-axis distance is a **diagnostic**, not a matching criterion;
- scale attenuation, raw-support null, median and positive-pair fraction determine the claim ceiling.

### Critical defense answer: “P=0.027, why not call it evidence for pollinator selection?”

> Because the mean is driven by magnitude rather than a majority shift: median ≈0, 49.3% of pairs are positive, BH q across scales is 0.0815, raw-support P=0.267 and the effect attenuates at 10/25 km. The result supports a weak local correspondence in predicted opportunity, not realized pollinator-mediated selection.

This restraint strengthens the method defense.

---

## 4. Event-based local departures versus residual analysis

### What is already established

Residual analysis and posterior predictive/model-checking approaches are established in ecology. Residual spatial structure is commonly examined to diagnose model inadequacy, and ecological Bayesian model checking recommends choosing discrepancy measures that correspond to scientifically meaningful patterns (e.g. Conn et al. 2018).

Relevant precedent:

- Conn, P. B. et al. 2018. *A guide to Bayesian model checking for ecologists*. Ecological Monographs 88:526–542. DOI: 10.1002/ecm.1314.
- Wright, W. J., Irvine, K. M. & Higgs, M. D. 2019. *Identifying occupancy model inadequacies: Can residuals separately assess detection and presence?* Ecology. DOI: 10.1002/ecy.2703.

### What is distinctive here

The current method does **not** define an anomaly as “large residual” or “observation beyond ±2 SD.” It defines a biologically interpretable **local configuration**:

> a pigmented focal cell embedded among at least three nearby environmentally similar cells that contain only observed white flowers.

The exact same event detector is then replayed on 10,000 cross-fitted natural predictive maps before human variables are inspected.

### Why is this not just residual mapping?

Residual = a pointwise discrepancy between observation and fitted expectation.

Current event = a **relational spatial configuration** involving:

- focal state;
- neighbour states;
- geographic radius;
- environmental similarity;
- minimum local support.

A cell can have a large scalar residual without satisfying this configuration, and a local configuration can be biologically interesting even when no arbitrary residual threshold is crossed.

Defense answer:

> Residuals ask “where did the model miss most at individual locations?” Our event asks “how often does this specific ecological neighbourhood configuration occur under the fitted natural process?” The latter has a direct biological interpretation and a natural predictive null.

### Why replay the detector on predictive maps?

Because observing 16 striking-looking sites does not imply they are unusual under the natural model. Applying the identical detector to model-generated maps calibrates the **event frequency**, not merely the residual magnitude.

Current result:

- observed candidates = 16;
- natural-map candidate-count P=0.27897;
- candidate-fraction upper-tail P=0.12609.

Therefore the correct conclusion is that such local departures are **not excessive under the natural model**.

### Why examine human variables only after event selection?

To prevent circularity. If population, roads or built-up area helped define the sites, then finding those variables elevated afterwards would be partly tautological. Human variables are deliberately absent from natural-model fitting, event definition, matching and candidate ranking.

### Method novelty phrase

> The contribution is an **ecologically defined, response-blind event detector calibrated against repeated natural predictive maps before post-selection human characterization**.

Again: posterior predictive checking is established; this exact event-based deployment is the distinctive part.

---

## 5. Is the whole study “strong” because data, method and conclusion are all new?

Yes, with careful wording.

A useful novelty matrix for defense:

| Dimension | Strength | Safe claim |
|---|---|---|
| Data | high | new national author-curated quantitative flower-colour dataset from a hiking-image stream |
| Biological result | high | new range-wide response-specific geography for *C. punctata* and new local/provenance constraints |
| Statistical building blocks | mostly established | mixture classification, INLA-SPDE, SDM, sign flips, predictive checks are not inventions |
| Analytical design | high | unusual dependent cross-scale integration; comparison unit changes with biological process |
| Mechanistic inference | intentionally limited | Bombus is weak local opportunity correspondence; human result is post-selection context, not cause |
| Reproducibility | high | code, derived data, seeds, explicit event definitions, checksum-locked evidence and CI validators |

The strongest overall defense is therefore:

> The paper does not depend on one spectacular P value. Its contribution is that a new national trait dataset enables new biological conclusions, while the analytical design prevents broad spatial covariance, environment-derived pollinator surfaces and striking local exceptions from being overinterpreted.

That is stronger than saying “all methods are novel.”

---

## 6. Questions likely to be asked in a defense

### “What is the single biggest novelty?”

> A dependent cross-scale trait-biogeography design: incidental hiking photographs are converted into quantitative traits; Broad environment and continuous space establish the natural geographical template; an environment-derived pollinator hypothesis is moved to independently selected local boundaries; and locally discordant states are calibrated as ecological events before human context is examined.

### “Which part could be published alone?”

All four methodological stages are individually substantive, but the paper is strongest because they are dependent:

- image stream → trait construction;
- trait construction → Broad geography;
- Broad geography → rationale for local Bombus comparison;
- Broad natural reference → calibrated departure detector.

### “Are you overengineering a simple regression problem?”

> A simple regression is sufficient only if observations are independent, predictors do not share geography in problematic ways, the same spatial scale is appropriate for all processes, and pointwise residuals answer the final biological question. None of those assumptions is defensible here. Complexity was added only where the biological comparison unit changes.

### “Does the spatial field erase the environmental signal?”

> It can compete with broad environmental gradients, which is precisely why coefficients are interpreted conditionally and promotion required blocked geographical transfer. The field is not treated as population history; it is unresolved continuous geography.

### “Why not genetics?”

> Genetics is the next causal layer, especially for residual geography and provenance. This study identifies where genetic/field sampling is most informative rather than pretending image and SDM data can replace it.

### “What would falsify the Bombus interpretation?”

- direct visitation does not covary with the local support contrast;
- flower contact/pollen transfer is unrelated to the colour boundary;
- genetic/fitness data show no differential selection across the boundary;
- repeated local transitions fail in independent field data.

The current manuscript already stops below those causal claims.

### “What would falsify the human/provenance hypothesis?”

- field history and vouchers show long-standing wild populations;
- genetics place candidates within local natural population structure rather than horticultural material;
- settlement-context association disappears under independent, less route-biased sampling.

---

## 7. Phrases to avoid

Avoid:

- “We invented a new INLA-SPDE method.”
- “This is the first study ever to use internet photographs for trait geography.”
- “Bombus SDM measures pollinator pressure.”
- “The Bombus result proves pollinator-mediated selection.”
- “The 16 sites are anthropogenic anomalies.”
- “Spatial random effects remove spatial confounding.”
- “YAMAP is unbiased.”

Prefer:

- “new application/design”;
- “no direct precedent identified in our targeted literature search”;
- “predicted habitat opportunity/support”;
- “weak local directional correspondence”;
- “calibrated local departures / field-provenance targets”;
- “accounts for residual continuous spatial dependence”;
- “alternative observation process with explicit sampling limitations.”

---

## 8. Thirty-second defense summary

> This study is novel on three levels. First, it builds a new national quantitative flower-colour dataset from author-validated hiking-app photographs. Second, it produces new biological inference by separating pigmentation state from pigmented-only intensity and by showing that national geography, local focal-Bombus opportunity and locally discordant states operate at different inferential scales. Third, the analysis is designed around those scale differences: continuous spatial dependence is modelled with an established INLA-SPDE framework, environment-derived Bombus surfaces are tested only at independently fixed local boundaries, and local departures are defined as ecological neighbourhood events and calibrated against 10,000 natural predictive maps before human context is examined. The novelty is therefore not that every statistical tool is new, but that new data, new results and a scale-aware analytical design jointly answer a question that a single regression would over-simplify.
