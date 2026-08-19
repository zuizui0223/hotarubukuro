#!/usr/bin/env python3
"""Synchronise the JBI paper with the final accepted analysis hierarchy.

The script is idempotent and runs after the continuous-isolation integration.
It aligns the manuscript and paper contract with:

1. the final observation-level environment + SPDE models;
2. PR #58's supported-term environmental distance against fixed space-only maps;
3. heterogeneous local Bombus boundaries; and
4. PR #57's continuous colour-isolation human-context analysis.
"""

from __future__ import annotations

import csv
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
MAIN = ROOT / "submission/jbi/JBI_main_manuscript_anonymized.md"
S3 = ROOT / "submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md"
BACKGROUND = ROOT / "submission/jbi/JBI_background_architecture.md"
CAPTIONS = ROOT / "submission/jbi/JBI_main_figure_captions.md"
PLAN = ROOT / "submission/jbi/JBI_main_figure_plan.md"
JA = ROOT / "submission/jbi/JBI_translated_abstract_ja.md"
COVER = ROOT / "submission/jbi/JBI_cover_letter.md"
CHECKLIST = ROOT / "submission/jbi/JBI_submission_checklist.md"
LOCK = ROOT / "config/paper_pipeline.lock.json"
ACTIVE_MAP = ROOT / "paper/active-file-map.csv"
PAPER_README = ROOT / "paper/README.md"
ANALYSIS_MAP = ROOT / "paper/analysis-map.md"
SUBMISSION_VALIDATOR = ROOT / "submission/jbi/validate_jbi_submission.py"
ALIGNMENT_VALIDATOR = ROOT / "validation/validate_jbi_repository_alignment.py"
INTEGRATION_NOTE = ROOT / "reproducibility/final_analysis_pipeline_integration_2026-08-19.md"

SUPPORTED_ARTIFACT_ID = 9350975649
SUPPORTED_ARTIFACT_SHA256 = "4d5a1d28b8313cc0fb6c85484d21c6d94535ac7cc0881e83dc7ed02678854f03"
SUPPORTED_RESULT_ROOT = "results/broad_supported_term_distance_space_null"
SUPPORTED_REQUIRED = [
    f"{SUPPORTED_RESULT_ROOT}/primary_supported_term_distance_test.csv",
    f"{SUPPORTED_RESULT_ROOT}/matched_distance_stratum_contrasts.csv",
    f"{SUPPORTED_RESULT_ROOT}/heldout_pair_supported_term_distance.csv",
    f"{SUPPORTED_RESULT_ROOT}/space_null_global_contrast_draws.csv",
    f"{SUPPORTED_RESULT_ROOT}/analysis_metadata.csv",
    f"{SUPPORTED_RESULT_ROOT}/RESULT_SUMMARY.md",
]


def words(text: str) -> list[str]:
    return re.findall(r"\b[\wÀ-ž*'’-]+\b", text)


def write_if_changed(path: Path, text: str) -> None:
    current = path.read_text(encoding="utf-8") if path.exists() else None
    if current != text:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")


def replace_line(text: str, label: str, replacement: str) -> str:
    pattern = re.compile(rf"^{re.escape(label)}.*$", re.MULTILINE)
    updated, count = pattern.subn(replacement, text, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one line beginning {label!r}; found {count}")
    return updated


def replace_section(text: str, start: str, end: str, replacement: str) -> str:
    pattern = re.compile(
        rf"^{re.escape(start)}\n.*?(?=^{re.escape(end)}\n)",
        re.MULTILINE | re.DOTALL,
    )
    updated, count = pattern.subn(replacement.rstrip() + "\n\n", text, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one section {start!r} -> {end!r}; found {count}")
    return updated


def replace_tail_section(text: str, start: str, replacement: str) -> str:
    pattern = re.compile(rf"^{re.escape(start)}\n.*\Z", re.MULTILINE | re.DOTALL)
    updated, count = pattern.subn(replacement.rstrip() + "\n", text, count=1)
    if count != 1:
        raise RuntimeError(f"Expected one tail section {start!r}; found {count}")
    return updated


def update_main() -> None:
    text = MAIN.read_text(encoding="utf-8")

    text = replace_line(
        text,
        "**Aim:**",
        "**Aim:** Geographical trait variation can reflect abiotic environment, unresolved history, biotic interactions and human context at different scales. We asked how these processes assemble flower-colour polymorphism while separating pigment presence from intensity after pigmentation is present.",
    )
    text = replace_line(text, "**Taxon:**", "**Taxon:** spotted bellflower (*Campanula punctata*).")
    text = replace_line(
        text,
        "**Methods:**",
        "**Methods:** We converted 1,922 author-screened YAMAP photographs into pigmentation state and pigmented-only intensity. Final environment + INLA-SPDE models estimated directional environmental associations and residual geography. Using already fixed held-out pairs and 500 cached space-only predictive maps, we then asked whether distance along the environmental terms supported by each final model ordered phenotype divergence beyond spatial continuity. Sixty-seven independently fixed local boundaries supplied the Bombus comparison. Human context was tested with same-colour isolation in all 1,305 cells and 10,000 natural predictive maps.",
    )
    text = replace_line(
        text,
        "**Results:**",
        "**Results:** Pigmentation was less likely toward warmer Temperature PC1. Temperature-separated locations were also more different in pigmentation state than fixed spatial continuity predicted (excess=0.0521; P=0.010), whereas the combined supported-term distance for conditional intensity did not exceed spatial expectation (P=0.263). At local boundaries, a positive mean focal-Bombus contrast coexisted with a near-zero median and 49.3% positive pairs. Among 674 pigmented cells, isolation correlated with 5-km population exposure more strongly than natural maps expected in raw (rho=0.252; P=0.0002) and density-corrected form (rho=0.285; P=0.0009).",
    )
    text = replace_line(
        text,
        "**Main conclusions:**",
        "**Main conclusions:** Quantitative phenotyping resolved a temperature-aligned white-pigmented switch from conditional variation within pigmented flowers. Local Bombus correspondence was heterogeneous rather than pervasive, and human context appeared as an excess positive isolation-population relationship within pigmented occurrences rather than proof of horticultural origin. Integrative trait biogeography gains resolution by connecting, rather than collapsing, processes operating at different scales and comparison units.",
    )

    text = replace_section(
        text,
        "### 1.1 The geographical mystery of flower-colour polymorphism",
        "### 1.2 The first hidden layer: state is not intensity",
        """### 1.1 The geographical mystery of flower-colour polymorphism

Why does one species remain white in some parts of its range and pigmented in others? This is not only a descriptive question. Intraspecific trait variation can alter individual performance, population persistence and species interactions, and different causes predict different responses to environmental change (Westerband et al., 2021). Yet a geographical pattern is easy to overinterpret. Climate, population history, pollinators, dispersal and human activity are all spatially structured, so several processes can draw similar trait maps.

Flower colour makes this problem especially revealing because anthocyanin pigmentation can affect both physiology and reproduction. Pigments change optical absorption and can contribute to thermal and water-stress responses, while the visible flower also acts as a signal to pollinators. A pigmented morph may therefore gain physiological or reproductive benefits in one setting while paying thermal, hydraulic or biosynthetic costs in another (Warren & Mackenzie, 2001; Kellenberger et al., 2019; Trunschke et al., 2021; Li et al., 2026). Spatial variation in that balance offers a plausible route by which white and pigmented flowers persist within one species.

The spotted bellflower (*Campanula punctata* Lam.; Campanulaceae) is unusually tractable for separating these processes. Its large tubular flowers vary conspicuously from white to pigmented across Japan, bumblebees are effective visitors, and the species is also cultivated. The ecological need is therefore not simply to add more predictors to one national regression, but to connect evidence while preserving the scale and comparison unit at which each process has a defensible biological meaning.""",
    )

    text = replace_section(
        text,
        "### 1.2 The first hidden layer: state is not intensity",
        "### 1.3 A new observation stream makes the national phenotype visible",
        """### 1.2 The first hidden layer: state is not intensity

The apparent white-to-dark continuum contains at least two observational questions. A white-pigmented transition can reflect whether the anthocyanin pathway is visibly expressed at all. Variation among flowers that are already pigmented can instead reflect pigment amount, chemistry and petal optics (Dick et al., 2011; van der Kooi et al., 2016; Tasaki et al., 2022). A process that changes whether pigmentation is switched on need not control how dark the flower becomes afterwards. We therefore separated pigmentation state from visible intensity before asking what geography explains either response.

This split gives environmental hypotheses more precise targets, but it does not imply a universal stress-darkening rule. Moderate low temperature can activate chalcone-synthase expression and anthocyanin accumulation in corollas, and broad comparative data associate floral pigmentation with cool or arid settings (Shvarts et al., 1997; Sullivan & Koski, 2021). Darker petals can also absorb more radiation. That absorption may be useful in cool conditions yet impose heat and water costs in warm conditions because floral cooling can require transpiration (Li et al., 2026). Water limitation can favour anthocyanin morphs in some systems, but its direction is taxon and context dependent (Warren & Mackenzie, 2001). In *C. punctata*, UV-B exclusion alters flavonoid accumulation in leaves (Hashiba et al., 2006), showing environmental responsiveness without establishing the petal mechanism. Temperature therefore supplied the strongest directional expectation, while moisture, seasonality, radiation and terrain were treated as interacting contexts rather than interchangeable stress indices.""",
    )

    text = replace_section(
        text,
        "### 1.3 A new observation stream makes the national phenotype visible",
        "### 1.4 From broad geography to local boundaries and human overlays",
        """### 1.3 A new observation stream makes the national phenotype visible

For this species, occurrence coordinates alone could not answer either colour question, and matched public image streams were too sparse for dense national, boundary and neighbourhood comparisons. We therefore built the phenotype from YAMAP, a hiking-navigation and activity-diary platform. Every candidate was screened for taxon identity, focal flower and usable petal region; duplicates and coordinate-image correspondence were audited; and colour was extracted with one deterministic pipeline. During the matched 2023–2025 window, YAMAP yielded 1,964 georeferenced focal-species records, compared with 516 iNaturalist observations with photographs—a 3.81-fold difference. The three YAMAP years were also closely balanced.

YAMAP is not spatially random. Its mountain-route frame enriches natural and semi-natural settings in which self-sustaining populations are plausible, but it does not prove wild provenance for any record. Nor can route conditioning be assumed to weaken human associations in only one direction. It can compress the represented urban-rural gradient, while trailheads, roads and accessible mountain margins can increase observation opportunity, disturbance and opportunities for human-mediated movement. Its contribution here is therefore an alternative observation process that could be curated into a recent quantitative-trait dataset, not a claim of globally superior or unbiased citizen-science data (Jarić et al., 2020; Appendices S1–S2).

Making the national pattern visible did not solve causal attribution; it exposed it. Climate and terrain are spatially structured, unresolved history is spatially structured, and Bombus SDMs are built from environmental predictors. We therefore first estimated the broad environmental and continuous spatial template. We then asked whether the environmental terms supported by that final model organized held-out phenotype divergence beyond a fixed space-only expectation. Only after those broad layers were established did we change comparison unit for local pollinator and human-context questions.""",
    )

    text = replace_section(
        text,
        "### 1.4 From broad geography to local boundaries and human overlays",
        "### 1.5 Predictions and exploratory human-context question",
        """### 1.4 From broad geography to local boundaries and human overlays

The local pollinator test has a species-specific natural-history basis. The large tubular flowers restrict the plausible set of effective visitors, and geographical changes in *Bombus* fauna have been linked to pollen-removal ecology and breeding-system change in *C. punctata*, including the bumblebee-absence hypothesis developed from the Izu Islands (Inoue & Amano, 1986; Inoue, 1988; Nagano et al., 2014). The Izu evidence motivates why bumblebees are focal; it does not show that bumblebee absence caused white flowers in the present data.

A national flower-colour–Bombus regression would remain ambiguous because both maps inherit climate and mountain geography. We therefore fixed 67 non-overlapping white-pigmented boundaries within 5 km before reading Bombus values, environmental values or contrast direction. The 5-km radius was the finest predeclared scale with enough replicated transitions, not an exact foraging or selection distance. Mean, median and sign proportion were all required because a minority of large local contrasts could raise the mean. Montane and alpine species were retained as a guardrail: if their stronger-looking national overlap disappeared after elevation matching, shared mountain geography—not a second pollinator mechanism—would be the defensible interpretation.

The broad natural template also makes contemporary human context testable without first declaring a small set of anomalies. For every 1-km flower cell, we measured distance to the nearest other cell with the same colour state and scaled that distance by local spacing of all flower cells. We then replayed the full geometry on 10,000 predictive maps. This all-cell analysis was developed after inspection of earlier human-context patterns and is explicitly post hoc. The earlier 16-event detector is retained in Appendix S6 to calibrate extreme local configurations and preserve field/provenance targets, not as the statistical foundation of the Main human-context claim.""",
    )

    text = replace_section(
        text,
        "### 1.5 Predictions and exploratory human-context question",
        "## 2. Materials and Methods",
        """### 1.5 Predictions and exploratory human-context question

The confirmatory sequence contained two dependent predictions. First, pigmentation state and pigmented-only intensity should show different broad environmental and spatial geography. Second, if focal bumblebees contribute locally to maintaining visible pigmentation, at least some nearby white-pigmented boundaries should show greater habitat support on the pigmented side; a uniform national or intensity-wide effect was not required.

Two supporting questions then tested how much stronger those interpretations could become. A model-informed distance analysis asked whether separation along the environmental terms supported by each final model ordered held-out phenotype divergence beyond fixed spatial continuity. The all-cell isolation analysis asked whether pigmented occurrences farther from other pigmented occurrences were more population-exposed than fitted natural geography predicted, after correcting for local flower-cell spacing. The latter is reported as an exploratory generalisation, not as preregistered confirmation.""",
    )

    text = replace_section(
        text,
        "### 2.2 From photographs to a two-part phenotype",
        "### 2.3 Broad environmental and continuous spatial geography",
        """### 2.2 From photographs to a two-part phenotype

The image pipeline retained source provenance, date, coordinates and image hashes. Display-referred sRGB pixels within the confirmed petal region were summarized and median RGB was converted to CIELAB under D65. CIELAB a* is used here as a reproducible human-visible red-green phenotype, not as calibrated spectral reflectance, UV contrast or anthocyanin concentration.

The white-pigmented boundary was estimated from a* alone before geography, environment, Bombus predictions or human data were used. Gaussian-mixture classification gave an operational boundary of a*=4.9688. The final dataset contained 966 white-like and 956 pigmented observations. Pigmentation state was analysed across all 1,922 observations, and standardized visible intensity was analysed only among pigmented flowers.

Intensity is therefore a conditional response. If measured or unmeasured factors influence both entry into the pigmented subset and intensity within it, conditioning on pigmentation can alter the composition of the analysed subset. Separating state from intensity remains necessary to avoid treating white flowers as merely very pale pigmented flowers, but the two fitted parts are not interpreted as causally independent pathways. Appendix S2 reports mixture, ambiguity and phenotype sensitivities.""",
    )

    text = replace_section(
        text,
        "### 2.3 Broad environmental and continuous spatial geography",
        "### 2.4 Zooming to local focal-Bombus boundaries",
        """### 2.3 Broad environmental and continuous spatial geography

Climate predictors came from CHELSA v2.1, soils from SoilGrids 2.0 and terrain from WorldClim-derived elevation products (Karger et al., 2017; Brun et al., 2022; Poggio et al., 2021; Fick & Hijmans, 2017). Eight response-blind axes represented warm-season temperature, climatic moisture, temperature seasonality, precipitation seasonality, terrain, two soil axes and shortwave radiation. Temperature represented regulatory and thermal-balance hypotheses; moisture represented both possible pigment benefit and hydraulic context; RSDS represented broad shortwave exposure rather than UV-B; and ruggedness represented relief, aspect, drainage and microclimatic heterogeneity rather than one monotonic stress axis.

Analyses used R 4.5.3 and INLA 25.10.19 (Rue et al., 2009). Pigmentation state used a Bernoulli likelihood and conditional intensity a Gaussian likelihood. Both final observation-level models retained all eight environmental axes, an East/West structural adjustment and a stationary Matérn SPDE field (Lindgren et al., 2011; Simpson et al., 2017). The intensity model additionally retained Temperature PC1 × temperature seasonality. The field represents coherent geography remaining after measured environment and may combine unmeasured environment, population structure, dispersal and sampling geometry; its range is not a dispersal distance or genetic boundary. Model extensions required ecological motivation, acceptable collinearity and improved prediction to held-out geographical blocks.

As one model-informed corroboration, we reused the already fixed held-out cell pairs, five geographical-distance strata and 500 posterior-predictive maps from an intercept + Matérn space-only model. No environment or spatial model was refitted. For pigmentation state, environmental distance was the absolute difference in training-fold-standardized Temperature PC1, the only measured environmental term whose final 95% credible interval excluded zero. For conditional intensity, distance was Euclidean across training-fold-standardized precipitation PC1, temperature seasonality, topography PC1 and the Temperature PC1 × temperature-seasonality product. Coefficients were not used as weights; ecological direction came from the final full-model coefficients. Within each fold-by-distance stratum, phenotype divergence in the upper environmental-distance quartile was contrasted with the lower quartile and compared with the identical statistic from the fixed space-only maps. Appendix S3 gives full selection, spatial and reproducibility details.""",
    )

    text = replace_section(
        text,
        "### 2.4 Zooming to local focal-Bombus boundaries",
        "### 2.5 Continuous colour isolation and natural-geography guardrails",
        """### 2.4 Zooming to local focal-Bombus boundaries

We built SDMs for five Japanese *Bombus* species over a common mainland domain using shared predictor screening and spatial blocks. The surfaces represent habitat support rather than abundance, visitation or pollen transfer (Renner & Warton, 2013; Guillera-Arroita, 2015). Species predictions were ranked against predictions at retained occurrence sites. The primary exposure combined occurrence-referenced support for *B. ardens* and *B. diversus*, the two broad-ranging focal pollinators documented in the system. The three montane/alpine species were retained as a confounding guardrail rather than folded into the primary index.

We identified pure white-pigmented transitions among 1-km cells within 5 km and selected non-overlapping pairs without Bombus values, environmental values or final contrast direction. Only after pair identities were fixed were they oriented white-to-pigmented. We calculated pigmented-minus-white focal support and tested the mean with 100,000 sign flips. The median and proportion of positive pairs were mandatory distributional diagnostics, and 5-, 10- and 25-km tests formed one family. Environment did not select, orient or weight the pairs; final-eight-axis distance was inspected only after selection as a balance diagnostic. Appendix S5 reports raw-support alternatives, intensity tests, community turnover and equal-elevation highland comparisons.""",
    )

    text = replace_section(
        text,
        "### 3.2 Broad models separated two environmental responses and coherent residual geography",
        "### 3.3 Local boundaries revealed a short-range Bombus signal",
        """### 3.2 Broad models separated two environmental responses and coherent residual geography

Pigmentation state and conditional intensity did not tell the same geographical story. Pigmentation became less likely toward warmer Temperature PC1. The posterior mean log-odds was -0.542 (95% CrI -1.033 to -0.049), equivalent to an odds ratio of about 0.58 per SD. No state interaction met the full promotion criteria.

Among already-pigmented flowers, intensity retained Temperature PC1 × temperature seasonality (mean -0.204; 95% CrI -0.302 to -0.107): the warm-climate decline strengthened where annual temperature variability was greater. Intensity was also lower toward wetter/moister climate (-0.174; -0.323 to -0.024) and steeper, more rugged terrain (-0.134; -0.224 to -0.043), while temperature seasonality at mean Temperature PC1 was positive (+0.207; +0.044 to +0.369). RSDS did not retain an independent national effect.

Coherent geography remained after these measured associations. Residual correlation range was 132.8 km (95% CrI 88.8–195.7) for pigmentation state and 65.7 km (31.0–132.6) for conditional intensity. These are correlation scales, not seed, pollen or colonization distances.

The fixed-null environmental-distance check connected the coefficients to a stronger pairwise question. For pigmentation state, the observed high-minus-low Temperature PC1 distance contrast was 0.100608, compared with a space-only median of 0.048475 (excess +0.052133; one-sided P=0.00998). Excess was positive in all five folds and 19 of 25 fold-by-distance strata. For conditional intensity, the combined distance across precipitation, temperature seasonality, terrain and the thermal interaction yielded observed contrast 0.047416 versus space-only median 0.020897 (excess +0.026519; P=0.26347). Thus the cool-climate state association was corroborated by held-out state divergence beyond spatial continuity, whereas the supported conditional-intensity gradients did not produce that stronger signature.""",
    )

    text = replace_section(
        text,
        "### 3.3 Local boundaries revealed a short-range Bombus signal",
        "### 3.4 Continuous isolation revealed a pigmented human-context overlay",
        """### 3.3 Local boundaries revealed heterogeneous Bombus correspondence

The 5-km design produced 67 sharp pure white-pigmented transitions with median separation 2.0 km. The fixed pairs occupied tighter environmental neighbourhoods than ordinary local edges: median final-eight-axis distance was 0.244 versus 0.318.

Mean focal-Bombus support was 0.03590 higher on the pigmented side (one-sided P=0.02716), but the distribution was not a pervasive shift. The median was -0.00277, only 49.3% of pairs were positive and the 5/10/25-km family gave q=0.08148. Mean contrast declined to +0.0084 at 10 km and +0.0029 at 25 km, raw SDM support did not reproduce the 5-km result (P=0.267), and no persuasive relation appeared for conditional intensity. A subset of strong boundaries therefore raised the mean.

A separate highland guardrail showed why the local comparison was necessary. Strong national overlap between pigmented highland flowers and montane/alpine *Bombus* disappeared when nearby white and pigmented endpoints were matched on elevation (all one-sided P>=0.755 at <=50 m). The broad highland pattern was consistent with shared mountain geography, not an independent pollinator mechanism.""",
    )

    text = replace_section(
        text,
        "### 4.1 The national trait dataset changed the biological question",
        "### 4.2 Broad modelling resolved environmental structure and exposed a second spatial layer",
        """### 4.1 The national trait dataset changed the biological question

The first contribution is empirical: a non-biodiversity hiking-image stream was converted into a dense, recent and auditable national flower-colour dataset. YAMAP supplied 3.81 times the matched iNaturalist records during the same three years, and exhaustive screening, image hashing and deterministic phenotyping turned that quantity into study-specific trait quality. The missing resource was not occurrence coordinates alone, but enough images to separate a white-pigmented switch from variation within the pigmented state.

The mountain-route frame has both value and cost. It enriches settings in which natural or self-sustaining populations are plausible, but it does not guarantee wild provenance. In the human-context analysis, the same frame can narrow the urban-rural gradient, while accessible routes and mountain margins can increase both observation and human exposure. Treating these directions explicitly is more defensible than assigning YAMAP one global bias score.

The coverage changed the biological question rather than merely increasing sample size. A single white-to-dark index would have averaged over two responses that differed in environmental coefficients, residual spatial range and divergence beyond spatial continuity.""",
    )

    text = replace_section(
        text,
        "### 4.2 Broad modelling resolved environmental structure and exposed a second spatial layer",
        "### 4.3 Local boundaries reveal where pollinator opportunity may matter",
        """### 4.2 Temperature organizes the state transition more strongly than conditional intensity

The broad result is not simply that several coefficients exclude zero. For pigmentation state, the full model identifies a cool-climate direction and the fixed-null check shows that Temperature PC1 separation also orders held-out state divergence beyond the same spatial-continuity expectation. These are non-equivalent lines of evidence: one estimates a partial directional association, while the other asks whether environmentally separated locations differ phenotypically more than fitted space alone predicts.

This combined result is consistent with environmentally responsive anthocyanin regulation and with longer-term environment-aligned population differentiation. Low temperature can induce floral anthocyanin synthesis, while petal pigmentation changes radiative absorption (Shvarts et al., 1997; Li et al., 2026). The present data do not distinguish plasticity from inherited differentiation and do not estimate fitness. The space-only field is not neutral FST, and the excess is not evidence that selection exceeds drift or that local adaptation has been demonstrated.

Conditional intensity carries a different ecological signature. Moisture, temperature seasonality, terrain and the Temperature × seasonality interaction describe directional conditional geography, but their joint distance did not create pairwise divergence beyond spatial continuity. This can arise when responses vary smoothly, when an interaction surface maps environmentally distant locations to similar intensities, or when conditioning on the pigmented subset changes the analysed composition. The result therefore rejects a universal stress-darkening account without implying that the intensity coefficients are false.

The remaining spatial fields add a second positive output. Their different ranges show that unresolved geography is response specific and provide a sampling guide for microclimate, ancestry, isolation-by-distance, isolation-by-environment and admixture tests. They are not labelled as genetics in advance.""",
    )

    text = replace_section(
        text,
        "### 4.3 Local boundaries reveal where pollinator opportunity may matter",
        "### 4.4 Human landscapes overlay the spatial geometry of pigmentation",
        """### 4.3 Boundary heterogeneity is the pollinator result

The tubular floral system and Izu pollinator/breeding-system history make bumblebees a focused natural-history hypothesis rather than one arbitrary SDM layer. The Izu evidence remains motivation, not proof that bumblebee absence caused white flowers. The primary achievement of the 5-km design is that it asks the pollinator question inside repeated local colour boundaries rather than through national map overlap.

The distribution of contrasts governs interpretation. A positive mean coexisted with a near-zero median, fewer than half the pairs in the predicted direction, family q>0.05, broader-scale attenuation and failure of raw support to reproduce the result. This is not a uniform pigmented-side advantage. It is a geographic mosaic in which a minority of boundaries may have strong Bombus correspondence while many do not.

That heterogeneity is scientifically useful. The 67 boundaries can be stratified into strongly positive, near-zero and negative groups for direct tests of species-resolved visitation, bee visual contrast, stigma contact, pollen deposition, seed set, selfing and selection gradients. The highland guardrail further shows that a visually stronger national pattern can disappear when shared elevation is controlled. SDM support remains habitat opportunity, not realized visitation or selection.""",
    )

    text = replace_section(
        text,
        "### 4.4 Human landscapes overlay the spatial geometry of pigmentation",
        "### 4.5 A spatially varying balance can maintain flower-colour polymorphism",
        """### 4.4 Human landscapes overlay the spatial geometry of pigmentation

The continuous analysis changes the human-context question from a comparison among a small set of selected events to an all-cell property of the pigmented distribution. Pigmented occurrences farther from other pigmented occurrences were more population-exposed than the fitted natural geography predicted. The relation remained when same-colour distance was scaled by local spacing of all flower cells and when predictive maps were restricted toward the observed pigmented-cell count.

The guardrails prevent the most eye-catching contrast from being overstated. White cells showed a negative raw isolation-population relation, but that sign disappeared after density correction, and raw white rho was almost exactly the natural expectation. The defensible conclusion is therefore an additional positive human-context association within pigmented occurrences, not perfectly reciprocal displacement of the two colour states.

The scale profile does not identify mechanism. Horticultural planting or escape, human-modified establishment conditions, fine-scale plasticity and access-linked observation remain alternatives. The analysis is explicitly post hoc, and even small Monte Carlo P values do not convert the pattern into provenance evidence. Repeated field history, vouchers, standardized spectra and pigment chemistry, local environmental measurements and genomic comparison with neighbouring white and horticultural material are required.

The threshold event family remains useful in Appendix S6. Its 16 local configurations were not more frequent than natural maps expected, so they are not evidence that nature fails to reproduce them. They remain reproducible extreme targets rather than the basis of the Main human-context inference.""",
    )

    text = replace_section(
        text,
        "### 4.5 A spatially varying balance can maintain flower-colour polymorphism",
        "## Acknowledgements",
        """### 4.5 A spatially varying balance can maintain flower-colour polymorphism

The study converges on one cross-scale model. Temperature can alter the regulation, physiological value and thermal cost of pigmentation. Unresolved historical and population processes can preserve or redistribute variants. Local bumblebee opportunity may modify the reproductive value of maintaining a visible pigmented state in some neighbourhoods. Human landscapes can overlay that natural geography by changing where isolated pigmented occurrences persist or are observed.

These processes are not interchangeable predictors. They leave different signatures on different phenotype components and at different comparison scales. Pigmentation state carries both a directional cool-climate coefficient and temperature-aligned divergence beyond space. Conditional intensity carries conditional moisture, seasonality and terrain gradients without the same divergence signature. Bombus correspondence is concentrated in a subset of local boundaries, while human context appears in the geometry of isolated pigmented occurrences.

The broader biogeographic advance is therefore scale-matched integration. Repurposed images recover the phenotype; broad models separate named environmental gradients from coherent geography; fixed spatial-null maps ask whether supported gradients organize extra divergence; local boundaries prevent national biotic overlap from becoming mechanism; and all-cell geometry asks how contemporary human context overlays the natural distribution. Positive, heterogeneous and null results perform different inferential jobs while defining direct physiological, pollination, field-history and genomic tests.""",
    )

    write_if_changed(MAIN, text)


def update_s3() -> None:
    text = S3.read_text(encoding="utf-8")
    old = (
        "- The **cross-fitted space-only divergence sensitivity** asks whether environmental difference orders held-out phenotype divergence beyond an intercept + Matérn SPDE continuity expectation at comparable geographical separation. It uses a separate frozen six-score broad/within environmental representation and does not reinterpret the spatial field as genetic drift.\n"
        "- The **1-km-cell cross-fitted pigmentation-state reference** generates natural predictive maps for Main 3. Its environmental basis is the same finalized eight measured abiotic axes used by the Broad state analysis, and the resulting event detector yields 16 observed local-departure candidates."
    )
    new = (
        "- The **fixed-null supported-term distance check** reuses already fixed held-out pairs and 500 cached intercept + Matérn SPDE predictive maps. It asks whether distance along the environmental terms supported by each final model orders phenotype divergence beyond spatial continuity without refitting the environment or spatial model.\n"
        "- The **1-km-cell cross-fitted pigmentation-state reference** generates 10,000 natural maps used by the continuous-isolation and supplementary event analyses. Its environmental basis is the same finalized eight measured abiotic axes used by the Broad state analysis."
    )
    if old in text:
        text = text.replace(old, new, 1)

    text = replace_section(
        text,
        "## Cross-fitted space-only phenotype-divergence sensitivity",
        "## Current 1-km natural reference passed to Main 3",
        """## Supported environmental-term distance versus fixed spatial continuity

This supporting analysis connects the final observation-level coefficients to a stronger pairwise question: at comparable geographical separation, are locations that differ more along the environmental terms supported by the final response-specific model also more phenotypically different than the already fitted space-only expectation predicts?

No environment or spatial model was refitted. The analysis reused the frozen 1-km cell table, the fixed response-specific held-out pair identities, five geographical-distance strata in each of five folds, and exactly 500 cached posterior-predictive maps from the cross-fitted `intercept + Matérn SPDE` null.

Environmental distance was response specific and scaled on the relevant training folds:

- **pigmentation state:** absolute difference in Temperature PC1, the only final measured environmental term whose 95% CrI excluded zero;
- **conditional intensity:** Euclidean distance across precipitation PC1, temperature seasonality, topography PC1 and the standardized Temperature PC1 × temperature-seasonality product.

No posterior coefficient was used as a distance weight. The test is unsigned; ecological direction comes from Table S3.2–S3.3. Within every fold-by-geographical-distance stratum, phenotype divergence among the upper environmental-distance quartile was contrasted with the lower quartile and compared with the identical statistic from each cached space-only map.

**Table S3.5. Supported-term phenotype divergence beyond fixed spatial continuity.** P is the predefined one-sided upper-tail posterior-predictive probability.

| Response | Supported environmental distance | Observed high − low divergence | Space-only median | Excess | One-sided P |
|---|---|---:|---:|---:|---:|
| Pigmentation state | Temperature PC1 | **0.100608** | **0.048475** | **+0.052133** | **0.00998** |
| Conditional visible intensity | precipitation + temperature seasonality + topography + Temperature × seasonality | 0.047416 | 0.020897 | +0.026519 | 0.26347 |

State excess was positive in all five geographical folds and 19 of 25 fold-by-distance strata. Intensity excess was positive in 13 of 25 strata and only two of five fold means. The supported-term result is therefore response specific: the directional cool-climate state association is corroborated by held-out state divergence beyond fixed spatial continuity, whereas the combined supported conditional-intensity gradients do not produce that stronger signature.

This remains model-informed corroboration rather than independent variable discovery. The spatial null represents unresolved geography. A positive excess does not distinguish plasticity, selection, population differentiation or omitted environmental processes and does not demonstrate local adaptation.""",
    )

    text = replace_section(
        text,
        "## Current 1-km natural reference passed to Main 3",
        "## Ecological interpretation and claim ceiling",
        """## Natural predictive maps passed to the human-context analyses

The downstream natural reference is distinct from the observation-level coefficient model but aligned with its finalized pigmentation-state environment. It uses binomial pigmentation counts for all 1,305 environment-complete cells, the same eight measured abiotic axes, five approximately 100-km geographical folds, a cross-fitted SPDE and 10,000 checksum-locked predictive maps under the observed cell geometry and trial counts.

The Main human-context analysis recomputes same-colour nearest-neighbour geometry on every map and compares pigmented isolation-population relationships with their natural expectation. Appendix S6 also retains a stricter event detector—a pigmented cell among at least three environmentally similar white neighbours within 10 km—as supplementary calibration and a reproducible field-target selector. Sixteen observed event cells are compatible with the natural-map count and fraction distributions and are not interpreted as nature-resistant or anthropogenic anomalies.""",
    )

    text = replace_section(
        text,
        "## Ecological interpretation and claim ceiling",
        "## Remaining biological gaps",
        """## Ecological interpretation and claim ceiling

The final Broad result is response specific.

- **Pigmentation state:** Temperature PC1 is the clearest measured environmental association, with greater pigmentation toward the cooler end. Distance along that same supported term orders held-out state divergence beyond fixed spatial continuity.
- **Conditional intensity:** moisture, temperature seasonality, terrain and the thermal interaction describe directional conditional geography, but their joint distance does not exceed the fixed space-only expectation.
- **Residual geography:** substantial continuous structure remains at different ranges and may combine unmeasured environment, population history, dispersal and sampling geometry.

The combined state evidence is consistent with environmental regulation and environment-aligned differentiation, but it does not distinguish plasticity from inherited differentiation, estimate fitness or demonstrate adaptation. Conditional intensity is observed only after pigmentation is present and can be affected by selection into that subset. The SPDE field and distance test do not identify a genetic mechanism, and neither result demonstrates selection or local adaptation.

The Broad model also does not turn an environment-derived Bombus SDM into an independent national causal predictor. Those limits motivate the separate local boundary design in Appendix S5 and the natural-map human-context design in Appendix S6.""",
    )

    text = replace_tail_section(
        text,
        "## Reproducibility resources",
        """## Reproducibility resources

Current observation-level Broad evidence is locked in:

- `reproducibility/broad_environment_spatial_final_model_2026-08-11.md`;
- `reproducibility/broad_environment_spatial_final_fixed_effects_2026-08-11.csv`;
- `reproducibility/broad_environment_spatial_final_hyperparameters_2026-08-11.csv`;
- `reproducibility/environment_interaction_inla_screen_spec_2026-08-11.md`;
- `reproducibility/broad_environment_spatial_audit_spec_2026-08-11.md`.

The final supported-term distance check is locked in:

- `scripts/fit_broad_supported_term_distance_space_null.R`;
- `.github/workflows/broad-supported-term-distance-space-null.yml`;
- `reproducibility/broad_supported_term_distance_space_null_2026-08-19.md`;
- workflow `32211506278`;
- artifact `9350975649`, SHA-256 `4d5a1d28b8313cc0fb6c85484d21c6d94535ac7cc0881e83dc7ed02678854f03`.

The integrated downstream handoff is documented in:

- `reproducibility/final_analysis_pipeline_integration_2026-08-19.md`;
- `reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md`;
- Appendices S5–S6.""",
    )
    write_if_changed(S3, text)


def update_background() -> None:
    text = """# Introduction plan — one mystery, four dependent reveals

This is the editorial lock for the Introduction, Discussion and four Main figures. The paper is one investigation, not four parallel analyses.

## Single biological question

> How is within-species flower-colour geography assembled from physiological, spatial, biotic and contemporary human-context processes that operate at different scales?

## Act 1 — Recover the phenotype that available databases did not provide

The gap is not that quantitative flower-colour studies do not exist. It is that no ready-made focal-species database supplied enough recent, georeferenced images to resolve both the white-pigmented switch and continuous variation among pigmented flowers across Japan.

YAMAP is an alternative observation process, not a globally unbiased replacement for biodiversity platforms. Its mountain-route frame enriches plausible natural settings while also conditioning access and human exposure. Author screening and deterministic phenotyping convert that image stream into two responses.

## Act 2 — Separate named environmental gradients from coherent geography

A dense trait map makes attribution harder because environment, history, sampling and environment-derived SDMs share space. The final environment + SPDE models provide directional coefficients and residual spatial ranges.

The supporting comparison uses the same model-supported environmental terms and fixed space-only maps. It does not rebuild the model or search across alternative axes. Pigmentation state passes both stages through Temperature PC1; conditional intensity retains directional coefficients but not divergence beyond space.

## Act 3 — Change comparison unit for the pollinator hypothesis

The tubular floral system and Izu natural history justify bumblebees as a focused hypothesis, but Izu does not prove colour selection. National overlap is insufficient because Bombus SDMs share climate and mountain geography.

The primary unit is therefore 67 independently fixed local white-pigmented boundaries. The result is heterogeneity: a positive mean is driven by a subset of boundaries, while the median, sign proportion, family q, raw-support and scale sensitivities prevent a pervasive-shift claim. Montane species provide an elevation-confounding guardrail.

## Act 4 — Test a continuous human overlay on the natural geometry

The Main human-context estimand is threshold free: nearest same-colour distance for every flower cell, corrected by local flower-cell spacing and replayed on 10,000 natural maps. The robust result is an additional positive isolation-population relationship within pigmented occurrences, not a reciprocal white-pigmented displacement and not provenance.

The earlier 16-event family remains in Appendix S6. It calibrates extreme local configurations and preserves reproducible field targets, but event frequency is compatible with natural maps and does not define the Main human claim.

## Locked causal chain

`incidental images -> two-part quantitative phenotype -> named environment + continuous space -> supported-term divergence beyond fixed space -> heterogeneous local Bombus boundaries -> continuous pigmented human-context overlay -> direct field, physiological and genomic tests`

## Forbidden promotions

- YAMAP records are all wild or human-bias free.
- The SPDE field is genetics, drift or dispersal distance.
- Supported-term divergence demonstrates selection or local adaptation.
- Bombus SDM support is visitation, pollen transfer or selection.
- The Izu contrast proves that bumblebee absence caused white flowers.
- The continuous isolation pattern proves planting, escape or horticultural origin.
- The 16 event cells are anthropogenic anomalies.

## Reader movement

1. The phenotype could not be measured nationally at the required resolution.
2. Once measured, state and intensity separate.
3. Once mapped, shared geography becomes the causal problem.
4. Temperature organizes the state switch beyond fixed spatial continuity; intensity does not show the same signature.
5. The pollinator question must move to repeated local boundaries.
6. Boundary heterogeneity replaces a national Bombus story.
7. Human context appears in all-cell pigmented geometry, while provenance remains unresolved.
"""
    write_if_changed(BACKGROUND, text)


def update_figure_text() -> None:
    captions = CAPTIONS.read_text(encoding="utf-8")
    captions = replace_section(
        captions,
        "## Figure 2. Pigmentation state and colour intensity follow different broad geography",
        "## Figure 3. Local boundaries identify a short-range Bombus correspondence",
        """## Figure 2. Pigmentation state and colour intensity follow different broad geography

Broad environmental and spatial patterns of *Campanula punctata* across Japan. (a) Environmental coefficients from the final INLA-SPDE models. Pigmentation state uses an additive model; conditional intensity includes Temperature PC1 × temperature seasonality. Intervals are 95% credible intervals. (b) Cross-fitted probability of pigmentation from the final eight-axis natural model. (c) Cross-fitted colour intensity among cells with pigmented observations. Map panels use WGS84 longitude/latitude and include a 100-km bar scale. (d) Residual spatial ranges and prediction to held-out geographical blocks. The ranges are remaining correlation scales, not dispersal or genetic boundaries. In the supporting fixed-null comparison, Temperature PC1 distance ordered pigmentation-state divergence beyond spatial continuity (excess +0.05213; P=0.00998), whereas the combined supported-term distance for conditional intensity did not (P=0.26347).""",
    )
    captions = captions.replace(
        "Together, the panels localize the plausible biotic contribution to short-range pigmentation-state boundaries;",
        "Together, the panels show heterogeneous rather than pervasive local correspondence and localize any plausible biotic contribution to a subset of pigmentation-state boundaries;",
    )
    write_if_changed(CAPTIONS, captions)

    plan = PLAN.read_text(encoding="utf-8")
    plan = plan.replace(
        "**Reveal:** state and intensity differ, and coherent spatial structure remains after measured environment.",
        "**Reveal:** state and intensity differ, coherent spatial structure remains, and only Temperature-supported state distance produces held-out divergence beyond fixed spatial continuity. The supported intensity terms do not.",
    )
    plan = plan.replace(
        "**Reveal:** focal-Bombus support is higher on the pigmented side on average at the finest replicated scale. Its concentration in a subset of boundaries, attenuation at 10 and 25 km and absence along the intensity axis localize the plausible contribution to short-range maintenance of pigmentation state.",
        "**Reveal:** a positive mean is driven by a subset of boundaries. The near-zero median, 49.3% positive pairs, family q, broader-scale attenuation, raw-support failure and intensity null make boundary heterogeneity—not a pervasive shift—the biological result.",
    )
    write_if_changed(PLAN, plan)


def update_japanese_abstract() -> None:
    text = """# 日本語要旨（Translated Abstract Not for Review 用）

**目的：** 種内の花色地理は、非生物環境、未測定の歴史的・空間的過程、送粉者、人間活動が異なるスケールで重なって形成されうる。本研究では、ホタルブクロの白／有色という色素発現状態と、有色花内の可視色強度を分け、各過程に合った比較単位で花色多型の地理を解析した。

**調査地域：** 日本。

**対象分類群：** ホタルブクロ *Campanula punctata*（キキョウ科）。

**方法：** 2023–2025年のYAMAP写真1,922件を目視確認し、二段階の定量形質を構築した。最終environment + INLA-SPDEモデルで環境係数と残存空間構造を推定した。さらに、既に固定された地点ペアと500枚のspace-only予測地図を再利用し、総モデルで支持された環境項目の距離が空間連続性を超える花色差に対応するかを検定した。送粉者については5 km以内の67個の白―有色境界を事前に固定した。人為的文脈は、全1,305セルの同色最近接距離を局所花セル密度で補正し、10,000枚の自然予測地図と比較した。

**結果：** 有色化確率は暖かいTemperature PC1側で低かった。Temperature PC1距離が大きい地点ほど白／有色stateの差がspace-only期待を上回った（超過0.0521、P=0.00998）。一方、有色花内強度で支持された降水、気温季節性、地形、温度×季節性の総合距離は空間期待を超えなかった（P=0.26347）。局所Bombus比較では平均差は有色側で正だったが、中央値はほぼ0、正のペアは49.3%であり、対応は一部境界に集中した。674個の有色セルでは、同じ有色セルから孤立するほど5 km人口曝露が高く（rho=0.252）、自然地図の期待を上回った（P=0.0002）。局所花セル間隔で補正しても残った（rho=0.285、P=0.0009）。白花の逆方向関係は密度補正後に残らなかった。

**主な結論：** 白／有色stateでは、冷涼側への方向的対応と、温度差に沿う空間期待以上の分化が一致した。有色花内強度は別の条件付き環境勾配をもつが、同じ強い分化署名は示さなかった。Bombus対応は一様ではなく局所的に異質であり、人為的文脈は孤立した有色分布への上乗せとして現れた。これらは局所適応、園芸由来、人為的因果を証明せず、次の生理・訪花・植栽史・遺伝解析を具体化する。
"""
    write_if_changed(JA, text)


def update_cover() -> None:
    text = """# Cover letter draft — Journal of Biogeography

Dear Senior Editors,

We submit our Research Article, “From broad geography to local boundaries: biogeography of flower-colour polymorphism from hiking photographs.” We converted screened YAMAP images into a national two-part flower-colour phenotype and used scale-matched comparisons to separate environmental, residual-spatial, local focal-Bombus and contemporary human-context layers. The final Broad analysis links a directional cool-climate coefficient to held-out pigmentation-state divergence beyond fixed spatial continuity without rebuilding the model. Local Bombus correspondence is heterogeneous rather than pervasive, and continuous all-cell geometry shows that isolated pigmented occurrences are more population-exposed than natural maps predict after sampling-density correction. The conceptual advance is integration that connects rather than collapses scales, while keeping adaptation, pollination and provenance claims within the evidence.

Sincerely,

[Corresponding author]
[Affiliation]
[Email]
"""
    write_if_changed(COVER, text)


def update_paper_overview() -> None:
    readme = """# Current paper

This directory maps the active JBI paper and its checksum-locked evidence chain. The paper asks how one within-species flower-colour geography is assembled by processes operating at different scales.

## Final scientific sequence

1. **Trait construction:** 1,922 screened YAMAP observations become pigmentation state and pigmented-only intensity.
2. **Broad environment + space:** the final observation-level INLA-SPDE models provide directional coefficients and residual spatial ranges. Pigmentation state is less likely toward warmer Temperature PC1. Conditional intensity retains precipitation, temperature-seasonality, terrain and Temperature × seasonality associations.
3. **Supported-term distance versus fixed space:** no model is rebuilt. Fixed held-out pairs and 500 cached space-only maps show that Temperature PC1 distance orders pigmentation-state divergence beyond spatial continuity (observed 0.100608; null 0.048475; excess 0.052133; P=0.00998). The combined supported intensity-term distance does not (P=0.26347).
4. **Local Bombus boundaries:** 67 fixed pairs give mean +0.03590, median -0.00277, 49.3% positive and q=0.08148. The result is boundary heterogeneity; equal-elevation analyses reject stronger-looking highland overlap as an independent mechanism.
5. **Continuous human-context geometry:** among 674 pigmented cells, raw isolation–5-km population rho=0.251980 exceeds natural mean 0.132980 (P=0.000200). Density-corrected rho=0.285498 exceeds natural mean 0.153616 (P=0.000900). A reciprocal white displacement is not robust.
6. **Supplementary event calibration:** 16 restrictive pigmented-among-white events are compatible with natural maps and remain field/provenance targets rather than the basis of the Main human claim.

## Claim ceiling

The spatial null is unresolved geography, not drift or neutral genetic differentiation. The results do not establish selection, local adaptation, realized pollination, horticultural origin, planting, escape, plasticity or gene flow.

## Entry points

- Manuscript: `submission/jbi/JBI_main_manuscript_anonymized.md`
- Broad details: `submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md`
- Human details: `submission/jbi/supporting/Appendix_S6_event_departures_human_context.md`
- Analysis map: `paper/analysis-map.md`
- Audit: `python run_pipeline.py audit`
- Reproduce: `python run_pipeline.py reproduce`
"""
    write_if_changed(PAPER_README, readme)

    amap = """# Current analysis map

| Layer | Question | Comparison unit | Final evidence |
|---|---|---|---|
| Trait construction | Can hiking photographs recover quantitative intraspecific colour geography? | 1,922 observations | state and conditional intensity |
| Broad full model | Which named environmental gradients remain after continuous space? | observations | state Temperature PC1; conditional precipitation, seasonality, terrain and interaction; response-specific ranges |
| Supported-term fixed null | Do supported environmental distances organize extra held-out divergence? | fixed cell pairs in 25 fold-distance strata | state excess +0.052133, P=0.00998; intensity P=0.26347 |
| Local Bombus | Does focal support change across fixed local colour boundaries? | 67 pairs | subset-driven mean +0.03590; median -0.00277; 49.3% positive; q=0.08148 |
| Continuous human context | Does same-colour isolation track population exposure beyond natural geography and sampling density? | all 1,305 cells | pigmented raw P=0.000200; relative P=0.000900 |
| Event calibration | Are restrictive local configurations excessive? | 16 cells | count/fraction null; supplementary targets |

## Active implementations

- Broad full model: `analysis_sensitivity/run_broad_environment_spatial_sensitivity.R`
- Supported-term fixed-null check: `scripts/fit_broad_supported_term_distance_space_null.R`
- Continuous isolation: `analysis_sensitivity/run_continuous_colour_isolation_human_context.R`
- Final pipeline contract: `config/paper_pipeline.lock.json`
- Final integration record: `reproducibility/final_analysis_pipeline_integration_2026-08-19.md`

The supported-term and continuous-isolation analyses are model-informed corroborations. They do not convert spatial association into adaptation or provenance.
"""
    write_if_changed(ANALYSIS_MAP, amap)


def update_lock_and_map() -> None:
    lock = json.loads(LOCK.read_text(encoding="utf-8"))
    lock["paper_version"] = "jbi-2026-08-19-final-supported-distance-continuous-isolation"
    lock["provenance_boundary"]["interpretation"] = (
        "The reproduce profile restores the accepted observation-level Broad evidence, "
        "the supported-term fixed-space result, seeded Bombus SDMs, final-eight natural maps, "
        "event calibration and validated continuous colour isolation before rebuilding figures "
        "and the JBI review package. The supported-term comparison does not refit the model; "
        "continuous isolation is explicitly post hoc with frozen guardrails."
    )
    lock["artifacts"]["accepted_broad_supported_term_distance"] = {
        "id": SUPPORTED_ARTIFACT_ID,
        "sha256": SUPPORTED_ARTIFACT_SHA256,
        "destination": SUPPORTED_RESULT_ROOT,
        "merge": False,
        "required": SUPPORTED_REQUIRED,
        "role": "validated no-refit supported-term environmental-distance comparison against fixed space-only maps",
        "references": [
            "scripts/fit_broad_supported_term_distance_space_null.R",
            ".github/workflows/broad-supported-term-distance-space-null.yml",
            "reproducibility/broad_supported_term_distance_space_null_2026-08-19.md",
            "submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md",
        ],
    }
    lock["stages"]["restore_broad_supported_term_distance"] = {
        "kind": "artifact",
        "artifact": "accepted_broad_supported_term_distance",
        "outputs": SUPPORTED_REQUIRED,
    }
    reproduce = [
        name for name in lock["profiles"]["reproduce"]
        if name not in {"run_broad_space_null_excess", "restore_broad_supported_term_distance"}
    ]
    insert_at = reproduce.index("restore_broad") + 1
    reproduce.insert(insert_at, "restore_broad_supported_term_distance")
    lock["profiles"]["reproduce"] = reproduce

    required_files = lock["alignment"]["required_files"]
    for item in (
        "scripts/fit_broad_supported_term_distance_space_null.R",
        ".github/workflows/broad-supported-term-distance-space-null.yml",
        "reproducibility/broad_supported_term_distance_space_null_2026-08-19.md",
        "scripts/upgrade_final_analysis_pipeline.py",
        "reproducibility/final_analysis_pipeline_integration_2026-08-19.md",
    ):
        if item not in required_files:
            required_files.append(item)

    checks = []
    for check in lock["alignment"]["checks"]:
        patterns = " ".join(check.get("patterns", []))
        if any(token in patterns for token in ("0\\.106802", "0\\.03393", "0\\.048562")):
            continue
        if check.get("label") in {
            "paper overview mirrors continuous and event roles",
            "paper overview mirrors all current results",
        }:
            continue
        checks.append(check)
    checks.extend([
        {
            "label": "supported-term distance beyond fixed space",
            "path": "submission/jbi/JBI_main_manuscript_anonymized.md",
            "patterns": ["0\\.100608", "0\\.048475", "0\\.052133", "P=0\\.00998", "P=0\\.26347"],
        },
        {
            "label": "paper overview mirrors final Broad and human hierarchy",
            "path": "paper/README.md",
            "patterns": ["1,922", "0\\.100608", "0\\.00998", "67", "0\\.03590", "674", "0\\.251980", "0\\.285498", "16"],
        },
    ])
    lock["alignment"]["checks"] = checks
    LOCK.write_text(json.dumps(lock, ensure_ascii=False, separators=(",", ":")) + "\n", encoding="utf-8")

    with ACTIVE_MAP.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    existing = {row["path"] for row in rows}
    additions = [
        ("analysis", "Broad", "scripts/fit_broad_supported_term_distance_space_null.R", "component"),
        ("workflow", "Broad", ".github/workflows/broad-supported-term-distance-space-null.yml", "component"),
        ("reproducibility", "Broad", "reproducibility/broad_supported_term_distance_space_null_2026-08-19.md", "provenance"),
        ("infrastructure", "integration", "scripts/upgrade_final_analysis_pipeline.py", "component"),
        ("reproducibility", "integration", "reproducibility/final_analysis_pipeline_integration_2026-08-19.md", "provenance"),
    ]
    for role, section, path, status in additions:
        if path not in existing:
            rows.append({"role": role, "section": section, "path": path, "status": status})
            existing.add(path)
    with ACTIVE_MAP.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=["role", "section", "path", "status"])
        writer.writeheader()
        writer.writerows(rows)


def update_validators_and_checklist() -> None:
    for path in (SUBMISSION_VALIDATOR, ALIGNMENT_VALIDATOR):
        text = path.read_text(encoding="utf-8")
        replacements = {
            "0.106802": "0.100608",
            "0.058240": "0.048475",
            "0.048562": "0.052133",
            "0.03393": "0.00998",
            "0\\.106802": "0\\.100608",
            "0\\.058240": "0\\.048475",
            "0\\.048562": "0\\.052133",
            "0\\.03393": "0\\.00998",
        }
        for old, new in replacements.items():
            text = text.replace(old, new)
        if path == SUBMISSION_VALIDATOR and "0.26347" not in text:
            marker = '        "0.00998",\n'
            if marker in text:
                text = text.replace(marker, marker + '        "0.26347",\n', 1)
        write_if_changed(path, text)

    checklist = CHECKLIST.read_text(encoding="utf-8")
    checklist = checklist.replace("0.106802", "0.100608")
    checklist = checklist.replace("0.058240", "0.048475")
    checklist = checklist.replace("0.048562", "0.052133")
    checklist = checklist.replace("0.03393", "0.00998")
    checklist = checklist.replace(
        "A cross-fitted space-only sensitivity further shows environment-aligned divergence beyond spatial continuity for pigmentation state but not conditional intensity",
        "A no-refit supported-term comparison further shows Temperature-aligned divergence beyond fixed spatial continuity for pigmentation state, while the combined supported conditional-intensity distance remains within the spatial expectation",
    )
    checklist = checklist.replace(
        "**Bombus killing part:** boundary heterogeneity is the Main result.",
        "**Bombus killing part:** boundary heterogeneity is the Main result; mean, median, sign proportion, family q, scale attenuation and raw-support failure must be reported together.",
    )
    write_if_changed(CHECKLIST, checklist)


def update_integration_note() -> None:
    text = """# Final analysis-pipeline manuscript integration

Date: 2026-08-19

## Integrated accepted results

1. Final observation-level environment + INLA-SPDE models.
2. PR #58 supported-term environmental distance against fixed space-only posterior maps, with no model refit.
3. The 67-boundary heterogeneous focal-Bombus comparison and elevation guardrail.
4. PR #57 continuous same-colour isolation as the Main exploratory human-context estimand.
5. The 16-event family retained only in Appendix S6 as calibration and field targeting.

## Final numerical hierarchy

- State Temperature PC1: -0.54185, 95% CrI -1.03294 to -0.04859.
- State supported-distance contrast: observed 0.100608; fixed-space median 0.048475; excess 0.052133; P=0.00998.
- Intensity supported-distance contrast: observed 0.047416; fixed-space median 0.020897; P=0.26347.
- Local Bombus: 67 pairs; mean +0.03590; median -0.00277; 49.3% positive; q=0.08148.
- Continuous pigmented isolation: raw rho=0.251980, natural mean=0.132980, P=0.000200; relative rho=0.285498, natural mean=0.153616, P=0.000900.
- Event calibration: 16 cells; count P=0.27897; fraction P=0.12609; event human maxT P=0.05479.

## Claim ceiling

The paper does not claim selection, local adaptation, neutral genetic divergence, realized pollination, horticultural origin, planting, escape, plasticity or gene flow. The supported-term test is model-informed corroboration, and continuous isolation is an explicitly post hoc analysis with frozen density and natural-map guardrails.
"""
    write_if_changed(INTEGRATION_NOTE, text)


def main() -> None:
    update_main()
    update_s3()
    update_background()
    update_figure_text()
    update_japanese_abstract()
    update_cover()
    update_paper_overview()
    update_lock_and_map()
    update_validators_and_checklist()
    update_integration_note()


if __name__ == "__main__":
    main()
