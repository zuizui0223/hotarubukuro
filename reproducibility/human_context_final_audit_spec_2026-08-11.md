# Final Broad-to-human-context audit specification

Date: 2026-08-11

## Purpose

This audit closes three linked questions before the JBI analysis is treated as final:

1. whether the Broad environmental model should use a hard VIF threshold of 5 rather than 10;
2. whether the final Broad environmental representation changes the cell-level natural reference and the local-departure targets used upstream of the human-context analysis; and
3. whether the post-selection human features correspond to biologically interpretable anthropogenic mechanisms rather than a proliferating collection of correlated urbanization indices.

The audit does not assume that a local colour departure has human origin. Human variables remain post-selection context variables.

## Collinearity policy

VIF thresholds are diagnostic conventions rather than universal model-validity boundaries. O'Brien (2007) cautioned explicitly against treating VIF=10 as an automatic rejection rule. Zuur, Ieno & Elphick (2010) showed that ecological applications may choose much stricter thresholds, even around 3, especially when inference about individual covariates is the goal. Dormann et al. (2013) likewise emphasized that collinearity can destabilize ecological coefficient inference and should be handled in relation to the inferential goal rather than by one mechanical threshold.

For this paper the final policy is therefore tiered:

- **VIF <5:** preferred and accepted without a collinearity warning;
- **VIF 5–10:** acceptable only after explicit stability review: the focal ecological term itself should preferably remain below 5, coefficient direction and interval must be stable to defensible reparameterizations/guardrails, spatial hyperparameters must remain stable and geographically blocked prediction must support the model;
- **VIF >10:** a candidate environmental expansion is not promoted unless there is exceptional mechanistic and predictive justification; in practice the present VPD expansions fail this rule independently of the threshold because their VIFs exceed 25 and transfer does not improve.

The stricter value 5 is therefore used as a **preferred diagnostic**, not a hard deletion threshold. A hard VIF<5 rule would make the retained conditional-intensity model depend on removing the structural East/West adjustment even though the focal thermal interaction itself has low VIF and the no-region spatial sensitivity did not show a transferable improvement.

References:

- O'Brien RM (2007) A caution regarding rules of thumb for variance inflation factors. *Quality & Quantity* 41:673–690. DOI: 10.1007/s11135-006-9018-6.
- Zuur AF, Ieno EN, Elphick CS (2010) A protocol for data exploration to avoid common statistical problems. *Methods in Ecology and Evolution* 1:3–14. DOI: 10.1111/j.2041-210X.2009.00001.x.
- Dormann CF et al. (2013) Collinearity: a review of methods to deal with it and a simulation study evaluating their performance. *Ecography* 36:27–46. DOI: 10.1111/j.1600-0587.2012.07348.x.

## Why downstream propagation must be tested

The current paper deliberately separates two Broad products:

1. an observation-level eight-axis INLA-SPDE model used for environmental coefficient interpretation; and
2. a cell-level cross-fitted natural reference based on four broad/within-50-km environmental PCs, used to generate natural maps and define/calibrate local departures.

The final observation-level model decision did not rewrite the second product. That separation is computationally valid, but it does not establish scientific invariance. The final eight environmental axes are already present in the 1-km cell table, so the downstream audit can test propagation without acquiring new data.

The audit therefore compares:

- current four-PC environmental matching + current four-PC natural reference;
- final eight-axis environmental matching + current natural reference;
- current matching + a refitted final-eight-axis cross-fitted presence model; and
- final eight-axis matching + the refitted final-eight-axis presence model.

The response, five geographical folds, cell geometry, SPDE mesh/prior machinery, observation counts and random seed scheme remain fixed. This isolates environmental-representation propagation rather than rebuilding the full project.

## Reduced human-context hypothesis family

The current human feature surface contains useful raw proxies but also strongly overlapping composites. The final audit treats the following seven variables as the primary mechanism-level family and retains the other composites only as descriptive diagnostics.

### H1. Local anthropogenic exposure / propagule opportunity

**Primary proxy:** 5-km population rank.

Ecological rationale: human population density is a broad proxy for the frequency of gardens, planted material, transport and repeated human contact. In ornamental-plant invasions, market availability and propagule pressure can strongly predict escape/invasion success (Dehnen-Schmutz et al. 2007). For this native *Campanula*, however, population density is not direct evidence of introduction; it is an exposure/opportunity proxy.

### H2. Dense-settlement exposure

**Primary proxy:** DID proximity rank.

DID is used as a spatial indicator of proximity to dense settlement, not as a mechanistic ecological variable. It is partially redundant with population density, so the population-DID alignment composite is not treated as independent evidence.

### H3. Transport/access corridor

**Primary proxy:** road proximity rank.

Roads can increase human-mediated propagule movement and disturbed establishment opportunities; traffic-mediated seed dispersal has been demonstrated in plant systems, and propagule pressure/dispersal can be concentrated along roads. At the same time, accessibility also affects volunteer recording locations. Therefore road proximity has two competing interpretations that must always be reported together: biological human-mediated movement/disturbance versus observation accessibility.

Relevant examples:

- Warren RJ et al. (2013) Habitat, dispersal and propagule pressure control exotic plant infilling within an invaded range. *Ecosphere* 4, Article 26. DOI: 10.1890/ES12-00393.1.
- Lemke A et al. (2019) How traffic facilitates population expansion of invasive species along roads: the case of common ragweed in Germany. *Journal of Applied Ecology*. DOI: 10.1111/1365-2664.13287.

### H4. Built/disturbed establishment context

**Primary proxy:** built-up fraction rank.

Built land is interpreted as managed/disturbed habitat context and exposure to planting, not as a direct cause of flower colour. The artificial-land composite is not counted as an additional mechanism because it is dominated by the same constituent land-use fractions.

### H5. Managed-natural interface

**Primary proxy:** forest-human edge rank.

Edges between forest and managed land can combine disturbance, access and semi-natural habitat suitable for persistence. This is an establishment/context hypothesis, not a provenance marker.

### H6–H7. Natural alternatives

**Primary proxies:** forest-cover rank and mountainness rank, both evaluated two-sided.

These variables prevent an anthropogenic interpretation from becoming tautological: a candidate that is not urban may instead occupy a distinct natural landscape context omitted from the candidate detector.

## Observation-bias alternative

YAMAP is opportunistic citizen-science imagery. Population and road variables therefore cannot be interpreted only as ecological exposure. Mair & Ruete (2016) found that road access and log population density consistently explained spatial recording effort across multiple citizen-science taxa. This makes accessibility/recording bias a mandatory alternative interpretation for any positive population or road signal.

Reference:

- Mair L, Ruete A (2016) Explaining spatial variation in the recording effort of citizen science data across multiple taxa. *PLOS ONE* 11:e0147796. DOI: 10.1371/journal.pone.0147796.

The existing effort-rank and independent-site diagnostics are therefore retained. A human-context association is not promoted to provenance evidence if it is compatible with access/sampling structure.

## Species-specific horticultural plausibility and claim ceiling

*Campanula punctata* has horticultural potential, and purple-flowered material has been deliberately produced by crossing *C. punctata* with var. *rubriflora* (Choi et al. 2012). Species-level breeding work therefore establishes that pigmented horticultural material and crossing are biologically plausible.

Reference:

- Choi NH et al. (2012) Breeding of purple flower-colored dwarf 'Jiknyeo' from hybridization of *Campanula punctata* Lam. × *C. punctata* Lam. var. *rubriflora* Mak. *Korean Journal of Horticultural Science & Technology* 30:338–341. DOI: 10.7235/hort.2012.12015.

Population-genetic work also shows substantial outcrossing in self-incompatible mainland/Oshima populations and strong geographical genetic structure (Inoue & Kawahara 1990). Gene exchange is therefore biologically possible, but the present photographs cannot identify cultivar ancestry or introgression.

Reference:

- Inoue K, Kawahara T (1990) Allozyme differentiation and genetic structure in island and mainland Japanese populations of *Campanula punctata*. *American Journal of Botany* 77:1440–1448. DOI: 10.1002/j.1537-2197.1990.tb12554.x.

Accordingly, the strongest admissible interpretation of a positive human-context result is:

> local pigmented departures are preferentially associated with anthropogenic exposure or managed-landscape context, consistent with—but not diagnostic of—planting, escape or introgression.

Direct provenance requires field history, vouchers/morphology, replicated population sampling and population-genetic comparison with neighbouring white populations and horticultural material.

## Multiplicity and model-selection rule

The reduced seven-feature family is evaluated with the same natural-map replay used by the current post-selection analysis. Directional alternatives are declared before comparison for the five human-exposure variables; forest cover and mountainness are two-sided natural alternatives. maxT familywise correction is retained.

Composite scores such as population-DID alignment, settlement density, artificial-land score and human-activity consensus may be shown descriptively but are not treated as independent confirmatory hypotheses when they are strongly correlated with their components.

## Final decision rule

The current human interpretation survives only if it is stable to both:

1. the final-eight-axis environmental matching sensitivity; and
2. a refitted final-eight-axis cross-fitted natural reference.

If candidate identities or human contrasts materially change, Appendix S6 must report the propagation sensitivity and downgrade any fixed 17-cell narrative. If the same small human signals remain familywise inconclusive, the final claim remains **provenance/field targets with suggestive short-scale anthropogenic exposure**, not anthropogenic anomalies.
