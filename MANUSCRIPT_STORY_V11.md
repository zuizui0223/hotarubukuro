# Manuscript story v11

## Working title

**Separating pigmentation expression from intensity reveals environmental,
spatial and biotic structure in nationwide social-media images of Campanula
punctata**

Alternative, more methods-forward title:

**A two-part spatial workflow for testing biotic and human signatures in
photograph-derived floral colour**

## One-sentence advance

Nationwide opportunistic photographs can support ecological inference on flower
colour when white-versus-pigmented expression is separated from intensity among
pigmented flowers, spatial structure is treated as an estimand, and biotic and
human hypotheses are evaluated as incremental, falsifiable layers.

## Why this species

Campanula punctata is not only a convenient polymorphic flower. It links four
otherwise separate questions:

1. mainland flowers show conspicuous white-to-pink variation;
2. Bombus is a known principal pollinator group;
3. a white-flowered island relative occurs where Bombus is scarce or absent;
4. horticultural forms include dark flowers and early-flowering traits.

These observations motivate competing broad-scale predictions. They do not by
themselves prove pollinator selection or horticultural escape.

## Results narrative

### 1. Measurement innovation

The focal flower and petal extraction region were visually confirmed before
scripted colour measurement. Dark observations were retained. A response-blind
mixture separated 966 white-like from 957 pigmented observations at a*=4.94;
124 observations were classification-ambiguous. The analysis then used a
binomial expression outcome and a conditional intensity outcome.

### 2. Environment and space are both results

The two outcomes retain different spatial scales after environmental adjustment.
SPDE is not introduced only to “remove autocorrelation”; it quantifies coherent
geography that measured environmental axes do not explain. Its biological source
remains unresolved.

### 3. Bombus overlap is real, local attribution is weak

W and A show opposite broad geographical gradients consistent with pollinator-
guild turnover. Yet W and A add almost no held-out information for pigmentation
presence after environment and space. W retains a small negative conditional-
intensity gradient (ΔR² about 0.01; SPDE credible interval excluding zero but
WAIC improvement only 0.54). A does not. This is evidence for shared broad
structure and a small residual association, not a causal selection gradient.

### 4. The horticultural workflow is informative because it can fail

H, R and H×R show a weak direct western association with pigmentation presence.
If dark horticultural escape drove the extreme anomalies, effects should become
stronger in upper residual tails and align with early flowering and access.
They do not. The upper-tail coefficients change sign, held-out prediction
worsens for conditional intensity, and western extreme residuals are not early-
flowering enriched. The study therefore generates candidates but does not claim
horticultural provenance.

## Suggested abstract logic

1. **Problem:** large image datasets provide coverage but flower-colour analyses
   often conflate pigment absence with low pigment intensity and environmental
   gradients with spatial co-distribution.
2. **Approach:** visually confirmed petal regions; response-blind pigmentation
   threshold; two-part outcomes; environmental axes; spatially blocked GAM and
   SPDE-INLA; ENMeval Bombus suitability; human-interface residual-tail tests.
3. **Core result:** strong broad colour geography and distinct residual spatial
   scales for expression and intensity.
4. **Biotic result:** guild-level overlap but negligible incremental prediction
   for expression; a small W-associated conditional-intensity gradient.
5. **Human result:** no increasing horticultural convergence in residual tails.
6. **Conclusion:** two-part, spatially explicit analysis turns opportunistic
   photographs into bounded tests of environmental, biotic and human hypotheses.

## Figure order

1. Workflow and a* mixture boundary; map white-like/pigmented observations.
2. Environmental effects and SPDE fields for presence and conditional intensity.
3. Bombus W/A evidence ladders: landscape coefficient versus environment+space
   coefficient and held-out gain.
4. Horticultural test: direct H×R estimates plus residual-tail ladder and early
   enrichment result.
5. Supplement: ENMeval metrics, species-specific montane audits, VIF/correlation,
   threshold rules, warnings and ranked candidates.

## Placement of the island observation

The Izu-island white-flower/Bombus observation belongs in the Introduction as a
biological motivation and in the Discussion as a testable comparative prediction.
It should not be presented as a data point in the present mainland model unless
island observations and comparable covariates are explicitly analysed.

## Reviewer-resistant wording

Prefer:

- “Bombus-associated suitability gradient”
- “broad co-geography”
- “incremental information after environmental and spatial adjustment”
- “optical pigmentation class”
- “candidate consistent with, but not diagnostic of, horticultural influence”

Avoid:

- “Bombus effect”
- “anthocyanin amount” for white flowers or uncalibrated images
- “SPDE removed spatial bias”
- “horticultural individual” or “escape” without external validation
- “three independent montane-species replications”

