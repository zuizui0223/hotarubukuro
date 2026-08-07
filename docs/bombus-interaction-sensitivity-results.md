# Bombus interaction-proxy sensitivity: 1,909 frozen analysis

This note records the completed exploratory robustness analysis run from the frozen 1,909 submission artifact. It does not replace the active claim registry. Its purpose is to ask whether the existing local Bombus-community result survives stricter environmental control and a more interpretable decomposition of the predicted community signal.

## Reproducible run

- workflow: `Bombus interaction sensitivity`
- Actions run: `31164779749`
- source head: `dbce3272af4f036fc0593473dd38c45fa13809aa`
- output artifact: `bombus-interaction-sensitivity-dbce3272af4f036fc0593473dd38c45fa13809aa`
- artifact SHA-256: `b8b5f047013dca50c29a9132a5b630a6af8b651295eec9a1b4945afbe010d8c3`
- frozen input artifact: canonical 1,909 submission run `31149006557`
- flower natural-model reference: 1,000 cross-fitted posterior-predictive maps

## Stricter design

The analysis retained the original response-blind 25-km, same-fold, five-species-common-support pair graph but added four safeguards.

1. Pairs were matched on the four existing broad/within-50-km environmental axes. The primary threshold was environmental RMS distance <= 0.75; 0.5 and 1.0 were fixed sensitivities.
2. For each held-out spatial fold, each Bombus species' within-species predicted-support rank was regressed on the four environmental axes using only the other four folds. Held-out residuals therefore represent the part of each predicted species surface not explained by those measured environmental axes in the training region.
3. The residual species profile was decomposed into (a) total-support change and (b) composition turnover after removing the across-species mean. The primary ecological predictor was the composition component.
4. The identical pair statistic was recomputed on all 1,000 flower natural-model predictive maps. Five leave-one-species-out community definitions and fold-wise observed coefficients were also checked.

The SDM surfaces themselves remain fixed archived inputs. GBIF sampling, ENMeval model-selection, fitted-SDM parameter and alternative-surface uncertainty are not propagated here.

## Primary result

At the primary environment-match threshold (0.75), environment-residualized Bombus **community-composition turnover** remained positively associated with both flower-colour responses.

| response | edges | beta | upper-tail predictive p | BH q across two responses | delta R2 |
|---|---:|---:|---:|---:|---:|
| pigmentation-share turnover | 2,408 | 0.0889 | 0.0050 | 0.0100 | 0.00646 |
| pigmented-only intensity turnover | 884 | 0.0754 | 0.0370 | 0.0370 | 0.00479 |

The corresponding two-sided predictive probabilities were 0.0100 for pigmentation share and 0.0739 for conditional intensity. The active local analysis uses the pre-specified positive upper-tail hypothesis; this sensitivity is exploratory and should not be presented as an independent confirmatory test.

## What did not explain the result

Environment-residualized **total Bombus habitat-support difference** was not positively associated with pigmentation-share turnover (beta = -0.0322, upper-tail p = 0.9001). Thus the surviving signal is not consistent with a simple `more predicted Bombus support -> more colour change` interpretation.

The environment residualization removed substantial measured abiotic structure from the individual species surfaces. Mean training R2 values across held-out-fold residualizers were approximately:

- *B. ardens*: 0.45
- *B. diversus*: 0.25
- *B. beaticola*: 0.75
- *B. consobrinus*: 0.66
- *B. honshuensis*: 0.75

The composition correspondence therefore survived after removing a large measured environmental component from several species' predicted distributions.

## Robustness

### Environmental matching

Pigmentation-share composition turnover remained positive at every fixed matching threshold:

| environment threshold | beta | upper-tail p |
|---|---:|---:|
| 0.50 | 0.0796 | 0.0180 |
| 0.75 | 0.0889 | 0.0050 |
| 1.00 | 0.0860 | 0.0030 |

Conditional intensity was also positive at all three thresholds (beta 0.075-0.093; upper-tail p 0.024-0.039), although its two-sided support was weaker at the tighter two thresholds.

### Leave one Bombus species out

For pigmentation-share turnover, the composition beta remained positive after omitting each species in turn (0.061-0.108). Upper-tail predictive p-values ranged from 0.002 to 0.031. No single Bombus surface was necessary for the positive community-level pattern.

### Fold-wise direction

Pigmentation-share beta was positive in all five spatial folds (0.039, 0.050, 0.088, 0.122, 0.146). Conditional intensity was positive in four folds and negative in one, so the state response is spatially more consistent than the intensity response.

## Directional guild probe

An a priori widespread-versus-montane residual guild axis was tested separately.

- pigmentation share: beta = -0.0308, two-sided p = 0.414; no directional guild shift
- conditional intensity: beta = -0.1232, two-sided p = 0.016, BH q = 0.032

The absence of a directional guild effect for pigmentation presence is informative. The presence result is better described as a **multidimensional community mosaic** than as a simple replacement of widespread Bombus by montane Bombus. The intensity guild result is exploratory: the negative sign means that, within the matched/residualized design, relatively more montane-leaning predicted communities corresponded to lower conditional visible intensity. No species-specific colour preference or selection mechanism is established by this analysis.

## Ecological interpretation

The stricter analysis changes the preferred ecological reading of the Bombus section.

The strongest repeatable signal is not total predicted pollinator support. It is **turnover in the species composition of predicted Bombus habitat support**. Among nearby cells with similar measured abiotic context, flower pigmentation-state turnover was larger where the species-specific predicted Bombus profile changed more strongly than expected from those abiotic axes. This pattern also survived removal of every single Bombus species from the community definition.

That result is consistent with a geographic mosaic hypothesis: different local pollinator assemblage contexts may coincide with different floral phenotype regimes. It is more specific than a raw SDM-colour correlation because measured environmental structure is removed out of fold and local pairs are environmentally matched. However, it remains a correspondence between predicted habitat-support profiles and flower traits. The fixed SDM surfaces can still encode unmeasured environment, dispersal history or sampling structure, and they do not measure visitation, pollen transfer, fitness or selection.

Accordingly, the manuscript-safe interpretation is:

> Local flower-colour turnover corresponds specifically to predicted Bombus community-composition turnover, rather than to total predicted Bombus habitat support, after conservative control for measured environment and the flower natural baseline. This is consistent with a pollinator-community geographic mosaic but is not evidence of pollinator-mediated selection.

A stronger interaction test would require newly fitted spatially cross-validated SDM ensembles (so SDM uncertainty is propagated) and, ultimately, species-resolved visitation or reproductive-success data from the identified transition zones.
