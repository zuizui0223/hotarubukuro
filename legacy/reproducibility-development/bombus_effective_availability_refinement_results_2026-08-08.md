# Refined effective-Bombus availability reanalysis: result

Date: 2026-08-08

Workflow run: `31262211605`  
Analysis branch: `agent/bombus-effective-availability-refined`  
Computation commit: `bc69679c7669512553eda7b07736b235ee808134`  
Result artifact: `9023137743`  
Artifact SHA-256: `d04c561b09b652591b9b479f6e26a779bb562c7c1b5f9b14e61d5e7ca8e2794b`

This is a **post-null exploratory refinement**. It does not replace the earlier null directional analysis.

## Biological refinement

The exposure was changed for an ecological reason rather than a significance-searching reason. The target hypothesis is potential effective Bombus pollination opportunity for *Campanula punctata*. The primary guild was therefore restricted to the two Bombus taxa directly documented as predominant pollinators in the relevant Japanese system, *B. ardens* and *B. diversus*. Each species' fresh SDM cloglog support was placed on an occurrence-referenced scale using the empirical distribution of selected-model predictions at that species' exact occurrence cells in the saved ENMeval object. The primary exposure was the maximum of these two occurrence-referenced scores, because support from either effective pollinator can in principle preserve the pollination benefit of pigmentation.

No low/high threshold was introduced. The local design remained response-blind and fixed at 25 km primary (10/50 km sensitivities), same held-out fold, common SDM support, environmental distance <=0.75, with the already-fitted Bombus-free environment+SPDE posterior-predictive maps as the natural null.

## Primary result

At 25 km, all eligible local edges:

- eligible edges = 2,397;
- nodes = 1,198;
- median effective-Bombus contrast = 0.0869 on the occurrence-referenced 0-1 scale;
- observed through-origin slope = 0.06123;
- natural-null mean slope = -0.01267;
- natural-null 95% interval = -0.18983 to 0.17044;
- one-sided empirical P = 0.21079;
- two-sided P = 0.42158;
- mean higher-minus-lower-availability pigmentation-share difference = +0.00504;
- empirical P for that descriptive mean difference = 0.33167.

**Conclusion:** the ecologically refined primary exposure still does not provide evidence that higher predicted effective-Bombus availability is associated with higher pigmentation beyond the fitted abiotic+spatial natural null.

## Scale robustness

Primary occurrence-referenced max exposure, all-edge design, pigmentation presence:

- 10 km: slope = +0.10134, P = 0.16284;
- 25 km: slope = +0.06123, P = 0.21079;
- 50 km: slope = +0.08418, P = 0.15485.

The slope is positive at all three prespecified scales, but none is individually significant; BH q across the three primary scales is 0.21079.

The raw-cloglog effective-guild maximum was also positive but non-significant:

- 10 km: slope = +0.04453, P = 0.38761;
- 25 km: slope = +0.20235, P = 0.18681;
- 50 km: slope = +0.28669, P = 0.14486.

Thus the null primary result is not an artifact of occurrence-referencing alone.

## One-to-one sensitivity

A response-blind non-overlapping pair sensitivity showed a nominal 10-km signal for the primary exposure (slope = +0.32363, P = 0.01499), but it did not persist at the primary 25-km scale (slope = +0.04897, P = 0.34266) or at 50 km (slope = -0.04854, P = 0.55045). Its all-sensitivity multiplicity-adjusted q was not significant. Therefore this isolated 10-km result is not treated as support for the main hypothesis.

## Species/exposure sensitivities at 25 km

None of the prespecified alternative availability metrics produced a significant directional result at the primary 25-km all-edge design:

- raw max of *B. ardens*/*B. diversus*: P = 0.18681;
- occurrence-referenced max of *B. ardens*/*B. diversus* (primary): P = 0.21079;
- all-five occurrence-referenced max: P = 0.26873;
- *B. ardens* alone: P = 0.49850;
- all-five occurrence-referenced mean: P = 0.50150;
- *B. diversus* alone: P = 0.65035;
- occurrence-referenced mean of *B. ardens*/*B. diversus*: P = 0.69031.

## Interpretation

The better-aligned availability proxy changes the estimated directional slope from near-zero toward a consistently positive value across 10/25/50 km, but uncertainty remains too large to distinguish it from the Bombus-free environment+space predictive null. Therefore the present observational SDM data do **not** support a simple claim that higher effective-Bombus habitat support maintains pigmentation.

This strengthens the distinction between two results:

1. **Directional effective-availability hypothesis:** not supported by either the original or refined analysis.
2. **Bombus-community turnover result:** previously supported for white-versus-pigmented turnover beyond the abiotic+spatial natural null; this remains a separate association and does not rescue the directional hypothesis.

The most direct next evidence for the relaxation mechanism would require information less environmentally entangled than SDM suitability, especially realized Bombus presence/absence, visitation frequency, pollen-transfer effectiveness, or a quasi-independent island accessibility contrast. Further redefinition of the SDM exposure after these results would risk significance-driven analysis and is not recommended as a main-paper strategy.