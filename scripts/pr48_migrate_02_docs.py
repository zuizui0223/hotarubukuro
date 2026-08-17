#!/usr/bin/env python3
from pathlib import Path
R=Path(__file__).resolve().parents[2]
def repl(path,a,b):
 p=R/path;t=p.read_text();assert t.count(a)==1,(path,t.count(a));p.write_text(t.replace(a,b,1))
a='**Interpretation limit:** the environmental pattern is a candidate physiological/adaptive context, not proof of local adaptation. The spatial field is unresolved geography, not a measured genetic structure or dispersal distance.\n\n## 3. Local focal-Bombus test'
b='''**Interpretation limit:** the environmental pattern is a candidate physiological/adaptive context, not proof of local adaptation. The spatial field is unresolved geography, not a measured genetic structure or dispersal distance.

### Cross-fitted spatial-null sensitivity

**Question:** At comparable geographical separation, does phenotype divergence increase with environmental difference more than a continuous space-only SPDE predicts?

**Answer:** For pigmentation state, yes: observed high-minus-low environmental divergence was 0.106802, the space-null median was 0.058240, the excess was +0.048562 and the one-sided posterior-predictive P was 0.03393. Conditional intensity showed no positive excess (-0.045891; P=0.87226).

**Role in the paper:** this is a cross-fitted spatial-null sensitivity of the accepted Broad result. It supports state-specific environmental alignment beyond fitted continuous spatial expectation; it does not replace the accepted model and is not selection or local adaptation.

**Evidence:**

- `scripts/fit_broad_space_null_phenotype_excess.R`
- `.github/workflows/broad-spatial-inertia-environment-tracking.yml`
- `docs/broad_spatial_inertia_environment_tracking.md`
- source Broad artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`

## 3. Local focal-Bombus test'''
repl(Path('paper/analysis-map.md'),a,b)
a='**Scientific gain:** the broad stage delivers both a candidate abiotic landscape and coherent unresolved geography for future microclimate and genomic tests.\n\nDetails: Appendix S3.'
b='''A cross-fitted spatial-null sensitivity then asked whether same-distance pairs that differed more environmentally also differed more phenotypically than a continuous space-only SPDE expected. Pigmentation-state divergence exceeded that null (+0.048562; one-sided posterior-predictive P=0.03393), whereas conditional intensity did not (-0.045891; P=0.87226). This narrows the environmental alignment to the state transition without turning it into evidence of selection or local adaptation.

**Scientific gain:** the broad stage delivers both a candidate abiotic landscape and coherent unresolved geography for future microclimate and genomic tests.

Details: Appendix S3 and [`docs/broad_spatial_inertia_environment_tracking.md`](../docs/broad_spatial_inertia_environment_tracking.md).'''
repl(Path('paper/README.md'),a,b)
a='Pigmentation was less common in warmer climates. Colour intensity followed a different pattern involving temperature seasonality, moisture and terrain. A strong spatial pattern remained after measured environment.'
repl(Path('README.md'),a,a+' A cross-fitted spatial-null sensitivity further found environment-aligned excess divergence for pigmentation state, but not for conditional intensity.')
a='''It then regenerates:

1. occurrence-referenced Bombus support;
2. 67 fixed local white-pigmented boundary tests;
3. the final-eight-axis environmental-balance audit;
4. 10,000-map natural-departure and human-context adjudication;
5. the four JBI main figures and their numerical validation;
6. the editable JBI review bundle and rendered PDF smoke tests;
7. the final manuscript/repository alignment report and provenance manifest.'''
b='''It then regenerates:

1. the 500-draw, five-fold cross-fitted Broad spatial-null phenotype-excess sensitivity;
2. occurrence-referenced Bombus support;
3. 67 fixed local white-pigmented boundary tests;
4. the final-eight-axis environmental-balance audit;
5. 10,000-map natural-departure and human-context adjudication;
6. the four JBI main figures and their numerical validation;
7. the editable JBI review bundle and rendered PDF smoke tests;
8. the final manuscript/repository alignment report and provenance manifest.'''
repl(Path('docs/reproduction-guide.md'),a,b)
a='The interaction screen and alternative spatial models remain focused diagnostic components. They are not rerun as a hidden alternative to the accepted model during paper reproduction.\n\n### 3. Bombus support and local boundaries'
b='''The interaction screen and alternative spatial models remain focused diagnostic components. They are not rerun as a hidden alternative to the accepted model during paper reproduction.

#### Cross-fitted spatial-null sensitivity

Stage `run_broad_space_null` fits an intercept + Matérn SPDE in four geographical folds and tests held-out pairs in the fifth. Pairs are stratified by geographical distance before high- versus low-environmental-distance phenotype divergence is contrasted. The exact run uses 500 posterior-predictive realizations, seed 20260725, five geographical folds and five distance strata.

Expected result: pigmentation-state excess +0.048562 with one-sided posterior-predictive P=0.03393; conditional-intensity excess -0.045891 with P=0.87226. This is environmental alignment beyond a fitted spatial expectation, not proof of selection, local adaptation or a unique environmental cause.

Evidence and standalone rerun: `docs/broad_spatial_inertia_environment_tracking.md` and `.github/workflows/broad-spatial-inertia-environment-tracking.yml`.

### 3. Bombus support and local boundaries'''
repl(Path('docs/reproduction-guide.md'),a,b)
Path(__file__).unlink()
