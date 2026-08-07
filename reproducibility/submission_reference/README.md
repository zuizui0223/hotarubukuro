# Active submission analysis reference

This directory is the compact scientific reference used to synchronize the active 1,909 manuscript. Large posterior objects and long null tables are omitted because they are regenerable from the locked source and input snapshot.

## Successful local-only Bombus canonical run

The flower-only national natural baseline, local stage-03 Bombus limitation gate, local-isolate analyses, joint isolate posterior-predictive sensitivity, submission inference lock and manuscript figures were run together successfully after the national Bombus comparator was removed from stage 02:

- PR head used for the numerical canonical analysis: `b1aa2364c5af0b35cdf691633d3694b93b632dc7`
- canonical Actions run: `31187715157`
- uploaded artifact: `analysis-1909-ded1ee6cc45667ef647618496aa49978942b1abd-31187715157`
- artifact SHA-256: `90e32c4b894d47f4809ebd27ab307bb012ea1e298773d19ace5bc94f6367a800`
- analysis population: 1,909 observations (955 white-like; 954 pigmented)
- numerical stage manifest: 28/28 PASS
- stage-03 status: `directionally_coherent_exploratory_support`
- isolate submission status: `crossfit_upper_tail_not_jointly_robust`

The canonical stage-02 draw manifest contains exactly two models: `national_environment_spde_presence` and `national_environment_spde_intensity`. A fresh run produces no national Bombus/fingerprint/common-support model checkpoint and no national Bombus paired-contrast result. Bombus enters the active inference only at local stage 03.

GitHub names pull-request artifacts with the PR merge SHA, whereas the run's `head_sha` identifies the branch head above. The subsequent commit `733091dd82cf02669be6f178e21f4a3ea8278b65` only shortens Figure 3 labels and changes no numerical estimand, model or result.

## Stage-03 design history

The active scientific question changed during PR #21 from unsigned community turnover to the directional **Bombus-limitation gate**. The initial gate-family design-development run was:

- Actions run: `31168019534`
- source head: `b956a1fa320c248fb8f8646ba77b4a74a36ac140`
- artifact SHA-256: `4a6dad8d98f212515eb1ec2f72c40f59b82f6f06b755ced3168bc5dcf77d9384`

The 0.33 lower-third gate was adopted as the manuscript-facing gate **after** inspecting the exploratory design-development grid. The active pipeline therefore retains the complete 0.10/0.20/0.25/0.33 grid and its across-grid multiplicity correction. C4 is `directionally_coherent_exploratory_support`, not a preregistered confirmatory finding.

The local-only canonical run reproduced the lower-third result: 22 one-to-one environmentally matched pairs, available-minus-limited pigmented-share difference 0.2235, upper-tail predictive p=0.0170 and across-grid BH q=0.1019. Conditional intensity differed by -0.6145 (p=0.9041), providing no support for darker flowers under greater predicted Bombus availability.

## Interpretation lock

Eighteen local pigmented isolates remain reproducible follow-up units. Their absolute count is compatible with the fitted natural baseline. Their fraction lies near the upper tail of the held-out cross-fitted reference, but the full-data joint spatial posterior-predictive reference does not support a robust excess. Human-context results remain post-selection and non-causal; horticultural origin is not demonstrated.

For the bumblebee hypothesis, predicted SDM support is interpreted as relative availability rather than abundance or visitation. The active result tests whether pigmentation is lower under locally matched low predicted bumblebee availability and does not establish attraction-mediated selection or pigment-production costs. The five Bombus surfaces remain fixed archived predictions, so SDM-fitting/model-selection uncertainty and unmeasured shared environment remain explicit limitations.

`manuscript_numeric_reference.csv`, `submission_inference_summary.csv`, `submission_claim_registry.csv` and `submission_analysis_validation.csv` are the compact synchronization set for the active paper.
