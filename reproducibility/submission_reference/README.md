# Frozen submission analysis reference

This directory is the compact, permanent scientific reference for the Ecology and Evolution submission analysis. It was copied verbatim from the successful canonical GitHub Actions run on `main`; large posterior objects and the 200,000-row null table are deliberately omitted because they are regenerable from the locked source and input snapshot.

- analysis source commit: `2084bceaef9c4dee71a3399c9d0c5aa917abe067`
- canonical Actions run: `31149006557`
- artifact: `analysis-1909-2084bceaef9c4dee71a3399c9d0c5aa917abe067-31149006557`
- artifact digest: `sha256:b66502976d55bdbacd6b46cab4c7140062f7a78385dacdbeac7834eabbefe052`
- analysis population: 1,909 observations (955 white-like; 954 pigmented)
- submission inference status: `crossfit_upper_tail_not_jointly_robust`
- deterministic 10,000-map scientific null SHA-256: `ca8606a6b2afcc23de33fd789808dc2a01cf661ac69ada75a5413ce4b7ad868f`
- joint probability checkpoint SHA-256: `9ab73385fdbd1fcd25f7af271c7edcaef2daaf85542ec363e69eed29c32e046e`
- final figure manifest SHA-256: `931e988130504add3f615a756c390e50454835cbb615746b6f46a832124db535`

## Interpretation lock

Eighteen local pigmented isolates are reproducible follow-up units. Their absolute count is compatible with the fitted natural baseline. Their fraction lies near the upper tail of the held-out cross-fitted reference, but its Monte Carlo interval crosses 0.05 and the full-data joint spatial posterior-predictive reference does not support a robust excess. Human-context results remain post-selection and non-causal; horticultural origin is not demonstrated.

`manuscript_numeric_reference.csv` is the compact table used to synchronize the final manuscript. `submission_inference_summary.csv`, `submission_claim_registry.csv`, and `submission_analysis_validation.csv` preserve the final inferential lock and its validation.