# Frozen JBI analysis state — 2026-08-18

This file records the accepted scientific state before repository cleanup and package consolidation.

## Immutable source state

- Commit: `ddb1c262b8332c94f4b94e572ab930a648e59553`
- Preservation branch: `archive/jbi-final-2026-08-18-pre-cleanup`
- Machine-readable lock: `reproducibility/frozen_release_2026-08-18.json`
- Canonical paper lock: `config/paper_pipeline.lock.json`

The commit SHA is the permanent content identity. The preservation branch is a convenience pointer and should not be force-updated.

## Frozen scientific results

### Broad cross-fitted spatial-null sensitivity

| Response | Observed high-env − low-env | Space-null median | Excess | One-sided P |
|---|---:|---:|---:|---:|
| Pigmentation state | 0.106802 | 0.058240 | +0.048562 | 0.03393 |
| Conditional intensity | -0.047179 | -0.001287 | -0.045891 | 0.87226 |

### Local focal Bombus

- 67 fixed white–pigmented boundaries within 5 km.
- Mean focal-Bombus support difference, pigmented minus white: +0.03590.
- 5/10/25-km family q = 0.08148.

### Calibrated departures and human context

- 16 naturally calibrated local departures.
- Candidate-count P = 0.27897.
- Candidate-fraction P = 0.12609.
- 5-km population contrast = +0.06744.
- Global maxT FWER P = 0.05479.

## Claim ceilings

1. The Broad spatial-null sensitivity is not FST/PST/QST and does not demonstrate selection, drift, local adaptation, or a unique causal environmental variable.
2. Bombus SDM support is habitat opportunity rather than realized visitation, pollen transfer, or selection.
3. The 16 departures are field/provenance targets rather than proven anthropogenic populations.

## Binary evidence identities

The exact-reproduction inputs remain checksum-addressed in the canonical lock:

- Broad accepted evidence: Actions artifact `9022276431`, SHA-256 `0135939a9c66d087ea2fc8e2e00a6e4802927a63b400c2011d63e5b86e004240`.
- Seeded Bombus SDM: Actions artifact `9020226937`, SHA-256 `d5d639e8e00d1ccc2f887c53fa8041465905b29f6bca1127f816e8c7a649d708`.
- Final-eight-axis posterior draws: Actions artifact `9094339466`, SHA-256 `413042ea03f1beff71410583df52cb036b9076b0476c99f6e2c885ab0bf42fa1`.

Final-head derivatives are also checksum locked:

- Integrated paper artifact `9303257361`: `b5a699c0a5caf28d16315ef9c14ca3e4353e8c171161a21203de7d9d18e5b35d`.
- JBI review bundle `9303288029`: `0ee355fcfb5563fb69880384933afab4e6e15019318851b39b67494b94a5aaeb`.
- JBI figure bundle `9303309344`: `4fe31b068eb72c3d37fdaccc7b434cb96b7daf533c8d4dc52608b9ddf7585c88`.

## Long-term archival boundary

Code, manuscript text, numerical locks, provenance, and the exact pre-cleanup state are now permanently addressable by Git commit. GitHub Actions artifacts are still retention-bound binary storage. A DOI-backed repository or GitHub Release asset remains the final binary-deposit gate; this document prevents any future replacement from changing the accepted scientific identity silently.
