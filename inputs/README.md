# Active inputs

The active analysis is the 1,909-observation pipeline.

- `canonical_snapshot.json` declares the immutable release asset and SHA-256 checksum for the active analysis-input snapshot. The historical filename is retained because the restoration scripts and release use it.
- `analysis_1909_expectations.csv` declares the three population invariants checked before modelling.
- `Data_S1.csv` at repository root contains the curated derived colour records and source identifiers.

The old 1,923 result references and input-recovery notes are archived under `legacy/published-1923/`. Active scripts must not read that directory.
