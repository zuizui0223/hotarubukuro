# Supplying the established analysis inputs

## Why this file exists

Every published number in the manuscript rests on two tables that were never
committed:

- `results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv`
  — the 1923-row phenotype analysis table;
- `results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv`
  — the 1-km cell context derived from it.

They are absent from this repository, absent from the recorded publication
commit `bcceb7c7`, and absent from every release. The only reproducible route to
them is regeneration from `Data_S1.csv` and the pinned public rasters, and that
route does not land on the published tables:

| quantity | published | regenerated |
|---|---:|---:|
| analysis observations | 1923 | 1909 |
| white-like | 966 | 955 |
| pigmented | 957 | 954 |

The analysis population is defined in `R/environment_spatial.R` by
`complete.cases` over the environmental covariates extracted at each
observation, so any coverage difference in a public raster changes which
observations survive. Fourteen do not.

Until the established tables are archived, the canonical workflow stops at
`01_audit_phenotype` — correctly, because that audit exists to refuse inputs
that are not the published ones.

## What to supply

The whole stage directories, not only the two tables. The frozen audits and the
downstream stages read the cross-fit logs, coefficient tables, held-out
summaries and measurement summary alongside the analysis table, and a directory
that mixes established and regenerated files would be internally inconsistent.

```
inputs/established_analysis_inputs/
├── ecological_v11_pigmentation_hurdle/     # the full stage directory (31 files)
│   ├── analysis_data_pigmentation_hurdle.csv
│   ├── pigmentation_measurement_summary.csv
│   ├── pigmentation_presence_bombus_crossfit_log.csv
│   └── …
└── ecological_v15_multiscale_hotspots/     # the full stage directory (25 files)
    ├── multiscale_hotspot_cells_1km.csv
    └── …
```

Commit that directory to the branch, or place it on the runner by any other
means before the reconstruction workflow runs.

## How they are used

`scripts/stage_canonical_snapshot.R --established-inputs=<dir>` stages these
directories in place of the regenerated ones. The
`raw-data-reconstruction.yml` workflow passes the path automatically when
`inputs/established_analysis_inputs/` exists, and warns rather than failing when
it does not.

Two things happen at staging time:

1. **The supplied tables are checked against `inputs/established_input_expectations.csv`.**
   A rerun, a partial export, or the regenerated tables copied by mistake are
   rejected, because publishing them under a snapshot that claims to hold the
   published inputs would be worse than having no snapshot at all.
2. **The divergence is recorded.** The reconstruction still rebuilds its own
   version from the public sources, and the row-count difference between the two
   is printed and kept. Archiving the established tables resolves the
   reproduction; it does not make the divergence go away, and the divergence
   remains worth investigating on its own terms.

The two components are staged with role `archived_input` rather than
`immutable_input`, matching how the *Bombus* prediction surfaces are treated:
inputs that the pipeline consumes but cannot regenerate from this repository.

## After supplying them

1. Add the `run-raw-reconstruction` label to the pull request (or dispatch the
   workflow once it is on the default branch). It republishes the snapshot with
   the established analysis inputs and prints the new descriptor.
2. Commit the printed `inputs/canonical_snapshot.json`.
3. Add the `run-canonical-analysis` label. The fidelity check should report
   1923 / 966 / 957 with status `PASS`, and the pipeline should proceed past
   `01_audit_phenotype`.
