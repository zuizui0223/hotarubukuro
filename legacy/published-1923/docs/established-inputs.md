# The established analysis inputs: what survives, and what is lost

## Summary

The two tables every published number rests on —

- `results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv`,
  the 1923-observation phenotype analysis table;
- `results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv`,
  the 1-km cell context derived from it —

**no longer exist.** They were never committed, they are absent from the
recorded publication commit `bcceb7c7`, they are absent from both published
analysis-input snapshots, they are absent from every retained Actions artifact,
and they are no longer on the author's machine.

This was established by search, not assumed. `scripts/recover_established_inputs.py`
and `.github/workflows/recover-established-inputs.yml` perform the search on a
runner, and the evidence is uploaded as an artifact on every run.

## What the search found

Run [30739860856](https://github.com/zuizui0223/hotarubukuro/actions/runs/30739860856)
retrieved every candidate GitHub still holds and measured each one directly.

| source | sha256 | rows | white-like | pigmented |
|---|---|---:|---:|---:|
| `analysis-input-snapshot-v1` | `3916ac6b…cad069f` | 1909 | 955 | 954 |
| `analysis-input-snapshot-v2` | `9fc36433…c3aee3ab` | 1909 | 955 | 954 |
| published analysis | — | **1923** | **966** | **957** |

Both snapshots carry a reconstruction, not the published table. The hash
`3916ac6b32c03e84f0de57eb8c2f4d2222c41ea0cb5f99e83662df1f8cad069f` is a real
recorded checksum — it appears in the snapshot-v1 descriptor at commit
`e8030ad8` — but the file it names has 1909 observations. It is the output of
the reconstruction that built snapshot v1, not a surviving copy of the
established table.

The canonical-analysis artifact of run 30727117525 contains
`results/ecological_v15_multiscale_hotspots`, but that directory was produced
*by* that run *from* snapshot v1, so it is derived from the 1909-observation
table and is likewise not the established cell context.

Nothing was assembled. A directory labelled "established analysis inputs" that
actually held a reconstruction would be worse than no directory: it would put a
false provenance inside a checksummed snapshot and every later run would inherit
it.

## What does survive, and is verified

`results/final_analysis_pipeline/final_input_checksums.csv` — the publication
lock — records an MD5 for each summary artifact the published run consumed. All
ten of those artifacts are committed, and all ten verify:

```
10 of 10 locked artifacts verify against the publication lock.
```

`scripts/verify_locked_artifacts.py` performs this check and PR checks run it on
every push. The match is under CRLF normalisation: the published run was on
Windows and wrote CRLF line endings, which git normalised to LF on commit. The
script tries both and records which one matched rather than accepting either
silently.

This is meaningful provenance. It establishes that the committed published
results — including `pigmentation_measurement_summary.csv`, the source of the
1923 / 966 / 957 counts — are byte-for-byte the published run's own output, and
therefore that `inputs/established_input_expectations.csv` and
`inputs/numerical_reference.csv` are anchored to the real publication rather
than to a later rebuild.

## What this means for the pipeline

Three things are kept strictly apart.

1. **The published analysis is not reproducible.** Its inputs are lost. No
   workflow in this repository claims otherwise, and the published quantities
   have not been re-baselined onto the reconstruction.
2. **The pipeline is reproducible.** A clean runner restores a pinned
   environment, verifies an immutable checksummed snapshot, materialises it, and
   executes the locked stage sequence. Everything downstream of the snapshot
   boundary works and is evidenced.
3. **The reconstruction is a separate finding.** Rebuilding from `Data_S1.csv`
   and the pinned public sources yields 1909 observations rather than 1923. The
   analysis population is defined in `R/environment_spatial.R` by
   `complete.cases` over the extracted environmental covariates, so a coverage
   difference in any public raster changes which observations survive. Fourteen
   do not. This is reported as a reproducibility finding in its own right, not
   as a substitute for the published analysis.

The frozen upstream audit `validation/audit_phenotype.R` continues to refuse the
reconstruction at stage 01, and that is correct behaviour: the run is not
operating on the published inputs, and the audit exists to say so.

## The stage-01 execution records

Even if the phenotype table were recovered, six audit-support files would still
be missing, because they were outputs of the original stage-01 execution and
were never archived:

| file | audit check it supports |
|---|---|
| `pigmentation_hurdle_inla_model_comparison.csv` | `inla_complete` |
| `pigmentation_presence_bombus_heldout.csv` | `primary_bombus_crossfit_warnings` |
| `pigmented_intensity_bombus_heldout.csv` | `primary_bombus_crossfit_warnings` |
| `pigmentation_presence_bombus_crossfit_log.csv` | `species_warning_isolated` |
| `pigmentation_residual_tail_HR_coefficients.csv` | `residual_tail_warnings` |
| `pigmentation_residual_tail_HR_heldout.csv` | `residual_tail_warnings` |

None of them carries a quantity that enters a published result; they exist so
the frozen audit can confirm the stage ran without convergence warnings. They
cannot be regenerated without rerunning stage 01, which would produce a
different analysis population and so could not describe the published run.

`scripts/recover_established_inputs.py` enumerates these and records where each
was and was not found, including recording that a file exists in the
reconstruction snapshot while refusing to copy it from there.
