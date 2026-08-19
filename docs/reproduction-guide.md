# Reproduction guide

The repository has one active execution front door. The accepted scientific result is frozen in `reproducibility/FINAL_RESULTS_2026-08-19.md`; later packaging changes must not silently alter it.

## Canonical commands

Fast audit:

```bash
python run_pipeline.py audit
```

One-shot accepted-evidence reproduction and JBI package rebuild:

```bash
python run_pipeline.py reproduce
```

Useful controls:

```bash
python run_pipeline.py reproduce --dry-run
python run_pipeline.py reproduce --from-stage restore_broad_supported_term_distance
python run_pipeline.py reproduce --only-stage validate_alignment
python run_pipeline.py reproduce --no-resume
```

The canonical GitHub Actions entry is `.github/workflows/paper-pipeline.yml`. Artifact identities, SHA-256 checksums, commands, seeds, expected outputs and manuscript locks are declared in `config/paper_pipeline.lock.json`.

## Final scientific sequence

1. Restore the accepted Broad flower-colour evidence and final supported-term distance result.
2. Restore seeded Bombus SDMs and final-eight predictive draws.
3. Rebuild occurrence-referenced Bombus support and the 67 fixed local boundary analysis.
4. Restore/revalidate continuous same-colour isolation; retain the 16-event family only as Supporting Information calibration.
5. Rebuild the four JBI figures and editable review package.
6. Validate manuscript/repository alignment and write the run manifest.

The supported-term fixed-space result is the current Broad corroboration: pigmentation state uses Temperature PC1 (observed 0.100608; space-only median 0.048475; excess +0.052133; P=0.00998), whereas the supported conditional-intensity distance does not exceed space-only expectation (P=0.26347).

The Main human-context result is continuous same-colour isolation across all 1,305 cells. Among 674 pigmented cells, raw isolation has rho=0.251980 versus natural mean 0.132980 (P=0.000200), and density-corrected relative isolation has rho=0.285498 versus natural mean 0.153616 (P=0.000900).

## Code organization

- `run_pipeline.py`: orchestration and stage contracts.
- `R/`: reusable analysis functions; `R/continuous_colour_isolation.R` is the shared library for the final human-context analysis.
- `analysis_sensitivity/`: scientific execution entry scripts/modules.
- `scripts/`: source/build, figure and submission utilities.
- `submission/jbi/`: manuscript, Supporting Information and submission validators.
- `reproducibility/`: accepted result locks and provenance records.

Original YAMAP photographs are third-party content and are not redistributed. Exact reproduction begins from checksum-locked accepted evidence rather than refreshing live sources or replacing the accepted Broad model family.
