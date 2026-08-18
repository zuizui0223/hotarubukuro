# hotarubukuro

Data, code and submission files for a range-wide study of flower-colour polymorphism in *Campanula punctata*.

## Start here

- **Biological story:** [`paper/README.md`](paper/README.md)
- **Result-to-evidence map:** [`paper/analysis-map.md`](paper/analysis-map.md)
- **Current JBI manuscript and Supporting Information:** [`submission/jbi/`](submission/jbi/)
- **Audit or reproduce the paper:** [`docs/reproduction-guide.md`](docs/reproduction-guide.md)
- **Derived-data dictionary:** [`docs/data-s1-dictionary.md`](docs/data-s1-dictionary.md)

## One execution front door

Check manuscript, evidence locks, claim ceilings and repository structure:

```bash
python run_pipeline.py audit
```

Reproduce the accepted Broad sensitivity, local Bombus analysis, natural-departure/human analysis, four JBI figures and review bundle:

```bash
python run_pipeline.py reproduce
```

The same interface is available as the **Paper pipeline** GitHub Actions workflow. Exact reproduction starts from checksum-locked accepted evidence. It does not silently refresh GBIF, CHELSA, SoilGrids or other live sources, and it does not redistribute the original third-party YAMAP photographs.

## The study in one dependent sequence

1. **Build the phenotype.** Author-screened YAMAP photographs yielded 1,922 flowers in 1,305 1-km cells: 966 white-like and 956 pigmented.
2. **Resolve broad geography.** Pigmentation state and pigmented-only intensity followed different environmental and residual spatial patterns. A cross-fitted sensitivity further showed that, at comparable geographical separation, pigmentation-state divergence exceeded a space-only expectation along environmental difference (one-sided posterior-predictive P=0.03393); conditional intensity did not (P=0.87226).
3. **Zoom to the local pollinator scale.** Sixty-seven white-pigmented boundaries within 5 km form the primary Bombus test. Mean focal-Bombus support was higher on pigmented sides, but the heterogeneous signal attenuated at broader scales and remains habitat opportunity rather than realized selection. A stronger-looking highland overlap vanished under equal-elevation comparison and therefore serves as a guardrail against shared mountain geography.
4. **Calibrate apparent exceptions.** Sixteen relational local departures were compatible with 10,000 natural predictive maps.
5. **Read human context last.** Population exposure within 5 km was the leading post-selection feature, but global maxT FWER P=0.05479; the sites remain provenance targets, not demonstrated human-origin populations.

## What the repository contributes

- a recent national quantitative trait dataset built from a hiking platform;
- a two-part biological representation of flower colour;
- explicit separation of measured environment from continuous unresolved geography;
- an FST/PST-inspired but explicitly non-genetic spatial-null sensitivity;
- a local-boundary Bombus design that preserves the current JBI story established in PR #51;
- event-based natural calibration before human-context follow-up;
- one checksum-locked pipeline for audit, reproduction, figures and submission files.

The paper does not claim that photographs prove adaptation, that an SDM is a visitation or selection measure, that the spatial-null result is FST/PST or proof of selection, or that the 16 local departures are human-made.

## Repository map

- `run_pipeline.py` — canonical audit/reproduction orchestrator
- `config/paper_pipeline.lock.json` — artifact checksums, commands, seeds, outputs and manuscript locks
- `.github/workflows/paper-pipeline.yml` — canonical one-click execution
- `paper/` — shortest route from biological question to evidence
- `submission/jbi/` — manuscript, Supporting Information, figures and submission checks
- `R/`, `scripts/`, `analysis_sensitivity/` — active analyses and focused diagnostics
- `source_build/` — source refresh and data construction; separate from exact reproduction
- `validation/`, `tests/` — independent checks
- `reproducibility/` — frozen decisions and evidence identities
- `legacy/` — development history outside the current paper path
