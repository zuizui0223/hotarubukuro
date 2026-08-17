# hotarubukuro

This repository contains the data, code and submission files for a range-wide study of flower-colour polymorphism in *Campanula punctata*.

## Start here

**Current paper — biology:** [`paper/README.md`](paper/README.md)  
**Current paper — execution:** [`run_pipeline.py`](run_pipeline.py)

Choose the page that matches what you want to do:

- **Understand the biology:** [`paper/README.md`](paper/README.md)
- **Check which result comes from which analysis:** [`paper/analysis-map.md`](paper/analysis-map.md)
- **Audit or reproduce the paper:** [`docs/reproduction-guide.md`](docs/reproduction-guide.md)
- **Read the current manuscript and Supporting Information:** [`submission/jbi/`](submission/jbi/)
- **Understand `Data_S1.csv`:** [`docs/data-s1-dictionary.md`](docs/data-s1-dictionary.md)

## One execution front door

Repository/manuscript alignment and submission-source checks:

```bash
python run_pipeline.py audit
```

Rebuild the accepted downstream analyses, four JBI figures and review bundle from checksum-locked evidence:

```bash
python run_pipeline.py reproduce
```

The same command is exposed as the **Paper pipeline** GitHub Actions workflow. A manual `reproduce` dispatch supplies the GitHub token and declared Ubuntu/R/Python environment, so the whole paper package is rebuilt from one entry point.

`reproduce` deliberately does **not** refresh GBIF, CHELSA, SoilGrids or other live sources. It starts from the accepted Broad evidence, seeded Bombus SDMs and final-eight-axis posterior draws recorded in [`config/paper_pipeline.lock.json`](config/paper_pipeline.lock.json). A source refresh is a new analysis and remains separate from exact manuscript reproduction.

Original YAMAP photographs are third-party content and are not redistributed. The exact paper path therefore begins at the public derived trait table and checksum-locked accepted evidence, not at raw photograph download.

## The study in four questions

1. **Can hiking photographs reveal a national flower-colour polymorphism?**  
   Yes. We built an author-screened dataset of 1,922 flowers and separated white/pigmented state from colour intensity among pigmented flowers.

2. **How does flower colour vary with environment and geography?**  
   Pigmentation was less common in warmer climates. Colour intensity followed a different pattern involving temperature seasonality, moisture and terrain. A strong spatial pattern remained after measured environment.

3. **Do local colour boundaries line up with bumblebee habitat opportunity?**  
   Only weakly. The 67 fixed white-pigmented boundaries showed a small mean contrast for *Bombus ardens* and *B. diversus*, but the signal was fragile and is not treated as evidence of pollinator-mediated selection.

4. **Are locally unusual pigmented populations more common than the natural model predicts?**  
   No. Sixteen local departures were compatible with 10,000 natural predictive maps. Short-range population exposure was the strongest human-context signal, but it remained just above the global familywise threshold.

## What is new here

The repository makes three contributions easy to audit:

- **New data:** a national quantitative flower-colour dataset built from a hiking platform rather than a biodiversity database.
- **New biological result:** pigmentation state and colour intensity show different geography, and any bumblebee correspondence is weak and local.
- **Scale-aware design:** broad environment, local pollinator opportunity and local human/provenance questions are tested at different spatial scales instead of being forced into one national regression.

The paper does **not** claim that photographs prove adaptation, that SDM values are visitation rates, or that the 16 local departures are human-made.

## Data boundary

`Data_S1.csv` is the public derived observation/trait table.

Original YAMAP photographs are third-party content and are **not redistributed**. The repository instead provides the derived measurements, source provenance, code, seeds, validation rules and evidence hashes needed to audit the manuscript.

## Repository map

- `run_pipeline.py` — canonical audit/reproduction orchestrator
- `config/paper_pipeline.lock.json` — artifact checksums, stage commands and manuscript locks
- `.github/workflows/paper-pipeline.yml` — one-click canonical execution
- `paper/` — shortest route from biological question to evidence
- `submission/jbi/` — manuscript, Supporting Information, figure captions and submission checks
- `R/`, `scripts/`, `analysis_sensitivity/` — analysis components called by the orchestrator or focused diagnostics
- `source_build/` — source refresh and data-construction tools; not silently mixed into exact reproduction
- `validation/`, `tests/` — independent checks
- `reproducibility/` — frozen decisions, evidence IDs and numerical locks
- `legacy/` — development history; not part of the current reproduction path
