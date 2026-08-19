# hotarubukuro

Analysis code for the *Campanula punctata* flower-colour project.

This public repository is intentionally limited to reusable analysis/source-building code, tests, safe method-level provenance, and the derived analysis input required for reproducibility. Manuscript and journal-submission materials are kept outside the public repository.

## Public reproducibility set

- `Data_S1.csv` — fixed derived analysis table (1,965 records)
- `Code_S1.py` — georeferencing/reconstruction utility retained with the public data workflow
- `run_pipeline.py` — one-command analysis reproduction front door

Validate the repository contract without running the heavy models:

```bash
python run_pipeline.py audit
```

Rebuild the public analysis chain from `Data_S1.csv`:

```bash
python run_pipeline.py reproduce
```

`reproduce` rebuilds the public environmental inputs and Bombus SDMs and then runs the two-part phenotype, 1-km cell context, final Broad environment+space analysis, supported-environment-distance comparison, local Bombus analysis, and continuous colour-isolation human-context analysis. Intermediate outputs and downloads are written only to ignored `results/`, `data/processed/`, and `.repro_cache/` locations.

Because public raster and occurrence services can change over time, this command is a **source reconstruction** from a fixed derived phenotype table plus public sources, not a claim of bit-identical archival reproduction of mutable third-party services. Seeds, software declarations, source URLs and run metadata are retained so differences can be audited.

## Main code areas

- `R/` — reusable statistical and spatial-analysis functions
- `scripts/` — analysis entry scripts
- `analysis_sensitivity/` — focused robustness and diagnostic analyses
- `source_build/` — public-source acquisition and data-construction code
- `config/` — analysis configuration
- `tests/` and `validation/` — unit and consistency checks
- `.github/workflows/` — analysis workflows

## Data boundary

`Data_S1.csv` is deliberately public because it is the stable derived input needed for reproducible analysis. Original third-party photographs, manuscript drafts, journal-submission materials, author metadata, cover letters, review bundles, and private paper-level binary payloads are not stored in the current public tree.
