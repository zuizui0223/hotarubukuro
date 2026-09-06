# source_build

Retained builders for public inputs used by the publication pipeline.

## Canonical colour source

- `reproduce_from_zenodo.py` — orchestrates the frozen Zenodo workbook through exact lean-table validation.
- `extract_color.py` — resolves workbook images and writes the **rich technical colour/QC extraction record**.
- `build_data_s1.py` — projects that record to the **lean deterministic 38-column analysis table** actually consumed downstream.
- `source_contract.py` — validates the lean table against `reproducibility/source_contract.json`.

The rich extraction retains run-time/audit-only information such as `processed_at`, candidate peak diagnostics and historical RGB comparisons. The lean table omits fields unused by retained analyses, normalises numeric text and is frozen by exact Git-blob and SHA-256 identities.

## Other public-source builders

- `download_rasters.R` / `prepare_rasters.R` — public environmental rasters.
- `fetch_bombus_occurrences.R` / `build_bombus_sdm_mainland.R` — Bombus occurrence and SDM inputs.
- `build_human_raster.R` — public MLIT human/forest context layer.

Derived colour tables, downloaded source archives and analysis outputs are generated locally under ignored cache/results locations rather than committed as inputs.
