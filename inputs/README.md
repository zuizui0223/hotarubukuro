# Input boundary

The manuscript-facing source boundary is explicit:

- `Data_S1.csv` at repository root is the distributable derived flower-colour/source table;
- the environment-complete ecological analysis contains 1,922 observations after documented raster-support exclusions;
- large raster-derived, fitted-model and predictive-draw evidence is restored from checksum-locked GitHub Actions artifacts listed in `paper/analysis-map.md` and dated reproducibility records;
- mutable external services are not allowed to silently replace manuscript evidence.

For local development, acquisition/source-construction code is under `source_build/` and source registries/configuration are under `config/`. For exact artifact restoration and workflow entry points, use `docs/reproduction-guide.md`.

The distributed `Data_S1.csv` column semantics and privacy/provenance boundary are documented in `docs/data-s1-dictionary.md`.
