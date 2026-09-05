# Legacy utilities

Files in this directory are retained for provenance but are **not part of the active publication analysis path**.

- `Code_S1_georeference.py` is the historical GPX/photo-time georeferencing utility that previously lived at repository root as `Code_S1.py`. It is not the image-colour extractor used by the current zero-to-analysis reconstruction.

The active image-colour reconstruction is:

`source_build/extract_color.py` -> `source_build/build_data_s1.py` -> `Data_S1_from_zenodo.csv`.

Git history preserves the original root path and prior submission references.