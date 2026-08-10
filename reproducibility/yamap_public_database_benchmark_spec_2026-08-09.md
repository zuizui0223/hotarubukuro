# YAMAP versus public biodiversity-image sources: benchmark specification

Date: 2026-08-09

## Purpose

This Supplementary benchmark quantifies what the YAMAP sampling frame contributes relative to purpose-built or aggregated public biodiversity databases. It is not intended to rank platforms globally or to claim that YAMAP is intrinsically unbiased.

The benchmark asks a narrower question relevant to this paper: for *Campanula punctata* in Japan during the same recent three-year window used by the manuscript (2023-01-01 through 2025-12-31), how many georeferenced image-bearing records are available from YAMAP, iNaturalist and GBIF, and what metadata/quality attributes make those records more or less directly usable for quantitative flower-colour geography?

## Locked taxonomic and temporal scope

- focal taxon: *Campanula punctata* sensu lato, including infraspecific descendants where the external API search includes descendants;
- country/place: Japan;
- dates: 2023-01-01 to 2025-12-31 inclusive;
- YAMAP source: current-input `Data_S1_current_analysis.csv`, restricted to `source_reference_type == yamap_activity`;
- iNaturalist taxon id: 128781;
- iNaturalist Japan place id: 6737;
- GBIF backbone species key: 5411557.

The taxon identifiers are fixed before retrieval and are not changed after seeing record counts.

## Comparable public-record filters

### iNaturalist

Primary public comparator:

- `taxon_id=128781`;
- `place_id=6737`;
- `d1=2023-01-01`;
- `d2=2025-12-31`;
- `photos=true`;
- `geo=true`.

Both all public observations and Research Grade observations are summarized. Records are paged from the official iNaturalist API and the API response metadata are archived with the workflow artifact.

### GBIF

Primary public comparator:

- `taxon_key=5411557`;
- `country=JP`;
- `year=2023,2025`;
- `media_type=StillImage`;
- `has_coordinate=true`;
- `occurrence_status=present`;
- `basis_of_record=HUMAN_OBSERVATION`.

A broader GBIF image+coordinate count without the HUMAN_OBSERVATION restriction is also reported descriptively. GBIF is an aggregator and can contain iNaturalist records, so GBIF and iNaturalist counts are **not additive independent samples**. Dataset contributions are archived to make overlap visible.

## YAMAP quantities

The workflow derives directly from the checksum-locked current-input artifact:

- YAMAP source rows;
- unique image hashes;
- unique exact sites;
- records per year;
- fraction with complete YYYY-MM-DD date;
- fraction with latitude and longitude;
- coordinate/photo-mapping QC categories;
- current manuscript phenotype-analysis count from `reanalysis_overview.csv`.

Taxonomic/subject validation is a pre-inclusion author-review step and is described in the manuscript. The `manual_review_status` column in `Data_S1_current_analysis.csv` refers to image-QC escalation and must **not** be interpreted as the taxonomic screening log.

## Quality dimensions and interpretation

The comparison separates dimensions rather than assigning one global "quality" score.

1. **Volume in the matched period**: record/observation count and attached-photo count where available.
2. **Temporal comparability**: annual counts and completeness of full calendar dates.
3. **Spatial provenance**: coordinate availability, public geoprivacy/obscuration where exposed by the API, coordinate-uncertainty metadata where exposed, and number of unique sites/datasets.
4. **Trait readiness**: whether the paper's required focal flower/petal region was author-screened and converted through a deterministic colour-QC pipeline. This is a property of the study workflow, not a native YAMAP platform field.
5. **Observation process**: YAMAP is a hiking/recreation platform, iNaturalist is a purpose-built biodiversity-observation platform, and GBIF is a heterogeneous aggregator. These different observation processes imply different biases rather than an assumption that one source is bias-free.

## Claim ceilings

Allowed if supported by the frozen benchmark:

- YAMAP supplied more same-period usable or candidate image records for this focal species than a named public comparator under the locked filters;
- the three-year YAMAP series is temporally dense/balanced relative to the public comparator;
- all retained YAMAP source rows have study-usable date/coordinate provenance if verified by the source table;
- author screening and deterministic image phenotyping provide a trait-ready layer not supplied natively by occurrence portals.

Not allowed:

- "YAMAP is less biased than iNaturalist/GBIF" without a direct bias analysis;
- "YAMAP observations are all wild/native";
- "research-purpose bias is absent";
- adding GBIF and iNaturalist counts as independent observations;
- treating public Research Grade/community identification as inferior to author review.

Preferred wording is that YAMAP offers a **complementary observation process**: incidental hiking documentation can recover many mountain-plant photographs that were never created as formal biodiversity records, while the study's exhaustive retrieval frame and author screening convert those records into a quantitative trait dataset.

## Manuscript role

Main text: one concise data-source novelty paragraph and one Discussion paragraph.

Supporting Information: frozen count/metadata table, annual-count comparison, query manifest, and explicit strengths/limitations matrix.

This benchmark is descriptive and does not alter the biological Main 1/2/3 analyses.
