# Appendix S1. Matched-period YAMAP versus public biodiversity-image sources

Scope fixed before public-source retrieval: *Campanula punctata* sensu lato, Japan, 2023-01-01 through 2025-12-31. Public comparators required an image and a georeference. Counts are descriptive of this focal taxon/time window only.

## Taxonomic scope: hotarubukuro and yamahotarubukuro

The image records were not divided into the Japanese forms commonly called ホタルブクロ (hotarubukuro) and ヤマホタルブクロ (yamahotarubukuro). Their diagnostic morphological distinction is concentrated in the calyx rather than in the corolla-colour trait analysed here, and many photographs do not show the calyx characters consistently enough for reliable retrospective assignment. Preliminary unpublished genetic data from the study system detected no clear genetic differentiation between the two forms. We therefore treated both as one analytical unit, *C. punctata* sensu lato, to avoid adding calyx-view-dependent classification error to the flower-colour analysis.

This pooling is an analytical decision, not a formal taxonomic revision. The unpublished genetic observation was not analysed as a result in the present study, and voucher-based genomic work could later reassess whether the two forms differ in frequency, ancestry or flower-colour geography.

## Data volume and temporal coverage

| Source | 2023 | 2024 | 2025 | Matched observation/study-row count | Image-object count | Quality subset / overlap |
|---|---:|---:|---:|---:|---:|---|
| YAMAP study retrieval | 642 | 687 | 635 | **1,964** author-screened rows | **1,963** unique image hashes | 1,922 final trait-analysis observations |
| iNaturalist | 95 | 156 | 265 | **516** public photo+geo observations | **882** attached photographs | 472 Research Grade; 34 Needs ID; 10 Casual |
| GBIF | 69 | 112 | 212 | **393** HUMAN_OBSERVATION + StillImage + coordinate records | **679** media entries | 389/393 (99.0%) syndicated iNaturalist; 4/393 Pl@ntNet-linked |

Derived matched-period contrasts:

- YAMAP / iNaturalist photo+geo observation count = **3.81x**;
- YAMAP / iNaturalist Research Grade count = **4.16x**;
- YAMAP unique study images / all attached iNaturalist photographs = **2.23x**;
- YAMAP / matched GBIF record count = **5.00x**, but GBIF is almost entirely overlapping iNaturalist content here and must not be treated as an independent third sample;
- annual count CV: YAMAP **0.043**, iNaturalist **0.501**, GBIF **0.560**.

## Data quality and observation-process comparison

| Dimension | YAMAP study retrieval | iNaturalist matched set | GBIF matched set |
|---|---|---|---|
| Original observation purpose | Hiking/navigation/activity documentation | Purpose-built biodiversity observation | Aggregation of provider records |
| Taxonomic/subject validation used here | Every recovered candidate visually screened by the study author before inclusion; similar/incorrect campanuloid subjects removed | Community identification; 472/516 Research Grade | Provider-dependent; 389/393 records inherited from iNaturalist |
| Full calendar date in frozen matched set | 1,964/1,964 | 516/516 | 393/393 |
| Georeferenced in matched set | 1,964/1,964; route/activity-linked source coordinates; photo-coordinate QC retained | 516/516 under `geo=true`; 23 publicly obscured | 393/393 under `hasCoordinate=true`; metadata heterogeneous |
| Reported positional/coordinate uncertainty | Study source and photo-coordinate QC rather than a directly comparable platform accuracy field | positional accuracy reported for 427/516; median 8 m | coordinate uncertainty reported for 331/393; median 8 m |
| Duplicate/image identity audit | SHA-256 image hashes; exact duplicates and photo-coordinate conflicts audited | not part of the matched count benchmark | provider-dependent; syndicated records possible |
| Flower/petal region validation | Study author confirmed focal flower/petal region used for phenotyping | not a native platform field | not a native occurrence field |
| Quantitative flower-colour trait | Deterministic RGB -> CIELAB + QC -> pigmentation state + conditional intensity | not native; would require additional image phenotyping | not native; would require additional image phenotyping |
| Mountain-habitat enrichment | Strong by platform use; useful for natural/semi-natural mountain trait geography, but not proof of wild provenance | not restricted to mountains | not restricted to mountains |
| Main remaining bias | route/access, subject choice, flower conspicuousness, hidden locations, uneven mountain use | observer/access/taxon-reporting bias; cultivated/obscured records possible | heterogeneous provider biases and non-independence/record syndication |

## Interpretation

The benchmark does not assign one global quality score. iNaturalist has a strong native community-identification/Research-Grade system. YAMAP's advantage for this study arises from a different observation process plus a study-specific quality layer: exhaustive candidate review, taxon/subject/petal-ROI validation, image-hash and coordinate audit, and deterministic quantitative phenotyping. The mountain-route frame is advantageous for sampling the natural geographical context of the focal wild herb but can compress the urban-rural gradient used in the later human-context analysis.

## Reproducibility

- benchmark specification: `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`
- result note: `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`
- frozen retrieval run/artifact: `31289927019` / `9031041034`
- independent GBIF-provider audit run/artifact: `31290095532` / `9031085975`