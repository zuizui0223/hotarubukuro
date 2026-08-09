# YAMAP versus public biodiversity-image sources: matched-period benchmark results

Date: 2026-08-09

## Purpose

This benchmark quantifies the data-source contribution of the YAMAP sampling frame for the focal study. It does **not** rank platforms globally and does not claim that YAMAP is unbiased. The comparison is restricted to *Campanula punctata* in Japan during the same recent period used by the manuscript, 2023-01-01 through 2025-12-31.

The comparison specification was committed before retrieving the public-source counts:

- specification: `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`
- specification commit: `80dd344c93e5bc4b634634458f3c1b49ad0a9435`
- retrieval workflow run: `31289927019`
- retrieval artifact: `9031041034`
- retrieval artifact SHA-256: `3e53669395cfd926a0942b3488f844720dca2cb97b9ea210627262691e69f31a`

A separate audit re-read the frozen raw GBIF records because the first summary used the absent field `datasetTitle` instead of GBIF's returned `datasetName` when identifying syndicated providers:

- overlap-audit workflow run: `31290095532`
- overlap-audit artifact: `9031085975`
- audit computation commit: `cb56e220262cf04eb25049b9192e06b74ae2d868`

The raw retrieval counts were unchanged by this correction; only provider attribution within GBIF was corrected.

## 1. Same-period data volume

Under the fixed Japan + 2023-2025 + image + georeference scope:

| Source | Matched observations / study rows | Image objects | Additional quality subset |
|---|---:|---:|---:|
| YAMAP study retrieval | **1,964** author-screened YAMAP rows | **1,963** unique image hashes | 1,922 final manuscript trait observations |
| iNaturalist | **516** public photo+geo observations | **882** attached photos | 472 Research Grade observations |
| GBIF | **393** HUMAN_OBSERVATION + StillImage + coordinate records | **679** media entries | all-basis image+coordinate count was also 393 |

Thus, within the matched three-year window, the YAMAP study retrieval contained:

- **3.81 times** as many georeferenced photo records as the iNaturalist photo+geo observation set;
- **4.16 times** as many source rows as the iNaturalist Research Grade subset;
- **5.00 times** as many source rows as the matched GBIF human-observation set;
- even when counting all attached iNaturalist photos rather than observations, the YAMAP archive contained **2.23 times** as many unique study images (1,963 vs 882).

These ratios describe this focal taxon, country and time window only. They are not platform-wide sampling-efficiency estimates.

## 2. GBIF is not an independent third photo pool here

The independent overlap audit of all 393 frozen GBIF records found:

- **389 / 393 (99.0%)** were syndicated from the dataset `iNaturalist research-grade observations` / institution `iNaturalist`;
- the remaining **4 / 393 (1.0%)** were linked to Pl@ntNet;
- no matched GBIF record formed a large independent image pool outside those providers.

Accordingly, iNaturalist and GBIF counts must not be added together. In this focal comparison, GBIF primarily exposes a subset of the same iNaturalist observation stream through an aggregator.

## 3. The YAMAP series is recent and unusually even across the three study years

Annual YAMAP source counts were:

- 2023: **642**;
- 2024: **687**;
- 2025: **635**.

The coefficient of variation across the three annual counts was only **0.043**, and the largest annual count was 1.08 times the smallest.

For comparison, matched iNaturalist counts were 95, 156 and 265 (annual CV approximately 0.501), and matched GBIF counts were 69, 112 and 212 (CV approximately 0.560). The public-source increase over this period may reflect platform use, reporting and/or biological sampling and is not interpreted mechanistically. The methodological advantage for this study is narrower: YAMAP supplied a dense and temporally balanced recent image series, which minimizes the need to combine observations separated by decades and makes calendar year explicit and comparable within the study window.

## 4. Spatial and temporal metadata

For the 1,964 YAMAP rows in the benchmark:

- complete YYYY-MM-DD date: **1,964 / 1,964**;
- finite latitude and longitude: **1,964 / 1,964**;
- unique exact study sites: **1,926**;
- 1,962 photo-coordinate mappings were resolved by workbook cell + image hash;
- 2 duplicate-photo-at-multiple-coordinate cases were explicitly flagged for coordinate review.

The matched iNaturalist and GBIF queries also required georeferenced records and had complete dates under the final retrieval. iNaturalist reported positional accuracy for 427/516 observations (median 8 m), with 23 observations publicly obscured. GBIF reported coordinate uncertainty for 331/393 records (median 8 m). Therefore the manuscript does **not** claim that YAMAP has intrinsically higher coordinate accuracy. Its study-specific strength is route/activity-linked spatial provenance combined with explicit source-row, image-hash and coordinate-QC audit.

## 5. Quality is multidimensional: platform-native identification versus study-specific trait readiness

iNaturalist has an important native quality mechanism that YAMAP does not: community identification and Research Grade status. GBIF inherits provider-dependent identification and metadata. These are not treated as inferior to author review.

The advantage of the YAMAP workflow for the present question is different. The study did not use platform labels as final phenotypes. Within the predefined retrieval frame, all recovered candidate records were brought through author screening before inclusion. The author checked the focal organism/flower and removed taxonomic look-alikes or erroneous subjects (including campanuloid *Adenophora* images where encountered), audited repeated photographs and confirmed the petal region used for colour measurement. The final data construction then retained image hashes, dates, coordinates, colour-extraction method and QC provenance before converting images to RGB/CIELAB values.

Consequently, `Data_S1` is not merely an occurrence export. It is a **study-curated quantitative-trait table** built from a non-biodiversity digital image stream:

`hiking activity/photo -> spatial/date provenance -> exhaustive candidate review -> taxon/subject/ROI validation -> image-hash duplicate audit -> deterministic pixel summary -> sRGB/CIELAB conversion -> image QC -> two-part flower-colour phenotype`.

Purpose-built occurrence portals could also be processed this way, but those petal-level quantitative trait fields are not native occurrence variables and would require an additional study-specific screening/phenotyping workflow.

## 6. Why the mountain-route sampling frame is useful—and what it cannot guarantee

YAMAP is organized around hiking activities, so the retained sample is strongly enriched for mountain and trail environments. For a native mountain/woodland herb such as *C. punctata*, this is useful because it concentrates sampling in habitats where self-sustaining wild populations are plausible and reduces the proportion of observations drawn from the full garden-to-urban continuum. It does **not** prove that every photographed plant is wild, and trailheads, roadsides, temples, tourist sites or planted individuals can still occur within mountain activities.

This same property matters for the human-context analysis. A mountain-route sampling frame can compress the urban-rural gradient and therefore reduce statistical power to detect a broad anthropogenic association. Conversely, hiking access can overrepresent trailheads, roads and accessible mountain margins. The weak human-context result in the main analysis is therefore interpreted within this restricted mountain sampling frame rather than as evidence that human influence is absent.

## 7. Does a non-research platform reduce observer bias?

Only a narrower claim is justified. Because YAMAP users are primarily documenting hikes rather than submitting a formal species record, the initial photograph stream is not conditioned on participation in a biodiversity survey or on achieving a community identification for *C. punctata*. This can reduce **taxon-reporting- or research-purpose-conditioned selection** relative to a dataset assembled specifically to report the focal species.

It does **not** eliminate observer bias. Hikers still choose routes and subjects, conspicuous flowers are more likely to be photographed, user activity varies among mountains, and photographs with hidden locations are unavailable to the geographical analysis. We therefore describe YAMAP as a **complementary observation process with different biases**, not as a generally less biased database.

## 8. Manuscript placement

### Main text

Use the benchmark only to establish the methodological contribution:

> In a matched 2023-2025 Japan benchmark, the author-screened YAMAP retrieval yielded 1,964 georeferenced photo records (1,963 unique images), compared with 516 iNaturalist photo+geo observations (472 Research Grade). The matched GBIF image set contained 393 records and was 99% syndicated from iNaturalist. Thus a recreation platform supplied several-fold more focal-species images over the same short period, while requiring study-specific taxonomic and trait validation.

### Supporting Information

Retain:

- full annual count table;
- observation and attached-photo counts;
- iNaturalist quality-grade/geoprivacy/accuracy summaries;
- GBIF provider-overlap audit;
- YAMAP source/date/coordinate/hash/QC summaries;
- strengths/limitations matrix;
- exact query manifest and frozen raw API responses.

## Claim ceiling

**Supported:** YAMAP supplied substantially more same-period georeferenced focal-species photographs than the locked iNaturalist/GBIF comparators; the series was recent and annually balanced; the study added exhaustive author screening and a deterministic trait-phenotyping/QC layer; mountain-route sampling is advantageous for targeting natural mountain trait geography and can reduce the human-gradient range represented.

**Not supported:** YAMAP is globally superior, unbiased, always more accurate, or guaranteed to contain only wild plants; observer bias is absent; the weak human result proves that mountain populations are unaffected by people.
