# YAMAP as an iEcology source: manuscript and reviewer rationale

## Core claim

The use of YAMAP is a methodological contribution because it recovers a spatially structured image stream created for **hiking and activity documentation**, not for biodiversity recording. This fits the iEcology concept of extracting ecological information from digital resources accumulated for other purposes.

The defensible novelty is **not** that YAMAP is unbiased or globally better than iNaturalist/GBIF. The defensible novelty is stronger and more specific: for this focal mountain plant and recent time window, YAMAP provides a **complementary observation process** that was empirically much more image-rich, while route/activity provenance and exhaustive study-specific visual screening make the images suitable for quantitative trait geography.

## Matched-period benchmark: this is not merely a hypothetical data source

The benchmark was specified before public-source retrieval for *Campanula punctata* in Japan, 2023-01-01 through 2025-12-31, requiring images and georeferences in the public comparators.

- YAMAP study retrieval: **1,964** author-screened rows, **1,963** unique image hashes, 1,922 final trait observations.
- iNaturalist: **516** photo+geo observations, **882** attached photos, **472** Research Grade.
- GBIF: **393** human-observation + StillImage + coordinate records; **389/393 (99.0%)** were syndicated from iNaturalist and 4 were Pl@ntNet-linked.

Thus YAMAP supplied **3.81 times** the matched iNaturalist observation count and **2.23 times** the total number of attached iNaturalist photos within the same three-year window. GBIF and iNaturalist are not additive independent image pools here.

Annual YAMAP counts were **642 / 687 / 635** for 2023/2024/2025 (annual CV=0.043), providing a dense and temporally balanced recent series. The corresponding iNaturalist counts were 95 / 156 / 265. These trends are descriptive and should not be interpreted as biological change or general platform performance.

Frozen benchmark documentation:

- `reproducibility/yamap_public_database_benchmark_spec_2026-08-09.md`
- `reproducibility/yamap_public_database_benchmark_results_2026-08-09.md`
- retrieval artifact `9031041034`
- independent GBIF-provider audit artifact `9031085975`

## Why YAMAP is useful here

1. Public hiking activities can retain GPS-linked routes and photograph positions.
2. The original user task is documenting a hike, not submitting a formal *C. punctata* record or scoring flower colour.
3. Mountain/activity organization creates a sampling frame concentrated in mountain and semi-natural habitats relevant to wild *C. punctata* geography.
4. Within the predefined three-year retrieval frame, recovered candidates were not subsampled before author screening.
5. The author reviewed taxon identity, focal flower and petal region, removing incorrect/similar campanuloid subjects where encountered.
6. Image hashes, photo-coordinate mappings, extraction method and QC provenance were retained before RGB/CIELAB phenotyping.
7. The result is suitable for local neighbourhood and transition analyses because photographs retain spatial provenance at the level required by the design.

## Mountain sampling: a targeted advantage, not proof of wild provenance

YAMAP's mountain/trail concentration is often described only as sampling bias, but for this study it also aligns the sample with the biological target. A mountain-route frame enriches natural or semi-natural habitats where self-sustaining wild *C. punctata* populations are plausible, compared with a general occurrence portal spanning the full garden-to-urban continuum.

This does **not** establish that every YAMAP photograph is a wild plant. Planted individuals, roadsides, trailheads, temples, tourist facilities and horticultural escape can occur in mountain activities. The correct claim is enrichment of the relevant natural-habitat sampling frame, not certainty of provenance.

The same property has an important consequence for Main 3: because YAMAP undersamples the full urban-rural continuum, it can compress human-modification variation and reduce power to detect a broad anthropogenic gradient. Conversely, access infrastructure can overrepresent human-modified mountain edges. This two-sided sampling effect belongs explicitly in the Discussion.

## Does a non-research platform have less observer bias?

Do not say `observer bias was avoided`. A narrower argument is defensible.

Because users are documenting hikes rather than participating in a focal-species survey, the initial photograph stream is not directly conditioned on the aim of submitting *C. punctata* to a biodiversity database or obtaining a community identification. This may reduce **taxon-reporting-, inventory-, or research-purpose-conditioned selection** relative to a purpose-built focal-species occurrence dataset.

However, substantial observer bias remains:

- route/access choice;
- user and subject-selection bias;
- flower-conspicuousness bias;
- hidden/private location loss;
- GPS error;
- uneven mountain visitation.

Therefore the correct comparison is **different observation processes**, not `biased versus unbiased`.

iNaturalist also has a native strength that YAMAP does not: community identification and Research Grade. This should be acknowledged explicitly. YAMAP's study-specific advantage comes from combining a different image stream with exhaustive author screening and quantitative trait extraction, not from claiming native platform identification is superior.

## What author review and generated public-table construction add

Author review can reduce:

- taxonomic misidentification, including similar campanuloid subjects such as *Adenophora* where encountered;
- non-focal flowers or petal regions;
- exact/repeated image problems;
- image-processing ROI error.

The subsequent pipeline retains:

- source-row and observation IDs;
- complete dates for the YAMAP benchmark rows;
- coordinates and photo-coordinate QC;
- SHA-256 image hashes;
- deterministic colour-extraction method;
- RGB and CIELAB values;
- image/QC provenance;
- the response-blind two-part phenotype.

The study therefore constructs:

`hiking activity/photo -> date/GPS provenance -> exhaustive candidate review -> taxon/subject/ROI validation -> image-hash duplicate audit -> deterministic pixel summary -> RGB/CIELAB -> QC -> pigmentation state + conditional intensity`.

The generated public observation table is thus a curated quantitative-trait dataset, not a direct occurrence export. It is rebuilt from the canonical Zenodo workbook and is not committed as a second source. Purpose-built occurrence portals could also support this transformation, but petal-level colour is not a native occurrence field and would require an additional image-screening and phenotyping workflow.

## Relation to quantitative flower-colour geography

The YAMAP contribution becomes stronger when paired with the two-part phenotype. Broad flower-colour studies often use categorical morph frequencies because standardized quantitative measurements are unavailable range-wide. Those data are valuable for morph geography but cannot resolve continuous variation among already pigmented flowers. Photograph-based CIELAB phenotyping provides a scalable intermediate layer between categorical morph records and small-sample calibrated spectroscopy.

The manuscript therefore claims a combined methodological advance:

`repurposed GPS-linked images -> exhaustive human ecological validation -> quantitative two-part phenotype -> spatially explicit trait geography`.

## Relation to the pollinator analysis

YAMAP is not used to solve Bombus/environment confounding. That is handled by the analysis design:

- broad environment + space is modelled first;
- the Bombus question changes scale to abrupt nearby white/pigmented transitions;
- pair selection is Bombus-blind;
- *B. ardens* + *B. diversus* define the main directional exposure because they are the documented broad focal pollinators;
- montane/alpine taxa are retained as a guardrail because their apparent flower-colour association disappears after near-equal-elevation comparison.

Thus data-source innovation and causal restraint remain separate contributions.

## Relation to the human-context analysis

YAMAP's mountain-route sampling can plausibly **compress the represented urban-rural gradient**, because the sample is not drawn uniformly across highly urbanized landscapes. This may reduce power for a broad anthropogenic association and is a reasonable Discussion point.

However, this cannot be used as a one-directional excuse for a weak human result. Roads, trailheads, cableways and accessible mountain margins may increase both photographic observation opportunity and human modification. The correct interpretation is that the YAMAP sampling frame can affect human-context contrasts in both directions.

## Reviewer-facing one-paragraph summary

> We used YAMAP not because a recreational platform is assumed to be unbiased, but because it provides a complementary iEcology observation stream that was unusually productive for the focal mountain plant. In a matched 2023–2025 benchmark, the author-screened YAMAP retrieval contained 1,964 georeferenced records, compared with 516 iNaturalist photo+geo observations; the matched GBIF image set was almost entirely syndicated from iNaturalist. Public hiking activities provide route-linked photograph locations and incidental plant images generated for recreation rather than formal species reporting. We combined this alternative observation process with exhaustive author validation of taxon identity and the focal flower region, image-hash/coordinate audit and deterministic colour phenotyping. Route/access, conspicuousness and subject-selection biases remain explicit limitations. The methodological contribution is therefore both data acquisition and trait construction: a large recent mountain-image stream was converted into a reproducible quantitative phenotype suitable for spatially explicit ecological tests.

## Claims to avoid

- `YAMAP is more accurate than iNaturalist/GBIF.`
- `YAMAP removes observer bias.`
- `All YAMAP records are wild.`
- `All YAMAP photo coordinates are public.`
- `Mountain sampling proves human influence is absent.`
- `Author review makes the dataset representative of Japanese populations.`
- `GBIF + iNaturalist provide 909 independent matched records` (they strongly overlap here).

## Preferred paper-level novelty sentence

> The methodological novelty lies not in adding more predictor families, but in recovering an unusually dense, recent mountain-trait image series from a non-biodiversity digital platform, converting it through exhaustive study-specific validation into a quantitative phenotype, and then changing spatial scale and inferential object as the ecological question narrows.
