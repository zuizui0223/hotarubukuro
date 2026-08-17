#!/usr/bin/env python3
"""Validate the current JBI submission package and achievement-forward science locks."""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent
REPO_ROOT = ROOT.parent.parent
MAIN = ROOT / "JBI_main_manuscript_anonymized.md"
COVER = ROOT / "JBI_cover_letter.md"
FIGURE_CAPTIONS = ROOT / "JBI_main_figure_captions.md"
FIGURE_README = ROOT / "figures" / "README.md"
FIGURE_RENDER = REPO_ROOT / "scripts" / "render_jbi_main_figures.R"
TRANSLATED_ABSTRACT = ROOT / "JBI_translated_abstract_ja.md"
CHECKLIST = ROOT / "JBI_submission_checklist.md"
SUPPORTING = (
    ROOT / "supporting" / "Appendix_S1_yamap_public_benchmark.md",
    ROOT / "supporting" / "Appendix_S2_image_phenotyping.md",
    ROOT / "supporting" / "Appendix_S3_broad_environment_spatial_model.md",
    ROOT / "supporting" / "Appendix_S4_bombus_sdm_occurrence_support.md",
    ROOT / "supporting" / "Appendix_S5_local_pollinator_robustness.md",
    ROOT / "supporting" / "Appendix_S6_event_departures_human_context.md",
)
FORBIDDEN_IDENTIFIERS = ("zuizui0223", "rachelzhang", "ZHANG Ruiqi", "張瑞琪")


def words(text: str) -> list[str]:
    return re.findall(r"\b[\wÀ-ž*'’-]+\b", text)


def fail(message: str) -> None:
    raise SystemExit(message)


def assert_anonymous(label: str, body: str) -> None:
    for forbidden in FORBIDDEN_IDENTIFIERS:
        if forbidden.casefold() in body.casefold():
            fail(f"Potential identifying string in {label}: {forbidden}")


def require_tokens(label: str, body: str, tokens: tuple[str, ...]) -> None:
    missing = [token for token in tokens if token not in body]
    if missing:
        fail(f"{label} is missing current token(s): {', '.join(missing)}")


text = MAIN.read_text(encoding="utf-8")
lines = text.splitlines()
if not lines or not lines[0].startswith("# "):
    fail("Missing manuscript title")

title = lines[0][2:].strip()
if len(title) > 115:
    fail(f"Title exceeds 115 characters: {len(title)}")
if "Campanula punctata" in title or "C. punctata" in title:
    fail("JBI title must not contain a Latin binomial")

m = re.search(r"\*\*Running title:\*\*\s*(.+)", text)
if not m:
    fail("Missing running title")
running = m.group(1).strip()
if len(running) >= 40:
    fail(f"Running title must be <40 characters: {len(running)}")

if "## Abstract" not in text or "**Keywords:**" not in text:
    fail("Missing abstract or keywords")
abstract = text.split("## Abstract", 1)[1].split("**Keywords:**", 1)[0]
for heading in ("Aim", "Location", "Taxon", "Methods", "Results", "Main conclusions"):
    if f"**{heading}:**" not in abstract:
        fail(f"Structured abstract missing heading: {heading}")
abstract_n = len(words(abstract))
if abstract_n > 300:
    fail(f"Abstract exceeds 300 words: {abstract_n}")
require_tokens(
    "Abstract species-name format",
    abstract,
    ("spotted bellflower (*Campanula punctata*)",),
)

keyword_line = re.search(r"\*\*Keywords:\*\*\s*(.+)", text)
if not keyword_line:
    fail("Missing keyword line")
keywords = [x.strip() for x in keyword_line.group(1).split(",") if x.strip()]
if not 6 <= len(keywords) <= 10:
    fail(f"Keyword count must be 6-10: {len(keywords)}")
if [k.casefold() for k in keywords] != sorted(k.casefold() for k in keywords):
    fail("Keywords are not alphabetical")

required = [
    "## 1. Introduction",
    "## 2. Materials and Methods",
    "## 3. Results",
    "## 4. Discussion",
    "## Acknowledgements",
    "## References",
    "## Data Accessibility Statement",
]
positions = []
for header in required:
    if header not in text:
        fail(f"Missing required section: {header}")
    positions.append(text.index(header))
if positions != sorted(positions):
    fail("Required sections are not in the expected order")

numbered_subsections = (
    "### 1.1 The geographical mystery of flower-colour polymorphism",
    "### 1.5 Predictions",
    "### 2.1 Study system and YAMAP sampling",
    "### 2.6 Reproducibility and inferential order",
    "### 3.1 A new image stream revealed a national quantitative polymorphism",
    "### 3.5 Post-selection analysis identified a short-range human-context clue",
    "### 4.1 The national trait dataset changed the biological question",
    "### 4.6 A spatially varying balance can maintain flower-colour polymorphism",
)
require_tokens("Numbered Main structure", text, numbered_subsections)

main_body = text.split("## 1. Introduction", 1)[1].split("## Acknowledgements", 1)[0]
body_n = len(words(main_body))
if body_n > 6000:
    fail(f"Introduction-through-Discussion exceeds 6000 words: {body_n}")

assert_anonymous("anonymized manuscript", text)

require_tokens(
    "JBI species and software style",
    main_body,
    (
        "spotted bellflower (*Campanula punctata* Lam.; Campanulaceae)",
        "R 4.5.3 (R Core Team, 2026)",
        "'INLA' package version 25.10.19",
    ),
)

require_tokens(
    "Main manuscript science locks",
    text,
    (
        "1,922",
        "3.81 times",
        "67 sharp transitions",
        "Sixteen pigmented cells",
        "0.0548",
        "calibrated field targets",
    ),
)

for citation, reference in (
    ("Soberón, 2007", "Soberón, J. (2007)."),
    ("Araújo & Rozenfeld, 2014", "Araújo, M. B., & Rozenfeld, A. (2014)."),
    ("Roberts et al., 2017", "Roberts, D. R., et al. (2017)."),
    ("Valavi et al., 2019", "Valavi, R., Elith, J., Lahoz-Monfort, J. J., & Guillera-Arroita, G. (2019)."),
    ("R Core Team, 2026", "R Core Team. (2026)."),
    ("Rue et al., 2009", "Rue, H., Martino, S., & Chopin, N. (2009)."),
):
    if citation not in main_body:
        fail(f"Current Main citation missing: {citation}")
    if reference not in text.split("## References", 1)[1]:
        fail(f"Current Main reference missing: {reference}")

appendix_texts: dict[str, str] = {}
for appendix in SUPPORTING:
    if not appendix.is_file():
        fail(f"Missing current Supporting Information file: {appendix.name}")
    appendix_text = appendix.read_text(encoding="utf-8")
    appendix_texts[appendix.name] = appendix_text
    appendix_lines = appendix_text.splitlines()
    if not appendix_lines or not appendix_lines[0].startswith("# Appendix S"):
        fail(f"Supporting Information file lacks an Appendix S title: {appendix.name}")
    assert_anonymous(f"Supporting Information {appendix.name}", appendix_text)

s6 = appendix_texts["Appendix_S6_event_departures_human_context.md"]
require_tokens("Appendix S6", s6, ("**16**", "0.27897", "0.05479"))
if "The horticultural trade and ornamental plant invasions in Britain" in s6:
    fail("Appendix S6 contains the superseded alternative Dehnen-Schmutz 2007 citation")

if not FIGURE_CAPTIONS.is_file():
    fail("Missing current Main-figure captions")
figure_text = FIGURE_CAPTIONS.read_text(encoding="utf-8")
figure_headings = re.findall(r"^## Figure ([1-4])\.", figure_text, flags=re.MULTILINE)
if figure_headings != ["1", "2", "3", "4"]:
    fail(f"Figure captions must contain Figures 1-4 exactly once and in order: {figure_headings}")
require_tokens("Main-figure captions", figure_text, ("1,922", "Sixty-seven", "**16**", "0.05479"))
if re.search(r"\([A-D]\)", figure_text):
    fail("JBI figure captions must use lowercase panel labels (a)-(d)")
for number in range(1, 5):
    block = figure_text.split(f"## Figure {number}.", 1)[1]
    if number < 4:
        block = block.split(f"## Figure {number + 1}.", 1)[0]
    for token in ("*Campanula punctata*", "Japan", "100-km bar scale"):
        if token not in block:
            fail(f"Figure {number} legend is not standalone for JBI; missing {token}")
assert_anonymous("Main-figure captions", figure_text)

if not FIGURE_RENDER.is_file():
    fail("Missing current Main-figure renderer")
render_text = FIGURE_RENDER.read_text(encoding="utf-8")
require_tokens(
    "Main-figure renderer",
    render_text,
    (
        "add_100km_scale",
        'tag_panel(fig1a, "a")',
        'tag_panel(fig2a, "a")',
        'tag_panel(fig3a, "a")',
        'tag_panel(fig4a, "a")',
    ),
)
if re.search(r'tag_panel\([^\n]+,\s*"[A-D]"\)', render_text):
    fail("Main-figure renderer still contains uppercase JBI panel tags")

if not FIGURE_README.is_file():
    fail("Missing generated-figure policy README")
figure_readme_text = FIGURE_README.read_text(encoding="utf-8")
for required_phrase in ("600-dpi PNG", "vector PDF", "Actions artifact"):
    if required_phrase not in figure_readme_text:
        fail(f"Figure README is missing required output-policy phrase: {required_phrase}")

if not TRANSLATED_ABSTRACT.is_file():
    fail("Missing translated abstract")
translated = TRANSLATED_ABSTRACT.read_text(encoding="utf-8")
require_tokens("Japanese translated abstract", translated, ("1,922", "67", "16地点", "0.0548"))
if "17地点" in translated:
    fail("Japanese translated abstract still contains superseded 17-site result")

if not CHECKLIST.is_file():
    fail("Missing submission checklist")
checklist = CHECKLIST.read_text(encoding="utf-8")
require_tokens(
    "Submission checklist",
    checklist,
    (
        f"{body_n:,} words",
        f"{abstract_n} words",
        "16 current-Broad departures",
        "R 4.5.3",
        "INLA 25.10.19",
        "Taxon authority",
    ),
)

cover = COVER.read_text(encoding="utf-8")
cover_body = cover.split("Dear Senior Editors,", 1)[-1].split("Sincerely,", 1)[0]
cover_n = len(words(cover_body))
if cover_n > 100:
    fail(f"Cover-letter interest statement exceeds conservative 100-word target: {cover_n}")

print(f"PASS title_chars={len(title)}")
print(f"PASS running_title_chars={len(running)}")
print(f"PASS abstract_words={abstract_n}")
print(f"PASS keywords={len(keywords)}")
print(f"PASS intro_to_discussion_words={body_n}")
print(f"PASS supporting_appendices={len(SUPPORTING)}")
print(f"PASS main_figure_captions={len(figure_headings)}")
print("PASS species_authority_and_software_versions=1")
print("PASS achievement_forward_science_locks=1")
print("PASS current_science_crossfile_consistency=1")
print("PASS jbi_figure_label_and_map_scale_consistency=1")
print(f"PASS cover_interest_words={cover_n}")
