#!/usr/bin/env python3
"""Lightweight submission-format checks for the Journal of Biogeography draft.

This validates only stable manuscript-format constraints captured from the Wiley
Author Guidelines on 2026-08-09. The official portal must still be re-checked
immediately before submission.
"""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent
MAIN = ROOT / "JBI_main_manuscript_anonymized.md"
COVER = ROOT / "JBI_cover_letter.md"
SUPPORTING = (
    ROOT / "supporting" / "Appendix_S1_yamap_public_benchmark.md",
    ROOT / "supporting" / "Appendix_S2_image_phenotyping.md",
    ROOT / "supporting" / "Appendix_S3_broad_environment_spatial_model.md",
    ROOT / "supporting" / "Appendix_S4_bombus_sdm_occurrence_support.md",
    ROOT / "supporting" / "Appendix_S5_local_pollinator_robustness.md",
)
FORBIDDEN_IDENTIFIERS = ("zuizui0223", "rachelzhang", "ZHANG Ruiqi", "張瑞琪")


def words(text: str) -> list[str]:
    return re.findall(r"\b[\wÀ-ž*'’-]+\b", text)


def fail(message: str) -> None:
    raise SystemExit(message)


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

keyword_line = re.search(r"\*\*Keywords:\*\*\s*(.+)", text)
keywords = [x.strip() for x in keyword_line.group(1).split(",") if x.strip()]
if not 6 <= len(keywords) <= 10:
    fail(f"Keyword count must be 6-10: {len(keywords)}")
if [k.casefold() for k in keywords] != sorted(k.casefold() for k in keywords):
    fail("Keywords are not alphabetical")

required = [
    "## Introduction",
    "## Materials and Methods",
    "## Results",
    "## Discussion",
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

main_body = text.split("## Introduction", 1)[1].split("## Acknowledgements", 1)[0]
body_n = len(words(main_body))
if body_n > 6000:
    fail(f"Introduction-through-Discussion exceeds 6000 words: {body_n}")

# Double-anonymous safety: known identifying repository/user strings should not appear.
for forbidden in FORBIDDEN_IDENTIFIERS:
    if forbidden.casefold() in text.casefold():
        fail(f"Potential identifying string in anonymized manuscript: {forbidden}")

for appendix in SUPPORTING:
    if not appendix.is_file():
        fail(f"Missing current Supporting Information file: {appendix.name}")
    appendix_text = appendix.read_text(encoding="utf-8")
    appendix_lines = appendix_text.splitlines()
    if not appendix_lines or not appendix_lines[0].startswith("# Appendix S"):
        fail(f"Supporting Information file lacks an Appendix S title: {appendix.name}")
    for forbidden in FORBIDDEN_IDENTIFIERS:
        if forbidden.casefold() in appendix_text.casefold():
            fail(f"Potential identifying string in Supporting Information: {appendix.name}: {forbidden}")

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
print(f"PASS cover_interest_words={cover_n}")
