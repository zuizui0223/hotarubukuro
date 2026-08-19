#!/usr/bin/env python3
"""Use direct spatial-continuity language and lock pooled taxon scope.

The active analysis asks whether supported environmental separation orders
phenotype divergence beyond fitted spatial continuity. Appendix S1 treats
hotarubukuro and yamahotarubukuro as one C. punctata sensu-lato analytical
unit without presenting that choice as a formal taxonomic revision.
"""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

ACTIVE_FILES = (
    ROOT / "submission/jbi/JBI_main_manuscript_anonymized.md",
    ROOT / "submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md",
    ROOT / "submission/jbi/JBI_background_architecture.md",
    ROOT / "submission/jbi/JBI_main_figure_plan.md",
    ROOT / "submission/jbi/JBI_submission_checklist.md",
    ROOT / "paper/README.md",
    ROOT / "paper/analysis-map.md",
)

S1 = ROOT / "submission/jbi/supporting/Appendix_S1_yamap_public_benchmark.md"
UPGRADE = ROOT / "scripts/upgrade_final_analysis_pipeline.py"
VALIDATOR = ROOT / "submission/jbi/validate_jbi_submission.py"

TOKEN_PATTERN = r"(?<![A-Za-z0-9])(?:F[_–—-]?ST|P[_–—-]?ST|Q[_–—-]?ST)(?![A-Za-z0-9])"
GENETIC_ANALOGY = re.compile(TOKEN_PATTERN, flags=re.IGNORECASE)
GENETIC_SENTENCE = re.compile(
    rf"[^.!?\n]*{TOKEN_PATTERN}[^.!?\n]*[.!?]",
    flags=re.IGNORECASE,
)

NEUTRAL_SENTENCE = (
    "The comparison asks whether environmental separation orders phenotype "
    "divergence beyond fitted spatial continuity; it does not identify the "
    "underlying mechanism or demonstrate selection or local adaptation."
)


def write_if_changed(path: Path, text: str) -> None:
    old = path.read_text(encoding="utf-8")
    if old != text:
        path.write_text(text, encoding="utf-8")


def neutralize(text: str, *, remove_all_tokens: bool) -> str:
    replacements = {
        "This FST/PST-inspired comparison is explicitly non-genetic: the spatial field is an empirical continuity expectation rather than drift, and any excess is environmental alignment rather than proof of selection or local adaptation":
            "This comparison asks only whether environmental separation orders phenotype divergence beyond fitted spatial continuity; it does not identify the underlying mechanism or demonstrate selection or local adaptation",
        "Neither the SPDE field nor the distance test is FST, PST or QST, and no result supports `selection > drift` language.":
            "The SPDE field and distance test do not identify a genetic mechanism, and neither result demonstrates selection or local adaptation.",
        "The spatial null represents unresolved geography, not neutral genetic divergence or drift. A positive excess":
            "The spatial null represents unresolved geography. A positive excess",
        "do not estimate fitness.The comparison":
            "do not estimate fitness. The comparison",
        "FST/PST-inspired": "spatial-continuity",
        "FST–PST-inspired": "spatial-continuity",
        "FST-PST-inspired": "spatial-continuity",
        "FST/PST analogy": "spatial-continuity comparison",
        "FST–PST analogy": "spatial-continuity comparison",
        "FST-PST analogy": "spatial-continuity comparison",
    }
    for old, new in replacements.items():
        text = text.replace(old, new)

    text = re.sub(
        r"This\s+(?:FST/PST|FST–PST|FST-PST)-inspired comparison.*?\(Appendix S3\)\.",
        NEUTRAL_SENTENCE[:-1] + " (Appendix S3).",
        text,
        flags=re.DOTALL,
    )

    if not remove_all_tokens:
        return text

    text = GENETIC_SENTENCE.sub(NEUTRAL_SENTENCE, text)

    normalized: list[str] = []
    for line in text.splitlines(keepends=True):
        if not GENETIC_ANALOGY.search(line):
            normalized.append(line)
            continue
        ending = "\n" if line.endswith("\n") else ""
        stripped = line.lstrip()
        if stripped.startswith("#"):
            normalized.append("## Spatial-continuity comparison" + ending)
        elif stripped.startswith("-"):
            normalized.append("- " + NEUTRAL_SENTENCE + ending)
        else:
            normalized.append(NEUTRAL_SENTENCE + ending)
    return "".join(normalized)


def update_active_files() -> None:
    for path in ACTIVE_FILES:
        if path.is_file():
            write_if_changed(
                path,
                neutralize(path.read_text(encoding="utf-8"), remove_all_tokens=True),
            )


def update_generator() -> None:
    text = UPGRADE.read_text(encoding="utf-8")
    text = neutralize(text, remove_all_tokens=False)
    write_if_changed(UPGRADE, text)


def update_validator() -> None:
    text = VALIDATOR.read_text(encoding="utf-8")
    insertion = '''\ns1 = appendix_texts["Appendix_S1_yamap_public_benchmark.md"]\nrequire_tokens(\n    "Appendix S1 pooled taxonomic scope",\n    s1,\n    (\n        "ホタルブクロ",\n        "ヤマホタルブクロ",\n        "diagnostic morphological distinction is concentrated in the calyx",\n        "unpublished genetic data",\n        "no clear genetic differentiation",\n        "not a formal taxonomic revision",\n    ),\n)\n'''
    marker = 's6 = appendix_texts["Appendix_S6_event_departures_human_context.md"]\n'
    if "Appendix S1 pooled taxonomic scope" not in text:
        if marker not in text:
            raise RuntimeError("Could not locate Supporting Information validator insertion point")
        text = text.replace(marker, insertion + "\n" + marker, 1)

    active_check = '''\nfor relative in (\n    "submission/jbi/JBI_main_manuscript_anonymized.md",\n    "submission/jbi/supporting/Appendix_S3_broad_environment_spatial_model.md",\n    "submission/jbi/JBI_background_architecture.md",\n    "submission/jbi/JBI_main_figure_plan.md",\n    "paper/README.md",\n    "paper/analysis-map.md",\n):\n    active_text = (REPO_ROOT / relative).read_text(encoding="utf-8")\n    if re.search(r"(?<![A-Za-z0-9])(?:F[_–—-]?ST|P[_–—-]?ST|Q[_–—-]?ST)(?![A-Za-z0-9])", active_text, flags=re.IGNORECASE):\n        fail(f"Active paper still contains a genetic-differentiation analogy: {relative}")\n'''
    marker2 = 'assert_anonymous("anonymized manuscript", text)\n'
    if "Active paper still contains a genetic-differentiation analogy" not in text:
        if marker2 not in text:
            raise RuntimeError("Could not locate active-paper validator insertion point")
        text = text.replace(marker2, marker2 + active_check, 1)
    write_if_changed(VALIDATOR, text)


def validate() -> None:
    s1 = S1.read_text(encoding="utf-8")
    required = (
        "ホタルブクロ",
        "ヤマホタルブクロ",
        "diagnostic morphological distinction is concentrated in the calyx",
        "Preliminary unpublished genetic data",
        "no clear genetic differentiation",
        "not a formal taxonomic revision",
    )
    missing = [token for token in required if token not in s1]
    if missing:
        raise RuntimeError("Appendix S1 taxonomic scope is incomplete: " + ", ".join(missing))

    offenders: list[str] = []
    for path in ACTIVE_FILES:
        if path.is_file() and GENETIC_ANALOGY.search(path.read_text(encoding="utf-8")):
            offenders.append(str(path.relative_to(ROOT)))
    if offenders:
        raise RuntimeError("Genetic-analogy wording remains in active paper: " + ", ".join(offenders))


def main() -> None:
    update_active_files()
    update_generator()
    update_validator()
    validate()
    print("PASS active spatial language is direct and Appendix S1 taxon pooling is locked")


if __name__ == "__main__":
    main()
