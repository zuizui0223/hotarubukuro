#!/usr/bin/env python3
"""Run the continuous-isolation paper-contract synchroniser idempotently.

Some manuscript, validator and renderer edits were committed by the earlier
prose-only integration before the complete paper contract was introduced. The
underlying synchroniser intentionally raises when an expected legacy form is
absent. During this transition an absent legacy form is acceptable only when
the exact integrated form, or a uniquely identifying integrated sentinel, is
already present; all genuinely unmatched edits still fail loudly.
"""

from __future__ import annotations

import importlib.util
import json
import re
from pathlib import Path

module_path = Path(__file__).with_name(
    "register_continuous_isolation_paper_contract.py"
)
spec = importlib.util.spec_from_file_location(
    "continuous_isolation_paper_contract", module_path
)
if spec is None or spec.loader is None:
    raise RuntimeError(f"Cannot load paper-contract synchroniser: {module_path}")
contract = importlib.util.module_from_spec(spec)
spec.loader.exec_module(contract)

_original_replace_once = contract.replace_once
_original_replace_regex = contract.replace_regex


def replace_once_if_needed(text: str, old: str, new: str, label: str) -> str:
    if old in text:
        return _original_replace_once(text, old, new, label)
    if new in text:
        return text
    raise RuntimeError(
        f"Neither legacy nor integrated form found for {label}; refusing silent drift"
    )


def replace_regex_if_needed(
    text: str, pattern: str, replacement: str, label: str
) -> str:
    if re.search(pattern, text, flags=re.MULTILINE | re.DOTALL):
        return _original_replace_regex(text, pattern, replacement, label)
    if replacement.strip() in text:
        return text
    integrated_sentinels = {
        "Figure 4 renderer block":
            'source("scripts/render_jbi_figure4_continuous_isolation.R")',
    }
    sentinel = integrated_sentinels.get(label)
    if sentinel is not None and sentinel in text:
        return text
    raise RuntimeError(
        f"Neither legacy nor integrated form found for {label}; refusing silent drift"
    )


contract.replace_once = replace_once_if_needed
contract.replace_regex = replace_regex_if_needed
contract.main()

# Preserve the manuscript's existing, defensible prose while allowing the lock
# to recognise equivalent claim-ceiling and field-target wording. Then collapse
# the three generated checks to one row each so repeated integration is truly
# idempotent.
lock_path = contract.ROOT / "config/paper_pipeline.lock.json"
lock = json.loads(lock_path.read_text(encoding="utf-8"))
generated_labels = {
    "continuous colour-isolation human context",
    "supplementary event calibration and field targets",
    "paper overview mirrors continuous and event roles",
}
for check in lock["alignment"]["checks"]:
    label = check.get("label")
    if label == "continuous colour-isolation human context":
        check["patterns"] = [
            (
                r"(?:not as proof.*horticultural origin|"
                r"does not establish.*horticultural origin)"
                if pattern == "does not establish horticultural origin"
                else pattern
            )
            for pattern in check["patterns"]
        ]
    elif label == "supplementary event calibration and field targets":
        check["patterns"] = [
            (
                r"(?:field-target selector|field targets|"
                r"field/provenance targets)"
                if pattern == "field/provenance targets"
                else pattern
            )
            for pattern in check["patterns"]
        ]

deduplicated_checks = []
seen_generated_labels: set[str] = set()
for check in lock["alignment"]["checks"]:
    label = check.get("label")
    if label in generated_labels:
        if label in seen_generated_labels:
            continue
        seen_generated_labels.add(label)
    deduplicated_checks.append(check)
lock["alignment"]["checks"] = deduplicated_checks

# An output-producing workflow cannot know its eventual accepted artifact ID.
# Point the accepted continuous result only to files that actually record both
# the immutable ID and ZIP digest.
continuous_artifact = lock["artifacts"]["accepted_continuous_colour_isolation"]
continuous_artifact["references"] = [
    "reproducibility/continuous_colour_isolation_human_context_result_2026-08-18.md",
    "reproducibility/continuous_colour_isolation_manuscript_integration_2026-08-18.md",
]

lock_path.write_text(
    json.dumps(lock, ensure_ascii=False, separators=(",", ":")) + "\n",
    encoding="utf-8",
)

# The alignment validator requires every artifact reference to expose its ID
# and digest. The concise analysis map therefore ends with a generated registry
# rather than silently dropping the provenance table when its narrative is
# rewritten.
analysis_map_path = contract.ROOT / "paper/analysis-map.md"
analysis_map = analysis_map_path.read_text(encoding="utf-8")
analysis_map = re.sub(
    r"\n## Checksum-locked artifact registry\n.*\Z",
    "",
    analysis_map,
    flags=re.MULTILINE | re.DOTALL,
).rstrip()
registry_lines = [
    "",
    "## Checksum-locked artifact registry",
    "",
    "| Artifact key | GitHub Actions artifact ID | ZIP SHA-256 |",
    "|---|---:|---|",
]
for name, artifact in lock["artifacts"].items():
    registry_lines.append(
        f"| `{name}` | {artifact['id']} | `{artifact['sha256']}` |"
    )
analysis_map_path.write_text(
    analysis_map + "\n" + "\n".join(registry_lines) + "\n",
    encoding="utf-8",
)

# Keep the structured JBI abstract below the 300-word ceiling without dropping
# the post hoc status, the two guarded human-context estimates or the distinct
# roles of the continuous and event analyses.
abstract_text = """## Abstract

**Aim:** Geographical trait variation can reflect environment, population history, biotic interactions and human activity at different scales. We tested how these processes assemble flower-colour geography while separating pigment presence from pigmented-flower intensity.

**Location:** Japan.

**Taxon:** spotted bellflower (*Campanula punctata*).

**Methods:** We converted 1,922 author-screened YAMAP photographs into pigmentation state and pigmented-only intensity. INLA-SPDE models separated environmental associations from residual geography, and 67 independently fixed white-pigmented boundaries supplied the local bumblebee comparison. For human context, we measured distance to the nearest occurrence of the same colour in all 1,305 cells, scaled it by local flower-cell spacing and replayed the geometry on 10,000 natural predictive maps. This continuous analysis was post hoc; a threshold event detector was retained only for calibration and field targeting.

**Results:** Pigmentation state, but not pigmented-only intensity, exceeded a cross-fitted spatial expectation along environmental difference. Focal-bumblebee support was higher on pigmented sides of local boundaries, whereas stronger-looking highland overlap disappeared after elevation matching. Among 674 pigmented cells, isolation correlated with 5-km population exposure (rho=0.252) more strongly than natural maps expected (mean 0.133; P=0.0002). The relationship remained after local flower-cell spacing was removed (rho=0.285; natural mean 0.154; P=0.0009). The raw white relationship did not retain its opposite sign after density correction. Sixteen event-defined populations were not excessive under natural maps.

**Main conclusions:** Flower-colour geography contains distinct environmental, spatial, local biotic and contemporary human-context layers. Human context was expressed most clearly as an excess positive isolation-population relationship within pigmented occurrences, not as reciprocal colour displacement or proof of horticultural origin. Integrative trait biogeography gains resolution by connecting, rather than collapsing, processes operating at different scales and comparison units.

"""
main_path = contract.ROOT / "submission/jbi/JBI_main_manuscript_anonymized.md"
main_text = main_path.read_text(encoding="utf-8")
main_text, abstract_replacements = re.subn(
    r"## Abstract\n.*?(?=\*\*Keywords:\*\*)",
    abstract_text,
    main_text,
    count=1,
    flags=re.MULTILINE | re.DOTALL,
)
if abstract_replacements != 1:
    raise RuntimeError(
        f"Expected one structured abstract block; found {abstract_replacements}"
    )
main_path.write_text(main_text, encoding="utf-8")

abstract_word_count = len(re.findall(r"\b[\wÀ-ž*'’-]+\b", abstract_text))
if abstract_word_count > 300:
    raise RuntimeError(
        f"Integrated abstract exceeds JBI ceiling: {abstract_word_count} words"
    )
checklist_path = contract.ROOT / "submission/jbi/JBI_submission_checklist.md"
checklist = checklist_path.read_text(encoding="utf-8")
checklist = re.sub(
    r"Current abstract = \*\*\d+ words\*\* by repository validator\.",
    f"Current abstract = **{abstract_word_count} words** by repository validator.",
    checklist,
)
checklist_path.write_text(checklist, encoding="utf-8")
