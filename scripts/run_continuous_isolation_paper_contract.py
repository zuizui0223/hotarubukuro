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
