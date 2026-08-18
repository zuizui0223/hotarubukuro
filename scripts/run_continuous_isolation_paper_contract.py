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
