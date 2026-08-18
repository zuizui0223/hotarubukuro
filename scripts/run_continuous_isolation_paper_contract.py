#!/usr/bin/env python3
"""Run the continuous-isolation paper-contract synchroniser idempotently.

Some manuscript and validator edits were committed by the earlier prose-only
integration before the complete paper contract was introduced. The underlying
synchroniser intentionally raises when an expected legacy token is absent.
For this transition commit, an absent legacy token is acceptable only when its
exact replacement is already present; all genuinely unmatched edits still
fail loudly.
"""

from __future__ import annotations

import scripts.register_continuous_isolation_paper_contract as contract

_original_replace_once = contract.replace_once


def replace_once_if_needed(text: str, old: str, new: str, label: str) -> str:
    if old in text:
        return _original_replace_once(text, old, new, label)
    if new in text:
        return text
    raise RuntimeError(
        f"Neither legacy nor integrated form found for {label}; refusing silent drift"
    )


contract.replace_once = replace_once_if_needed
contract.main()
