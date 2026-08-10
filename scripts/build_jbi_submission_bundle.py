#!/usr/bin/env python3
"""Safe entry point for the current JBI submission-bundle builder.

The core module owns document assembly. This entry point supplies the Markdown
inline renderer, whose italic rule deliberately excludes scientific notation
such as CIELAB a*, L* and C* while retaining explicit Markdown italics such as
*Campanula punctata*.
"""

from __future__ import annotations

import re

from docx.shared import Pt, RGBColor

import jbi_submission_bundle_core as core


def add_inline_runs(paragraph, text: str) -> None:
    text = core.clean_markdown_text(text)
    token_pattern = re.compile(
        r"(\*\*[^*\n]+?\*\*|`[^`\n]+`|\[[^\]]+\]\([^)]+\)|(?<![\w*])\*[^*\n]+?\*(?![\w*]))"
    )
    position = 0
    for match in token_pattern.finditer(text):
        if match.start() > position:
            paragraph.add_run(text[position : match.start()])
        token = match.group(0)
        if token.startswith("**") and token.endswith("**"):
            run = paragraph.add_run(token[2:-2])
            run.bold = True
        elif token.startswith("`") and token.endswith("`"):
            run = paragraph.add_run(token[1:-1])
            run.font.name = "Courier New"
            run.font.size = Pt(9)
        elif token.startswith("*") and token.endswith("*"):
            run = paragraph.add_run(token[1:-1])
            run.italic = True
        else:
            label, url = core.normalize_markdown_link(token)
            paragraph.add_run(label)
            if url:
                run = paragraph.add_run(f" ({url})")
                run.font.color.rgb = RGBColor(60, 90, 130)
        position = match.end()
    if position < len(text):
        paragraph.add_run(text[position:])


core.add_inline_runs = add_inline_runs


if __name__ == "__main__":
    raise SystemExit(core.main())
