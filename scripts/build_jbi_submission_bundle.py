#!/usr/bin/env python3
"""Safe entry point for the current JBI submission-bundle builder.

The core module owns document assembly. This entry point supplies the Markdown
inline renderer. Bold spans may contain scientific notation such as a*, L* and
C*, while the italic rule deliberately requires word boundaries so those
scientific suffixes are never mistaken for Markdown emphasis.
"""

from __future__ import annotations

import re

from docx.shared import Pt, RGBColor

import jbi_submission_bundle_core as core


TOKEN_PATTERN = re.compile(
    r"(\*\*[^\n]+?\*\*|`[^`\n]+`|\[[^\]]+\]\([^)]+\)|(?<![\w*])\*[^*\n]+?\*(?![\w*]))"
)


def _style_run(run, *, bold: bool, italic: bool) -> None:
    if bold:
        run.bold = True
    if italic:
        run.italic = True


def _append_inline(paragraph, text: str, *, bold: bool = False, italic: bool = False) -> None:
    position = 0
    for match in TOKEN_PATTERN.finditer(text):
        if match.start() > position:
            run = paragraph.add_run(text[position : match.start()])
            _style_run(run, bold=bold, italic=italic)
        token = match.group(0)
        if token.startswith("**") and token.endswith("**"):
            _append_inline(paragraph, token[2:-2], bold=True, italic=italic)
        elif token.startswith("`") and token.endswith("`"):
            run = paragraph.add_run(token[1:-1])
            _style_run(run, bold=bold, italic=italic)
            run.font.name = "Courier New"
            run.font.size = Pt(9)
        elif token.startswith("*") and token.endswith("*"):
            run = paragraph.add_run(token[1:-1])
            _style_run(run, bold=bold, italic=True)
        else:
            label, url = core.normalize_markdown_link(token)
            run = paragraph.add_run(label)
            _style_run(run, bold=bold, italic=italic)
            if url:
                suffix = paragraph.add_run(f" ({url})")
                _style_run(suffix, bold=bold, italic=italic)
                suffix.font.color.rgb = RGBColor(60, 90, 130)
        position = match.end()
    if position < len(text):
        run = paragraph.add_run(text[position:])
        _style_run(run, bold=bold, italic=italic)


def add_inline_runs(paragraph, text: str) -> None:
    _append_inline(paragraph, core.clean_markdown_text(text))


core.add_inline_runs = add_inline_runs


if __name__ == "__main__":
    raise SystemExit(core.main())
