"""Console entry points for the consolidated analysis package."""

from __future__ import annotations


def color_main() -> int | None:
    from source_build.extract_color import main

    return main()
