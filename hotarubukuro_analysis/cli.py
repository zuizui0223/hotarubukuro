"""Console entry points for the consolidated analysis package.

The historical module locations remain importable so existing scripts and the
manuscript-facing ``python run_pipeline.py`` command do not break. New installs
should enter through this package namespace.
"""

from __future__ import annotations


def paper_main() -> int | None:
    from run_pipeline import main

    return main()


def color_main() -> int | None:
    from source_build.extract_color import main

    return main()
