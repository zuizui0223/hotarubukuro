#!/usr/bin/env python3
"""Backward-compatible entry point for the versioned colour extractor.

The implementation lives in :mod:`scripts.extract_color_v2`.  Keeping this
small wrapper means existing users can run ``python Code_S2.py --help`` while
the reusable functions remain importable by the test suite.
"""

from scripts.extract_color_v2 import main


if __name__ == "__main__":
    raise SystemExit(main())
