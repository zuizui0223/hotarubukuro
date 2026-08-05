"""Verify the surviving published artifacts against the publication lock.

The stage directories behind the manuscript are gone, but the publication lock
itself is committed, and so are the summary artifacts it names.
``results/final_analysis_pipeline/final_input_checksums.csv`` records an MD5 for
each of them, written by the original run.

Checking those MD5s is the strongest provenance still available: it establishes
that the committed summaries are byte-for-byte what the published run produced,
independently of anything reconstructed afterwards.

One wrinkle has to be handled honestly rather than worked around. The published
run was on Windows and wrote CRLF line endings; git normalised them to LF on
commit. So a committed file whose LF bytes hash differently may still be the
original. The check therefore tries both normalisations and records which one
matched, rather than silently accepting either.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import sys
from pathlib import Path


def md5(data: bytes) -> str:
    return hashlib.md5(data).hexdigest()


def verify(lock_path: Path, root: Path) -> list[dict[str, str]]:
    with lock_path.open("r", newline="", encoding="utf-8-sig") as handle:
        entries = list(csv.DictReader(handle))

    rows = []
    for entry in entries:
        artifact = entry["artifact"]
        expected = entry["md5"]
        path = root / artifact
        if not path.exists():
            rows.append({
                "artifact": artifact,
                "recorded_md5": expected,
                "observed_md5": "",
                "line_endings": "",
                "status": "MISSING",
            })
            continue
        raw = path.read_bytes()
        as_is = md5(raw)
        as_crlf = md5(raw.replace(b"\n", b"\r\n"))
        if as_is == expected:
            status, endings, observed = "PASS", "LF", as_is
        elif as_crlf == expected:
            status, endings, observed = "PASS", "CRLF (git-normalised on commit)", as_crlf
        else:
            status, endings, observed = "MISMATCH", "", as_is
        rows.append({
            "artifact": artifact,
            "recorded_md5": expected,
            "observed_md5": observed,
            "line_endings": endings,
            "status": status,
        })
    return rows


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--lock",
        default="results/final_analysis_pipeline/final_input_checksums.csv",
    )
    parser.add_argument("--root", default=".")
    parser.add_argument("--report-dir", default="reproducibility")
    arguments = parser.parse_args()

    lock_path = Path(arguments.lock)
    if not lock_path.exists():
        print(f"Publication lock not found: {lock_path}", file=sys.stderr)
        return 1

    rows = verify(lock_path, Path(arguments.root))
    report_dir = Path(arguments.report_dir)
    report_dir.mkdir(parents=True, exist_ok=True)
    output = report_dir / "locked_artifact_verification.csv"
    with output.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["artifact", "recorded_md5", "observed_md5",
                        "line_endings", "status"],
        )
        writer.writeheader()
        writer.writerows(rows)

    width = max(len(row["artifact"]) for row in rows)
    for row in rows:
        print(f"{row['status']:9s} {row['artifact']:{width}s}  {row['line_endings']}")

    failures = [row for row in rows if row["status"] != "PASS"]
    print(f"\n{len(rows) - len(failures)} of {len(rows)} locked artifacts verify "
          f"against the publication lock.")
    if failures:
        print("\nNot verified:", file=sys.stderr)
        for row in failures:
            print(f"  {row['artifact']}: {row['status']}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
