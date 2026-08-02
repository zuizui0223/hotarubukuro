"""Recover the established analysis inputs from existing GitHub provenance.

The published phenotype and 1-km cell stages were never committed as
directories and no longer exist on the author's machine. What does still exist
is provenance that GitHub is holding: a canonical-analysis artifact, and two
published analysis-input snapshots.

This script retrieves what it can from those sources, verifies it by checksum
and by internal consistency, enumerates exactly what remains missing, and
assembles ``inputs/established_analysis_inputs/``.

It is deliberately gated. The phenotype analysis table is only accepted as the
established one when its row count and its class counts match the committed
expectations. A table that does not match is reported, never relabelled --
assembling a directory that claims to hold the published inputs when it does
not would be worse than assembling nothing.

Files that cannot be recovered are reported as unrecoverable. Nothing is
substituted from the reconstruction to fill a gap: a cross-fit log from the
1909-observation run does not describe the 1923-observation analysis, and
placing one beside the established table would manufacture a provenance that
never existed.
"""

from __future__ import annotations

import argparse
import csv
import dataclasses
import hashlib
import json
import shutil
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# What the established phenotype stage has to provide, and who needs it.
#
# Derived by reading the consumers, not by listing the directory: a file that
# nothing reads does not need recovering, and a file that a frozen audit reads
# does, whether or not it looks important.
# ---------------------------------------------------------------------------
REQUIRED_V11 = [
    ("analysis_data_pigmentation_hurdle.csv", "numerical", "stages 02-05, figures, validation/audit_phenotype.R"),
    ("pigmentation_measurement_summary.csv", "numerical", "R/final_registry.R result registry, validation/audit_phenotype.R"),
    ("pigmentation_mixture_components.csv", "numerical", "scripts/build_publication_figures.R"),
    ("pigmentation_hurdle_inla_fixed_effects.csv", "numerical", "scripts/build_publication_figures.R"),
    ("pigmentation_hurdle_inla_hyperparameters.csv", "numerical", "scripts/build_publication_figures.R"),
    ("pigmentation_hurdle_inla_model_comparison.csv", "audit_support", "validation/audit_phenotype.R check inla_complete"),
    ("pigmentation_presence_bombus_heldout.csv", "audit_support", "validation/audit_phenotype.R check primary_bombus_crossfit_warnings"),
    ("pigmented_intensity_bombus_heldout.csv", "audit_support", "validation/audit_phenotype.R check primary_bombus_crossfit_warnings"),
    ("pigmentation_presence_bombus_crossfit_log.csv", "audit_support", "validation/audit_phenotype.R check species_warning_isolated"),
    ("pigmentation_residual_tail_HR_coefficients.csv", "audit_support", "validation/audit_phenotype.R check residual_tail_warnings"),
    ("pigmentation_residual_tail_HR_heldout.csv", "audit_support", "validation/audit_phenotype.R check residual_tail_warnings"),
]

PHENOTYPE_TABLE = "analysis_data_pigmentation_hurdle.csv"
CELL_TABLE = "multiscale_hotspot_cells_1km.csv"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def read_csv_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


@dataclasses.dataclass
class Source:
    """A place a recovered file may legitimately come from."""

    name: str
    root: Path
    eligible: bool
    note: str

    def find(self, filename: str) -> Path | None:
        if not self.root.exists():
            return None
        for candidate in self.root.rglob(filename):
            if candidate.is_file():
                return candidate
        return None


def describe_phenotype_table(path: Path) -> dict[str, object]:
    rows = read_csv_rows(path)
    classes: dict[str, int] = {}
    for row in rows:
        value = (row.get("pigmented_mixture50") or "").strip()
        classes[value] = classes.get(value, 0) + 1
    return {
        "path": str(path),
        "sha256": sha256(path),
        "bytes": path.stat().st_size,
        "rows": len(rows),
        "white": classes.get("0", 0),
        "pigmented": classes.get("1", 0),
        "classes": classes,
    }


def load_expectations(path: Path) -> dict[str, float]:
    return {
        row["quantity"]: float(row["expectation"]) for row in read_csv_rows(path)
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--artifact-dir", required=True,
                        help="Extracted canonical-analysis artifact")
    parser.add_argument("--snapshot-v1", required=True,
                        help="Extracted analysis-input-snapshot-v1")
    parser.add_argument("--snapshot-v2", default="",
                        help="Extracted analysis-input-snapshot-v2 (reconstruction; searched but never used)")
    parser.add_argument("--repo-results", default="results",
                        help="Committed result files in the working tree")
    parser.add_argument("--expectations", default="inputs/established_input_expectations.csv")
    parser.add_argument("--expected-phenotype-sha256", default="",
                        help="Recorded SHA-256 of the established phenotype table")
    parser.add_argument("--out", default="inputs/established_analysis_inputs")
    parser.add_argument("--report-dir", default="reproducibility")
    arguments = parser.parse_args()

    artifact = Path(arguments.artifact_dir)
    snapshot_v1 = Path(arguments.snapshot_v1)
    snapshot_v2 = Path(arguments.snapshot_v2) if arguments.snapshot_v2 else None
    expectations = load_expectations(Path(arguments.expectations))
    report_dir = Path(arguments.report_dir)
    report_dir.mkdir(parents=True, exist_ok=True)

    log: list[str] = []

    def say(text: str = "") -> None:
        print(text)
        log.append(text)

    say("# Established-input recovery")
    say()

    # ------------------------------------------------------------------
    # 1. The phenotype analysis table, and whether it is the established one.
    # ------------------------------------------------------------------
    say("## Phenotype analysis table")
    say()
    candidates: list[tuple[str, Path]] = []
    for label, root in (
        ("analysis-input-snapshot-v1", snapshot_v1),
        ("canonical-analysis artifact", artifact),
        ("analysis-input-snapshot-v2 (reconstruction)", snapshot_v2),
    ):
        if root is None:
            continue
        for found in sorted(root.rglob(PHENOTYPE_TABLE)):
            candidates.append((label, found))

    if not candidates:
        say("No phenotype analysis table was found in any retrieved source.")
        Path(report_dir / "established_input_recovery.md").write_text(
            "\n".join(log) + "\n", encoding="utf-8"
        )
        return 1

    described = []
    for label, path in candidates:
        detail = describe_phenotype_table(path)
        detail["source"] = label
        described.append(detail)
        say(f"- **{label}** `{path}`")
        say(f"  - sha256 `{detail['sha256']}`")
        say(f"  - {detail['bytes']} bytes, {detail['rows']} rows, "
            f"{detail['white']} white-like, {detail['pigmented']} pigmented")
    say()

    expected_rows = expectations.get("analysis_observations")
    expected_white = expectations.get("white_observations")
    expected_pigmented = expectations.get("pigmented_observations")

    established = None
    for detail in described:
        matches = (
            detail["rows"] == expected_rows
            and detail["white"] == expected_white
            and detail["pigmented"] == expected_pigmented
        )
        if matches:
            established = detail
            break

    if established is None:
        say("### Verdict: the established phenotype table was NOT recovered")
        say()
        say(f"Every retrieved candidate differs from the published analysis "
            f"({expected_rows:.0f} observations, {expected_white:.0f} white-like, "
            f"{expected_pigmented:.0f} pigmented).")
        say()
        say("Nothing has been assembled. The retrieved tables are outputs of the "
            "reconstruction from `Data_S1.csv` and the public rasters, not the "
            "tables the manuscript was computed from, and labelling one of them "
            "as established would put a false provenance into the snapshot.")
        Path(report_dir / "established_input_recovery.md").write_text(
            "\n".join(log) + "\n", encoding="utf-8"
        )
        return 2

    if arguments.expected_phenotype_sha256 and \
            established["sha256"] != arguments.expected_phenotype_sha256:
        say("### Verdict: checksum disagreement")
        say()
        say(f"A table matching the published counts was found at "
            f"`{established['path']}`, but its SHA-256 is "
            f"`{established['sha256']}` rather than the recorded "
            f"`{arguments.expected_phenotype_sha256}`. Nothing has been "
            f"assembled; the discrepancy needs resolving first.")
        Path(report_dir / "established_input_recovery.md").write_text(
            "\n".join(log) + "\n", encoding="utf-8"
        )
        return 3

    say("### Verdict: the established phenotype table was recovered")
    say()
    say(f"`{established['source']}` carries a table with "
        f"{established['rows']} observations, {established['white']} white-like "
        f"and {established['pigmented']} pigmented, matching the published "
        f"analysis, at sha256 `{established['sha256']}`.")
    say()

    # ------------------------------------------------------------------
    # 2. Assemble what is recoverable.
    # ------------------------------------------------------------------
    sources = [
        Source("committed in the repository", Path(arguments.repo_results),
               True, "version-controlled output of the published run"),
        Source("analysis-input-snapshot-v1", snapshot_v1, True,
               "published Release asset, checksummed in its descriptor"),
        Source("canonical-analysis artifact", artifact, True,
               "Actions artifact retained from the run that consumed snapshot v1"),
    ]
    if snapshot_v2 is not None:
        sources.append(Source(
            "analysis-input-snapshot-v2", snapshot_v2, False,
            "reconstruction output; describes a different analysis population "
            "and must not stand in for an established file",
        ))

    out = Path(arguments.out)
    v11_out = out / "ecological_v11_pigmentation_hurdle"
    v15_out = out / "ecological_v15_multiscale_hotspots"
    v11_out.mkdir(parents=True, exist_ok=True)
    v15_out.mkdir(parents=True, exist_ok=True)

    established_path = Path(established["path"])
    shutil.copy2(established_path, v11_out / PHENOTYPE_TABLE)

    recovery_rows = []
    say("## Phenotype stage files")
    say()
    say("| file | class | required by | recovered from | status |")
    say("|---|---|---|---|---|")
    for filename, file_class, consumer in REQUIRED_V11:
        if filename == PHENOTYPE_TABLE:
            recovered_from = established["source"]
            status = "recovered"
        else:
            recovered_from = ""
            status = "unrecoverable"
            for source in sources:
                found = source.find(filename)
                if found is None:
                    continue
                if not source.eligible:
                    recovered_from = source.name
                    status = "found only in an ineligible source"
                    break
                shutil.copy2(found, v11_out / filename)
                recovered_from = source.name
                status = "recovered"
                break
        say(f"| `{filename}` | {file_class} | {consumer} | "
            f"{recovered_from or '-'} | {status} |")
        recovery_rows.append({
            "file": filename,
            "file_class": file_class,
            "required_by": consumer,
            "recovered_from": recovered_from,
            "status": status,
        })
    say()

    # ------------------------------------------------------------------
    # 3. The 1-km cell stage, recovered whole from the artifact.
    # ------------------------------------------------------------------
    say("## 1-km cell stage")
    say()
    cell_dirs = [
        path for path in artifact.rglob("ecological_v15_multiscale_hotspots")
        if path.is_dir()
    ]
    if not cell_dirs:
        say("The artifact does not contain `ecological_v15_multiscale_hotspots`.")
        recovery_rows.append({
            "file": "ecological_v15_multiscale_hotspots/",
            "file_class": "numerical",
            "required_by": "stages 02-05",
            "recovered_from": "",
            "status": "unrecoverable",
        })
    else:
        cell_dir = cell_dirs[0]
        copied = 0
        for path in sorted(cell_dir.rglob("*")):
            if not path.is_file():
                continue
            destination = v15_out / path.relative_to(cell_dir)
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, destination)
            copied += 1
        say(f"Recovered {copied} files from `{cell_dir}`.")
        cell_table = v15_out / CELL_TABLE
        if cell_table.exists():
            rows = read_csv_rows(cell_table)
            total = 0
            for row in rows:
                try:
                    total += int(float(row.get("n_observations") or 0))
                except ValueError:
                    pass
            say(f"`{CELL_TABLE}`: {len(rows)} cells, {total} observations, "
                f"sha256 `{sha256(cell_table)}`.")
            if expected_rows is not None and total != expected_rows:
                say()
                say(f"> The cell table accounts for {total} observations but the "
                    f"established phenotype table has {expected_rows:.0f}. These "
                    f"two stages do not describe the same analysis population.")
        recovery_rows.append({
            "file": "ecological_v15_multiscale_hotspots/",
            "file_class": "numerical",
            "required_by": "stages 02-05",
            "recovered_from": "canonical-analysis artifact",
            "status": f"recovered ({copied} files)",
        })
    say()

    # ------------------------------------------------------------------
    # 4. Record.
    # ------------------------------------------------------------------
    unrecoverable = [
        row for row in recovery_rows if not row["status"].startswith("recovered")
    ]
    say("## Outcome")
    say()
    say(f"{len(recovery_rows) - len(unrecoverable)} of {len(recovery_rows)} "
        f"required items recovered.")
    if unrecoverable:
        say()
        say("Not recovered:")
        say()
        for row in unrecoverable:
            say(f"- `{row['file']}` ({row['file_class']}) — {row['required_by']}")
        say()
        numerical_gaps = [
            row for row in unrecoverable if row["file_class"] == "numerical"
        ]
        if numerical_gaps:
            say("At least one of these is a **numerical** input rather than an "
                "audit-support record, so the published analysis cannot be "
                "reproduced from what remains.")
        else:
            say("All of these are **audit-support** records — cross-fit logs and "
                "model-comparison tables written by the original stage-01 "
                "execution. They carry no quantity that enters a published "
                "result; they exist so the frozen audit can confirm that the "
                "stage ran without warnings. That execution's logs were not "
                "archived, and they cannot be regenerated without rerunning "
                "stage 01, which would produce a different analysis population.")

    with (report_dir / "established_input_recovery.csv").open(
        "w", newline="", encoding="utf-8"
    ) as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["file", "file_class", "required_by", "recovered_from",
                        "status"],
        )
        writer.writeheader()
        writer.writerows(recovery_rows)

    provenance = {
        "phenotype_table": {
            "source": established["source"],
            "sha256": established["sha256"],
            "bytes": established["bytes"],
            "rows": established["rows"],
            "white": established["white"],
            "pigmented": established["pigmented"],
        },
        "recovered": recovery_rows,
    }
    (out / "RECOVERY.json").write_text(
        json.dumps(provenance, indent=2) + "\n", encoding="utf-8"
    )
    (report_dir / "established_input_recovery.md").write_text(
        "\n".join(log) + "\n", encoding="utf-8"
    )
    (out / "RECOVERY.md").write_text("\n".join(log) + "\n", encoding="utf-8")

    return 0


if __name__ == "__main__":
    sys.exit(main())
