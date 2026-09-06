#!/usr/bin/env python3
"""Validate the deterministic public table rebuilt from the frozen Zenodo source."""

from __future__ import annotations

import csv
import hashlib
import json
import math
from collections import Counter
from pathlib import Path
from typing import Any, Mapping

ROOT = Path(__file__).resolve().parents[1]
CONTRACT_PATH = ROOT / "reproducibility" / "source_contract.json"


class SourceContractError(RuntimeError):
    pass


def git_blob_sha(path: Path) -> str:
    data = path.read_bytes()
    return hashlib.sha1(f"blob {len(data)}\0".encode() + data).hexdigest()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def load_contract(path: Path = CONTRACT_PATH) -> dict[str, Any]:
    try:
        contract = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise SourceContractError(f"cannot read source contract {path}: {error}") from error
    required = {
        "zenodo_record",
        "zenodo_doi",
        "zenodo_filename",
        "zenodo_md5",
        "expected_rows",
        "expected_public_table_git_blob",
        "required_public_columns",
        "qc_status_counts",
        "manual_review_status_counts",
    }
    missing = sorted(required.difference(contract))
    if missing:
        raise SourceContractError("source contract missing keys: " + ", ".join(missing))
    return contract


def _read_table(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    if not path.is_file():
        raise SourceContractError(
            f"generated public table not found: {path}. "
            "Run `python source_build/reproduce_from_zenodo.py` first."
        )
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames is None:
            raise SourceContractError(f"generated public table has no header: {path}")
        header = [str(value).strip() for value in reader.fieldnames]
        rows = list(reader)
    return header, rows


def _count(rows: list[dict[str, str]], column: str) -> dict[str, int]:
    values = Counter((row.get(column) or "").strip() for row in rows)
    return dict(sorted(values.items()))


def validate_public_table(
    path: Path,
    contract: Mapping[str, Any] | None = None,
    *,
    require_exact_blob: bool = True,
) -> dict[str, Any]:
    contract = dict(contract or load_contract())
    header, rows = _read_table(path)
    header_set = set(header)
    required_columns = set(contract["required_public_columns"])
    missing_columns = sorted(required_columns.difference(header_set))
    if missing_columns:
        raise SourceContractError(
            "generated public table missing columns: " + ", ".join(missing_columns)
        )

    expected_rows = int(contract["expected_rows"])
    if len(rows) != expected_rows:
        raise SourceContractError(
            f"generated public table row count {len(rows)} != {expected_rows}"
        )

    identifiers: set[str] = set()
    for index, row in enumerate(rows, start=2):
        identifier = (row.get("observation_id") or "").strip()
        if not identifier:
            raise SourceContractError(f"empty observation_id at CSV row {index}")
        if identifier in identifiers:
            raise SourceContractError(f"duplicate observation_id: {identifier}")
        identifiers.add(identifier)
        for column in ("latitude", "longitude", "R", "G", "B"):
            try:
                value = float(row.get(column, ""))
            except (TypeError, ValueError) as error:
                raise SourceContractError(
                    f"non-numeric {column} for {identifier}: {row.get(column)!r}"
                ) from error
            if not math.isfinite(value):
                raise SourceContractError(f"non-finite {column} for {identifier}")
        latitude = float(row["latitude"])
        longitude = float(row["longitude"])
        if not (-90 <= latitude <= 90 and -180 <= longitude <= 180):
            raise SourceContractError(f"coordinates outside geographic bounds for {identifier}")
        for column in ("R", "G", "B"):
            value = float(row[column])
            if not 0 <= value <= 255:
                raise SourceContractError(f"{column} outside 0--255 for {identifier}")

    qc_counts = _count(rows, "qc_status")
    review_counts = _count(rows, "manual_review_status")
    expected_qc = {str(key): int(value) for key, value in contract["qc_status_counts"].items()}
    expected_review = {
        str(key): int(value) for key, value in contract["manual_review_status_counts"].items()
    }
    if qc_counts != expected_qc:
        raise SourceContractError(f"qc_status counts changed: {qc_counts} != {expected_qc}")
    if review_counts != expected_review:
        raise SourceContractError(
            f"manual_review_status counts changed: {review_counts} != {expected_review}"
        )

    observed_blob = git_blob_sha(path)
    expected_blob = str(contract["expected_public_table_git_blob"])
    exact = observed_blob == expected_blob
    if require_exact_blob and not exact:
        raise SourceContractError(
            "rebuilt public table does not match the frozen exact-output contract: "
            f"{observed_blob} != {expected_blob}"
        )

    return {
        "path": str(path),
        "rows": len(rows),
        "columns": header,
        "qc_status_counts": qc_counts,
        "manual_review_status_counts": review_counts,
        "git_blob": observed_blob,
        "expected_git_blob": expected_blob,
        "exact_public_contract": exact,
        "sha256": sha256(path),
    }
