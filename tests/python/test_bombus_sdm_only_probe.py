"""Synthetic contract tests for the standalone SDM-only Bombus probe."""
import csv
import importlib.util
from pathlib import Path

import numpy as np


def load_probe():
    root = Path(__file__).resolve().parents[2]
    path = root / "scripts" / "check_bombus_sdm_only.py"
    spec = importlib.util.spec_from_file_location("bombus_sdm_only", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_rows(path, rows):
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def test_mean_ranks_and_negative_rank_relationship():
    probe = load_probe()
    assert np.allclose(probe.mean_ranks([1, 2, 2, 4]), [1, 2.5, 2.5, 4])
    assert np.isclose(probe.spearman([0, 1, 2, 3], [3, 2, 1, 0]), -1)


def test_only_focal_sdm_is_used_as_predictor(tmp_path):
    probe = load_probe()
    cells = [
        {"exact_site_id": "a", "n_observations": 1, "n_pigmented": 0,
         "n_white": 1, "temperature": "NOT_A_NUMBER"},
        {"exact_site_id": "b", "n_observations": 2, "n_pigmented": 2,
         "n_white": 0, "temperature": "NOT_A_NUMBER"},
        {"exact_site_id": "c", "n_observations": 2, "n_pigmented": 0,
         "n_white": 2, "temperature": "NOT_A_NUMBER"},
        {"exact_site_id": "d", "n_observations": 3, "n_pigmented": 2,
         "n_white": 1, "temperature": "NOT_A_NUMBER"},
    ]
    sdm = [
        {"exact_site_id": label, "effective_occmax": occ,
         "effective_rawmax": raw}
        for label, occ, raw in [("a", 0.1, 0.2), ("b", 0.8, 0.9),
                                ("c", 0.3, 0.4), ("d", 0.6, 0.7)]
    ]
    pairs = []
    for radius in (5, 10, 25):
        pairs.append({
            "radius_km": radius, "transition_threshold": 1,
            "white_site": "a", "pigmented_site": "b",
            "white_n_observations": 1, "pigmented_n_observations": 2,
            "delta_effective_occmax": 0.7, "delta_effective_rawmax": 0.7,
            "environmental_distance": "NOT_A_NUMBER",
            "absolute_elevation_difference": "NOT_A_NUMBER",
        })
    c, s, p = (tmp_path / f for f in ("cells.csv", "sdm.csv", "pairs.csv"))
    for path, rows in [(c, cells), (s, sdm), (p, pairs)]:
        write_rows(path, rows)
    result = probe.calculate(c, s, p, tmp_path / "out", nperm=100)
    assert result["environmental_covariates_used"] is False
    assert result["n_flower_images"] == 8
    assert result["n_pairs_5km"] == 1
    assert (tmp_path / "out" / "sdm_only_pair_results.csv").exists()


def test_sdm_difference_must_match_frozen_input(tmp_path):
    probe = load_probe()
    cells = [
        {"exact_site_id": i, "n_observations": 1, "n_pigmented": int(i == "b"),
         "n_white": int(i == "a")}
        for i in ("a", "b")
    ]
    supports = [
        {"exact_site_id": "a", "effective_occmax": 0.1, "effective_rawmax": 0.2},
        {"exact_site_id": "b", "effective_occmax": 0.9, "effective_rawmax": 0.8},
    ]
    pairs = [
        {"radius_km": r, "transition_threshold": 1, "white_site": "a",
         "pigmented_site": "b", "white_n_observations": 1,
         "pigmented_n_observations": 1, "delta_effective_occmax": 0.5,
         "delta_effective_rawmax": 0.6}
        for r in (5, 10, 25)
    ]
    c, s, p = (tmp_path / f for f in ("cells.csv", "sdm.csv", "pairs.csv"))
    for path, rows in [(c, cells), (s, supports), (p, pairs)]:
        write_rows(path, rows)
    try:
        probe.calculate(c, s, p, tmp_path / "out", nperm=100)
    except ValueError as exc:
        assert "disagrees with frozen output" in str(exc)
    else:
        raise AssertionError("Frozen SDM-contrast mismatch should fail")
