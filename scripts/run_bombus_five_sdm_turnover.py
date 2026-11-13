#!/usr/bin/env python3
"""SDM-only five-bumblebee guild turnover versus photographed colour divergence.

This is a *correspondence* test on five habitat-suitability surfaces, not a
test of visitation, flower constancy, frequency-dependent fitness, or causality.
No temperature, elevation, soil or other environmental predictor is loaded.
Coordinates determine fixed neighbour edges and region-permutation blocks.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.spatial import cKDTree
from scipy.stats import rankdata

SPECIES = ("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
SCORE_COLS = tuple(f"{s}_occurrence_reference" for s in SPECIES)


def rank_correlation(x: np.ndarray, y: np.ndarray) -> float:
    """Pearson correlation of tied mean ranks (Spearman's rho)."""
    a = rankdata(np.asarray(x, dtype=float))
    b = rankdata(np.asarray(y, dtype=float))
    a -= a.mean()
    b -= b.mean()
    denom = np.linalg.norm(a) * np.linalg.norm(b)
    return float(a @ b / denom) if denom > 0 else float("nan")


def load_inputs(cell_csv, sdm_csv):
    colour = pd.read_csv(cell_csv)
    sdm = pd.read_csv(sdm_csv)
    for frame, required in (
        (colour, ("exact_site_id", "x_km", "y_km", "n_observations", "n_pigmented")),
        (sdm, ("exact_site_id", *SCORE_COLS)),
    ):
        if not set(required).issubset(frame.columns):
            raise ValueError("Missing required data columns: " + repr(set(required)-set(frame.columns)))
        if not frame["exact_site_id"].is_unique:
            raise ValueError("Duplicate site ID")
    data = colour.merge(sdm[["exact_site_id", *SCORE_COLS]],
                        on="exact_site_id", validate="one_to_one", how="left")
    if len(data) != len(colour):
        raise ValueError("Site join changed number of cells")
    scores = data[list(SCORE_COLS)].to_numpy(dtype=float)
    if not np.isfinite(scores).all():
        raise ValueError("Five-species SDM coverage incomplete: do not replace missing values with zero")
    if (scores < 0).any() or (scores > 1).any():
        raise ValueError("Expected occurrence-referenced scores within [0,1]")
    valid = scores.sum(axis=1) > 1e-12
    profile = np.full_like(scores, np.nan)
    profile[valid] = scores[valid] / scores[valid].sum(axis=1)[:, None]
    count = data["n_observations"].to_numpy(dtype=int)
    pigmented = data["n_pigmented"].to_numpy(dtype=int)
    if (count <= 0).any() or (pigmented < 0).any() or (pigmented > count).any():
        raise ValueError("Invalid photographed flower counts")
    coords = data[["x_km", "y_km"]].to_numpy(dtype=float)
    if not np.isfinite(coords).all():
        raise ValueError("Missing coordinates for spatial graph")
    return data, profile, valid, coords, count, pigmented


def test_five_species(cell_csv, sdm_csv, out, radii=(5, 10, 25),
                      block_km=50, nperm=10000, seed=20261107,
                      block_shift_km=0):
    """Run the frozen primary and secondary-window tests, no abiotic covariates."""
    if nperm < 100 or block_km <= 0 or any(r <= 0 for r in radii):
        raise ValueError("Invalid permutation count or spatial window")
    data, profile, valid, coords, count, pig = load_inputs(cell_csv, sdm_csv)
    sqrt_profile = np.sqrt(profile)
    coords_for_blocks = coords + block_shift_km
    region = np.floor(coords_for_blocks / block_km).astype(int)
    groups = {}
    for idx, (i, j) in enumerate(region):
        groups.setdefault((int(i), int(j)), []).append(idx)
    groups = [np.array(g, dtype=int) for g in groups.values() if len(g) > 1]
    tree = cKDTree(coords)
    rows = []
    for radius in radii:
        edges = np.array(sorted(tree.query_pairs(radius)), dtype=int).reshape(-1, 2)
        if not len(edges):
            raise ValueError(f"No flower-cell edges within {radius} km")
        n_original = len(edges)
        edges = edges[valid[edges].all(axis=1)]
        if not len(edges):
            raise ValueError("All pair profiles are undefined")
        i, j = edges.T
        bee_difference = np.linalg.norm(sqrt_profile[i] - sqrt_profile[j], axis=1) / np.sqrt(2)
        response = abs(pig[i] / count[i] - pig[j] / count[j])
        replicated = (count[i] >= 2) & (count[j] >= 2)
        rho = rank_correlation(bee_difference, response)
        rho_rep = (rank_correlation(bee_difference[replicated], response[replicated])
                   if replicated.sum() >= 3 else float("nan"))
        rng = np.random.default_rng(seed + radius * 117 + block_km * 13)
        simulated = []
        perm = np.arange(len(data))
        for _ in range(nperm):
            perm[:] = np.arange(len(data))
            for indices in groups:
                perm[indices] = indices[rng.permutation(len(indices))]
            permuted_response = pig[perm] / count[perm]
            stat = rank_correlation(bee_difference, abs(permuted_response[i] -
                                                      permuted_response[j]))
            if np.isfinite(stat):
                simulated.append(stat)
        simulated = np.asarray(simulated)
        rows.append({
            "radius_km": radius,
            "n_species": len(SPECIES),
            "n_cells": len(data),
            "n_edges": len(edges),
            "excluded_undefined_profile_edges": n_original - len(edges),
            "undefined_profile_cells": int((~valid).sum()),
            "n_edges_both_cells_n_ge_2": int(replicated.sum()),
            "spearman_rho": rho,
            "rho_both_cells_n_ge_2": rho_rep,
            "null_mean_rho": float(simulated.mean()),
            "null_025": float(np.quantile(simulated, 0.025)),
            "null_975": float(np.quantile(simulated, 0.975)),
            "upper_tail_p": (1 + int((simulated >= rho).sum())) / (1 + len(simulated)),
            "n_permutations": int(len(simulated)),
            "null_block_km": block_km,
            "null_shift_km": block_shift_km,
        })
    out = Path(out)
    out.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(out / "guild_colour_turnover.csv", index=False)
    (out / "run_scope.json").write_text(json.dumps({
        "source_cells": str(cell_csv), "source_sdm": str(sdm_csv),
        "species": list(SPECIES), "SDM_only_explanatory_information": True,
        "environmental_predictors_in_test": False,
        "spatial_coordinates_role": "neighbor graph and blockwise label randomization",
        "interpretation": "Observed spatial co-structure only; SDMs are environment-derived, and frequency-dependent selection is not tested."
    }, indent=2) + "\n", encoding="utf-8")
    return pd.DataFrame(rows)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--cells", default="results/analysis_cells/analysis_cells_1km.csv")
    ap.add_argument("--sdm", default="results/bombus_occurrence_reference_support/cell_effective_bombus_support.csv")
    ap.add_argument("--out", default="results/bombus_five_sdm_turnover")
    ap.add_argument("--radii", default="5,10,25")
    ap.add_argument("--block-km", type=int, default=50)
    ap.add_argument("--block-shift-km", type=int, default=0)
    ap.add_argument("--nperm", type=int, default=10000)
    ap.add_argument("--seed", type=int, default=20261107)
    args = ap.parse_args()
    result = test_five_species(args.cells, args.sdm, args.out,
                               radii=tuple(int(v) for v in args.radii.split(",")),
                               block_km=args.block_km, nperm=args.nperm,
                               seed=args.seed, block_shift_km=args.block_shift_km)
    print(result.to_string(index=False))


if __name__ == "__main__":
    main()
