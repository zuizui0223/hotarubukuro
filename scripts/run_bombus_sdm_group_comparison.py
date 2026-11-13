#!/usr/bin/env python3
"""SDM-only widespread-vs-montane Bombus comparison, with spatial-block audit.

Predictors: national ranks of five raw Bombus SDM scores, grouped into
(ardens, diversus) vs (beaticola, consobrinus, honshuensis).
Coordinates only define 50-km blocks for uncertainty and permutation;
NO environmental covariates enter the model. Associational, not causal.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import numpy as np
import pandas as pd
from scipy.stats import rankdata, norm

TAXA = ("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")


def read_and_rank(cell_file, region_file):
    cells = pd.read_csv(cell_file)
    needed = {"exact_site_id", "x_km", "y_km", "n_observations",
              "n_pigmented", "n_white", *(f"bee_{s}" for s in TAXA)}
    if not needed.issubset(cells.columns):
        raise ValueError(f"Missing input fields: {needed - set(cells.columns)}")
    if cells.exact_site_id.duplicated().any():
        raise ValueError("Duplicate 1-km cell identity")
    raw = cells[[f"bee_{s}" for s in TAXA]].to_numpy(float)
    if not np.isfinite(raw).all():
        raise ValueError("All five real SDM scores are required; do not impute")
    if not (cells.n_pigmented + cells.n_white == cells.n_observations).all():
        raise ValueError("Photograph counts inconsistent")
    if (cells.n_observations < 1).any():
        raise ValueError("Cell without photographed flowers")
    ranks = np.column_stack([
        rankdata(raw[:, j], method="average") / len(cells)
        for j in range(len(TAXA))
    ])
    # The rank scale matches the original central-Honshu regional model.
    cells["widespread10"] = ranks[:, :2].mean(axis=1) / 0.1
    cells["montane10"] = ranks[:, 2:].mean(axis=1) / 0.1
    region = set(Path(region_file).read_text(encoding="utf-8").split())
    if not region:
        raise ValueError("Empty fixed regional footprint")
    central = cells.loc[cells.exact_site_id.isin(region)].copy()
    if central.empty:
        raise ValueError("Footprint did not match flower-cell IDs")
    return cells, central, region


def grid_groups(df, width=50, shift=0):
    bins = np.floor((df[["x_km", "y_km"]].to_numpy(float) + shift) / width).astype(int)
    keys = [f"{x}_{y}" for x, y in bins]
    parts = {}
    for i, k in enumerate(keys):
        parts.setdefault(k, []).append(i)
    return keys, [np.asarray(v, dtype=int) for v in parts.values() if len(v) > 1]


def irls(X, outcome, max_iter=80):
    y = outcome[:, 0]
    n = outcome.sum(axis=1)
    beta = np.zeros(X.shape[1])
    beta[0] = np.log((y.sum() + 0.5) / (outcome[:, 1].sum() + 0.5))
    for _ in range(max_iter):
        p = 1 / (1 + np.exp(-np.clip(X @ beta, -20, 20)))
        hessian = (X.T * np.maximum(n * p * (1 - p), 1e-12)) @ X
        step = np.linalg.solve(hessian, X.T @ (y - n * p))
        beta += step
        if np.max(abs(step)) < 1e-9:
            break
    else:
        raise RuntimeError("Binomial IRLS did not converge")
    p = 1 / (1 + np.exp(-np.clip(X @ beta, -20, 20)))
    return beta, p


def cov_by_blocks(X, outcome, probability, labels):
    total = outcome.sum(axis=1)
    bread = np.linalg.inv((X.T * (total * probability * (1 - probability))) @ X)
    scores = {}
    for row, label in enumerate(labels):
        scores[label] = scores.get(label, np.zeros(X.shape[1])) + X[row] * (
            outcome[row, 0] - total[row] * probability[row]
        )
    meat = sum(np.outer(v, v) for v in scores.values())
    n, k = X.shape
    g = len(scores)
    return (bread @ meat @ bread) * g / (g - 1) * (n - 1) / (n - k)


def design(df, photo_weighted=True):
    X = np.column_stack((np.ones(len(df)), df.widespread10, df.montane10))
    if photo_weighted:
        Y = df[["n_pigmented", "n_white"]].to_numpy(dtype=float)
    else:
        share = df.n_pigmented.to_numpy(float) / df.n_observations.to_numpy(float)
        Y = np.column_stack((share, 1 - share))
    return X, Y


def fit_effects(df, name, photo_weighted=True):
    X, Y = design(df, photo_weighted)
    beta, probability = irls(X, Y)
    labels, _ = grid_groups(df)
    covariance = cov_by_blocks(X, Y, probability, labels)
    contrasts = {
        "widespread": np.array([0, 1, 0]),
        "montane": np.array([0, 0, 1]),
        "widespread_minus_montane": np.array([0, 1, -1]),
    }
    result = []
    for term, c in contrasts.items():
        v = float(c @ beta)
        se = float(np.sqrt(c @ covariance @ c))
        result.append({
            "scope": name, "weighting": "photo" if photo_weighted else "equal_cell",
            "term": term, "n_cells": len(df),
            "n_photos": int(df.n_observations.sum()), "n_blocks": len(set(labels)),
            "coefficient": v, "se_block_cluster": se,
            "odds_ratio": float(np.exp(v)),
            "ci95_low": float(np.exp(v - 1.96 * se)),
            "ci95_high": float(np.exp(v + 1.96 * se)),
            "wald_p": float(2 * norm.sf(abs(v / se))),
        })
    return result


def shuffle_test(df, scope, block_km=50, shift=0, reps=10000, seed=20261113):
    X, Y = design(df)
    observed, _ = irls(X, Y)
    target = observed[1] - observed[2]
    _, groups = grid_groups(df, block_km, shift)
    rng = np.random.default_rng(seed + block_km + shift)
    null = np.empty(reps)
    for repeat in range(reps):
        permutation = np.arange(len(df))
        for group in groups:
            permutation[group] = rng.permutation(group)
        b, _ = irls(X, Y[permutation])
        null[repeat] = b[1] - b[2]
    mid = float(np.median(null))
    return {
        "scope": scope, "block_km": block_km, "block_shift": shift,
        "permutations": reps, "n_perm_cells": sum(map(len, groups)),
        "observed_contrast": float(target), "null_median": mid,
        "null_025": float(np.quantile(null, .025)),
        "null_975": float(np.quantile(null, .975)),
        "two_sided_centered_p":
            (1 + int((abs(null - mid) >= abs(target - mid)).sum())) / (reps + 1),
        "lower_tail_p": (1 + int((null <= target).sum())) / (reps + 1),
    }


def analyze(cells_path, region_path, output, reps=10000, sensitivity=False):
    nationwide, central, ids = read_and_rank(cells_path, region_path)
    expected = (len(nationwide), int(nationwide.n_observations.sum()),
                len(central), int(central.n_observations.sum()))
    if expected != (1305, 1922, 583, 875):
        raise ValueError(f"Frozen analysis cohort mismatch: {expected}")
    effects, shuffles = [], []
    for label, df in (("national", nationwide), ("central_honshu", central)):
        for suffix, sample in (("all", df), ("n_ge_2", df[df.n_observations >= 2])):
            for weighted in (True, False):
                effects.extend(fit_effects(sample, label + "_" + suffix, weighted))
        schemes = [(50, 0, reps)]
        if sensitivity:
            schemes += [(25, 0, min(reps, 2500)), (50, 25, min(reps, 2500)),
                        (100, 0, min(reps, 2500)), (100, 50, min(reps, 2500))]
        for width, shift, n in schemes:
            shuffles.append(shuffle_test(df, label, width, shift, n))
    out = Path(output)
    out.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(effects).to_csv(out / "group_sdm_only_effects.csv", index=False)
    pd.DataFrame(shuffles).to_csv(out / "group_block_permutation.csv", index=False)
    (out / "scope.json").write_text(json.dumps({
        "national_cells": len(nationwide),
        "national_photos": int(nationwide.n_observations.sum()),
        "central_cells": len(central), "central_photos": int(central.n_observations.sum()),
        "central_footprint_ids": len(ids),
        "environmental_predictors_in_model": False,
        "predictors": [f"bee_{s}" for s in TAXA],
        "scale": "national ranks per species, group means per 0.1 rank",
        "limits": "SDM-derived geography is not direct pollination or selection"
    }, indent=2) + "\n", encoding="utf-8")
    return pd.DataFrame(effects), pd.DataFrame(shuffles)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cells", default="results/analysis_cells/analysis_cells_1km.csv")
    parser.add_argument("--region", default="config/central_honshu_cells.txt")
    parser.add_argument("--out", default="results/bombus_sdm_group_comparison")
    parser.add_argument("--nperm", type=int, default=10000)
    parser.add_argument("--sensitivity", action="store_true")
    args = parser.parse_args()
    effect, perm = analyze(args.cells, args.region, args.out, args.nperm, args.sensitivity)
    print(effect[(effect.scope.str.endswith("_all")) & (effect.weighting == "photo")].to_string(index=False))
    print(perm.to_string(index=False))


if __name__ == "__main__":
    main()
