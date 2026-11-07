#!/usr/bin/env python3
"""Exploratory colour-clustering and focal-Bombus boundary tests, not PFDS.

CSV inputs are the public GitHub Actions artifact snapshots identified in
docs/bombus_constancy_executed_probe_2026-11-07.md. Requires numpy pandas scipy.
No network access, raw-image extraction, or model refit is performed.
"""
import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.spatial import cKDTree


def analyze(cells_path, pairs_path, output_dir, nperm=10000, seed=20261107):
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    cells = pd.read_csv(cells_path)
    raw_pairs = pd.read_csv(pairs_path)
    pairs = raw_pairs.loc[(raw_pairs.radius_km == 5) &
                          np.isclose(raw_pairs.transition_threshold, 1)].copy()
    if not cells.exact_site_id.is_unique or len(pairs) == 0:
        raise ValueError("Expected unique site IDs and nonempty 5-km pure pairs")
    pos = {site: i for i, site in enumerate(cells.exact_site_id)}
    if not (set(pairs.white_site) | set(pairs.pigmented_site)) <= set(pos):
        raise ValueError("Pair endpoints absent from cell table")
    xy = cells[["x_km", "y_km"]].to_numpy(dtype=float)
    label = cells.pigmented.to_numpy(dtype=bool)
    sample = cells.n_observations.to_numpy(dtype=int)
    pure = ((cells.n_pigmented == 0) | (cells.n_white == 0)).to_numpy()
    single = sample == 1
    if not np.array_equal(label, (cells.n_pigmented > 0).to_numpy()):
        raise ValueError("Pipeline label does not match any-pigmented indicator")
    edge = np.array(sorted(cKDTree(xy).query_pairs(5.000001)), dtype=int)
    if edge.size == 0:
        raise ValueError("No 5-km cell graph edges")
    edge_sets = {
        "all_cells": edge,
        "pure_only": edge[pure[edge].all(axis=1)],
        "both_single_photo": edge[single[edge].all(axis=1)],
    }
    observed = {
        key: float(np.mean(label[e[:, 0]] == label[e[:, 1]]))
        for key, e in edge_sets.items()
    }
    settings = [
        (25, 0, False), (50, 0, False), (50, 25, False),
        (100, 0, False), (100, 50, False),
        (50, 0, True), (100, 0, True),
    ]
    results = []
    for width, shift, effort_stratified in settings:
        tile = np.floor((xy + shift) / width).astype(int)
        groups = {}
        for i, (a, b) in enumerate(tile):
            key = ((int(a), int(b), int(single[i]))
                   if effort_stratified else (int(a), int(b)))
            groups.setdefault(key, []).append(i)
        indices = [np.array(v, dtype=int) for v in groups.values() if len(v) > 1]
        rng = np.random.default_rng(
            seed + width * 101 + shift * 3 + int(effort_stratified)
        )
        draws = {key: np.empty(nperm) for key in edge_sets}
        for k in range(nperm):
            perm = label.copy()
            for members in indices:
                perm[members] = perm[members][rng.permutation(len(members))]
            for key, e in edge_sets.items():
                draws[key][k] = (perm[e[:, 0]] == perm[e[:, 1]]).mean()
        for key, vals in draws.items():
            results.append({
                "block_km": width, "offset_km": shift,
                "effort_stratified": effort_stratified,
                "edge_set": key, "n_edges": len(edge_sets[key]),
                "observed_same_fraction": observed[key],
                "null_mean": float(vals.mean()),
                "excess": float(observed[key] - vals.mean()),
                "null_025": float(np.quantile(vals, 0.025)),
                "null_975": float(np.quantile(vals, 0.975)),
                "one_sided_p": float(
                    (1 + np.sum(vals >= observed[key])) / (nperm + 1)
                ),
                "n_active_cells": sum(len(v) for v in indices),
            })
    pd.DataFrame(results).to_csv(out / "block_permutations.csv", index=False)

    tree = cKDTree(xy)
    context = []
    for _, pair in pairs.iterrows():
        i, j = pos[pair.white_site], pos[pair.pigmented_site]
        near_white = [
            k for k in tree.query_ball_point(xy[i], 5.000001)
            if k not in (i, j)
        ]
        near_pigmented = [
            k for k in tree.query_ball_point(xy[j], 5.000001)
            if k not in (i, j)
        ]
        support_white = sum(
            pure[k] and not label[k] for k in near_white
        )
        support_pigmented = sum(
            pure[k] and label[k] for k in near_pigmented
        )
        context.append({
            "white_site": pair.white_site,
            "pigmented_site": pair.pigmented_site,
            "n_white_images": pair.white_n_observations,
            "n_pigmented_images": pair.pigmented_n_observations,
            "environment_distance_final8": pair.environmental_distance_final8,
            "white_same_colour_neighbours_5km": support_white,
            "pigmented_same_colour_neighbours_5km": support_pigmented,
            "both_sides_one_neighbour": int(
                support_white >= 1 and support_pigmented >= 1
            ),
            "both_sides_two_neighbours": int(
                support_white >= 2 and support_pigmented >= 2
            ),
            "delta_occurrence_referenced_focal_bombus":
                pair.delta_effective_occmax,
        })
    context = pd.DataFrame(context)
    context.to_csv(out / "boundary_context.csv", index=False)

    nwhite = pairs.white_n_observations.to_numpy()
    npigmented = pairs.pigmented_n_observations.to_numpy()
    subsets = {
        "all_67": np.ones(len(pairs), dtype=bool),
        "at_least_one_endpoint_replicated":
            np.maximum(nwhite, npigmented) >= 2,
        "both_endpoints_replicated":
            np.minimum(nwhite, npigmented) >= 2,
        "final8_environment_lower_half":
            pairs.environmental_distance_final8.to_numpy()
            <= pairs.environmental_distance_final8.median(),
        "elevation_difference_le_50m":
            pairs.absolute_elevation_difference.to_numpy() <= 50,
        "both_sides_have_5km_same_colour_neighbour":
            context.both_sides_one_neighbour.to_numpy() == 1,
    }
    rng = np.random.default_rng(seed)
    sensitivity = []
    for name, mask in subsets.items():
        d = pairs.loc[mask, "delta_effective_occmax"].to_numpy(dtype=float)
        if len(d) > 1:
            null = (rng.choice([-1, 1], size=(100000, len(d))) * d).mean(axis=1)
            pv = float((1 + np.sum(null >= d.mean())) / (len(null) + 1))
        else:
            pv = float("nan")
        sensitivity.append({
            "subset": name, "n_pairs": len(d),
            "mean_difference": float(d.mean()) if len(d) else np.nan,
            "median_difference": float(np.median(d)) if len(d) else np.nan,
            "fraction_positive": float((d > 0).mean()) if len(d) else np.nan,
            "exploratory_signflip_p": pv,
        })
    pd.DataFrame(sensitivity).to_csv(
        out / "bombus_subset_sensitivity.csv", index=False
    )
    # Field-check candidates only: both observed states at least twice within
    # one 1-km grid cell; these need not be the same biological population.
    mixed = cells.loc[(cells.n_pigmented >= 2) & (cells.n_white >= 2)].copy()
    mixed["min_colour_count"] = mixed[["n_pigmented", "n_white"]].min(axis=1)
    mixed["observed_pigmented_fraction"] = mixed.n_pigmented / mixed.n_observations
    mixed.sort_values(["min_colour_count", "n_observations"], ascending=False,
                      inplace=True)
    mixed[["exact_site_id", "longitude", "latitude", "n_observations",
           "n_pigmented", "n_white", "min_colour_count",
           "observed_pigmented_fraction"]].to_csv(
               out / "mixed_colour_1km_candidate_cells.csv", index=False)
    summary = {
        "n_cells": len(cells),
        "n_observations": int(sample.sum()),
        "n_single_photo_cells": int(single.sum()),
        "mixed_colour_1km_candidate_cells_with_two_each": len(mixed),
        "n_cells_with_both_colours_observed": int(
            ((cells.n_pigmented > 0) & (cells.n_white > 0)).sum()
        ),
        "n_sharp_selected_pairs_5km": len(pairs),
        "pairs_both_endpoints_single_photo": int(
            ((nwhite == 1) & (npigmented == 1)).sum()
        ),
        "pairs_either_endpoint_single_photo": int(
            ((nwhite == 1) | (npigmented == 1)).sum()
        ),
        "pairs_both_endpoints_replicated": int(
            ((nwhite >= 2) & (npigmented >= 2)).sum()
        ),
        "pairs_both_sides_supported_by_one_other_same_colour_cell_5km":
            int(context.both_sides_one_neighbour.sum()),
        "pairs_both_sides_supported_by_two_other_same_colour_cells_5km":
            int(context.both_sides_two_neighbours.sum()),
        "observed_same_colour_edge_fraction_5km":
            observed["all_cells"],
        "observed_same_colour_single_photo_edge_fraction_5km":
            observed["both_single_photo"],
        "claim_ceiling": (
            "Local colour co-clustering versus regional label randomization "
            "is not a test of flower constancy or frequency-dependent fitness."
        ),
    }
    (out / "summary.json").write_text(
        json.dumps(summary, indent=2) + "\n", encoding="utf-8"
    )
    print(json.dumps(summary, indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cells", required=True)
    parser.add_argument("--pairs", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--nperm", type=int, default=10000)
    parser.add_argument("--seed", type=int, default=20261107)
    args = parser.parse_args()
    if args.nperm < 100:
        parser.error("--nperm must be at least 100")
    analyze(args.cells, args.pairs, args.out, args.nperm, args.seed)


if __name__ == "__main__":
    main()
