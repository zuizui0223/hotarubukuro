#!/usr/bin/env python3
"""Recalculate Campanula colour versus focal Bombus SDM; no environmental predictors.

This is an exploratory audit of frozen observation-level/cell-level datasets.
It does not test flower constancy, causal pollinator effects or fitness.
"""
import argparse
import csv
import json
from pathlib import Path

import numpy as np


def read_csv(path):
    with open(path, newline='', encoding='utf-8-sig') as f:
        return list(csv.DictReader(f))


def mean_ranks(x):
    x = np.asarray(x, dtype=float)
    order = np.argsort(x, kind='stable')
    sorted_x = x[order]
    cuts = np.r_[0, np.flatnonzero(sorted_x[1:] != sorted_x[:-1]) + 1, len(x)]
    ranks = np.empty(len(x), dtype=float)
    for first, end in zip(cuts[:-1], cuts[1:]):
        ranks[order[first:end]] = (first + 1 + end) / 2
    return ranks


def spearman(x, y):
    return float(np.corrcoef(mean_ranks(x), mean_ranks(y))[0, 1])


def signflip_upper_p(d, nperm=100000, seed=20260808):
    d = np.asarray(d, dtype=float)
    rng = np.random.default_rng(seed)
    target = d.mean()
    bigger = 0
    for offset in range(0, nperm, 2048):
        count = min(2048, nperm - offset)
        sims = (rng.choice([-1, 1], size=(count, len(d))) * d).mean(axis=1)
        bigger += int((sims >= target).sum())
    return (bigger + 1) / (nperm + 1)


def calculate(cells_csv, support_csv, pairs_csv, output_dir, nperm=100000):
    cells = read_csv(cells_csv)
    supports = read_csv(support_csv)
    pairs = read_csv(pairs_csv)
    if len(cells) != len(set(r['exact_site_id'] for r in cells)):
        raise ValueError('Duplicate cell IDs')
    if len(supports) != len(set(r['exact_site_id'] for r in supports)):
        raise ValueError('Duplicate SDM-support cell IDs')
    lookup = {r['exact_site_id']: r for r in supports}
    observed = []
    for row in cells:
        sid = row['exact_site_id']
        if sid not in lookup:
            raise ValueError('Missing SDM support for cell ' + sid)
        n = int(row['n_observations'])
        pig = int(row['n_pigmented'])
        white = int(row['n_white'])
        if n <= 0 or pig + white != n:
            raise ValueError('Invalid cell sample counts for ' + sid)
        score = float(lookup[sid]['effective_occmax'])
        raw = float(lookup[sid]['effective_rawmax'])
        if not (np.isfinite(score) and np.isfinite(raw)):
            raise ValueError('Non-finite focal SDM support for ' + sid)
        observed.append((sid, pig, white, n, score, raw))
    score = np.array([r[4] for r in observed])
    raw = np.array([r[5] for r in observed])
    prop = np.array([r[1]/r[3] for r in observed])
    cut = np.quantile(score, [.25, .5, .75])
    q = np.digitize(score, cut, right=True)
    groups = []
    for g in range(4):
        rows = [observed[i] for i in np.flatnonzero(q == g)]
        nflowers = sum(r[3] for r in rows)
        npig = sum(r[1] for r in rows)
        groups.append({
            'SDM_quartile_low_to_high': g+1,
            'n_cells': len(rows),
            'n_images': nflowers,
            'n_pigmented_images': npig,
            'pigmented_image_share': npig/nflowers if nflowers else float('nan'),
            'mean_cell_pigmented_share': float(prop[q==g].mean()) if rows else float('nan'),
            'sdm_median': float(np.median(score[q==g])) if rows else float('nan'),
        })
    pair_results = []
    for km in (5, 10, 25):
        target = [r for r in pairs if float(r['radius_km']) == km and
                  float(r['transition_threshold']) == 1]
        if not target:
            raise ValueError(f'Missing fixed sharp pairs at {km} km')
        for measure, col, seed in (('occurrence_referenced', 4, 20260808),
                                   ('raw_cloglog', 5, 20260809)):
            d = []
            for p in target:
                w, pig = p['white_site'], p['pigmented_site']
                if w not in lookup or pig not in lookup:
                    raise ValueError('Missing SDM support for selected pair')
                sdm_field = 'effective_occmax' if col == 4 else 'effective_rawmax'
                diff = float(lookup[pig][sdm_field]) - float(lookup[w][sdm_field])
                original = float(p['delta_effective_occmax' if col == 4 else 'delta_effective_rawmax'])
                if not np.isclose(diff, original, rtol=0, atol=1e-9):
                    raise ValueError('Pair SDM contrast disagrees with frozen output')
                d.append(diff)
            d = np.asarray(d)
            pair_results.append({
                'radius_km': km, 'SDM_scale': measure, 'n_pairs': len(d),
                'mean_pigmented_minus_white': float(d.mean()),
                'median_pigmented_minus_white': float(np.median(d)),
                'n_positive': int((d > 0).sum()),
                'positive_fraction': float((d > 0).mean()),
                'recalculated_signflip_upper_p': float(signflip_upper_p(d, nperm, seed)),
                'n_both_cells_have_2plus_images': sum(
                    int(p['white_n_observations']) >= 2 and
                    int(p['pigmented_n_observations']) >= 2 for p in target
                ),
                'n_either_cell_1_image': sum(
                    int(p['white_n_observations']) == 1 or
                    int(p['pigmented_n_observations']) == 1 for p in target
                ),
            })
    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)
    for filename, rows in [('sdm_only_quartiles.csv', groups),
                           ('sdm_only_pair_results.csv', pair_results)]:
        with open(out / filename, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
    summary = {
        'n_cells': len(observed),
        'n_flower_images': sum(r[3] for r in observed),
        'n_pairs_5km': len([p for p in pairs if float(p['radius_km']) == 5 and float(p['transition_threshold']) == 1]),
        'national_spearman_pigmented_fraction_vs_focal_sdm': spearman(prop, score),
        'national_spearman_pigmented_fraction_vs_raw_sdm': spearman(prop, raw),
        'environmental_covariates_used': False,
        'interpretation': 'Descriptive associations; SDMs inherit environmental geography and neither pollination mechanism nor frequency-dependent fitness is identified.',
    }
    (out / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n', encoding='utf-8')
    print(json.dumps(summary, indent=2))
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cells', required=True)
    parser.add_argument('--support', required=True)
    parser.add_argument('--pairs', required=True)
    parser.add_argument('--out', required=True)
    parser.add_argument('--nperm', type=int, default=100000)
    args = parser.parse_args()
    if args.nperm < 100:
        parser.error('nperm must be >= 100')
    calculate(args.cells, args.support, args.pairs, args.out, args.nperm)


if __name__ == '__main__':
    main()
