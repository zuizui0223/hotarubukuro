"""Smoke and invariance tests for the standalone five-species SDM-only audit."""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd
import pytest


def module():
    path = Path(__file__).resolve().parents[2] / "scripts/run_bombus_five_sdm_turnover.py"
    spec = importlib.util.spec_from_file_location("five_bombus_sdm", path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def test_rank_correlation():
    check = module().rank_correlation
    assert np.isclose(check([1, 2, 2, 4], [4, 3, 3, 1]), -1)
    assert np.isnan(check([1, 1, 1], [1, 2, 3]))


def test_five_species_and_no_abiotic_predictor(tmp_path):
    code = module()
    ids = ["a", "b", "c", "d", "e"]
    cells = pd.DataFrame({
        "exact_site_id": ids, "x_km": [0., 1., 2., 3., 4.],
        "y_km": [0., 1., 1., 0., 0.],
        "n_observations": [1, 3, 2, 2, 4],
        "n_pigmented": [0, 2, 2, 0, 4],
        "temperature": ["not-a-number"] * 5,
        "elevation": ["not-a-number"] * 5,
    })
    scores = pd.DataFrame({
        "exact_site_id": ids,
        "ardens_occurrence_reference": [.2, .3, .1, .9, .5],
        "diversus_occurrence_reference": [.2, .4, .8, .1, .4],
        "beaticola_occurrence_reference": [.6, .1, .1, .1, .2],
        "consobrinus_occurrence_reference": [.1, .2, .2, .1, .1],
        "honshuensis_occurrence_reference": [.1, .1, .1, .2, .5],
    })
    c = tmp_path / "cells.csv"
    s = tmp_path / "sdm.csv"
    cells.to_csv(c, index=False)
    scores.to_csv(s, index=False)
    r = code.test_five_species(c, s, tmp_path / "outputs", radii=(5,),
                               block_km=50, nperm=101)
    assert len(r) == 1
    assert r.iloc[0].n_species == 5
    assert r.iloc[0].n_edges > 0
    assert 0 <= r.iloc[0].upper_tail_p <= 1
    assert (tmp_path / "outputs/guild_colour_turnover.csv").exists()
    assert bool(pd.read_json(tmp_path / "outputs/run_scope.json", typ="series")[
        "environmental_predictors_in_test"]) is False
    scores["beaticola_occurrence_reference"] = np.nan
    scores.to_csv(s, index=False)
    with pytest.raises(ValueError, match="coverage incomplete"):
        code.load_inputs(c, s)


def test_selected_habitat_suitability_zero_is_not_imputed(tmp_path):
    code = module()
    cells = pd.DataFrame({
        "exact_site_id": ["a", "b"],
        "x_km": [0., 1.], "y_km": [0., 1.],
        "n_observations": [1, 1], "n_pigmented": [1, 0],
    })
    scores = pd.DataFrame({
        "exact_site_id": ["a", "b"],
        **{col: [0., .3] for col in code.SCORE_COLS}
    })
    c, s = tmp_path / "cells.csv", tmp_path / "sdm.csv"
    cells.to_csv(c, index=False)
    scores.to_csv(s, index=False)
    _, _, valid, _, _, _ = code.load_inputs(c, s)
    assert valid.tolist() == [False, True]
    with pytest.raises(ValueError, match="All pair profiles"):
        code.test_five_species(c, s, tmp_path / "out", radii=(5,), nperm=100)
