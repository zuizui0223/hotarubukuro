"""Stand-alone SDM-only group analysis: predictor scope and numeric contracts."""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd
import pytest


def load():
    source = Path(__file__).resolve().parents[2] / "scripts/run_bombus_sdm_group_comparison.py"
    spec = importlib.util.spec_from_file_location("sdm_group_compare", source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def synthetic_data(tmp_path):
    n = 40
    rng = np.random.default_rng(20)
    data = pd.DataFrame({
        "exact_site_id": [f"site{i}" for i in range(n)],
        "x_km": np.repeat([0, 60, 120, 180], 10).astype(float) + np.arange(n)%10,
        "y_km": np.arange(n)%10,
        "n_observations": np.repeat(4, n),
        "n_pigmented": rng.integers(1, 4, n),
        "n_white": [0]*n,
        "env_Temperature_PC1": ["ignored"]*n,
        "elevation": ["ignored"]*n,
    })
    data["n_white"] = data.n_observations-data.n_pigmented
    for sp in load().TAXA:
        data[f"bee_{sp}"] = rng.uniform(0.01, .99, n)
    cells = tmp_path / "cells.csv"
    region = tmp_path / "region.txt"
    data.to_csv(cells, index=False)
    region.write_text("\n".join(data.exact_site_id[:20])+"\n")
    return cells, region


def test_no_abiotic_predictors_or_synthetic_zeros(tmp_path):
    code=load()
    cells, region = synthetic_data(tmp_path)
    nationwide, central, ids = code.read_and_rank(cells, region)
    assert len(nationwide) == 40 and len(central) == 20 and len(ids) == 20
    assert np.all(np.isfinite(nationwide[["widespread10", "montane10"]]))
    rows=code.fit_effects(nationwide, "test")
    assert {r["term"] for r in rows} == {
        "widespread", "montane", "widespread_minus_montane"
    }
    assert all(r["n_blocks"] == 4 for r in rows)
    p=code.shuffle_test(nationwide,"test",reps=101)
    assert 0 < p["two_sided_centered_p"] <= 1
    assert p["n_perm_cells"] == 40
    broken=pd.read_csv(cells)
    broken.loc[0, "bee_beaticola"] = np.nan
    broken.to_csv(cells,index=False)
    with pytest.raises(ValueError,match="All five real SDM scores"):
        code.read_and_rank(cells,region)


def test_irls_is_stable_on_aggregated_counts():
    code=load()
    X=np.column_stack([np.ones(10), np.linspace(-1,1,10),np.linspace(1,2,10)**2])
    p=np.array([1,2,3,4,5,6,7,6,5,4])
    Y=np.column_stack([p,np.full(10,10)-p])
    b,hat=code.irls(X,Y)
    assert np.isfinite(b).all() and np.isfinite(hat).all()
    assert ((hat > 0) & (hat < 1)).all()
    assert abs(float(np.sum(X.T@(Y[:,0]-Y.sum(axis=1)*hat)))) < 1e-6
