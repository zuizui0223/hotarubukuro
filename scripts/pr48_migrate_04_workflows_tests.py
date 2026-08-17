#!/usr/bin/env python3
import re
from pathlib import Path
R=Path(__file__).resolve().parents[2]
p=R/'.github/workflows/paper-pipeline.yml';t=p.read_text().replace('timeout-minutes: 240','timeout-minutes: 360',1).replace('scripts/(setup_r_environment|build_bombus_occurrence_reference_support','scripts/(setup_r_environment|fit_broad_space_null_phenotype_excess|build_bombus_occurrence_reference_support',1).replace('            results/paper_pipeline\n            results/bombus_occurrence_reference_support','            results/paper_pipeline\n            results/broad_space_null_phenotype_excess\n            results/bombus_occurrence_reference_support',1);p.write_text(t)
p=R/'.github/workflows/broad-spatial-inertia-environment-tracking.yml';t=p.read_text().replace('      - agent/broad-spatial-inertia-environment-tracking','      - main',1)
pat=r'''      - name: Fit cross-fitted space null and test phenotype excess\n.*?(?=      - name: Validate output contract)'''
step='''      - name: Fit cross-fitted space null and test phenotype excess
        shell: bash
        run: |
          set -euo pipefail
          Rscript scripts/fit_broad_space_null_phenotype_excess.R \\
            --cells=reference-artifact/results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv \\
            --output=results/broad_space_null_phenotype_excess \\
            --samples=${EXCESS_POSTERIOR_SAMPLES} --seed=${EXCESS_SEED} \\
            --max-pairs-per-fold=${EXCESS_MAX_PAIRS_PER_FOLD} --geo-bins=${EXCESS_GEO_BINS}

'''
t,n=re.subn(pat,step,t,flags=re.S);assert n==1,n;p.write_text(t)
p=R/'tests/python/test_run_pipeline.py';t=p.read_text();f='''

def test_broad_space_null_is_part_of_exact_reproduction() -> None:
    data = lock()
    reproduce = data["profiles"]["reproduce"]
    assert reproduce.index("run_broad_space_null") == reproduce.index("restore_broad") + 1
    stage = data["stages"]["run_broad_space_null"]
    assert "scripts/fit_broad_space_null_phenotype_excess.R" in stage["command"]
    assert stage["command"][stage["command"].index("--samples") + 1] == "500"
    bootstrap = data["stages"]["bootstrap_r"]["command"]
    assert bootstrap[bootstrap.index("--skip-inla") + 1] == "false"
'''
if 'test_broad_space_null_is_part_of_exact_reproduction' not in t:p.write_text(t.rstrip()+f+'\n')
(R/'.github/workflows/upgrade-pr48-current.yml').unlink(missing_ok=True);Path(__file__).unlink()
