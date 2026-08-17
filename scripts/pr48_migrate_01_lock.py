#!/usr/bin/env python3
import json
from pathlib import Path
R=Path(__file__).resolve().parents[2]
p=R/'config/paper_pipeline.lock.json'; x=json.loads(p.read_text())
x['paper_version']='jbi-2026-08-18'
x['provenance_boundary']['interpretation']='The reproduce profile regenerates the accepted Broad spatial-null sensitivity, downstream analyses, figures, and JBI review bundle without refreshing biological sources.'
r=x['profiles']['reproduce']
if 'run_broad_space_null' not in r:r.insert(r.index('restore_broad')+1,'run_broad_space_null')
c=x['stages']['bootstrap_r']['command']; c[c.index('--skip-inla')+1]='false'
x['stages']['run_broad_space_null']={'kind':'command','command':['Rscript','scripts/fit_broad_space_null_phenotype_excess.R','--cells','results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv','--output','results/broad_space_null_phenotype_excess','--samples','500','--seed','20260725','--max-pairs-per-fold','15000','--geo-bins','5'],'outputs':['results/broad_space_null_phenotype_excess/primary_space_null_excess_test.csv','results/broad_space_null_phenotype_excess/matched_distance_stratum_contrasts.csv','results/broad_space_null_phenotype_excess/heldout_pair_space_null_excess.csv','results/broad_space_null_phenotype_excess/heldout_space_null_predictions.csv','results/broad_space_null_phenotype_excess/secondary_pair_diagnostics.csv','results/broad_space_null_phenotype_excess/analysis_metadata.csv']}
for q in ['.github/workflows/broad-spatial-inertia-environment-tracking.yml','docs/broad_spatial_inertia_environment_tracking.md']:
 if q not in x['artifacts']['broad']['references']:x['artifacts']['broad']['references'].append(q)
q='docs/broad_spatial_inertia_environment_tracking.md'; req=x['alignment']['required_files']
if q not in req:req.insert(req.index('submission/jbi/JBI_main_manuscript_anonymized.md'),q)
labels={'Broad spatial-null phenotype-excess sensitivity','Broad sensitivity is mapped without replacing the accepted model'}
checks=[z for z in x['alignment']['checks'] if z['label'] not in labels]
i=next(i for i,z in enumerate(checks) if z['label']=='Broad fixed effects and spatial scales')+1
checks[i:i]=[{'label':'Broad spatial-null phenotype-excess sensitivity','path':q,'patterns':['0\\.106802','0\\.058240','\\+0\\.048562','0\\.03393','-0\\.045891','0\\.87226','does not by itself distinguish causal environmental effects']},{'label':'Broad sensitivity is mapped without replacing the accepted model','path':'paper/analysis-map.md','patterns':['cross-fitted spatial-null sensitivity','0\\.03393','not selection or local adaptation']}]
for z in checks:
 if z['label']=='single execution front door' and 'run_broad_space_null' not in z['patterns']:z['patterns'].append('run_broad_space_null')
x['alignment']['checks']=checks;p.write_text(json.dumps(x,ensure_ascii=False,indent=2)+'\n')
Path(__file__).unlink()
