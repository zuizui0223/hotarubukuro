#!/usr/bin/env python3
import csv
from pathlib import Path
R=Path(__file__).resolve().parents[2];p=R/'validation/validate_jbi_repository_alignment.py';t=p.read_text()
D={
'r"evidence is therefore not that bumblebees created the national colour pattern)"':'r"evidence is therefore not that bumblebees created the national colour pattern|"\n        r"SDMs still measure habitat opportunity rather than realized selection)"',
'r"human context leaves a provenance clue, not an origin answer)"':'r"human context leaves a provenance clue, not an origin answer|"\n        r"The result does not assign horticultural origin)"',
'r"pattern disappeared when nearby white and pigmented endpoints were constrained to similar elevation)"':'r"pattern disappeared when nearby white and pigmented endpoints were constrained to similar elevation|"\n            r"disappeared after elevation was matched)"'}
for a,b in D.items():assert a in t,a;t=t.replace(a,b)
p.write_text(t)
p=R/'paper/active-file-map.csv'
with p.open(newline='') as h:rows=list(csv.DictReader(h))
rows=[r for r in rows if r['path']!='FINAL_PIPELINE_SUPP_MANUSCRIPT_AUDIT_2026-08-12.md']
extra=[('entrypoint','execution','run_pipeline.py'),('configuration','execution','config/paper_pipeline.lock.json'),('workflow','execution','.github/workflows/paper-pipeline.yml'),('validation','alignment','validation/validate_jbi_repository_alignment.py'),('testing','Python','tests/python/test_run_pipeline.py'),('analysis','Main1_sensitivity','scripts/fit_broad_space_null_phenotype_excess.R'),('analysis','Main1_sensitivity','scripts/run_broad_spatial_inertia_environment_tracking.R'),('analysis','Main1_sensitivity','R/broad_spatial_inertia_environment_tracking.R'),('workflow','Main1_sensitivity','.github/workflows/broad-spatial-inertia-environment-tracking.yml'),('documentation','Main1_sensitivity','docs/broad_spatial_inertia_environment_tracking.md'),('documentation','Main2','docs/bombus-inference-current.md')]
d={r['path']:r for r in rows}
for role,sec,path in extra:d.setdefault(path,{'role':role,'section':sec,'path':path,'status':'component'})
primary={'README.md','run_pipeline.py','config/paper_pipeline.lock.json','.github/workflows/paper-pipeline.yml','.github/workflows/paper-checks.yml','paper/README.md','paper/analysis-map.md','paper/active-file-map.csv','docs/reproduction-guide.md','submission/jbi/JBI_main_manuscript_anonymized.md','submission/jbi/validate_jbi_submission.py','validation/validate_jbi_repository_alignment.py','pyproject.toml'}
for path,r in d.items():
 r['status']='primary' if path in primary else ('data' if path in {'Data_S1.csv','Code_S1.py'} else ('submission' if path.startswith('submission/jbi/') else ('provenance' if path.startswith('reproducibility/') or path=='docs/broad_spatial_inertia_environment_tracking.md' else 'component')))
miss=[q for q in d if not (R/q).is_file()];assert not miss,miss
rank={'primary':0,'submission':1,'data':2,'component':3,'provenance':4};rows=sorted(d.values(),key=lambda r:(rank[r['status']],r['section'],r['path']))
with p.open('w',newline='') as h:w=csv.DictWriter(h,fieldnames=['role','section','path','status'],lineterminator='\n');w.writeheader();w.writerows(rows)
Path(__file__).unlink()
