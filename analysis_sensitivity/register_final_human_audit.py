from pathlib import Path

path = Path('paper/active-file-map.csv')
text = path.read_text(encoding='utf-8').rstrip('\n')
rows = [
    'analysis,Main3_sensitivity,analysis_sensitivity/run_human_context_highrep_final.R,active',
    'workflow,Main3_sensitivity,.github/workflows/human-context-highrep-final.yml,active',
    'reproducibility,Main3_final,reproducibility/human_context_final_audit_spec_2026-08-11.md,active',
    'reproducibility,Main3_final,reproducibility/human_context_final_audit_results_2026-08-11.md,active',
]
for row in rows:
    if row not in text.splitlines():
        text += '\n' + row
path.write_text(text + '\n', encoding='utf-8')
print('registered final human-context audit files')
