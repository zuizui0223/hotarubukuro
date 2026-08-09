from pathlib import Path

replacements = {
    'paper/README.md': {
        'reproducibility/bombus_local_sharp_transition_spec_2026-08-08.md': 'reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md',
        'reproducibility/bombus_local_sharp_transition_results_2026-08-08.md': 'reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md',
    },
    'paper/analysis-map.md': {
        'reproducibility/bombus_local_sharp_transition_spec_2026-08-08.md': 'reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md',
        'reproducibility/bombus_local_sharp_transition_results_2026-08-08.md': 'reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md',
    },
    'paper/active-file-map.csv': {
        'reproducibility/bombus_local_sharp_transition_spec_2026-08-08.md': 'reproducibility/bombus_local_sharp_transition_current_spec_2026-08-09.md',
        'reproducibility/bombus_local_sharp_transition_results_2026-08-08.md': 'reproducibility/bombus_local_sharp_transition_current_results_2026-08-09.md',
    },
}

for filename, mapping in replacements.items():
    p = Path(filename)
    text = p.read_text(encoding='utf-8')
    for old, new in mapping.items():
        if old not in text:
            raise SystemExit(f'{filename}: missing expected old path {old}')
        text = text.replace(old, new)
    if filename == 'paper/active-file-map.csv' and 'R/local_pair_graph.R' not in text:
        marker = 'analysis,Main2,scripts/build_bombus_occurrence_reference_support.R,active\n'
        if marker not in text:
            raise SystemExit('active file map Main2 marker not found')
        text = text.replace(marker, marker + 'analysis,Main2,R/local_pair_graph.R,active\n')
    p.write_text(text, encoding='utf-8')

print('Updated current paper maps to clean Main 2 files.')
