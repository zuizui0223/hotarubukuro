from pathlib import Path
p = Path('paper/active-file-map.csv')
line = 'reproducibility,Main3_final,reproducibility/human_context_figure4_final_summary_2026-08-11.csv,active'
text = p.read_text(encoding='utf-8').rstrip('\n')
if line not in text.splitlines():
    text += '\n' + line
p.write_text(text + '\n', encoding='utf-8')
