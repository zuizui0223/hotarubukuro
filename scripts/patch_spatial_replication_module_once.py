from pathlib import Path

p = Path('scripts/run_bombus_spatial_replication_test.R')
text = p.read_text(encoding='utf-8')
old = 'source("R/pipeline_support.R")\narg_value <- function(name, default = NULL) hb_arg_value(args, name, default)\nhb_load_modules("local_bombus_turnover")\n'
new = 'source("R/pipeline_support.R")\nsource("R/local_pair_graph.R")\narg_value <- function(name, default = NULL) hb_arg_value(args, name, default)\n'
if old not in text:
    raise SystemExit('old local_bombus_turnover load not found')
text = text.replace(old, new, 1)
text = text.replace('v17_require_columns(', 'lp_require_columns(')
text = text.replace('v17_pair_graph(', 'lp_pair_graph(')
if 'v17_' in text:
    raise SystemExit('unresolved v17_ dependency remains in spatial replication script')
p.write_text(text, encoding='utf-8')
print('Patched spatial replication to current local_pair_graph module.')
