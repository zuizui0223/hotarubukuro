#!/usr/bin/env Rscript

# The established plotting implementation remains the base for Figures 1, 2,
# and 4. Historical hard-coded labels are replaced before evaluation. Figure 3
# is then regenerated from the active Bombus-limitation stage and the manuscript
# points only to that new figure stem.
core_path <- "scripts/internal/build_publication_figures_core.R"
code <- readLines(core_path, warn = FALSE, encoding = "UTF-8")
text <- paste(code, collapse = "\n")
text <- gsub(
  'subtitle = "Median sRGB at 1,923 author-confirmed locations"',
  'subtitle = paste0("Median sRGB at ", format(nrow(analysis), big.mark = ","), " author-confirmed locations")',
  text, fixed = TRUE
)
text <- gsub(
  'title = "a   Local pigmented isolates (n = 16)"',
  'title = paste0("a   Local pigmented isolates (n = ", nrow(candidates), ")")',
  text, fixed = TRUE
)
text <- gsub('observed = 16', 'observed = nrow(candidates)', text, fixed = TRUE)
text <- gsub(
  'observed = 0.0448',
  paste0(
    'observed = read_num(hb_read_csv("results/ecological_v20_local_white_isolates/',
    'local_isolate_natural_null_summary.csv")$observed_value[',
    'hb_read_csv("results/ecological_v20_local_white_isolates/',
    'local_isolate_natural_null_summary.csv")$configuration == ',
    '"primary_10km_env1_all_white" & ',
    'hb_read_csv("results/ecological_v20_local_white_isolates/',
    'local_isolate_natural_null_summary.csv")$metric == "candidate_fraction"])'
  ),
  text, fixed = TRUE
)
text <- gsub(
  'ggplot2::scale_x_continuous(breaks = c(1, 4, 8, 12, 16))',
  'ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5))',
  text, fixed = TRUE
)
# The retained core still contains the archived national/turnover Figure 3.
# Remove that block before evaluation; the active Figure 3 is local-only.
text <- sub(
  "(?s)# Figure 3: incremental national information and local turnover tests\\..*?# Figure 4: local isolates and human-context follow-up\\.",
  "# Figure 4: local isolates and human-context follow-up.",
  text, perl = TRUE
)
text <- gsub(
  "figure_3_bombus_turnover", "figure_3_bombus_limitation",
  text, fixed = TRUE
)

parsed <- parse(text = text, keep.source = FALSE)
eval(parsed, envir = new.env(parent = globalenv()))

# Build the manuscript-facing Figure 3 from the local limitation gate only.
source("scripts/internal/build_bombus_limitation_figure.R", local = new.env(parent = globalenv()))
