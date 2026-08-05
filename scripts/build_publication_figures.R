#!/usr/bin/env Rscript

# The established plotting implementation is retained as an internal source,
# but its historical hard-coded labels are replaced before evaluation. Plotted
# values and labels therefore come from the fresh 1,909 run.
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
text <- gsub(
  'observed = 16',
  'observed = nrow(candidates)',
  text, fixed = TRUE
)
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
parsed <- parse(text = text, keep.source = FALSE)
eval(parsed, envir = new.env(parent = globalenv()))
