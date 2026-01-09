## =========================================================
## SEM combined figure: diversus & honshuensis (SIGNIFICANT PATHS ONLY)
## =========================================================

library(dplyr)
library(lavaan)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(scales)

alpha <- 0.05   # ← 有意水準

## =========================================================
## ラベル定義
## =========================================================

label_map <- c(
  elevation="elevation",
  roughness="topographic roughness",
  Temperature_PC1="temperature",
  precip_PC1="precipitation",
  soil_PC1="soil conditions",
  RSDS="solar radiation",
  bee_prob="bumblebee SDM prediction",
  Pigment="floral pigmentation",
  Lm="darkness (−L*)",
  a="red–green axis (a*)",
  C="chroma (c*)"
)

pretty   <- function(x) ifelse(x %in% names(label_map), label_map[x], x)
make_id  <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
sp_label <- function(sp) paste0("Bombus ", sp)

preds <- c(
  "elevation","roughness","precip_PC1",
  "Temperature_PC1","soil_PC1","RSDS"
)

## =========================================================
## サブグラフ生成（★有意パスのみ）
## =========================================================

make_sem_subgraph <- function(fit, sp, tag_id, tag_label){
  
  ss <- as.data.frame(lavaan::standardizedSolution(fit))
  
  ## ---------------------------
  ## regression paths（有意のみ）
  ## ---------------------------
  reg_edges <- ss %>%
    filter(
      op == "~",
      lhs %in% c("bee_prob","Pigment"),
      pvalue < alpha
    )
  
  reg_txt <- paste(sprintf(
    '%s_%s -> %s_%s [label="%.2f", color="%s", penwidth=%.2f];',
    tag_id, make_id(reg_edges$rhs),
    tag_id, make_id(reg_edges$lhs),
    reg_edges$est.std,
    ifelse(reg_edges$est.std >= 0, "#2B8CBE", "#E34A33"),
    rescale(abs(reg_edges$est.std), c(1.5, 3.5))
  ), collapse="\n")
  
  ## ---------------------------
  ## measurement model（全表示）
  ## ---------------------------
  meas_edges <- ss %>% filter(op=="=~", lhs=="Pigment")
  
  meas_txt <- paste(sprintf(
    '%s_Pigment -> %s_%s [label="%.2f", color="#636363", penwidth=1.8];',
    tag_id,
    tag_id, make_id(meas_edges$rhs),
    meas_edges$est.std
  ), collapse="\n")
  
  nodes <- c(preds, "bee_prob", "Pigment", "Lm", "a", "C")
  
  node_txt <- paste(sprintf(
    '%s_%s [label="%s"];',
    tag_id, make_id(nodes), pretty(nodes)
  ), collapse="\n")
  
  paste0(
    'subgraph cluster_', tag_id, ' {\n',
    'label="', tag_label, '  ', sp_label(sp), '";\n',
    'labelloc="t"; fontsize=26; fontname="Helvetica-Bold";\n',
    'node [shape=box, style=rounded, fontname="Helvetica"];\n',
    node_txt, '\n',
    reg_txt, '\n',
    meas_txt, '\n',
    '}\n'
  )
}

## =========================================================
## 2 種を 1 図にまとめる（上下）
## =========================================================

gr <- paste0(
  'digraph G {\n',
  'rankdir=TB;\n',
  
  make_sem_subgraph(
    fit = fits[["diversus"]],
    sp  = "diversus",
    tag_id = "A",
    tag_label = "(A)"
  ),
  
  make_sem_subgraph(
    fit = fits[["honshuensis"]],
    sp  = "honshuensis",
    tag_id = "B",
    tag_label = "(B)"
  ),
  
  '}\n'
)

## =========================================================
## 書き出し
## =========================================================

svg <- export_svg(grViz(gr))

out_file <- file.path(
  diag_dir,
  "Fig_SEM_diversus_honshuensis_beeprob_sigonly.png"
)

rsvg_png(
  charToRaw(svg),
  out_file,
  width  = 2400,
  height = 1200
)

cat("✅ Significant-path SEM figure exported:\n", out_file, "\n")