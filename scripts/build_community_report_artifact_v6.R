args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[1] else
  "results/ecological_v13_community_threshold"

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}

read_result <- function(name) {
  utils::read.csv(
    file.path(output_dir, name), check.names = FALSE, stringsAsFactors = FALSE
  )
}

presence <- read_result("community_presence_threshold_cv_summary.csv")
edge <- read_result("community_edge_spatial_cv_summary.csv")
spde <- read_result("community_spde_model_comparison.csv")
candidates <- read_result("community_horticultural_candidates.csv")
facets <- read_result("community_horticultural_facet_summary.csv")
rank_trends <- read_result("community_horticultural_facet_rank_trends.csv")
curves <- read_result("community_presence_partial_dependence.csv")
audit <- read_result("community_analysis_audit.csv")

axis_labels <- c(
  bombus_availability = "総利用可能性",
  bombus_effective_richness = "有効種数 (Hill N1)",
  bombus_alpine_share = "高山性3種の構成比"
)
support_labels <- c(
  nationwide_zero = "全国・支持域外0",
  common_support = "5種共通支持域"
)
edge_labels <- c(
  availability_level = "利用可能性の平均水準",
  availability_change = "利用可能性の差",
  effective_richness_change = "有効種数の差",
  community_turnover = "群集組成ターンオーバー",
  alpine_composition_change = "高山性種構成比の差",
  terrain_endpoint_proxy = "端点の地形差（代理）"
)
facet_labels <- c(
  z_H = "人口曝露",
  z_R = "人為―森林境界",
  z_A = "道路アクセス",
  intensity_natural_residual_v13 = "有色内の濃色残差",
  early_natural_score_v13 = "早咲残差（日）",
  n_exact_sites = "独立地点数",
  n_years = "独立年数"
)

presence_smooth <- presence[presence$model == "smooth", , drop = FALSE]
presence_smooth$axis_label <- unname(axis_labels[presence_smooth$axis])
presence_smooth$support_label <- unname(support_labels[presence_smooth$support])
presence_smooth$model_label <- "平滑効果"

presence$axis_label <- unname(axis_labels[presence$axis])
presence$support_label <- unname(support_labels[presence$support])
presence$model_label <- c(
  linear = "線形", smooth = "平滑", threshold = "折れ線閾値"
)[presence$model]

edge$model_label <- unname(edge_labels[edge$model])
edge$support_label <- unname(support_labels[edge$support])

spde$model_label <- c(
  presence_space_only = "白／有色：空間のみ",
  presence_environment_space = "白／有色：環境＋空間",
  intensity_space_only = "有色内強度：空間のみ",
  intensity_environment_space = "有色内強度：環境＋空間"
)[spde$model]

facet_table <- data.frame(
  analysis = "候補－対照の平均差",
  facet = unname(facet_labels[facets$facet]),
  n = facets$n_pairs,
  estimate = facets$mean_candidate_minus_control,
  lower_95 = facets$lower_95,
  upper_95 = facets$upper_95,
  BH_q = facets$BH_q,
  stringsAsFactors = FALSE
)
facet_table <- rbind(
  facet_table,
  data.frame(
    analysis = "逸脱強度1 SDあたりの順位傾向",
    facet = unname(facet_labels[rank_trends$facet]),
    n = rank_trends$n_pairs,
    estimate = rank_trends$slope_per_1sd_anomaly,
    lower_95 = rank_trends$lower_95,
    upper_95 = rank_trends$upper_95,
    BH_q = rank_trends$BH_q,
    stringsAsFactors = FALSE
  )
)

candidate_plot <- candidates[, c(
  "candidate_rank", "longitude", "latitude", "descriptive_longitude_band",
  "anomaly_strength", "neighbour_pigment_share",
  "natural_presence_probability_v13", "colour_a", "DOY", "z_H", "z_R",
  "z_A", "intensity_natural_residual_v13", "early_natural_score_v13"
)]
names(candidate_plot)[names(candidate_plot) == "descriptive_longitude_band"] <-
  "longitude_band"

availability_curve <- curves[
  curves$support == "common_support" & curves$axis == "bombus_availability",
  , drop = FALSE
]

nationwide_base <- unique(presence[presence$support == "nationwide_zero",
                                    c("base_log_loss")])
presence_cv_raw <- read_result("community_presence_threshold_spatial_cv.csv")
base_rows <- presence_cv_raw[
  presence_cv_raw$support == "nationwide_zero" &
    presence_cv_raw$axis == "bombus_availability" &
    presence_cv_raw$model == "base", , drop = FALSE
]
nationwide_auc <- stats::weighted.mean(
  base_rows$auc, base_rows$n_test_observations
)
spde_presence_gain <- spde$WAIC[spde$model == "presence_space_only"] -
  spde$WAIC[spde$model == "presence_environment_space"]

metric_auc <- data.frame(
  national_cell_auc = nationwide_auc,
  national_cell_log_loss = nationwide_base,
  n_observations = unique(presence$n_heldout_observations[
    presence$support == "nationwide_zero"
  ])[1]
)
metric_spde <- data.frame(
  presence_delta_WAIC = spde_presence_gain,
  presence_spatial_range_km = read_result(
    "community_spde_hyperparameters.csv"
  )$mean[read_result("community_spde_hyperparameters.csv")$model ==
           "presence_environment_space" &
           read_result("community_spde_hyperparameters.csv")$hyperparameter ==
           "Range for spatial"]
)
metric_candidates <- data.frame(
  candidate_cells = nrow(candidates),
  west_candidate_cells = sum(candidates$descriptive_longitude_band == "West"),
  matched_controls = unique(facets$n_pairs)[1]
)

generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
sources <- list(
  list(
    id = "source_presence_cv", label = "花色有無・群集閾値の空間交差検証",
    path = "results/ecological_v13_community_threshold/community_presence_threshold_cv_summary.csv",
    query = list(
      language = "R", executed_at = generated_at,
      description = "1-kmセルの白／有色を、全国環境＋空間GAMに群集軸を追加して5-fold空間交差検証した集計。",
      tables_used = list("community_presence_threshold_spatial_cv.csv"),
      filters = list("East/West固定効果なし", "DOY・人口・園芸ファセットを背景モデルから除外"),
      metric_definitions = list(
        "delta_log_loss = 背景モデルの保持外log loss - 群集軸追加モデルの保持外log loss。正なら改善。",
        "閾値は各訓練フォールド内の候補からAIC最小を選び、保持外フォールドで評価。"
      )
    )
  ),
  list(
    id = "source_spde", label = "全国SPDE基礎モデル",
    path = "results/ecological_v13_community_threshold/community_spde_model_comparison.csv",
    query = list(
      language = "R-INLA", executed_at = generated_at,
      description = "白／有色と有色内強度を別応答にした、地域固定効果なしの全国SPDE比較。",
      tables_used = list("community_spde_model_comparison.csv"),
      filters = list("有色内強度では白花a*を除外", "Bombus固定効果なし"),
      metric_definitions = list(
        "delta_WAIC_within_set = 同一応答内の最小WAICとの差。0が最良。",
        "空間rangeはkm単位の投影座標で推定。"
      )
    )
  ),
  list(
    id = "source_edges", label = "局所接触帯の端点非共有交差検証",
    path = "results/ecological_v13_community_threshold/community_edge_spatial_cv_summary.csv",
    query = list(
      language = "R", executed_at = generated_at,
      description = "25 km以内の局所辺で、訓練辺とテスト辺が端点を共有しない5-fold評価。",
      tables_used = list("community_edge_spatial_cv.csv"),
      filters = list("地理距離25 km以下", "環境距離と交差予測された自然花色背景を調整"),
      metric_definitions = list(
        "flower_discordant = 辺の両端で白／有色が異なる。",
        "community_turnover = 5種の連続ENMeval組成のHellinger距離。"
      )
    )
  ),
  list(
    id = "source_candidates", label = "有色花の局所飛び地候補",
    path = "results/ecological_v13_community_threshold/community_horticultural_candidates.csv",
    query = list(
      language = "R", executed_at = generated_at,
      description = "25 km内に5セル以上あり有色率10%以下の近傍に現れた有色1-kmセル。",
      tables_used = list("community_horticultural_candidates.csv"),
      filters = list("候補定義に人口・林縁・道路・濃色・早咲を使用しない"),
      metric_definitions = list(
        "anomaly_strength = 局所有色逸脱と環境＋空間背景からの有色逸脱の幾何平均。"
      )
    )
  ),
  list(
    id = "source_facets", label = "園芸仮説の独立ファセット比較",
    path = "results/ecological_v13_community_threshold/community_horticultural_facet_summary.csv",
    query = list(
      language = "R", executed_at = generated_at,
      description = "候補19セルと非再利用の有色対照19セルのマッチド差、および候補順位との傾き。",
      tables_used = list(
        "community_horticultural_facet_summary.csv",
        "community_horticultural_facet_rank_trends.csv",
        "community_horticultural_matching_sensitivity.csv"
      ),
      filters = list("各ファセットを別々に検定", "100-km空間ブロックbootstrap", "BH補正"),
      metric_definitions = list(
        "早咲残差は観測DOYが自然予測より早いほど正。",
        "道路アクセスz_Aは高いほど主要道路に近い。"
      )
    )
  ),
  list(
    id = "source_audit", label = "v13解析監査",
    path = "results/ecological_v13_community_threshold/community_analysis_audit.csv",
    query = list(
      language = "R", executed_at = generated_at,
      description = "応答分離、支持域、空間CV、共線性、残差、候補独立性、SPDEを検証した再現可能監査。",
      tables_used = list("community_analysis_audit.csv")
    )
  )
)

# The reader requires executable SQL provenance for structured cards/charts.
# DuckDB can query the saved CSV artifacts directly, so these statements are
# runnable reproductions of the rows exposed by the report rather than prose.
source_sql <- list(
  source_presence_cv = paste(
    "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_presence_threshold_cv_summary.csv')",
    "UNION ALL BY NAME",
    "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_presence_threshold_spatial_cv.csv')"
  ),
  source_spde = paste(
    "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_spde_model_comparison.csv')",
    "UNION ALL BY NAME",
    "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_spde_hyperparameters.csv')"
  ),
  source_edges = "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_edge_spatial_cv_summary.csv')",
  source_candidates = "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_horticultural_candidates.csv')",
  source_facets = paste(
    "SELECT 'candidate_minus_control' AS analysis, * FROM read_csv_auto('results/ecological_v13_community_threshold/community_horticultural_facet_summary.csv')",
    "UNION ALL BY NAME",
    "SELECT 'rank_slope_per_1sd_anomaly' AS analysis, * FROM read_csv_auto('results/ecological_v13_community_threshold/community_horticultural_facet_rank_trends.csv')"
  ),
  source_audit = "SELECT * FROM read_csv_auto('results/ecological_v13_community_threshold/community_analysis_audit.csv')"
)
for (index in seq_along(sources)) {
  source <- sources[[index]]
  source$query <- list(
    engine = "DuckDB", language = "SQL", executed_at = generated_at,
    sql = source_sql[[source$id]],
    description = paste("Saved v13 CSV evidence for", source$label),
    tables_used = list(source$path)
  )
  sources[[index]] <- source
}

manifest <- list(
  version = 1L,
  surface = "report",
  title = "ホタルブクロ花色の全国構造・マルハナバチ群集・園芸逸脱",
  description = "全国SPDE、群集閾値、局所接触帯、園芸候補を一方向の仮説系として再解析した技術報告。",
  generatedAt = generated_at,
  sources = sources,
  cards = list(
    list(
      id = "card_auc", dataset = "metric_auc", sourceId = "source_presence_cv",
      description = "全国1-kmセルの環境＋空間基準モデル。",
      metrics = list(
        list(label = "全国セルAUC", field = "national_cell_auc", format = "number"),
        list(label = "保持外log loss", field = "national_cell_log_loss", format = "number")
      )
    ),
    list(
      id = "card_spde", dataset = "metric_spde", sourceId = "source_spde",
      description = "白／有色で環境を空間場へ加えた改善。",
      metrics = list(
        list(label = "白／有色 ΔWAIC", field = "presence_delta_WAIC", format = "number"),
        list(label = "残存空間range (km)", field = "presence_spatial_range_km", format = "number")
      )
    ),
    list(
      id = "card_candidates", dataset = "metric_candidates",
      sourceId = "source_candidates",
      description = "人為・濃色・早咲を使わず抽出した局所有色飛び地。",
      metrics = list(
        list(label = "有色飛び地候補", field = "candidate_cells", format = "number"),
        list(label = "西側", field = "west_candidate_cells", format = "number")
      )
    )
  ),
  charts = list(
    list(
      id = "chart_presence_cv", title = "群集軸追加による保持外log loss改善",
      subtitle = "平滑効果。正の値ほど環境＋空間基準モデルより改善。",
      type = "bar", intent = "comparison",
      question = "利用可能性・有効種数・種組成は花色有無の保持外予測を改善するか。",
      rationale = "同じ単位の3群集軸を2つの支持域で比較するため、グループ化した棒が最も直接的。",
      comparisonContext = list(
        baseline = "全国環境＋空間GAM", denominator = "保持外画像観測",
        grain = "1-kmセル", unit = "log loss差"
      ),
      dataset = "presence_smooth", sourceId = "source_presence_cv",
      encodings = list(
        x = list(field = "axis_label", type = "nominal", label = "群集軸"),
        y = list(field = "delta_log_loss", type = "quantitative", label = "Δ log loss"),
        color = list(field = "support_label", type = "nominal", label = "支持域")
      ),
      combinationRationale = "色は全国の構造的0解析と5種共通支持域感度を区別する。",
      layout = "full"
    ),
    list(
      id = "chart_availability_curve", title = "5種共通支持域の総利用可能性と有色確率",
      subtitle = "全データ平滑モデルの平均部分依存。閾値の保持外優位性は別途CVで判定。",
      type = "line", intent = "relationship",
      question = "共通支持域で総利用可能性が増えると有色確率はどう変化するか。",
      rationale = "連続軸に沿う非線形形状を示すため線グラフを使う。",
      comparisonContext = list(
        normalization = "各種ENMeval値の全国順位を平均",
        grain = "平均部分依存", unit = "予測有色確率"
      ),
      dataset = "availability_curve", sourceId = "source_presence_cv",
      encodings = list(
        x = list(field = "axis_value", type = "quantitative", label = "総利用可能性"),
        y = list(field = "average_predicted_pigmentation", type = "quantitative",
                 label = "平均予測有色確率")
      ),
      layout = "full"
    ),
    list(
      id = "chart_edge_cv", title = "局所花色接触帯に対する追加変数の保持外改善",
      subtitle = "25 km以内・端点非共有CV。正の値ほど接触帯予測を改善。",
      type = "bar", intent = "comparison",
      question = "花色接触帯は群集の変化量・平均水準・地形端点差と対応するか。",
      rationale = "同一評価指標で複数の独立な局所仮説を比較するため棒グラフを使う。",
      comparisonContext = list(
        baseline = "距離＋環境差＋自然花色背景", grain = "局所辺",
        unit = "log loss差"
      ),
      dataset = "edge_summary", sourceId = "source_edges",
      encodings = list(
        x = list(field = "model_label", type = "nominal", label = "追加仮説"),
        y = list(field = "delta_log_loss", type = "quantitative", label = "Δ log loss"),
        color = list(field = "support_label", type = "nominal", label = "支持域")
      ),
      combinationRationale = "色は全国構造的0と5種共通支持域の結果を区別する。",
      layout = "full"
    ),
    list(
      id = "chart_candidates", title = "局所有色飛び地候補の位置",
      subtitle = "19候補中15候補が記述上の西側。点サイズは自然逸脱強度。",
      type = "scatter", intent = "relationship",
      question = "自然モデルから外れる有色セルはどこに分布するか。",
      rationale = "19セルの地理的位置・東西記述区分・逸脱強度を同時に保持する。",
      comparisonContext = list(grain = "候補1-kmセル", unit = "経緯度"),
      dataset = "candidate_plot", sourceId = "source_candidates",
      encodings = list(
        x = list(field = "longitude", type = "quantitative", label = "経度"),
        y = list(field = "latitude", type = "quantitative", label = "緯度"),
        color = list(field = "longitude_band", type = "nominal", label = "記述区分"),
        size = list(field = "anomaly_strength", type = "quantitative", label = "逸脱強度"),
        label = list(field = "candidate_rank", type = "ordinal", label = "候補順位")
      ),
      combinationRationale = "色は記述的な東西区分、点サイズは候補定義に使った自然逸脱強度。",
      layout = "full"
    )
  ),
  tables = list(
    list(
      id = "table_spde", title = "全国SPDEモデル比較",
      subtitle = "白／有色と有色内強度を別応答にした同一priorの比較。",
      dataset = "spde", sourceId = "source_spde", density = "spacious",
      defaultSort = list(field = "model_label", direction = "asc"),
      columns = list(
        list(field = "model_label", label = "モデル", type = "text"),
        list(field = "n", label = "n", format = "number"),
        list(field = "WAIC", label = "WAIC", format = "number"),
        list(field = "delta_WAIC_within_set", label = "応答内ΔWAIC", format = "number"),
        list(field = "mean_neglogCPO", label = "平均−log CPO", format = "number")
      ),
      layout = "full"
    ),
    list(
      id = "table_threshold", title = "群集軸の線形・平滑・閾値モデル比較",
      subtitle = "全5空間フォールド。正のΔ log loss・Δ AUCほど追加モデルを支持。",
      dataset = "presence_all", sourceId = "source_presence_cv", density = "dense",
      defaultSort = list(field = "delta_log_loss", direction = "desc"),
      columns = list(
        list(field = "support_label", label = "支持域", type = "text"),
        list(field = "axis_label", label = "群集軸", type = "text"),
        list(field = "model_label", label = "形", type = "text"),
        list(field = "delta_log_loss", label = "Δ log loss", format = "number"),
        list(field = "delta_auc", label = "Δ AUC", format = "number"),
        list(field = "median_change_point", label = "閾値中央値", format = "number")
      ),
      layout = "full"
    ),
    list(
      id = "table_facets", title = "園芸仮説の独立ファセット",
      subtitle = "候補－対照の平均差と、候補上位ほど効果が強まるかの傾き。",
      dataset = "facet_table", sourceId = "source_facets", density = "dense",
      defaultSort = list(field = "BH_q", direction = "asc"),
      columns = list(
        list(field = "analysis", label = "解析", type = "text"),
        list(field = "facet", label = "ファセット", type = "text"),
        list(field = "n", label = "n", format = "number"),
        list(field = "estimate", label = "効果量", format = "number"),
        list(field = "lower_95", label = "95%下限", format = "number"),
        list(field = "upper_95", label = "95%上限", format = "number"),
        list(field = "BH_q", label = "BH q", format = "number")
      ),
      layout = "full"
    ),
    list(
      id = "table_audit", title = "解析監査",
      subtitle = "応答分離・支持域・空間CV・共線性・残差・候補独立性の14項目。",
      dataset = "audit", sourceId = "source_audit", density = "dense",
      defaultSort = list(field = "check", direction = "asc"),
      columns = list(
        list(field = "check", label = "検査", type = "text"),
        list(field = "passed", label = "通過", type = "text"),
        list(field = "evidence", label = "根拠", type = "text"),
        list(field = "consequence", label = "解釈上の意味", type = "text")
      ),
      layout = "full"
    )
  ),
  blocks = list(
    list(id = "title", type = "markdown",
         body = "# ホタルブクロ花色の全国構造・マルハナバチ群集・園芸逸脱"),
    list(
      id = "technical_summary", type = "markdown",
      body = paste(
        "## 技術要約",
        "全国の白／有色は強い空間構造を持つ一方、環境をSPDEへ加えるとWAICが3.70改善した。したがって、長野周辺の有色集中は単なる地域ラベルでも、環境だけでもなく、連続環境勾配と約129 kmの残存空間場の双方で記述するのが妥当である。",
        "有色花内の色素強度では温度PC1と地形PC1に条件付き勾配が見えるが、環境追加によるWAIC改善はなく、空間のみモデルと同等だった。",
        "マルハナバチ5種の共通支持域では総利用可能性と有効種数が保持外予測を改善した。しかし全国の支持域外0解析では総利用可能性が再現せず、局所接触帯でも群集ターンオーバーや利用可能性の変化量は改善しなかった。現結果は、明瞭な群集変化の閾値より、共通支持域における連続的な利用可能性水準との対応を支持する。",
        "自然要因だけで抽出した有色飛び地は19セルあり15セルが西側だったが、人口・林縁・道路・濃色・早咲・反復性のどの独立ファセットも、マッチ距離感度を通じて園芸逸出を一貫して支持しなかった。",
        sep = "\n\n"
      )
    ),
    list(id = "headline_metrics", type = "metric-strip",
         cardIds = list("card_auc", "card_spde", "card_candidates")),
    list(
      id = "national_heading", type = "markdown", sourceId = "source_spde",
      body = paste(
        "## 全国構造は環境と空間の両方で説明される",
        "白／有色では環境＋空間SPDEが空間のみよりWAICを3.70改善した。温度PC1と土壌PC1の95%信用区間は0をまたがず、環境成分は残る。ただし空間場の標準偏差は2.06、rangeは約129 kmで、環境を入れても大きな地理構造が残った。",
        "有色内強度では環境＋空間のWAICが空間のみより0.36高く、モデル全体として環境追加を支持しない。個別係数だけを根拠に『色素量の環境適応』とは結論しない。",
        sep = "\n\n"
      )
    ),
    list(id = "spde_table_block", type = "table", tableId = "table_spde"),
    list(
      id = "pollinator_heading", type = "markdown", sourceId = "source_presence_cv",
      body = paste(
        "## マルハナバチ信号は共通支持域の利用可能性水準に限られる",
        "5種共通支持域では、総利用可能性の平滑項が保持外log lossを0.022改善し、有効種数も0.010改善した。総利用可能性の折れ線候補は各フォールドで0.572–0.593と安定したが、線形・平滑モデルの方が同等以上に予測したため、急な閾値が固有に支持されたとはいえない。",
        "全国で高山性3種の投影支持域外を0とした場合、総利用可能性の改善は消えた。従って『全国でマルハナバチ利用可能性が花色を決める』ではなく、『5種を同じ支持域で比較できる範囲に連続的対応がある』が現在の主張上限である。",
        sep = "\n\n"
      )
    ),
    list(id = "presence_cv_chart_block", type = "chart",
         chartId = "chart_presence_cv"),
    list(
      id = "availability_curve_note", type = "markdown",
      body = "平滑曲線は効果方向の記述用であり、保持外性能の根拠ではない。共通支持域では総利用可能性が増えるほど平均有色確率が上がるが、因果的な送粉者選択圧や訪花頻度を表すものではない。"
    ),
    list(id = "availability_curve_block", type = "chart",
         chartId = "chart_availability_curve"),
    list(id = "threshold_table_block", type = "table",
         tableId = "table_threshold"),
    list(
      id = "contact_heading", type = "markdown", sourceId = "source_edges",
      body = paste(
        "## 接触帯は群集の急変より高い利用可能性帯に位置する",
        "25 km以内の局所辺を端点非共有で評価すると、5種共通支持域で利用可能性の平均水準だけがlog lossを0.0076改善した。利用可能性差、有効種数差、Hellinger群集ターンオーバー、高山性種構成比差はいずれも改善しなかった。",
        "局所辺の基準AUCは全国セルの0.878に対して0.681である。これは同じ距離・環境背景に近い境界という難しい問題を予測しているためで、AUC低下自体は失敗ではない。むしろ群集変化量に増分がないという否定的結果を保持外で示している。",
        sep = "\n\n"
      )
    ),
    list(id = "edge_chart_block", type = "chart", chartId = "chart_edge_cv"),
    list(
      id = "horticulture_heading", type = "markdown", sourceId = "source_candidates",
      body = paste(
        "## 西側の有色飛び地は候補になるが、園芸由来の証拠はまだ収束しない",
        "人為変数を一切使わず、25 km近傍が90%以上白で、環境＋空間モデルも有色を低確率としたセルを順位付けすると19候補が得られ、15候補は経度136.5°未満だった。この空間的非対称性は園芸逸出仮説を検討する理由になる。",
        "ただし候補群はマッチした自然有色対照より人口曝露・道路アクセス・濃色残差・早咲残差が一貫して高くなかった。上位ほど早咲・濃色・林縁が強まる方向は一部設定で見えたが、マッチ感度と多重性補正を通じて安定しない。道路アクセスは上位ほど低い方向で、単純な道路沿い逸出予測に反する。",
        sep = "\n\n"
      )
    ),
    list(id = "candidate_chart_block", type = "chart", chartId = "chart_candidates"),
    list(id = "facet_table_block", type = "table", tableId = "table_facets"),
    list(
      id = "scope_heading", type = "markdown",
      body = paste(
        "## 応答・データ・推定対象",
        "花弁領域は被写体抽出後に著者が手動確認・指定した。白／有色の境界は地理・環境・マルハナバチ・人為変数を見ずにCIELAB a*のGaussian mixtureで推定した。白花のa*は色測定ノイズとして扱い、色素強度解析から除外した。",
        "主な局所単位は座標揺れを吸収する1-kmセルで、1,923画像は1,307セル（白633、有色586、混在88）になった。ENMevalは各種内順位へ変換して種間スケール差を抑え、総利用可能性、Hill N1、有色高山性3種比率、Hellinger組成距離を別の推定対象にした。",
        sep = "\n\n"
      )
    ),
    list(
      id = "method_heading", type = "markdown",
      body = paste(
        "## モデル仕様と検証",
        "全国基礎は、白／有色のBernoulli SPDEと、有色花だけのGaussian SPDEで、空間のみ対環境＋空間を比較した。東西区分、マルハナバチ、人為変数、DOYは自然背景から除外した。",
        "群集閾値は、環境＋空間GAMを基準に、各群集軸の線形・平滑・折れ線モデルを5-fold空間交差検証した。折れ点は訓練内だけで選んだ。局所接触帯では、テスト辺の両端が訓練辺に現れないよう分割した。",
        "園芸候補の順位は局所花色逸脱と自然有色確率だけで決め、各ファセットは非再利用対照との別々の差として100-kmブロックbootstrapし、BH補正した。ファセットの同時成立や合算点数は要求していない。",
        sep = "\n\n"
      )
    ),
    list(
      id = "limitations_heading", type = "markdown",
      body = paste(
        "## 限界と頑健性",
        "高山性3種のENMeval支持域は全国を覆わず、5種共通支持域は584セル（44.7%）に限られる。支持域外0解析と共通支持域解析が一致しないため、全国群集効果の一般化は避ける。",
        "群集軸間の最大相関は0.968である。これは同じ群集構造の別表現なので、共線性を理由に排除せず別モデルで問いを分けたが、相互に独立な係数とは読めない。環境のみの最大VIFは3.57だった。",
        "現在の地形解析は端点の標高・地形差であり、山稜・谷・河川などの介在障壁を測っていない。接触帯と地理的障壁の仮説を正式に検証するには、DEMから線分上最大標高・累積勾配・地形抵抗距離を作る必要がある。",
        "園芸候補19セルはすべて独立地点・独立年の反復差がなく、画像だけから栽培品種、逸出、遺伝子浸透を同定できない。現段階は候補抽出と反証可能な予測の評価である。",
        sep = "\n\n"
      )
    ),
    list(id = "audit_table_block", type = "table", tableId = "table_audit"),
    list(
      id = "next_steps_heading", type = "markdown",
      body = paste(
        "## 次に優先する解析",
        "1. DEM由来の介在障壁指標を局所辺へ追加し、群集変化量と障壁を同じ保持外枠で比較する。",
        "2. 5種共通支持域の境界をモデル投影域として明示し、支持域外0・完全ケース・2広域種のみの三者感度を図示する。",
        "3. 東京―長野を事後的な一本の線として選ばず、全国で応答盲検に抽出した接触帯群から代表断面を選び、利用可能性・Hill N1・高山性種比率・花色確率を同じ距離軸に並べる。",
        "4. 西側19候補は現地確認の優先順位として使い、栽培痕跡、集落・寺社・庭園からの距離、植栽履歴、遺伝データのどれか一つでも外部証拠を追加する。",
        sep = "\n"
      )
    ),
    list(
      id = "questions_heading", type = "markdown",
      body = paste(
        "## 残る問い",
        "- 共通支持域で見えた利用可能性勾配は、広域2種の総量か、高山性3種が加わる群集構造か。",
        "- 接触帯の位置は現在の環境より、歴史的分布・遺伝構造・山地障壁で固定されているか。",
        "- 西側飛び地の早咲傾向は真の形質差か、SNS撮影日の選択・栽培環境のフェノロジー差か。",
        sep = "\n"
      )
    )
  )
)

snapshot <- list(
  version = 1L,
  generatedAt = generated_at,
  status = "ready",
  datasets = list(
    metric_auc = metric_auc,
    metric_spde = metric_spde,
    metric_candidates = metric_candidates,
    presence_smooth = presence_smooth,
    presence_all = presence,
    availability_curve = availability_curve,
    edge_summary = edge,
    spde = spde,
    candidate_plot = candidate_plot,
    facet_table = facet_table,
    audit = audit
  )
)

artifact <- list(
  surface = "report", manifest = manifest, snapshot = snapshot, sources = sources
)
jsonlite::write_json(
  artifact, file.path(output_dir, "community_threshold_report_artifact.json"),
  auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 10
)
cat(file.path(output_dir, "community_threshold_report_artifact.json"), "\n")
