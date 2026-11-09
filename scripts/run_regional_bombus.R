# Fit observation-level pigmentation models on the fixed central Honshu footprint.
# Inputs are the standard phenotype and 1-km cell stages of run_pipeline.py.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
observations <- read.csv(args[1])
cells <- read.csv(args[2])
footprint <- readLines("config/central_honshu_cells.txt")
species <- c("ardens", "diversus", "beaticola", "consobrinus", "honshuensis")
scores <- as.matrix(cells[paste0("bee_", species)])
id <- paste0("cell-1km-", floor(observations$x_km), "_", floor(observations$y_km))
index <- match(id, cells$exact_site_id)
stopifnot(nrow(cells) == 1305, nrow(observations) == 1922,
          sum(observations$pigmented_mixture50) == 956,
          !anyDuplicated(cells$exact_site_id), !anyNA(index), all(is.finite(scores)),
          all(tabulate(index, nrow(cells)) == cells$n_observations),
          length(footprint) == 584, !anyDuplicated(footprint))

# Rank across national cells before subsetting; coefficients are per 0.1 in rank.
ranks <- apply(scores, 2, rank) / nrow(cells)
colnames(ranks) <- species
observations$all5 <- rowMeans(ranks)[index] / 0.1
observations$widespread <- rowMeans(ranks[, 1:2])[index] / 0.1
observations$montane <- rowMeans(ranks[, 3:5])[index] / 0.1
observations$ardens <- ranks[index, "ardens"] / 0.1
observations$diversus <- ranks[index, "diversus"] / 0.1
observations$year <- factor(observations$year)
observations$cell <- factor(id)
central <- droplevels(observations[id %in% footprint, ])
stopifnot(nrow(central) == 875, nlevels(central$cell) == 583)

library(mgcv)
controls <- c("env_Temperature_PC1", "env_precip_PC1", "env_TemperatureSeasonality",
              "env_PrecipSeasonality", "env_topo_PC1", "env_soil_PC1", "env_soil_PC2",
              "env_RSDS", "year", "s(DOY,k=5)", "s(x_km,y_km,k=40)", "s(cell,bs='re')")
designs <- list(five = "all5", five_elevation = "all5",
                groups = c("widespread", "montane"), ardens = c("ardens", "montane"),
                diversus = c("diversus", "montane"), joint = c("ardens", "diversus", "montane"))
fit_design <- function(name) {
  predictors <- designs[[name]]
  terms <- c(predictors, controls)
  if (name != "five") terms <- c(terms, "s(elevation,k=5)")
  model <- bam(reformulate(terms, "pigmented_mixture50"), data = central,
               family = binomial(), method = "fREML", discrete = TRUE)
  stopifnot(model$converged, nobs(model) == nrow(central))
  contrast <- diag(length(predictors))
  rownames(contrast) <- predictors
  if (name == "joint") contrast <- rbind(contrast, diversus_minus_ardens = c(-1, 1, 0))
  if (name == "groups") contrast <- rbind(contrast, montane_minus_widespread = c(-1, 1))
  estimate <- drop(contrast %*% coef(model)[predictors])
  covariance <- vcov(model, unconditional = TRUE)[predictors, predictors, drop = FALSE]
  se <- sqrt(diag(contrast %*% covariance %*% t(contrast)))
  data.frame(model = name, term = rownames(contrast), photos = nobs(model),
             cells = nlevels(central$cell), OR = exp(estimate),
             lower = exp(estimate - 1.96 * se), upper = exp(estimate + 1.96 * se),
             p = 2 * pnorm(-abs(estimate / se)), row.names = NULL)
}
effects <- do.call(rbind, lapply(names(designs), fit_design))
dir.create(dirname(args[3]), recursive = TRUE, showWarnings = FALSE)
write.csv(effects, args[3], row.names = FALSE)

# Figure 3 compares coefficients within their respective models; all use the same rank unit.
keep <- with(effects, model %in% c("five", "five_elevation", "groups", "joint") &
               !grepl("_minus_", term) & !(model == "joint" & term == "montane"))
figure <- effects[keep, ]
labels <- c("Five-species mean", "Five-species mean", "Widespread pair", "Montane trio",
            "B. ardens", "B. diversus")
rows <- c(9, 7, 5.1, 4.3, 2.4, 1.6)
png(file.path(dirname(args[3]), "regional_bombus.png"), width = 1800, height = 1764, res = 270)
par(mar = c(4.3, 0.4, 0.4, 0.6), xaxs = "i", yaxs = "i", family = "sans")
plot.new()
plot.window(xlim = log(c(0.026, 4.4)), ylim = c(0.9, 10.2))
ticks <- c(0.25, 0.5, 1, 2, 4)
axis(1, at = log(ticks), labels = ticks, pos = 1.1, tck = -0.012, cex.axis = 0.9)
mtext("Odds ratio per 0.1 increase in suitability rank", side = 1, line = 2.3)
segments(0, 1.15, 0, 9.3, col = "grey65", lty = 2)
text(log(0.028), c(9.7, 7.7, 5.8, 3.1),
     c("Five species", "Five species + elevation", "Groups + elevation", "Species + elevation"),
     adj = 0, font = 2, cex = 0.93)
text(log(0.033), rows, labels, adj = 0, font = c(1, 1, 1, 1, 3, 3), cex = 0.9)
segments(log(figure$lower), rows, log(figure$upper), rows, lwd = 2, col = "#31566B")
points(log(figure$OR), rows, pch = 19, cex = 1.2, col = "#204859")
dev.off()
