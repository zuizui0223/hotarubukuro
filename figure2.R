## =========================================================
## Bombus SDMs (5 spp) + Species count map
##  - 3 × 2 panel
##  - SDMs: cool colors (viridis)
##  - Species count: warm colors (YlOrRd)
## =========================================================

library(terra)
library(ggplot2)
library(patchwork)
library(dplyr)
library(viridis)

## -------------------------
## directories
## -------------------------
bee_dir <- "~/Desktop/bee5"
outfig  <- file.path(bee_dir, "Fig_Bombus_SDMs_and_SpeciesCount.pdf")

## -------------------------
## species list
## -------------------------
sp_names <- c(
  "ardens",
  "beaticola",
  "consobrinus",
  "diversus",
  "honshuensis"
)

## -------------------------
## load SDM rasters
## -------------------------
sdm_list <- lapply(sp_names, function(sp){
  f <- list.files(
    bee_dir,
    pattern = paste0(sp, ".*\\.tif$"),
    full.names = TRUE
  )
  if (length(f) == 0) stop("SDM tif not found for: ", sp)
  rast(f[1])
})
names(sdm_list) <- sp_names

## -------------------------
## species count (summed suitability)
## -------------------------
r_count <- Reduce(`+`, sdm_list)

## -------------------------
## raster → data.frame
## -------------------------
rast_to_df <- function(r){
  as.data.frame(r, xy = TRUE, na.rm = TRUE) |>
    rename(value = 3)
}

sdm_dfs <- lapply(sdm_list, rast_to_df)
cnt_df  <- rast_to_df(r_count)

## common SDM scale
sdm_lim <- range(
  unlist(lapply(sdm_dfs, \(x) x$value)),
  na.rm = TRUE
)

## -------------------------
## SDM plots (cool colors)
## -------------------------
p_sdm <- lapply(names(sdm_dfs), function(sp){
  
  sp_label <- bquote(italic("B.") ~ italic(.(sp)))
  
  ggplot(sdm_dfs[[sp]], aes(x, y, fill = value)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(
      name   = "Predicted\nsuitability",
      limits = sdm_lim,
      option = "D"
    ) +
    labs(title = sp_label) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
})

## -------------------------
## Species count plot (warm colors)
## -------------------------
p_count <- ggplot(cnt_df, aes(x, y, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_distiller(
    name      = "Species count",
    palette  = "YlOrRd",
    direction = 1
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right"
  )

## -------------------------
## layout (3 × 2) — NO overall title
## -------------------------
final_fig <-
  (p_sdm[[1]] | p_sdm[[2]] | p_sdm[[3]]) /
  (p_sdm[[4]] | p_sdm[[5]] | p_count)

## -------------------------
## save
## -------------------------
ggsave(
  outfig,
  final_fig,
  width  = 12,
  height = 8
)

cat("✅ Figure saved to:\n", outfig, "\n")