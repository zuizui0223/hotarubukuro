## ============================================================
## Packages
## ============================================================
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(ggspatial)
library(scales)
library(patchwork)
library(tidyr)

## ============================================================
## Data preparation
## ============================================================

df_plot <- df |>
  left_join(
    df2 |> select(longitude, latitude, Pigment),
    by = c("longitude", "latitude")
  ) |>
  filter(
    is.finite(longitude),
    is.finite(latitude),
    is.finite(R), is.finite(G), is.finite(B),
    is.finite(Pigment)
  )

## ============================================================
## RGB → hex (observed colour)
## ============================================================

rng <- range(c(df_plot$R, df_plot$G, df_plot$B), na.rm = TRUE)
is_01 <- (rng[1] >= 0 && rng[2] <= 1)

R <- df_plot$R
G <- df_plot$G
B <- df_plot$B

if (is_01) {
  R <- round(R * 255)
  G <- round(G * 255)
  B <- round(B * 255)
}

df_plot$col_hex <- rgb(R, G, B, maxColorValue = 255)

## ============================================================
## Map data (Japan)
## ============================================================

jp <- ne_countries(
  scale = "large",
  country = "Japan",
  returnclass = "sf"
)

## ============================================================
## (A) RGB map (Observed colour)
## ============================================================

map_rgb <- ggplot() +
  geom_sf(
    data = jp,
    fill = "grey15",
    color = "grey40",
    linewidth = 0.25
  ) +
  geom_point(
    data = df_plot,
    aes(x = longitude, y = latitude, color = col_hex),
    size = 2.2,
    alpha = 1,
    stroke = 0
  ) +
  scale_color_identity() +
  coord_sf(
    xlim = c(128, 146),
    ylim = c(30, 46),
    expand = FALSE
  ) +
  annotate(
    "text",
    x = 128.3, y = 45.8,
    label = "(A)",
    hjust = 0,
    vjust = 1,
    size = 6,
    fontface = "bold",
    color = "white"
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA)
  )

## ============================================================
## RGB colour wheel (legend for A)
## ============================================================

rgb_wheel <- expand_grid(
  r = seq(0, 1, length.out = 200),
  g = seq(0, 1, length.out = 200)
) |>
  mutate(
    b = pmax(0, 1 - r - g),
    col = rgb(r, g, b)
  ) |>
  filter(r + g + b <= 1)

p_rgb_wheel <- ggplot(rgb_wheel) +
  geom_point(aes(r, g, color = col), size = 0.7) +
  scale_color_identity() +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA)
  )

map_rgb_final <- map_rgb +
  inset_element(
    p_rgb_wheel,
    left   = 0.74,
    bottom = 0.02,
    right  = 0.98,
    top    = 0.32,
    align_to = "panel"
  )

## ============================================================
## (B) Latent Pigment map
## ============================================================

pig_min <- min(df_plot$Pigment, na.rm = TRUE)
pig_max <- max(df_plot$Pigment, na.rm = TRUE)

# 凡例に出したい代表値（実数値）
legend_breaks <- c(-10, 0, 20, 40)

pigment_cols <- c("white", "#f4b6c2", "#d36aa3", "#7b1e7a")

map_pigment_final <- ggplot() +
  geom_sf(
    data = jp,
    fill = "grey15",
    color = "grey40",
    linewidth = 0.25
  ) +
  geom_point(
    data = df_plot,
    aes(x = longitude, y = latitude, color = Pigment),
    size = 2.2,
    alpha = 0.9
  ) +
  scale_color_gradientn(
    colours = pigment_cols,
    limits  = c(pig_min, pig_max),
    breaks  = legend_breaks,
    labels  = legend_breaks,
    oob     = scales::squish,
    name    = "Latent pigment index"
  ) +
  coord_sf(
    xlim = c(128, 146),
    ylim = c(30, 46),
    expand = FALSE
  ) +
  annotate(
    "text",
    x = 128.3, y = 45.8,
    label = "(B)",
    hjust = 0,
    vjust = 1,
    size = 6,
    fontface = "bold",
    color = "white"
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.position  = "right",
    legend.text  = element_text(color = "white", size = 12),
    legend.title = element_text(color = "white", size = 14)
  )

## ============================================================
## Combine panels
## ============================================================

p_combined <- map_rgb_final + map_pigment_final +
  plot_layout(ncol = 2)

p_combined