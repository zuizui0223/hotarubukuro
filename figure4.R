## =========================================================
## GAM figures with draw()
##  - no titles
##  - points (rug)
##  - p-values extracted from summary
## =========================================================
library(gratia)
library(mgcv)
library(ggplot2)
library(dplyr)

## =========================================================
## output dirs
## =========================================================
fig_root <- file.path(out_root, "GAM_figs_pub")
dir_main <- file.path(fig_root, "Main_weighted")
dir_supp <- file.path(fig_root, "Supp_unweighted")

dir.create(dir_main, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_supp, recursive = TRUE, showWarnings = FALSE)

## =========================================================
## theme
## =========================================================
theme_pub <- function(){
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(face="bold"),
      plot.title = element_blank()
    )
}

## =========================================================
## helper: extract smooth p-values
## =========================================================
get_smooth_p <- function(model, label){
  s <- summary(model)$s.table
  rn <- rownames(s)
  i <- grep(label, rn, fixed = TRUE)
  if (!length(i)) return(NA)
  s[i[1], "p-value"]
}

fmt_p <- function(p){
  if (is.na(p)) return("p = NA")
  if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))
}

## =========================================================
## 1D smooths (DOY / elevation / pop)
## =========================================================
plot_1d_draw <- function(model, term, xlab, outfile){
  
  pval <- fmt_p(get_smooth_p(model, term))
  
  p <- draw(
    model,
    select = term,
    rug = TRUE,
    residuals = TRUE,          # ← + を消す
    residuals.colour = "grey50",
    residuals.alpha  = 0.35,
    residuals.size   = 0.8
  ) +
    labs(x = xlab, y = "partial effect") +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = pval,
      hjust = 1.1, vjust = 1.3,
      size = 4
    ) +
    theme_pub()
  
  ggsave(outfile, p, width = 6, height = 4)
}

## weighted
plot_1d_draw(
  m_dark_w, "s(DOY)",
  "Day of year",
  file.path(dir_main, "Fig_main_a_DOY_weighted.pdf")
)

plot_1d_draw(
  m_dark_w, "s(elevation_z)",
  "Elevation (z)",
  file.path(dir_main, "Fig_main_b_elevation_weighted.pdf")
)

plot_1d_draw(
  m_dark_w, "s(pop_density)",
  "Human population density (z)",
  file.path(dir_main, "Fig_main_c_population_weighted.pdf")
)

## unweighted
plot_1d_draw(
  m_dark_unw, "s(DOY)",
  "Day of year",
  file.path(dir_supp, "FigS_DOY_unweighted.pdf")
)

plot_1d_draw(
  m_dark_unw, "s(elevation_z)",
  "Elevation (z)",
  file.path(dir_supp, "FigS_elevation_unweighted.pdf")
)

plot_1d_draw(
  m_dark_unw, "s(pop_density)",
  "Human population density (z)",
  file.path(dir_supp, "FigS_population_unweighted.pdf")
)

## =========================================================
## 2D interaction: ti(elevation_z, pop_density)
## =========================================================
plot_ti_draw <- function(model, outfile){
  
  pval <- fmt_p(get_smooth_p(model, "ti(elevation_z,pop_density)"))
  
  p <- draw(
    model,
    select = "ti(elevation_z,pop_density)",
    contour = TRUE
  ) +
    labs(
      x = "Elevation (z)",
      y = "Human population density (z)",
      fill = "partial effect"
    ) +
    annotate("text", x = Inf, y = Inf,
             label = pval,
             hjust = 1.1, vjust = 1.3, size = 4) +
    theme_pub()
  
  ggsave(outfile, p, width = 7, height = 6)
}

plot_ti_draw(
  m_dark_w,
  file.path(dir_main, "Fig_main_d_ti_weighted.pdf")
)

plot_ti_draw(
  m_dark_unw,
  file.path(dir_supp, "FigS_ti_unweighted.pdf")
)

## =========================================================
## 2D spatial smooth
## =========================================================
plot_space_draw <- function(model, outfile){
  
  pval <- fmt_p(get_smooth_p(model, "s(longitude,latitude)"))
  
  p <- draw(
    model,
    select = "s(longitude,latitude)",
    contour = TRUE
  ) +
    labs(
      x = "Longitude",
      y = "Latitude",
      fill = "partial effect"
    ) +
    annotate("text", x = Inf, y = Inf,
             label = pval,
             hjust = 1.1, vjust = 1.3, size = 4) +
    theme_pub()
  
  ggsave(outfile, p, width = 7, height = 6)
}

plot_space_draw(
  m_dark_w,
  file.path(dir_main, "Fig_main_e_space_weighted.pdf")
)

plot_space_draw(
  m_dark_unw,
  file.path(dir_supp, "FigS_space_unweighted.pdf")
)

cat("✅ draw() figures exported successfully\n")

