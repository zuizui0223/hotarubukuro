
## =========================================================
## 1) Fixed effects forest plot (paper-ready)
## =========================================================

fx <- fit_full$summary.fixed %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  mutate(
    term = gsub("^\\(Intercept\\)$", "intercept", term),
    sig  = (`0.025quant` * `0.975quant`) > 0
  )

## optional: nicer labels (あなたの論文用表記に合わせる)
label_fx <- c(
  intercept   = "intercept",
  z_elev      = "elevation",
  z_rough     = "topographic roughness",
  z_RSDS      = "solar radiation",
  z_TempPC1   = "temperature",
  z_PrecipPC1 = "precipitation",
  z_SoilPC1   = "soil conditions",
  z_BeeRich   = "predicted pollinator richness"
)

fx$term_lab <- ifelse(fx$term %in% names(label_fx), label_fx[fx$term], fx$term)

## drop intercept if you don't want it in the figure
fx_plot <- fx %>% filter(term != "intercept") %>%
  mutate(term_lab = factor(term_lab, levels = rev(unique(term_lab))))

p_fx <- ggplot(fx_plot, aes(x = mean, y = term_lab)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(aes(xmin = `0.025quant`, xmax = `0.975quant`), height = 0.2) +
  geom_point(aes(shape = sig), size = 2.6) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  labs(
    x = "standardized effect (posterior mean ± 95% credible interval)",
    y = NULL,
    title = "Fixed effects (SPDE–INLA)"
  ) +
  theme_pub()

save2(p_fx, dir_main, "Fig_INLA_fixed_effects_forest", w=7.2, h=4.2)