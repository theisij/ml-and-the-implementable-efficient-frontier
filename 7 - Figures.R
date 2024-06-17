# Figures -------------------------
output_fig <- function(path, name, format, width, height) {
  path <- paste0(path, "/", name)   # <- change path as desired 
  if (format == "tex") {
    tikz(str_c(path, ".tex"), width = width, height = height) 
  }
  if (format == "pdf") {
    pdf(str_c(path, ".pdf"), width = width, height = height)
  }
  if (format == "jpg") {
    w_pix <- width / (2.54 / 96 / 2.54)
    h_pix <- height / (2.54 / 96 / 2.54)
    jpeg(str_c(path, ".jpg"), width = w_pix, height = h_pix)
  }
  if (format == "eps") {
    cairo_ps(filename = str_c(path, ".eps"),
             width = width, height = height)
  }
  if (format == "tiff") {
    tiff(filename = str_c(path, ".tiff"), units="in", width=width, height=height, res=500)
  }
}
# For Paper -----------------------------------------
# Efficient frontier
output$ef + theme(text = element_text(size=txt_size))
ggsave("Figures/ief_by_wealth.pdf", width = fig_w, height = fig_h, units = "in")

# Efficient frontier all methods
output$ef_all + theme(text = element_text(size=txt_size))
ggsave("Figures/ief_by_method.pdf", width = fig_w, height = fig_h, units = "in")

# Portfolios: Cumulative Performance
output$ts
ggsave("Figures/cumret_pf.pdf", width = fig_w, height = fig_h, units = "in")

# Portfollios: Stats over time
output$comp_stats + theme(text = element_text(size=txt_size))
ggsave("Figures/stats_ts.pdf", width = fig_w, height = fig_h*1.5, units = "in")

# Example: Apple vs. Xerox
output$example +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme(
    axis.text.y = element_text(size = 8)
  )
ggsave("Figures/example_weights.pdf", width = fig_w, height = fig_h*1.5, units = "in")

# Feature Importance
output$feature_importance + theme(text = element_text(size=txt_size))
ggsave("Figures/feat_imp.pdf", width = fig_w, height = fig_h, units = "in")

# Optimal Hyper-parameters [Expected return]
output$er_tuning
ggsave("Figures/optimal_hps_er.pdf", width = fig_w, height = fig_h, units = "in")

# Optimal Hyper-parameters
output$portfolio_tuning
ggsave("Figures/optimal_hps.pdf", width = fig_w, height = fig_h, units = "in")

# Feature importance - Returns
output$fi_returns + theme(
  axis.text.x = element_text(angle = 360-20, hjust = 0, vjust = 1),
  text = element_text(size = 8),
  axis.title = element_text(size = 7),
  legend.text = element_text(size = 5),  # Adjust text size
  legend.title = element_text(size = 8),
  legend.key.size = unit(0.5, "cm")
)
ggsave("Figures/fi_returns.pdf", width = fig_w, height = fig_h, units = "in")

# Feature auto-correlation
output$ar1 + theme(axis.text.y = element_text(size=4))
ggsave("Figures/feature_ar1.pdf", width = fig_w, height = fig_h*2, units = "in")

# Efficient frontier with trading cost
output$cf_ef_tc
ggsave("Figures/ef_cf_tc.pdf", width = fig_w, height = fig_h, units = "in")

# Efficient frontier without trading cost
output$cf_ef_markowitz
ggsave("Figures/ef_cf_no_tc.pdf", width = fig_w, height = fig_h, units = "in")

# Performance across size distribution 
output$by_size
ggsave("Figures/by_size.pdf", width = fig_w, height = fig_h*1.5, units = "in")

# Shorting costs
output$shorting
ggsave("Figures/shorting.pdf", width = fig_w, height = fig_h, units = "in")

# RFF example
output_fig(path=output_path_fig, name = "rff_ex", format = format, width = fig_w, height = fig_h*1.5)
output$rff_specific0 + theme(strip.text.x = element_text(size = 8))
dev.off()

# Relation between liquidity and portfolio weight
output$w_liq
ggsave("Figures/w_liq.pdf", width = fig_w, height = fig_h, units = "in")

# Simulations
output$simulations + theme(text = element_text(size=txt_size))
ggsave("Figures/simulations.pdf", width = fig_w, height = fig_h, units = "in")


# FOR SLIDES ----------------
if (FALSE) {
  # EF slides
  output_fig(path=output_path_fig, name = "ef_slide0", format = format, width = w, height = h)
  output$ef_slide0 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide1", format = format, width = w, height = h)
  output$ef_slide1 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide2", format = format, width = w, height = h)
  output$ef_slide2 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide3", format = format, width = w, height = h)
  output$ef_slide3 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide4", format = format, width = w, height = h)
  output$ef_slide4 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide5", format = format, width = w, height = h)
  output$ef_slide5 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide6", format = format, width = w, height = h)
  output$ef_slide6 + theme(text = element_text(size=txt_size))
  dev.off()
  output_fig(path=output_path_fig, name = "ef_slide7", format = format, width = w, height = h)
  output$ef_slide7 + theme(text = element_text(size=txt_size))
  dev.off()
}