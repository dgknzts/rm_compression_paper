library(ggplot2)

rm_colors <- c("Non-RM" = "#298c8c", "RM" = "#800074")

theme_scientific <- function(base_size = 12, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "grey20", linewidth = 0.5),
      axis.ticks = element_line(color = "grey20", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text = element_text(color = "grey20", size = rel(0.9)),
      axis.title = element_text(color = "grey10", size = rel(1.1), face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(color = "grey10", size = rel(1.0), face = "bold"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_text(face = "bold", size = rel(1.0), color = "grey10"),
      legend.text = element_text(size = rel(0.9), color = "grey20"),
      legend.position = "bottom",
      plot.title = element_text(size = rel(1.3), face = "bold", hjust = 0.5,
                                color = "grey10", margin = margin(b = 10)),
      panel.spacing = unit(1, "lines"),
      plot.margin = margin(20, 20, 20, 20)
    )
}
