library(tidyverse)

source("shared/themes.R")

joint <- read.csv("exp1/outputs/tables/joint_width.csv") %>%
  mutate(cond = if_else(cond == "Non-RM", "no-RM", cond),
         cond = factor(cond, levels = c("1-bar", "no-RM", "RM")))

cond_colors <- c("1-bar" = "grey50", "no-RM" = "#298c8c", "RM" = "#800074")

iso_arc <- function(radii, n = 200) {
  expand_grid(rmse = radii, theta = seq(0, pi, length.out = n)) %>%
    mutate(x = rmse * cos(theta), y = rmse * sin(theta)) %>%
    group_by(rmse) %>%
    arrange(theta, .by_group = TRUE) %>%
    ungroup()
}

centroids <- joint %>%
  group_by(cond) %>%
  summarise(
    n = sum(!is.na(width_bias) & !is.na(width_sd)),
    x_m = mean(width_bias, na.rm = TRUE),
    y_m = mean(width_sd,   na.rm = TRUE),
    x_se = sd(width_bias,  na.rm = TRUE) / sqrt(n),
    y_se = sd(width_sd,    na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

w_max <- max(sqrt(joint$width_bias^2 + joint$width_sd^2), na.rm = TRUE)
w_radii <- pretty(c(0, w_max), n = 5)
w_radii <- w_radii[w_radii > 0]
w_arcs <- iso_arc(w_radii)

fig <- ggplot() +
  geom_path(data = w_arcs, aes(x = x, y = y, group = rmse),
            linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(data = joint,
             aes(x = width_bias, y = width_sd, color = cond),
             alpha = 0.45, size = 1.6) +
  geom_segment(data = centroids,
               aes(x = 0, y = 0, xend = x_m, yend = y_m, color = cond),
               linewidth = 0.5, alpha = 0.7, show.legend = FALSE) +
  geom_errorbar(data = centroids,
                aes(x = x_m, ymin = y_m - y_se, ymax = y_m + y_se),
                width = 0, linewidth = 1, color = "black", inherit.aes = FALSE) +
  geom_errorbarh(data = centroids,
                 aes(y = y_m, xmin = x_m - x_se, xmax = x_m + x_se),
                 height = 0, linewidth = 1, color = "black", inherit.aes = FALSE) +
  geom_point(data = centroids, aes(x = x_m, y = y_m, fill = cond),
             shape = 21, size = 4, stroke = 1, color = "black",
             inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(values = cond_colors, name = NULL) +
  scale_fill_manual(values = cond_colors, name = NULL, guide = "none") +
  coord_fixed(xlim = c(-w_max, w_max), ylim = c(0, w_max * 1.05)) +
  labs(x = "Relative width deviation",
       y = "Relative width SD",
       title = "Width: deviation vs precision (iso-RMSE arcs)") +
  theme_scientific() +
  theme(legend.position = "bottom")

dir.create("exp1/outputs", recursive = TRUE, showWarnings = FALSE)
ggsave("exp1/outputs/figures/joint_width.png", fig,
       width = 7, height = 7, dpi = 300, bg = "white")

cat("Saved exp1/outputs/joint_width.png\n")
