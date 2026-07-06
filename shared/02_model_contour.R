library(dplyr)
library(tidyr)
library(ggplot2)

source("shared/model_functions.R")

W_act <- 0.4
pool_32 <- 3 / 2

contour_df <- expand.grid(
  gamma = seq(-0.2, 1.2, length.out = 200),
  alpha = seq(0.65, 1.15, length.out = 200)
) %>%
  mutate(W_dev = alpha * pool_32^gamma * W_act - W_act)

fig_contour <- ggplot(contour_df, aes(x = gamma, y = alpha, z = W_dev)) +
  geom_raster(aes(fill = W_dev)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey80", linewidth = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey80", linewidth = 0.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "#FFFFFF", high = "#B2182B",
                       midpoint = 0, limits = c(-0.25, 0.25),
                       oob = scales::squish,
                       breaks = c(-0.2, 0, 0.2),
                       labels = c("−", "0", "+")) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "γ (pooling parameter)",
       y = "α\n(size-scaling parameter)",
       fill = "Predicted\nwidth\ndeviation") +
  my_theme +
  theme(text = element_text(family = "Arial", size = 14, face = "bold", colour = "black"),
        plot.title = element_text(family = "Arial", size = 14, face = "bold", colour = "black"),
        plot.subtitle = element_text(family = "Arial", size = 14, face = "plain", colour = "black"),
        axis.title = element_text(family = "Arial", size = 14, face = "bold", colour = "black"),
        axis.text = element_text(family = "Arial", size = 14, face = "bold", colour = "black"),
        legend.text = element_text(family = "Arial", size = 14, face = "bold", colour = "black"),
        legend.title = element_text(family = "Arial", size = 14, face = "bold", colour = "black", hjust = 0.5),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 15))

dir.create("shared/outputs", recursive = TRUE, showWarnings = FALSE)

w_in <- 6.2
h_in <- 3
ggsave("shared/outputs/model_contour.svg", fig_contour,
       width = w_in, height = h_in, device = "svg")

ggsave("shared/outputs/model_contour.png", fig_contour,
       width = w_in, height = h_in, dpi = 300, bg = "white")
