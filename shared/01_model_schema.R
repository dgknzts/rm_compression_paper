library(ggplot2)
library(dplyr)
library(patchwork)

W_top         <- 0.35
W_bot_noRM    <- 0.22
W_bot_RM      <- 0.50
bar_h         <- 0.55
col_circle    <- "grey80"
col_bar       <- "black"
col_compress  <- "#4A90D9"
col_integrate <- "#DC143C"
circle_size   <- 22
line_width    <- 1.8

make_bars <- function(xs, y, widths, height = bar_h) {
  data.frame(
    xmin = xs - widths / 2,
    xmax = xs + widths / 2,
    ymin = y - height / 2,
    ymax = y + height / 2
  )
}

make_circles <- function(xs, y) {
  data.frame(x = xs, y = y)
}


# ===== LEFT: noRM — Compression =====

top_xL <- c(-2, 0, 2)
bot_xL <- top_xL

lines_L <- data.frame(x = top_xL, xend = bot_xL, y = 2, yend = 0)

bars_L <- bind_rows(
  make_bars(top_xL, 2, rep(W_top,     3)),
  make_bars(bot_xL, 0, rep(W_bot_noRM, 3))
)
circ_L <- bind_rows(
  make_circles(top_xL, 2),
  make_circles(bot_xL, 0)
)

fig_L <- ggplot() +
  geom_segment(data = lines_L,
               aes(x = x, xend = xend, y = y, yend = yend),
               colour = col_compress, linewidth = line_width) +
  geom_point(data = circ_L, aes(x = x, y = y),
             shape = 21, size = circle_size,
             fill = col_circle, colour = "black", stroke = 1) +
  geom_rect(data = bars_L,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = col_bar) +
  annotate("text", x = -2.35, y = 1, label = "\u03b1",
           colour = col_compress, size = 9, fontface = "italic") +
  annotate("text", x = 0, y = 2.75, label = "Presented",
           size = 4.5, fontface = "bold", colour = "grey30") +
  annotate("text", x = 0, y = -0.75, label = "Perceived (narrower)",
           size = 4.5, fontface = "bold", colour = "grey30") +
  coord_fixed(xlim = c(-3.3, 3.3), ylim = c(-1.1, 3.1)) +
  labs(title = "noRM: Compression") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 15,
                                  face = "bold", colour = "black",
                                  margin = margin(b = 10)))


# ===== RIGHT: RM — Integration =====

top_xR <- c(-2, 0, 2)
bot_xR <- c(-1, 1)

lines_R <- data.frame(
  x    = c(-2,  0,  0,  2),
  xend = c(-1, -1,  1,  1),
  y    = 2,
  yend = 0
)

bars_R <- bind_rows(
  make_bars(top_xR, 2, rep(W_top,   3)),
  make_bars(bot_xR, 0, rep(W_bot_RM, 2))
)
circ_R <- bind_rows(
  make_circles(top_xR, 2),
  make_circles(bot_xR, 0)
)

fig_R <- ggplot() +
  geom_segment(data = lines_R,
               aes(x = x, xend = xend, y = y, yend = yend),
               colour = col_integrate, linewidth = line_width) +
  geom_point(data = circ_R, aes(x = x, y = y),
             shape = 21, size = circle_size,
             fill = col_circle, colour = "black", stroke = 1) +
  geom_rect(data = bars_R,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = col_bar) +
  annotate("text", x = -1.55, y = 1, label = "\u03b3",
           colour = col_integrate, size = 9, fontface = "italic") +
  annotate("text", x = 0, y = 2.75, label = "Presented",
           size = 4.5, fontface = "bold", colour = "grey30") +
  annotate("text", x = 0, y = -0.75, label = "Perceived (wider)",
           size = 4.5, fontface = "bold", colour = "grey30") +
  coord_fixed(xlim = c(-3.3, 3.3), ylim = c(-1.1, 3.1)) +
  labs(title = "RM: Integration") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 15,
                                  face = "bold", colour = "black",
                                  margin = margin(b = 10)))


# ===== Combine =====

fig <- fig_L | fig_R

dir.create("shared/outputs", recursive = TRUE, showWarnings = FALSE)
ggsave("shared/outputs/model_schema_mechanism.png", fig,
       width = 11, height = 5.5, dpi = 300, bg = "white")
