library(tidyverse)
library(patchwork)

source("shared/themes.R")

rm_colors <- c("no-RM" = "#298c8c", "RM" = "#800074")

# --- PANEL A: Spacing Deviation (faceted by spacing category) -----------

df <- read.csv("exp2/data/processed.csv")
contrast_spacing <- read.csv("exp2/outputs/tables/spacing_deviation_rm_contrasts.csv")

spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

spacing_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "no-RM"), levels = c("no-RM", "RM")),
    correct_num = factor(correct_num),
    spacing_category = case_when(
      correct_space <= spacing_cut ~ "Smaller",
      correct_space <= 0.9        ~ "Middle",
      TRUE                          ~ "Larger"
    ),
    spacing_category = factor(spacing_category, levels = c("Smaller", "Middle", "Larger"))
  ) %>%
  drop_na(spacing_deviation)

sp_part <- spacing_data %>%
  group_by(subID, correct_num, spacing_category, rm_type) %>%
  summarise(mean_dev = mean(spacing_deviation), .groups = "drop")

sp_plot <- sp_part %>%
  group_by(correct_num, spacing_category, rm_type) %>%
  summarise(n = n(), grand_mean = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n),
            t_crit = qt(0.975, n - 1),
            ci_lower = grand_mean - t_crit * se,
            ci_upper = grand_mean + t_crit * se, .groups = "drop")

sp_stars <- contrast_spacing %>%
  mutate(
    spacing_category = factor(spacing_category, levels = c("Smaller", "Middle", "Larger")),
    correct_num = factor(correct_num),
    sig = case_when(p_fdr < 0.001 ~ "***", p_fdr < 0.01 ~ "**",
                    p_fdr < 0.05 ~ "*", TRUE ~ "")
  ) %>%
  filter(sig != "") %>%
  left_join(sp_plot %>% group_by(spacing_category, correct_num) %>%
              summarise(y_pos = max(ci_upper) + 0.015, .groups = "drop"),
            by = c("spacing_category", "correct_num"))

panel_a <- ggplot(sp_plot, aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1, linewidth = 0.5, position = position_dodge(0.4)) +
  geom_point(size = 2.5, position = position_dodge(0.4)) +
  geom_text(data = sp_stars, aes(x = correct_num, y = y_pos, label = sig),
            color = "black", size = 5, inherit.aes = FALSE) +
  facet_wrap(~spacing_category, labeller = as_labeller(function(x) paste("Spacing:", x))) +
  scale_color_manual(values = rm_colors, name = NULL) +
  labs(x = "Set Size", y = "Spacing Deviation (°)") +
  theme_scientific() +
  theme(legend.position = c(0.12, 0.18))


# --- PANEL B: Joint bias scatter (width × spacing) -----------------

joint <- read.csv("exp2/outputs/tables/joint_width_spacing.csv") %>%
  mutate(cond = if_else(cond == "Non-RM", "no-RM", cond),
         cond = factor(cond, levels = c("1-bar", "no-RM", "RM")))

bias_dat <- joint %>% filter(cond %in% c("no-RM", "RM")) %>% droplevels()

bias_centroids <- bias_dat %>%
  group_by(cond) %>%
  summarise(
    n = sum(!is.na(width_bias) & !is.na(spacing_bias)),
    x_m = mean(width_bias,   na.rm = TRUE),
    y_m = mean(spacing_bias, na.rm = TRUE),
    x_se = sd(width_bias,    na.rm = TRUE) / sqrt(n),
    y_se = sd(spacing_bias,  na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

panel_b <- ggplot(bias_dat, aes(color = cond)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(x = width_bias, ymin = spacing_bias - spacing_se,
                    ymax = spacing_bias + spacing_se),
                width = 0, alpha = 0.35, linewidth = 0.4) +
  geom_errorbarh(aes(y = spacing_bias, xmin = width_bias - width_se,
                     xmax = width_bias + width_se),
                 height = 0, alpha = 0.35, linewidth = 0.4) +
  geom_point(aes(x = width_bias, y = spacing_bias),
             alpha = 0.5, size = 1.4) +
  geom_errorbar(data = bias_centroids,
                aes(x = x_m, ymin = y_m - y_se, ymax = y_m + y_se),
                width = 0, linewidth = 1, color = "black", alpha = 0.7,
                inherit.aes = FALSE) +
  geom_errorbarh(data = bias_centroids,
                 aes(y = y_m, xmin = x_m - x_se, xmax = x_m + x_se),
                 height = 0, linewidth = 1, color = "black", alpha = 0.7,
                 inherit.aes = FALSE) +
  geom_point(data = bias_centroids, aes(x = x_m, y = y_m, fill = cond),
             shape = 21, size = 3, stroke = 0.8, color = "black",
             inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_fill_manual(values = rm_colors, name = NULL, guide = "none") +
  labs(x = "Relative Width Deviation",
       y = "Relative Spacing Deviation") +
  theme_scientific() +
  theme(legend.position = "none")


# --- PANEL C: Δwidth vs Δspacing across subjects ---------------

delta_dat  <- read.csv("exp2/outputs/tables/joint_width_spacing_delta.csv")
delta_corr <- read.csv("exp2/outputs/tables/joint_width_spacing_delta_corr.csv")

corr_label <- sprintf("italic(r) == %.2f * ',' ~ italic(p) == %.3f",
                      delta_corr$r, delta_corr$p)

panel_c <- ggplot(delta_dat, aes(x = delta_width, y = delta_spacing)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black",
              fill = "grey80", linewidth = 0.6, alpha = 0.4, se = TRUE) +
  geom_point(size = 1.8, alpha = 0.7, color = "grey25") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.96, vjust = 1.2,
           label = corr_label, parse = TRUE, size = 5) +
  labs(x = expression(bold(Delta * " Width Deviation")),
       y = expression(bold(Delta * " Spacing Deviation"))) +
  theme_scientific()


# --- PANEL D: Extent (length) deviation by set size --------------------
# Perceived array extent (response_stim_length - stim_length). RM compresses
# the perceived horizontal extent (radial compression of visual space).

length_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "no-RM"), levels = c("no-RM", "RM")),
    correct_num = factor(correct_num),
    length_deviation = response_stim_length - stim_length
  ) %>%
  drop_na(length_deviation)

len_part <- length_data %>%
  group_by(subID, correct_num, rm_type) %>%
  summarise(mean_dev = mean(length_deviation), .groups = "drop")

len_plot <- len_part %>%
  group_by(correct_num, rm_type) %>%
  summarise(n = n(), grand_mean = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n),
            t_crit = qt(0.975, n - 1),
            ci_lower = grand_mean - t_crit * se,
            ci_upper = grand_mean + t_crit * se, .groups = "drop")

contrast_length <- read.csv("exp2/outputs/tables/length_deviation_rm_contrasts_by_num.csv")

len_stars <- contrast_length %>%
  mutate(correct_num = factor(correct_num),
         sig = case_when(p_fdr < 0.001 ~ "***", p_fdr < 0.01 ~ "**",
                         p_fdr < 0.05 ~ "*", TRUE ~ "")) %>%
  filter(sig != "") %>%
  left_join(len_plot %>% group_by(correct_num) %>%
              summarise(y_pos = max(ci_upper) + 0.08, .groups = "drop"),
            by = "correct_num")

panel_d <- ggplot(len_plot, aes(x = correct_num, y = grand_mean,
                                color = rm_type, fill = rm_type, group = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_jitter(data = len_part, aes(x = correct_num, y = mean_dev, color = rm_type),
              alpha = 0.3, size = 1.1,
              position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.5, seed = 42),
              inherit.aes = FALSE, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.12, linewidth = 0.6, position = position_dodge(0.5)) +
  geom_point(size = 2.8, stroke = 0.7, shape = 21, color = "black",
             position = position_dodge(0.5)) +
  geom_text(data = len_stars, aes(x = correct_num, y = y_pos, label = sig),
            color = "black", size = 5, inherit.aes = FALSE) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_fill_manual(values = rm_colors, name = NULL) +
  coord_cartesian(ylim = c(-1.5, NA)) +
  labs(x = "Set Size", y = "Extent Deviation (°)") +
  theme_scientific() +
  theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1))


# --- PANEL E: Coverage-density deviation by set size --------------------
# Density = total bar coverage / array extent. RM vs no RM does not differ at
# any set size: perceived density is preserved.

density_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(rm_type = factor(if_else(number_deviation == -1, "RM", "no-RM"),
                          levels = c("no-RM", "RM")),
         correct_num = factor(correct_num)) %>%
  drop_na(width_density_deviation)

dens_part <- density_data %>%
  group_by(subID, correct_num, rm_type) %>%
  summarise(mean_dev = mean(width_density_deviation), .groups = "drop")

dens_plot <- dens_part %>%
  group_by(correct_num, rm_type) %>%
  summarise(n = n(), grand_mean = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n),
            t_crit = qt(0.975, n - 1),
            ci_lower = grand_mean - t_crit * se,
            ci_upper = grand_mean + t_crit * se, .groups = "drop")

contrast_density <- read.csv("exp2/outputs/tables/density_deviation_rm_contrasts_by_num.csv")

dens_labels <- contrast_density %>%
  mutate(correct_num = factor(correct_num),
         sig = case_when(p_fdr < 0.001 ~ "***", p_fdr < 0.01 ~ "**",
                         p_fdr < 0.05 ~ "*", TRUE ~ "n.s."),
         y_pos = 0.13)

panel_e <- ggplot(dens_plot, aes(x = correct_num, y = grand_mean,
                                 color = rm_type, fill = rm_type, group = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_jitter(data = dens_part, aes(x = correct_num, y = mean_dev, color = rm_type),
              alpha = 0.3, size = 1.1,
              position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.5, seed = 42),
              inherit.aes = FALSE, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.12, linewidth = 0.6, position = position_dodge(0.5)) +
  geom_point(size = 2.8, stroke = 0.7, shape = 21, color = "black",
             position = position_dodge(0.5)) +
  geom_text(data = dens_labels, aes(x = correct_num, y = y_pos, label = sig),
            color = "black", size = 5, inherit.aes = FALSE) +
  scale_color_manual(values = rm_colors, name = NULL) +
  scale_fill_manual(values = rm_colors, name = NULL) +
  coord_cartesian(ylim = c(-0.16, 0.16)) +
  labs(x = "Set Size", y = "Density Deviation") +
  theme_scientific() +
  theme(legend.position = "none")


# --- COMBINE & SAVE -----------------------------------------------------

layout <- "
AA
BC
DE
"

txt_size <- 14

fig <- panel_a + panel_b + panel_c + panel_d + panel_e +
  plot_layout(design = layout, heights = c(1, 1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(text = element_text(family = "Arial", size = txt_size, colour = "black"),
        plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(family = "Arial", size = txt_size, face = "bold"),
        axis.text = element_text(family = "Arial", size = txt_size),
        strip.text = element_text(family = "Arial", size = txt_size, face = "bold"),
        legend.text = element_text(family = "Arial", size = txt_size))

dir.create("exp2/outputs", recursive = TRUE, showWarnings = FALSE)
ggsave("exp2/outputs/figures/figure4_spacing_and_coupling.png", fig,
       width = 10, height = 12, dpi = 300, bg = "white")

cat("Saved exp2/outputs/figure4_spacing_and_coupling.png\n")

