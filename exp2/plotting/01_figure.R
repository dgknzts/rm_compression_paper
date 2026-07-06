library(tidyverse)
library(patchwork)
library(RColorBrewer)

source("shared/themes.R")
source("shared/model_functions.R")

rm_colors <- c("no-RM" = "#298c8c", "RM" = "#800074")


# --- PANEL A: Number Deviation ---

df <- read.csv("exp2/data/processed.csv")

num_participant_means <- df %>%
  filter(!is.na(number_deviation)) %>%
  group_by(subID, correct_num) %>%
  summarise(mean_dev = mean(number_deviation), .groups = "drop")

num_grand <- num_participant_means %>%
  group_by(correct_num) %>%
  summarise(grand_mean = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n()),
            t_crit = qt(0.975, n() - 1),
            ci_lower = grand_mean - t_crit * se,
            ci_upper = grand_mean + t_crit * se,
            .groups = "drop")

fig_a <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_hline(yintercept = -1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_point(data = num_participant_means,
             aes(x = factor(correct_num), y = mean_dev),
             position = position_jitter(width = 0.15, seed = 42),
             size = 1, alpha = 0.2) +
  geom_errorbar(data = num_grand,
                aes(x = factor(correct_num), y = grand_mean,
                    ymin = ci_lower, ymax = ci_upper),
                width = 0.08, linewidth = 1, color = "black") +
  geom_point(data = num_grand,
             aes(x = factor(correct_num), y = grand_mean),
             size = 3, color = "black") +
  scale_y_continuous(limits = c(-1.2, 0.5)) +
  labs(x = "Set Size", y = "Number Deviation") +
  theme_scientific()


# --- PANEL B: Width Deviation ---

contrast_width <- read.csv("exp2/outputs/tables/width_deviation_rm_contrasts.csv")

one_bar_data <- read.csv("exp2/data/one_bar_exp2.csv")
ob_part_w <- one_bar_data %>%
  mutate(correct_width = factor(correct_width)) %>%
  group_by(participant, correct_width) %>%
  summarise(mean_dev = mean(width_deviation), .groups = "drop")

ob_plot_w <- ob_part_w %>%
  group_by(correct_width) %>%
  summarise(n = n(), grand_mean = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n),
            t_crit = qt(0.975, n - 1),
            ci_lower = grand_mean - t_crit * se,
            ci_upper = grand_mean + t_crit * se, .groups = "drop") %>%
  mutate(correct_num = factor("1"), rm_type = "1-bar")

width_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "no-RM"), levels = c("no-RM", "RM")),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

w_part <- width_data %>%
  group_by(subID, correct_num, correct_width, rm_type) %>%
  summarise(mean_dev = mean(width_deviation), .groups = "drop")

w_plot <- w_part %>%
  group_by(correct_num, correct_width, rm_type) %>%
  summarise(n = n(), grand_mean = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n),
            t_crit = qt(0.975, n - 1),
            ci_lower = grand_mean - t_crit * se,
            ci_upper = grand_mean + t_crit * se, .groups = "drop")

w_stars <- contrast_width %>%
  mutate(
    correct_width = factor(correct_width),
    correct_num = factor(correct_num),
    sig = case_when(p_fdr < 0.001 ~ "***", p_fdr < 0.01 ~ "**",
                    p_fdr < 0.05 ~ "*", TRUE ~ "")
  ) %>%
  filter(sig != "") %>%
  left_join(w_plot %>% group_by(correct_width, correct_num) %>%
              summarise(y_pos = max(ci_upper) + 0.008, .groups = "drop"),
            by = c("correct_width", "correct_num"))

fig_b <- ggplot(w_plot, aes(x = correct_num, y = grand_mean, color = rm_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_errorbar(data = ob_plot_w, aes(x = correct_num, y = grand_mean,
                                       ymin = ci_lower, ymax = ci_upper, color = rm_type),
                width = 0.1, linewidth = 0.5, inherit.aes = FALSE) +
  geom_point(data = ob_plot_w, aes(x = correct_num, y = grand_mean, color = rm_type),
             size = 2.5, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.1, linewidth = 0.5, position = position_dodge(0.4)) +
  geom_point(size = 2.5, position = position_dodge(0.4)) +
  geom_text(data = w_stars, aes(x = correct_num, y = y_pos, label = sig),
            color = "black", size = 5, inherit.aes = FALSE) +
  facet_wrap(~correct_width, labeller = as_labeller(function(x) paste("Width:", x, "\u00b0"))) +
  scale_color_manual(values = c(rm_colors, "1-bar" = "grey50"), name = NULL) +
  labs(x = "Set Size", y = "Width Deviation (\u00b0)") +
  theme_scientific() +
  theme(legend.position = c(0.12, 0.18))


# --- PANEL C: Relative Width Deviation by Condition ---

one_bar_data <- read.csv("exp2/data/one_bar_exp2.csv")

one_bar_rel <- one_bar_data %>%
  mutate(width_deviation_relative = width_deviation / correct_width) %>%
  group_by(participant) %>%
  summarise(mean_dev = mean(width_deviation_relative, na.rm = TRUE), .groups = "drop") %>%
  mutate(condition = "1-bar")

multi_bar_rel <- width_data %>%
  mutate(width_deviation_relative = width_deviation / as.numeric(as.character(correct_width))) %>%
  group_by(subID, rm_type) %>%
  summarise(mean_dev = mean(width_deviation_relative, na.rm = TRUE), .groups = "drop") %>%
  rename(condition = rm_type)

density_data <- bind_rows(
  one_bar_rel %>% select(condition, mean_dev),
  multi_bar_rel %>% select(condition, mean_dev)
) %>%
  mutate(condition = factor(condition, levels = c("1-bar", "no-RM", "RM")))

condition_colors <- c("1-bar" = "grey50", "no-RM" = "#298c8c", "RM" = "#800074")

ttest_results <- read.csv("exp2/outputs/tables/ttest_results.csv")
get_p <- function(label) ttest_results$p_fdr[ttest_results$comparison == label]

format_sig <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "n.s.")))
}

sig_vs_zero <- data.frame(
  condition = factor(c("1-bar", "no-RM", "RM"), levels = c("1-bar", "no-RM", "RM")),
  label = format_sig(c(get_p("1-bar vs 0"),
                       get_p("Non-RM vs 0"),
                       get_p("RM vs 0")))
)

sig_rm_1bar <- format_sig(get_p("RM vs 1-bar"))
sig_norm_1bar <- format_sig(get_p("Non-RM vs 1-bar"))

rel_summary <- density_data %>%
  group_by(condition) %>%
  summarise(mean_dev_grp = mean(mean_dev),
            se = sd(mean_dev) / sqrt(n()),
            t_crit = qt(0.975, n() - 1),
            ci_lower = mean_dev_grp - t_crit * se,
            ci_upper = mean_dev_grp + t_crit * se,
            .groups = "drop")

y_range <- range(c(density_data$mean_dev, rel_summary$ci_upper, rel_summary$ci_lower))
y_span <- diff(y_range)
y_b1 <- y_range[2] + y_span * 0.12
y_b2 <- y_b1 + y_span * 0.15
y_tick <- y_span * 0.03

fig_c <- ggplot(density_data, aes(x = condition, y = mean_dev, fill = condition)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_col(data = rel_summary, aes(x = condition, y = mean_dev_grp, fill = condition),
           width = 0.6, alpha = 0.8, color = "grey30", linewidth = 0.4,
           inherit.aes = FALSE) +
  geom_jitter(aes(color = condition), width = 0.1, size = 1.5, alpha = 0.4, show.legend = FALSE) +
  geom_errorbar(data = rel_summary,
                aes(x = condition, ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 0.5, color = "black",
                inherit.aes = FALSE) +
  geom_text(data = sig_vs_zero, aes(x = condition, label = label, color = condition),
            y = y_range[2] + y_span * 0.05, size = 5, fontface = "bold",
            show.legend = FALSE, inherit.aes = FALSE) +
  annotate("segment", x = 1, xend = 3, y = y_b1, yend = y_b1, linewidth = 0.4) +
  annotate("segment", x = 1, xend = 1, y = y_b1 - y_tick, yend = y_b1, linewidth = 0.4) +
  annotate("segment", x = 3, xend = 3, y = y_b1 - y_tick, yend = y_b1, linewidth = 0.4) +
  annotate("text", x = 2, y = y_b1 + y_tick * 2, label = sig_rm_1bar, size = 5) +
  annotate("segment", x = 1, xend = 2, y = y_b2, yend = y_b2, linewidth = 0.4) +
  annotate("segment", x = 1, xend = 1, y = y_b2 - y_tick, yend = y_b2, linewidth = 0.4) +
  annotate("segment", x = 2, xend = 2, y = y_b2 - y_tick, yend = y_b2, linewidth = 0.4) +
  annotate("text", x = 1.5, y = y_b2 + y_tick * 2, label = sig_norm_1bar, size = 5) +
  scale_fill_manual(values = condition_colors) +
  scale_color_manual(values = condition_colors) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Relative Width Deviation") +
  theme_scientific() +
  theme(legend.position = "none",
        plot.margin = margin(t = 25, r = 5, b = 5, l = 5))


# --- PANEL D: Width Precision ---

one_bar_prec <- one_bar_data %>%
  group_by(participant, correct_width) %>%
  summarise(sd_raw = sd(width_deviation, na.rm = TRUE), .groups = "drop") %>%
  mutate(sd_rel = sd_raw / correct_width) %>%
  group_by(participant) %>%
  summarise(precision = mean(sd_rel, na.rm = TRUE), .groups = "drop") %>%
  transmute(condition = "1-bar", precision)

multi_bar_prec <- width_data %>%
  mutate(correct_width = as.numeric(as.character(correct_width))) %>%
  group_by(subID, rm_type, correct_width) %>%
  summarise(sd_raw = sd(width_deviation, na.rm = TRUE), .groups = "drop") %>%
  mutate(sd_rel = sd_raw / correct_width) %>%
  group_by(subID, rm_type) %>%
  summarise(precision = mean(sd_rel, na.rm = TRUE), .groups = "drop") %>%
  transmute(condition = as.character(rm_type), precision)

precision_data <- bind_rows(one_bar_prec, multi_bar_prec) %>%
  mutate(condition = factor(condition, levels = c("1-bar", "no-RM", "RM"))) %>%
  filter(!is.na(precision))

prec_tests <- read.csv("exp2/outputs/tables/precision_tests.csv")
get_prec_p <- function(label) prec_tests$p_fdr[prec_tests$comparison == label]

prec_summary <- precision_data %>%
  group_by(condition) %>%
  summarise(mean_prec = mean(precision),
            se = sd(precision) / sqrt(n()),
            t_crit = qt(0.975, n() - 1),
            ci_lower = mean_prec - t_crit * se,
            ci_upper = mean_prec + t_crit * se,
            .groups = "drop")

yp_range <- range(c(precision_data$precision, prec_summary$ci_upper))
yp_span  <- diff(yp_range)

prec_brackets <- tibble(
  x1 = c(2, 1, 1),
  x2 = c(3, 3, 2),
  y  = yp_range[2] + yp_span * c(0.10, 0.30, 0.50),
  label = c(format_sig(get_prec_p("RM vs Non-RM")),
            format_sig(get_prec_p("RM vs 1-bar")),
            format_sig(get_prec_p("Non-RM vs 1-bar")))
)
yp_tick <- yp_span * 0.03

fig_d <- ggplot(precision_data, aes(x = condition, y = precision, fill = condition)) +
  geom_col(data = prec_summary, aes(x = condition, y = mean_prec, fill = condition),
           width = 0.6, alpha = 0.8, color = "grey30", linewidth = 0.4,
           inherit.aes = FALSE) +
  geom_jitter(aes(color = condition), width = 0.1, size = 1.5,
              alpha = 0.4, show.legend = FALSE) +
  geom_errorbar(data = prec_summary,
                aes(x = condition, ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 0.5, color = "black",
                inherit.aes = FALSE) +
  geom_segment(data = prec_brackets,
               aes(x = x1, xend = x2, y = y, yend = y),
               inherit.aes = FALSE, linewidth = 0.4) +
  geom_segment(data = prec_brackets,
               aes(x = x1, xend = x1, y = y - yp_tick, yend = y),
               inherit.aes = FALSE, linewidth = 0.4) +
  geom_segment(data = prec_brackets,
               aes(x = x2, xend = x2, y = y - yp_tick, yend = y),
               inherit.aes = FALSE, linewidth = 0.4) +
  geom_text(data = prec_brackets,
            aes(x = (x1 + x2) / 2, y = y + yp_tick * 2, label = label),
            inherit.aes = FALSE, size = 5) +
  scale_fill_manual(values = condition_colors) +
  scale_color_manual(values = condition_colors) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Precision (SD / W)") +
  theme_scientific() +
  theme(legend.position = "none",
        plot.margin = margin(t = 25, r = 5, b = 5, l = 5))


# --- PANEL E: Width Deviation vs Precision (iso-RMSE arcs) ---

joint_e <- read.csv("exp2/outputs/tables/joint_width_spacing.csv") %>%
  mutate(cond = if_else(cond == "Non-RM", "no-RM", cond),
         cond = factor(cond, levels = c("1-bar", "no-RM", "RM")))

iso_arc_e <- function(radii, n = 200) {
  expand_grid(rmse = radii, theta = seq(-pi/2, pi/2, length.out = n)) %>%
    mutate(x = rmse * cos(theta), y = rmse * sin(theta)) %>%
    group_by(rmse) %>% arrange(theta, .by_group = TRUE) %>% ungroup()
}

centroids_e <- joint_e %>%
  group_by(cond) %>%
  summarise(
    n = sum(!is.na(width_bias) & !is.na(width_sd)),
    x_m = mean(width_sd,   na.rm = TRUE),
    y_m = mean(width_bias, na.rm = TRUE),
    x_se = sd(width_sd,    na.rm = TRUE) / sqrt(n),
    y_se = sd(width_bias,  na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

w_max_e   <- max(sqrt(joint_e$width_bias^2 + joint_e$width_sd^2), na.rm = TRUE)
dev_lim_e <- max(abs(joint_e$width_bias), na.rm = TRUE) * 1.1
w_radii_e <- pretty(c(0, w_max_e), n = 5)
w_radii_e <- w_radii_e[w_radii_e > 0]
w_arcs_e  <- iso_arc_e(w_radii_e)

fig_e <- ggplot() +
  geom_path(data = w_arcs_e, aes(x = x, y = y, group = rmse),
            linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(data = joint_e,
             aes(x = width_sd, y = width_bias, color = cond),
             alpha = 0.3, size = 1) +
  geom_segment(data = centroids_e,
               aes(x = 0, y = 0, xend = x_m, yend = y_m, color = cond),
               linewidth = 0.5, alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = centroids_e,
                aes(x = x_m, ymin = y_m - y_se, ymax = y_m + y_se),
                width = 0, linewidth = 1, color = "black", alpha = 0.6,
                inherit.aes = FALSE) +
  geom_errorbarh(data = centroids_e,
                 aes(y = y_m, xmin = x_m - x_se, xmax = x_m + x_se),
                 height = 0, linewidth = 1, color = "black", alpha = 0.6,
                 inherit.aes = FALSE) +
  geom_point(data = centroids_e, aes(x = x_m, y = y_m, fill = cond),
             shape = 21, size = 3, stroke = 0.8, color = "black",
             inherit.aes = FALSE, show.legend = FALSE) +
  scale_color_manual(values = condition_colors, name = NULL) +
  scale_fill_manual(values = condition_colors, name = NULL, guide = "none") +
  coord_fixed(xlim = c(0, w_max_e * 1.05), ylim = c(-dev_lim_e, dev_lim_e)) +
  labs(x = "Precision", y = "Relative Width Deviation") +
  theme_scientific() +
  theme(legend.position = "none")


# --- PANEL F: Model Comparison (delta-AIC) ---

all_fits <- readRDS("exp2/outputs/tables/model_fits.rds")

null_aic <- all_fits %>%
  filter(Model == "Null") %>%
  select(Participant, AIC_null = AIC)

delta_aic <- all_fits %>%
  filter(Model != "Null") %>%
  left_join(null_aic, by = "Participant") %>%
  mutate(dAIC = AIC - AIC_null,
         Model = fct_recode(Model,
                            "M1" = "Perfect Pooling",
                            "M2" = "Partial Pooling",
                            "M3" = "Compression + Pooling",
                            "M4" = "Compression + Partial Pooling",
                            "M5" = "Linear Compression + Partial Pooling"))

fig_f <- ggplot(delta_aic, aes(x = Model, y = dAIC)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA, fill = "grey80") +
  geom_jitter(width = 0.12, alpha = 0.4, size = 1.5) +
  labs(x = "", y = "\u0394AIC (model \u2212 M0)") +
  theme_scientific()


# --- PANEL G: Effective alpha by bar width ---

params <- readRDS("exp2/outputs/tables/param_estimates.rds")
W_vals <- c(0.25, 0.4, 0.55)

W_labels <- factor(paste0(W_vals, "\u00b0"), levels = paste0(W_vals, "\u00b0"))

alpha_by_width <- do.call(rbind, lapply(W_vals, function(w) {
  params$params_m5 %>%
    mutate(W_act = w,
           W_label = factor(paste0(w, "\u00b0"), levels = levels(W_labels)),
           eff_alpha = alpha + a1 * w)
}))

alpha_sum <- alpha_by_width %>%
  group_by(W_label, W_act) %>%
  summarise(mean_alpha = mean(eff_alpha),
            se = sd(eff_alpha) / sqrt(n()),
            t_crit = qt(0.975, n() - 1),
            ci_lower = mean_alpha - t_crit * se,
            ci_upper = mean_alpha + t_crit * se,
            .groups = "drop")

mean_a0 <- mean(params$params_m5$alpha)
mean_a1 <- mean(params$params_m5$a1)

line_data <- data.frame(
  W_label = W_labels,
  y = mean_a0 + mean_a1 * W_vals
)

fig_g <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  geom_line(data = line_data, aes(x = W_label, y = y, group = 1),
            linetype = "solid", color = "grey60", linewidth = 0.8) +
  geom_point(data = alpha_by_width,
             aes(x = W_label, y = eff_alpha),
             size = 1, alpha = 0.2,
             position = position_jitter(width = 0.15, seed = 42)) +
  geom_errorbar(data = alpha_sum,
                aes(x = W_label, y = mean_alpha, ymin = ci_lower, ymax = ci_upper),
                width = 0.1, linewidth = 1, color = "black") +
  geom_point(data = alpha_sum, aes(x = W_label, y = mean_alpha),
             size = 3, color = "black") +
  labs(x = "Bar Width", y = "\u03b1(W) (size-scaling parameter)") +
  theme_scientific()


# --- PANEL H: Gamma (pooling parameter) ---

gamma_vals <- params$params_m5 %>%
  select(Participant, gamma) %>%
  mutate(dominant = ifelse(gamma < 0.5, "lost", "pooled"))

gamma_sum <- gamma_vals %>%
  summarise(mean_g = mean(gamma), sd_g = sd(gamma), n = n(),
            se = sd_g / sqrt(n),
            t_crit = qt(0.975, n - 1),
            ci_lower = mean_g - t_crit * se,
            ci_upper = mean_g + t_crit * se)

n_lost <- sum(gamma_vals$gamma < 0.5)
n_total <- nrow(gamma_vals)
cat("Panel H: gamma < 0.5 (more lost than pooled):", n_lost, "/", n_total, "\n")

dom_colors <- c("lost" = "#D6604D", "pooled" = "#4393C3")

fig_h <- ggplot(gamma_vals, aes(x = "", y = gamma)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.5,
           fill = "#D6604D", alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = Inf,
           fill = "#4393C3", alpha = 0.07) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.6) +
  geom_hline(yintercept = 0.5, linetype = "solid", color = "grey30", linewidth = 0.7) +
  geom_point(data = data.frame(dominant = c("lost", "pooled"), gamma = c(-99, -99)),
             aes(color = dominant), size = 1.8) +
  geom_point(aes(color = dominant), size = 1.8, alpha = 0.75,
             position = position_jitter(width = 0.15, seed = 42)) +
  geom_errorbar(data = gamma_sum,
                aes(x = "", y = mean_g, ymin = ci_lower, ymax = ci_upper),
                width = 0.08, linewidth = 1, color = "black") +
  geom_point(data = gamma_sum, aes(x = "", y = mean_g),
             size = 3, color = "black") +
  scale_color_manual(values = dom_colors, name = NULL,
                     limits = c("lost", "pooled"), drop = FALSE,
                     labels = c("More lost than pooled", "More pooled than lost"),
                     guide = guide_legend(override.aes = list(colour = c("#D6604D", "#4393C3")))) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(-0.2, 1.05)) +
  labs(x = "", y = "\u03b3 (pooling parameter)") +
  theme_scientific() +
  theme(legend.position = c(0.5, 0.9),
        legend.justification = c(0.5, 0.5),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0.02, "cm"),
        legend.background = element_rect(fill = "#FFFFFFB3", color = NA),
        legend.key = element_rect(fill = "#FFFFFF00", color = NA))


# --- COMBINE ---

layout <- "
ABB
###
CDE
###
FGH
"

txt_size <- 14

fig_combined <- fig_a + fig_b + fig_c + fig_d + fig_e + fig_f + fig_g + fig_h +
  plot_layout(design = layout, heights = c(1, 0.08, 1, 0.08, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(text = element_text(family = "Arial", size = txt_size, colour = "black"),
        plot.margin = margin(t = 2, r = 5, b = 2, l = 5),
        plot.tag = element_text(size = 18, face = "bold"),
        axis.title = element_text(family = "Arial", size = txt_size, face = "bold"),
        axis.text = element_text(family = "Arial", size = txt_size),
        strip.text = element_text(family = "Arial", size = txt_size, face = "bold"),
        legend.title = element_text(family = "Arial", size = txt_size, face = "bold"),
        legend.text = element_text(family = "Arial", size = txt_size),
        plot.title = element_text(family = "Arial", size = txt_size, face = "bold"),
        plot.subtitle = element_text(family = "Arial", size = txt_size))

dir.create("exp2/outputs", recursive = TRUE, showWarnings = FALSE)
ggsave("exp2/outputs/figures/comprehensive_results.png", fig_combined,
       width = 12, height = 14, dpi = 300, bg = "white")

ggsave("exp2/outputs/figures/panel_a_number_deviation.png", fig_a, width = 5, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_b_width_deviation.png", fig_b, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_c_relative_width.png", fig_c, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_d_precision.png", fig_d, width = 5, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_e_iso_rmse.png", fig_e, width = 5, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_f_delta_aic.png", fig_f, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_g_compression.png", fig_g, width = 6, height = 5, dpi = 300, bg = "white")
ggsave("exp2/outputs/figures/panel_h_pooling.png", fig_h, width = 4, height = 5, dpi = 300, bg = "white")
