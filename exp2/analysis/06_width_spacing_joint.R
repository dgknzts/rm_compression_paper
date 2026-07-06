library(tidyverse)

df <- read.csv("exp2/data/processed.csv")
one_bar <- read.csv("exp2/data/one_bar_exp2.csv")

per_subj <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(cond = if_else(number_deviation == -1, "RM", "Non-RM")) %>%
  group_by(subID, cond) %>%
  summarise(
    n_w = sum(!is.na(width_deviation_relative)),
    n_s = sum(!is.na(spacing_deviation_relative)),
    width_bias   = mean(width_deviation_relative,   na.rm = TRUE),
    spacing_bias = mean(spacing_deviation_relative, na.rm = TRUE),
    width_sd     = sd(width_deviation_relative,     na.rm = TRUE),
    spacing_sd   = sd(spacing_deviation_relative,   na.rm = TRUE),
    width_se     = width_sd   / sqrt(n_w),
    spacing_se   = spacing_sd / sqrt(n_s),
    .groups = "drop"
  ) %>%
  mutate(
    rmse_width    = sqrt(width_bias^2   + width_sd^2),
    rmse_spacing  = sqrt(spacing_bias^2 + spacing_sd^2),
    bias_total    = sqrt(width_bias^2   + spacing_bias^2),
    sd_total      = sqrt(width_sd^2     + spacing_sd^2),
    rmse_total    = sqrt(bias_total^2   + sd_total^2),
    subID = as.character(subID)
  )

ob_subj <- one_bar %>%
  mutate(width_rel = width_deviation / correct_width) %>%
  group_by(participant) %>%
  summarise(
    n_w = sum(!is.na(width_rel)),
    width_bias = mean(width_rel, na.rm = TRUE),
    width_sd   = sd(width_rel,   na.rm = TRUE),
    width_se   = width_sd / sqrt(n_w),
    .groups = "drop"
  ) %>%
  mutate(
    rmse_width = sqrt(width_bias^2 + width_sd^2),
    cond = "1-bar",
    subID = as.character(participant)
  ) %>%
  select(subID, cond, n_w, width_bias, width_sd, width_se, rmse_width)

joint_long <- bind_rows(per_subj, ob_subj) %>%
  mutate(cond = factor(cond, levels = c("1-bar", "Non-RM", "RM")))

dir.create("exp2/outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(joint_long, "exp2/outputs/tables/joint_width_spacing.csv", row.names = FALSE)

group_summary <- joint_long %>%
  group_by(cond) %>%
  summarise(
    n            = n(),
    width_bias_M    = mean(width_bias,   na.rm = TRUE),
    width_bias_SE   = sd(width_bias,     na.rm = TRUE) / sqrt(sum(!is.na(width_bias))),
    spacing_bias_M  = mean(spacing_bias, na.rm = TRUE),
    spacing_bias_SE = sd(spacing_bias,   na.rm = TRUE) / sqrt(sum(!is.na(spacing_bias))),
    width_sd_M      = mean(width_sd,     na.rm = TRUE),
    width_sd_SE     = sd(width_sd,       na.rm = TRUE) / sqrt(sum(!is.na(width_sd))),
    spacing_sd_M    = mean(spacing_sd,   na.rm = TRUE),
    spacing_sd_SE   = sd(spacing_sd,     na.rm = TRUE) / sqrt(sum(!is.na(spacing_sd))),
    .groups = "drop"
  )

cat("\n=== exp2: Joint width/spacing per-subject summary ===\n")
print(group_summary)

write.csv(group_summary, "exp2/outputs/tables/joint_width_spacing_group.csv", row.names = FALSE)


delta <- per_subj %>%
  select(subID, cond, width_bias, spacing_bias) %>%
  pivot_wider(names_from = cond,
              values_from = c(width_bias, spacing_bias)) %>%
  mutate(
    delta_width   = width_bias_RM   - `width_bias_Non-RM`,
    delta_spacing = spacing_bias_RM - `spacing_bias_Non-RM`
  ) %>%
  drop_na(delta_width, delta_spacing)

ct <- cor.test(delta$delta_width, delta$delta_spacing, method = "pearson")

delta_corr <- tibble(
  n   = nrow(delta),
  r   = round(unname(ct$estimate), 4),
  df  = unname(ct$parameter),
  t   = round(unname(ct$statistic), 4),
  p   = round(ct$p.value, 4),
  ci_lower = round(ct$conf.int[1], 4),
  ci_upper = round(ct$conf.int[2], 4)
)

cat("\n=== exp2: RM-induced shift correlation (Δwidth vs Δspacing, n =", nrow(delta), ") ===\n")
print(delta_corr)

write.csv(delta,       "exp2/outputs/tables/joint_width_spacing_delta.csv", row.names = FALSE)
write.csv(delta_corr,  "exp2/outputs/tables/joint_width_spacing_delta_corr.csv", row.names = FALSE)
