library(tidyverse)

df <- read.csv("exp1/data/processed.csv")
one_bar <- read.csv("exp1/data/one_bar_exp1.csv")

per_subj <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(cond = if_else(number_deviation == -1, "RM", "Non-RM")) %>%
  group_by(subID, cond) %>%
  summarise(
    n_w = sum(!is.na(width_deviation_relative)),
    width_bias = mean(width_deviation_relative, na.rm = TRUE),
    width_sd   = sd(width_deviation_relative,   na.rm = TRUE),
    width_se   = width_sd / sqrt(n_w),
    .groups = "drop"
  ) %>%
  mutate(rmse_width = sqrt(width_bias^2 + width_sd^2),
         subID = as.character(subID))

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

dir.create("exp1/outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(joint_long, "exp1/outputs/tables/joint_width.csv", row.names = FALSE)

group_summary <- joint_long %>%
  group_by(cond) %>%
  summarise(
    n              = n(),
    width_bias_M   = mean(width_bias, na.rm = TRUE),
    width_bias_SE  = sd(width_bias,   na.rm = TRUE) / sqrt(sum(!is.na(width_bias))),
    width_sd_M     = mean(width_sd,   na.rm = TRUE),
    width_sd_SE    = sd(width_sd,     na.rm = TRUE) / sqrt(sum(!is.na(width_sd))),
    .groups = "drop"
  )

cat("\n=== exp1: Joint width per-subject summary ===\n")
print(group_summary)

write.csv(group_summary, "exp1/outputs/tables/joint_width_group.csv", row.names = FALSE)


rmse_wide <- joint_long %>%
  select(subID, cond, rmse_width) %>%
  pivot_wider(names_from = cond, values_from = rmse_width)

matched <- rmse_wide %>% drop_na()

t_rm_norm <- t.test(matched$RM,       matched$`Non-RM`, paired = TRUE)
t_rm_ob   <- t.test(matched$RM,       matched$`1-bar`,  paired = TRUE)
t_norm_ob <- t.test(matched$`Non-RM`, matched$`1-bar`,  paired = TRUE)

dz <- function(tt) unname(tt$statistic / sqrt(tt$parameter + 1))

tests  <- list(t_rm_norm, t_rm_ob, t_norm_ob)
labels <- c("RM vs Non-RM", "RM vs 1-bar", "Non-RM vs 1-bar")

rmse_results <- tibble(
  comparison = labels,
  t  = map_dbl(tests, ~ unname(.x$statistic)),
  df = map_dbl(tests, ~ unname(.x$parameter)),
  mean_diff = map_dbl(tests, ~ unname(.x$estimate)),
  p  = map_dbl(tests, ~ .x$p.value),
  dz = map_dbl(tests, dz)
) %>%
  mutate(p_fdr = p.adjust(p, method = "BH")) %>%
  mutate(across(c(t, mean_diff, p, p_fdr, dz), ~ round(.x, 4)))

cat("\n=== exp1: Per-subject RMSE paired comparisons (n =", nrow(matched), ", FDR: BH) ===\n")
print(rmse_results)

write.csv(rmse_results, "exp1/outputs/tables/joint_width_rmse_tests.csv", row.names = FALSE)
