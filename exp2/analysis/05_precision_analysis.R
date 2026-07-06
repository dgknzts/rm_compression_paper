library(tidyverse)

df <- read.csv("exp2/data/processed.csv")
one_bar <- read.csv("exp2/data/one_bar_exp2.csv")

scatter_by_cell <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(sd_raw = sd(width_deviation, na.rm = TRUE), .groups = "drop") %>%
    mutate(sd_rel = sd_raw / correct_width)
}

rm_scatter <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(cond = if_else(number_deviation == -1, "RM", "Non-RM")) %>%
  scatter_by_cell(subID, cond, correct_width) %>%
  group_by(subID, cond) %>%
  summarise(scatter = mean(sd_rel, na.rm = TRUE), .groups = "drop") %>%
  mutate(subID = as.character(subID))

ob_scatter <- one_bar %>%
  scatter_by_cell(participant, correct_width) %>%
  group_by(participant) %>%
  summarise(scatter = mean(sd_rel, na.rm = TRUE), .groups = "drop") %>%
  rename(subID = participant) %>%
  mutate(subID = as.character(subID), cond = "1-bar")

all_scatter <- bind_rows(rm_scatter, ob_scatter) %>%
  mutate(cond = factor(cond, levels = c("1-bar", "Non-RM", "RM"))) %>%
  pivot_wider(names_from = cond, values_from = scatter)

matched <- all_scatter %>% drop_na()

t_rm_norm <- t.test(matched$RM, matched$`Non-RM`, paired = TRUE)
t_rm_ob   <- t.test(matched$RM, matched$`1-bar`, paired = TRUE)
t_norm_ob <- t.test(matched$`Non-RM`, matched$`1-bar`, paired = TRUE)

dz <- function(tt) unname(tt$statistic / sqrt(tt$parameter + 1))

tests <- list(t_rm_norm, t_rm_ob, t_norm_ob)
labels <- c("RM vs Non-RM", "RM vs 1-bar", "Non-RM vs 1-bar")

results <- tibble(
  comparison = labels,
  t  = map_dbl(tests, ~ unname(.x$statistic)),
  df = map_dbl(tests, ~ unname(.x$parameter)),
  mean_diff = map_dbl(tests, ~ unname(.x$estimate)),
  p  = map_dbl(tests, ~ .x$p.value),
  dz = map_dbl(tests, dz)
) %>%
  mutate(p_fdr = p.adjust(p, method = "BH")) %>%
  mutate(across(c(t, mean_diff, p, p_fdr, dz), ~ round(.x, 4)))

summary_tbl <- all_scatter %>%
  pivot_longer(-subID, names_to = "cond", values_to = "scatter") %>%
  group_by(cond) %>%
  summarise(mean = round(mean(scatter, na.rm = TRUE), 4),
            sd = round(sd(scatter, na.rm = TRUE), 4),
            n = sum(!is.na(scatter)),
            .groups = "drop")

cat("\n=== exp2: Precision (mean over widths of cell SD / W) ===\n")
print(summary_tbl)
cat("\nPaired comparisons (n =", nrow(matched), "matched subjects, FDR: BH):\n")
print(results)

dir.create("exp2/outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(summary_tbl, "exp2/outputs/tables/precision_summary.csv", row.names = FALSE)
write.csv(results, "exp2/outputs/tables/precision_tests.csv", row.names = FALSE)
