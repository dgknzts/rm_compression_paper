library(tidyverse)

df <- read.csv("exp1/data/processed.csv")
one_bar <- read.csv("exp1/data/one_bar_exp1.csv")

rel_dev <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(condition = if_else(number_deviation == -1, "RM", "Non-RM"),
         rel = width_deviation / correct_width) %>%
  group_by(subID, condition) %>%
  summarise(mean_dev = mean(rel, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = mean_dev) %>%
  mutate(subID = as.character(subID))

ob_dev <- one_bar %>%
  mutate(rel = width_deviation / correct_width) %>%
  group_by(participant) %>%
  summarise(`1-bar` = mean(rel, na.rm = TRUE), .groups = "drop") %>%
  rename(subID = participant) %>%
  mutate(subID = as.character(subID))

all_dev <- rel_dev %>% left_join(ob_dev, by = "subID")
matched <- all_dev %>% drop_na()

t_rm_zero   <- t.test(all_dev$RM, mu = 0)
t_norm_zero <- t.test(all_dev$`Non-RM`, mu = 0)
t_ob_zero   <- t.test(all_dev$`1-bar`, mu = 0)
t_rm_ob     <- t.test(matched$RM, matched$`1-bar`, paired = TRUE)
t_norm_ob   <- t.test(matched$`Non-RM`, matched$`1-bar`, paired = TRUE)

dz <- function(tt) unname(tt$statistic / sqrt(tt$parameter + 1))

tests <- list(t_rm_zero, t_norm_zero, t_ob_zero, t_rm_ob, t_norm_ob)
labels <- c("RM vs 0", "Non-RM vs 0", "1-bar vs 0", "RM vs 1-bar", "Non-RM vs 1-bar")

results <- tibble(
  comparison = labels,
  t  = map_dbl(tests, ~ unname(.x$statistic)),
  df = map_dbl(tests, ~ unname(.x$parameter)),
  p  = map_dbl(tests, ~ .x$p.value),
  dz = map_dbl(tests, dz)
) %>%
  mutate(p_fdr = p.adjust(p, method = "BH")) %>%
  mutate(across(c(t, p, p_fdr, dz), ~ round(.x, 4)))

cat("\n=== exp1: Accuracy t-tests (FDR: BH across 5 tests) ===\n")
print(results)

dir.create("exp1/outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(results, "exp1/outputs/tables/ttest_results.csv", row.names = FALSE)
