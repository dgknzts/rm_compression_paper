library(tidyverse)

df <- read.csv("exp1/data/processed.csv") %>%
  filter(!is.na(correct_num), !is.na(correct_width)) %>%
  mutate(correct_num   = factor(correct_num),
         correct_width = factor(correct_width))

per_subj <- df %>%
  group_by(subID, correct_num, correct_width) %>%
  summarise(
    n_RM    = sum(number_deviation == -1, na.rm = TRUE),
    n_NoRM  = sum(number_deviation ==  0, na.rm = TRUE),
    n_total = n_RM + n_NoRM,
    RM_pct  = if_else(n_total > 0, 100 * n_RM / n_total, NA_real_),
    .groups = "drop"
  )

summary_tbl <- per_subj %>%
  group_by(correct_num, correct_width) %>%
  summarise(
    n_subj       = n(),
    mean_RM_pct  = mean(RM_pct, na.rm = TRUE),
    sd_RM_pct    = sd(RM_pct,   na.rm = TRUE),
    .groups = "drop"
  )

dir.create("exp1/outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(summary_tbl, "exp1/outputs/tables/rm_strength_by_condition.csv",
          row.names = FALSE)

cat("exp1 rm_strength saved:", nrow(summary_tbl), "rows\n")
print(summary_tbl)
