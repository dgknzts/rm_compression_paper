library(tidyverse)

df <- read.csv("exp2/data/processed.csv")
spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

df <- df %>%
  filter(!is.na(correct_num), !is.na(correct_width)) %>%
  mutate(
    correct_num      = factor(correct_num),
    correct_width    = factor(correct_width),
    spacing_category = factor(case_when(
      correct_space <= spacing_cut ~ "Smaller",
      correct_space <= 0.9        ~ "Middle",
      TRUE                        ~ "Larger"),
      levels = c("Smaller", "Middle", "Larger"))
  )

compute_rm_pct <- function(data, ...) {
  grp_vars <- rlang::enquos(...)
  data %>%
    group_by(subID, !!!grp_vars) %>%
    summarise(
      n_RM    = sum(number_deviation == -1, na.rm = TRUE),
      n_NoRM  = sum(number_deviation ==  0, na.rm = TRUE),
      n_total = n_RM + n_NoRM,
      RM_pct  = if_else(n_total > 0, 100 * n_RM / n_total, NA_real_),
      .groups = "drop"
    ) %>%
    group_by(!!!grp_vars) %>%
    summarise(
      n_subj      = n(),
      mean_RM_pct = mean(RM_pct, na.rm = TRUE),
      sd_RM_pct   = sd(RM_pct,   na.rm = TRUE),
      .groups = "drop"
    )
}

tbl_2way    <- compute_rm_pct(df, correct_num, correct_width)
tbl_spacing <- compute_rm_pct(df, correct_num, spacing_category)

dir.create("exp2/outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(tbl_2way,    "exp2/outputs/tables/rm_strength_by_condition.csv",         row.names = FALSE)
write.csv(tbl_spacing, "exp2/outputs/tables/rm_strength_by_condition_spacing.csv", row.names = FALSE)

cat("exp2 rm_strength (num x width):", nrow(tbl_2way), "rows\n")
print(tbl_2way)
cat("\nexp2 rm_strength (num x spacing):", nrow(tbl_spacing), "rows\n")
print(tbl_spacing)
