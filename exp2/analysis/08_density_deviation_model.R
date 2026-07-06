library(tidyverse)
library(lme4)
library(emmeans)

format_num <- function(x, digits = 3) {
  formatC(x, format = 'f', digits = digits)
}

format_p <- function(p) {
  if (is.na(p)) 'p = NA'
  else if (p < 0.001) 'p < 0.001'
  else sprintf('p = %.3f', p)
}

df <- read.csv('exp2/data/processed.csv')

spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = factor(case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'), levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  drop_na(width_density_deviation)

model <- lmer(width_density_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 + rm_type | subID),
              data = analysis_data, REML = FALSE,
              control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

residual_sd <- sigma(model)

if (!dir.exists('exp2/outputs/tables')) {
  dir.create('exp2/outputs/tables', recursive = TRUE)
}

emm_density_setsize <- emmeans(model, ~ rm_type | correct_num + correct_width)

write.csv(as.data.frame(summary(emm_density_setsize)),
          'exp2/outputs/tables/density_deviation_emmeans_by_num_width.csv',
          row.names = FALSE)

density_dev_contrasts <- pairs(emm_density_setsize)
density_contrast_summary <- summary(density_dev_contrasts, infer = TRUE)

contrasts_with_d <- as_tibble(density_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

write.csv(as.data.frame(contrasts_with_d),
          'exp2/outputs/tables/density_deviation_rm_contrasts.csv',
          row.names = FALSE)

# RM vs Non-RM by set size (for the by-set-size figure panel), from the canonical model.
emm_density_num <- emmeans(model, ~ rm_type | correct_num)
density_num_contrasts <- as_tibble(summary(pairs(emm_density_num), infer = TRUE)) %>%
  mutate(cohen_d = estimate / residual_sd,
         p_fdr = p.adjust(p.value, method = 'BH'))

write.csv(as.data.frame(density_num_contrasts),
          'exp2/outputs/tables/density_deviation_rm_contrasts_by_num.csv',
          row.names = FALSE)

overall_emm <- emmeans(model, ~ rm_type)
overall_contrast <- summary(pairs(overall_emm), infer = TRUE)
cohen_d_overall <- overall_contrast$estimate / residual_sd

overall_out <- as_tibble(overall_contrast) %>%
  mutate(cohen_d = cohen_d_overall)

write.csv(as.data.frame(overall_out),
          'exp2/outputs/tables/density_deviation_overall.csv',
          row.names = FALSE)

vs_zero_overall <- summary(overall_emm, infer = TRUE, null = 0) %>%
  as_tibble() %>%
  mutate(grouping = 'overall', correct_num = NA)

vs_zero_by_num <- summary(emmeans(model, ~ rm_type | correct_num), infer = TRUE, null = 0) %>%
  as_tibble() %>%
  mutate(grouping = 'by_num')

vs_zero_out <- bind_rows(vs_zero_overall, vs_zero_by_num) %>%
  rename_with(~ 'lower.CL', any_of(c('asymp.LCL', 'lower.CL'))) %>%
  rename_with(~ 'upper.CL', any_of(c('asymp.UCL', 'upper.CL'))) %>%
  mutate(p_fdr = p.adjust(p.value, method = 'BH'))

write.csv(as.data.frame(vs_zero_out),
          'exp2/outputs/tables/density_deviation_vs_zero.csv',
          row.names = FALSE)

cat('\n=== exp2 density deviation: overall RM vs Non-RM ===\n')
cat(sprintf('estimate = %s, %s, d = %s\n',
            format_num(overall_contrast$estimate),
            format_p(overall_contrast$p.value),
            format_num(cohen_d_overall)))

cat('\n=== exp2 density deviation vs zero (perceived - physical) ===\n')
print(vs_zero_out %>% select(grouping, correct_num, rm_type, emmean, lower.CL, upper.CL, p.value, p_fdr))
