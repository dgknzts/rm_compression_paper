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
    spacing_category = case_when(
      correct_space <= spacing_cut ~ 'Smaller',
      correct_space <= 0.9 ~ 'Middle',
      TRUE ~ 'Larger'
    ),
    spacing_category = factor(spacing_category, levels = c('Smaller', 'Middle', 'Larger'))
  ) %>%
  drop_na(spacing_deviation)

model <- lmer(spacing_deviation ~ rm_type * spacing_category * correct_num * correct_width + (1 + rm_type | subID),
              data = analysis_data, REML = FALSE,
              control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

emm_spacing_setsize <- emmeans(model, ~ rm_type | spacing_category + correct_num)
spacing_dev_contrasts <- pairs(emm_spacing_setsize)

if (!dir.exists('exp2/outputs/tables')) {
  dir.create('exp2/outputs/tables', recursive = TRUE)
}

write.csv(as.data.frame(summary(emm_spacing_setsize)),
          'exp2/outputs/tables/spacing_deviation_emmeans_by_spacing_setsize.csv',
          row.names = FALSE)

spacing_contrast_summary <- summary(spacing_dev_contrasts, infer = TRUE)
residual_sd <- sigma(model)

contrasts_with_d <- as_tibble(spacing_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

write.csv(as.data.frame(contrasts_with_d),
          'exp2/outputs/tables/spacing_deviation_rm_contrasts.csv',
          row.names = FALSE)

overall_emm <- emmeans(model, ~ rm_type)
overall_contrast <- summary(pairs(overall_emm), infer = TRUE)

o_means <- as_tibble(summary(overall_emm))
o_rm <- o_means %>% filter(rm_type == 'RM')
o_norm <- o_means %>% filter(rm_type == 'Non-RM')

cohen_d_overall <- overall_contrast$estimate / residual_sd

spacing_emmeans_detailed <- summary(emm_spacing_setsize)
