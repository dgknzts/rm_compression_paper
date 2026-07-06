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

df <- read.csv('exp1/data/processed.csv')

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, 'RM', 'Non-RM'), levels = c('Non-RM', 'RM')),
    correct_num = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

model <- lmer(width_deviation ~ rm_type * correct_num * correct_width + (1 + rm_type | subID),
              data = analysis_data, REML = FALSE,
              control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))

emm_width_setsize <- emmeans(model, ~ rm_type | correct_width + correct_num)
width_dev_contrasts <- pairs(emm_width_setsize)

dir.create("exp1/outputs/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(as.data.frame(summary(emm_width_setsize)),
          'exp1/outputs/tables/width_deviation_emmeans_by_width_setsize.csv',
          row.names = FALSE)

width_contrast_summary <- summary(width_dev_contrasts, infer = TRUE)
residual_sd <- sigma(model)

contrasts_with_d <- as_tibble(width_contrast_summary) %>%
  mutate(
    cohen_d = estimate / residual_sd,
    p_fdr = p.adjust(p.value, method = 'BH')
  )

write.csv(as.data.frame(contrasts_with_d),
          'exp1/outputs/tables/width_deviation_rm_contrasts.csv',
          row.names = FALSE)

overall_emm <- emmeans(model, ~ rm_type)
overall_contrast <- summary(pairs(overall_emm), infer = TRUE)
