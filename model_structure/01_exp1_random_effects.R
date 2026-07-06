library(tidyverse)
library(lme4)

dir.create("model_structure/outputs", recursive = TRUE, showWarnings = FALSE)

df <- read.csv("exp1/data/processed.csv")

analysis_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"),
                     levels = c("Non-RM", "RM")),
    correct_num   = factor(correct_num),
    correct_width = factor(correct_width)
  ) %>%
  drop_na(width_deviation)

lmer_ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

m_ri <- lmer(width_deviation ~ rm_type * correct_num * correct_width +
               (1 | subID), data = analysis_data, REML = FALSE)
m_rs <- lmer(width_deviation ~ rm_type * correct_num * correct_width +
               (1 + rm_type | subID), data = analysis_data, REML = FALSE,
             control = lmer_ctrl)

lrt <- anova(m_ri, m_rs)

results <- tibble(
  outcome = "width_deviation",
  singular_fit = isSingular(m_rs),
  chi2 = lrt$Chisq[2],
  df = lrt$Df[2],
  p = lrt$`Pr(>Chisq)`[2]
)

print(results)
write.csv(results, "model_structure/outputs/exp1_random_effects_lrt.csv",
          row.names = FALSE)
