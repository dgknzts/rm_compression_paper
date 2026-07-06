library(tidyverse)
library(lme4)

dir.create("model_structure/outputs", recursive = TRUE, showWarnings = FALSE)

df <- read.csv("exp2/data/processed.csv")
spacing_cut <- quantile(df$correct_space, probs = 1/3, na.rm = TRUE)

base_data <- df %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    rm_type = factor(if_else(number_deviation == -1, "RM", "Non-RM"),
                     levels = c("Non-RM", "RM")),
    correct_num   = factor(correct_num),
    correct_width = factor(correct_width),
    spacing_category = factor(case_when(
      correct_space <= spacing_cut ~ "Smaller",
      correct_space <= 0.9        ~ "Middle",
      TRUE                        ~ "Larger"),
      levels = c("Smaller", "Middle", "Larger")),
    length_deviation = response_stim_length - stim_length
  )

lmer_ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

check_random_slope <- function(outcome, data) {
  d <- data %>% drop_na(all_of(outcome))
  fml <- as.formula(paste0(outcome,
    " ~ rm_type * spacing_category * correct_num * correct_width"))

  m_ri <- lmer(update(fml, . ~ . + (1 | subID)),
               data = d, REML = FALSE)
  m_rs <- lmer(update(fml, . ~ . + (1 + rm_type | subID)),
               data = d, REML = FALSE, control = lmer_ctrl)

  lrt <- anova(m_ri, m_rs)

  tibble(
    outcome = outcome,
    singular_fit = isSingular(m_rs),
    chi2 = lrt$Chisq[2],
    df = lrt$Df[2],
    p = lrt$`Pr(>Chisq)`[2]
  )
}

outcomes <- c("spacing_deviation", "width_deviation",
              "length_deviation", "width_density_deviation")

results <- bind_rows(lapply(outcomes, check_random_slope, data = base_data))

print(results)
write.csv(results, "model_structure/outputs/exp2_random_effects_lrt.csv",
          row.names = FALSE)
