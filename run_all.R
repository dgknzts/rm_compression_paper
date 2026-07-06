# Runs the full analysis pipeline for both experiments.
# Run from the repository root. Raw data (available on OSF) is only needed
# for the preprocessing step; all later steps run from the bundled
# processed data.

for (exp in c("exp1", "exp2")) {
  dir.create(file.path(exp, "outputs", "tables"),  recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(exp, "outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
}
dir.create("shared/outputs",          showWarnings = FALSE)
dir.create("model_structure/outputs", showWarnings = FALSE)

run_preprocessing <- function(exp, script) {
  if (length(list.files(file.path(exp, "data", "raw"), pattern = "\\.csv$")) > 0) {
    source(script)
  } else {
    message(exp, ": no raw data found (download from OSF) - using bundled processed.csv")
  }
}

cat("========== EXPERIMENT 1 (single-bar probe) ==========\n")

run_preprocessing("exp1", "exp1/analysis/01_preprocessing.R")
source("exp1/analysis/02_width_deviation_model.R")
source("exp1/analysis/03_ttest_analysis.R")
source("exp1/analysis/04_precision_analysis.R")
source("exp1/analysis/05_width_joint.R")
source("exp1/analysis/06_rm_strength.R")

source("exp1/modelling/01_fit_models.R")
source("exp1/modelling/02_model_comparison.R")

source("exp1/plotting/01_figure.R")
source("exp1/plotting/02_joint_width.R")

cat("========== EXPERIMENT 2 (multi-bar probe) ==========\n")

run_preprocessing("exp2", "exp2/analysis/01_preprocessing.R")
source("exp2/analysis/02_spacing_deviation_model.R")
source("exp2/analysis/03_width_deviation_model.R")
source("exp2/analysis/04_ttest_analysis.R")
source("exp2/analysis/05_precision_analysis.R")
source("exp2/analysis/06_width_spacing_joint.R")
source("exp2/analysis/07_length_deviation_model.R")
source("exp2/analysis/08_density_deviation_model.R")
source("exp2/analysis/09_rm_strength.R")

source("exp2/modelling/01_fit_models.R")
source("exp2/modelling/02_model_comparison.R")

source("exp2/plotting/01_figure.R")
source("exp2/plotting/02_joint_width_spacing.R")

cat("========== RANDOM-EFFECTS STRUCTURE ==========\n")

source("model_structure/01_exp1_random_effects.R")
source("model_structure/02_exp2_random_effects.R")

cat("========== FIGURE 1 MODEL PANELS ==========\n")

source("shared/01_model_schema.R")
source("shared/02_model_contour.R")

cat("========== SUPPLEMENTARY TABLES ==========\n")

source("supplementary/generate_supplementary_tables.R")

cat("========== DONE ==========\n")
