library(dplyr)

source("shared/model_functions.R")

run_lrt <- function(all_fits, model_null, model_alt, df_diff) {
  ll_0 <- all_fits %>% filter(Model == model_null) %>% arrange(Participant) %>% pull(LL)
  ll_1 <- all_fits %>% filter(Model == model_alt) %>% arrange(Participant) %>% pull(LL)
  chi2 <- -2 * (ll_0 - ll_1)
  p_vals <- pchisq(chi2, df = df_diff, lower.tail = FALSE)
  total_chi2 <- sum(chi2)
  total_df <- length(chi2) * df_diff
  group_p <- pchisq(total_chi2, df = total_df, lower.tail = FALSE)
  data.frame(
    Null = model_null, Alternative = model_alt, df = df_diff,
    group_chi2 = round(total_chi2, 2), group_df = total_df,
    group_p = group_p, n_sig = sum(p_vals < 0.05), n_total = length(p_vals)
  )
}

lrt_comparisons <- list(
  c("Null",                          "Partial Pooling",                      "1"),
  c("Perfect Pooling",               "Compression + Pooling",                "1"),
  c("Partial Pooling",               "Compression + Partial Pooling",        "1"),
  c("Compression + Pooling",         "Compression + Partial Pooling",        "1"),
  c("Compression + Partial Pooling", "Linear Compression + Partial Pooling", "1")
)

all_fits <- readRDS("exp1/outputs/tables/model_fits.rds")
out_tables <- "exp1/outputs/tables"

model_summary <- all_fits %>%
  group_by(Model) %>%
  summarise(k = first(k), mean_AIC = mean(AIC), mean_RMSE = mean(RMSE),
            .groups = "drop") %>%
  arrange(mean_AIC) %>%
  mutate(across(where(is.numeric) & !matches("^k$"), ~ round(., 3)))

best_pp <- all_fits %>%
  group_by(Participant) %>%
  slice_min(AIC, n = 1, with_ties = FALSE) %>%
  ungroup()
best_counts <- as.data.frame(table(best_pp$Model))
colnames(best_counts) <- c("Model", "Count")

lrt_results <- do.call(rbind, lapply(lrt_comparisons, function(x) {
  run_lrt(all_fits, x[1], x[2], as.integer(x[3]))
}))

params_m4 <- all_fits %>%
  filter(Model == "Compression + Partial Pooling") %>%
  select(Participant, alpha, gamma)
params_m5 <- all_fits %>%
  filter(Model == "Linear Compression + Partial Pooling") %>%
  select(Participant, alpha, a1, gamma)

gamma_summary <- params_m5 %>%
  summarise(
    n = n(),
    mean_gamma = round(mean(gamma), 3),
    sd_gamma = round(sd(gamma), 3),
    n_above_0 = sum(gamma > 0),
    n_below_1 = sum(gamma < 1),
    n_partial = sum(gamma > 0 & gamma < 1)
  )

readr::write_csv(model_summary, paste0(out_tables, "/aic_summary.csv"))
readr::write_csv(best_counts, paste0(out_tables, "/best_model_counts.csv"))
readr::write_csv(lrt_results, paste0(out_tables, "/lrt_results.csv"))
readr::write_csv(gamma_summary, paste0(out_tables, "/gamma_summary.csv"))
saveRDS(list(params_m4 = params_m4, params_m5 = params_m5),
        paste0(out_tables, "/param_estimates.rds"))


