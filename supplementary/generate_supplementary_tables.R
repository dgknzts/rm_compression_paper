library(tidyverse)

# Data
rm_a      <- read.csv("exp1/outputs/tables/rm_strength_by_condition.csv")
rm_b      <- read.csv("exp2/outputs/tables/rm_strength_by_condition.csv")
rm_b_spc  <- read.csv("exp2/outputs/tables/rm_strength_by_condition_spacing.csv")

fits_a    <- read.csv("exp1/outputs/tables/model_fits.csv")
fits_b    <- read.csv("exp2/outputs/tables/model_fits.csv")
lrt_a     <- read.csv("exp1/outputs/tables/lrt_results.csv")
lrt_b     <- read.csv("exp2/outputs/tables/lrt_results.csv")

# Helpers
fmt_msd   <- function(m, sd)   sprintf("%.1f (%.1f)", m, sd)
fmt_aic   <- function(x)       sprintf("$%s$%.2f", if (x < 0) "-" else "", abs(x))
fmt_p     <- function(p)       if (p < .001) "$<.001$" else sprintf("$= %.3f$", p)

model_order <- c("Null", "Perfect Pooling", "Partial Pooling",
                 "Compression + Pooling", "Compression + Partial Pooling",
                 "Linear Compression + Partial Pooling")
model_label <- c("M0: Null", "M1: Perfect Pooling", "M2: Partial Pooling",
                 "M3: Compression + Pooling", "M4: Compression + Partial Pooling",
                 "M5: Linear Compression + Partial Pooling")
names(model_label) <- model_order

lrt_comp <- c("M0 vs.\\ M2", "M1 vs.\\ M3", "M2 vs.\\ M4",
              "M3 vs.\\ M4", "M4 vs.\\ M5")
lrt_param <- c("$\\gamma$ (pooling)", "$\\alpha$ (compression)",
               "$\\alpha$ added to $\\gamma$",
               "partial vs.\\ perfect $\\gamma$",
               "width-dependent $\\alpha$")

# Build model summary from per-participant fits
build_model_tbl <- function(fits) {
  best <- fits %>% group_by(Participant) %>%
    slice_min(AIC, n = 1, with_ties = FALSE) %>%
    ungroup() %>% count(Model, name = "best_n")
  fits %>%
    group_by(Model, k) %>%
    summarise(mean_AIC = mean(AIC), mean_BIC = mean(BIC),
              mean_RMSE = mean(RMSE), .groups = "drop") %>%
    left_join(best, by = "Model") %>%
    mutate(best_n = replace_na(best_n, 0),
           Model  = factor(Model, levels = model_order)) %>%
    arrange(Model)
}
aic_a <- build_model_tbl(fits_a)
aic_b <- build_model_tbl(fits_b)

# TABLE S1: RM strength

make_rm_panel_a <- function(ta, tb) {
  rows <- character(0)
  for (i in seq_along(sort(unique(ta$correct_num)))) {
    num <- sort(unique(ta$correct_num))[i]
    if (i > 1) rows <- c(rows, "  \\addlinespace")
    sa <- ta %>% filter(correct_num == num) %>% arrange(correct_width)
    sb <- tb %>% filter(correct_num == num) %>% arrange(correct_width)
    for (j in seq_len(nrow(sa))) {
      rows <- c(rows, sprintf("  %s & %.2f & %s & %s \\\\",
        sa$correct_num[j],
        as.numeric(as.character(sa$correct_width[j])),
        fmt_msd(sa$mean_RM_pct[j], sa$sd_RM_pct[j]),
        fmt_msd(sb$mean_RM_pct[j], sb$sd_RM_pct[j])))
    }
  }
  rows
}

make_rm_panel_b <- function(tbl) {
  spc_order <- c("Smaller", "Middle", "Larger")
  rows <- character(0)
  for (i in seq_along(sort(unique(tbl$correct_num)))) {
    num <- sort(unique(tbl$correct_num))[i]
    if (i > 1) rows <- c(rows, "  \\addlinespace")
    sub <- tbl %>% filter(correct_num == num) %>%
      arrange(factor(spacing_category, levels = spc_order))
    for (j in seq_len(nrow(sub))) {
      rows <- c(rows, sprintf("  %s & %s & $-$ & %s \\\\",
        if (j == 1) as.character(sub$correct_num[j]) else "",
        sub$spacing_category[j],
        fmt_msd(sub$mean_RM_pct[j], sub$sd_RM_pct[j])))
    }
  }
  rows
}

table_s1 <- c(
  "\\begin{table}[H]",
  "\\raggedright",
  "\\caption{RM percentage by condition.}",
  "\\label{tab:rm-strength}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{c l r r}",
  "\\toprule",
  " & & \\multicolumn{2}{c}{RM\\% \\textit{M} (\\textit{SD})} \\\\",
  "\\cmidrule(lr){3-4}",
  "Set size & & Exp.\\ 1 (\\textit{N}\\,=\\,21) & Exp.\\ 2 (\\textit{N}\\,=\\,20) \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{By bar width (\\textdegree)}} \\\\[3pt]",
  make_rm_panel_a(rm_a, rm_b),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{By spacing category (Experiment~2 only)}} \\\\[3pt]",
  make_rm_panel_b(rm_b_spc),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste("\\item \\textit{Note.} RM trials = trials on which the observer",
        "underreported the number of bars by one (number deviation $= -1$);",
        "No-RM trials = correct number reports (number deviation $= 0$).",
        "RM\\% is the percentage of RM trials out of RM + No-RM trials,",
        "computed per participant and averaged across participants",
        "($-$ = not applicable). Spacing category (Smaller, Middle, Larger)",
        "was derived by splitting the range of presented inter-item spacings",
        "into tertiles. Values are means with standard deviations in parentheses."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# TABLE S2: Model fit scores

make_aic_rows <- function(tbl) {
  rows <- character(0)
  for (i in seq_len(nrow(tbl))) {
    r <- tbl[i, ]
    rows <- c(rows, sprintf(
      "  %-44s & %d & %s & %s & %.3f & %d \\\\",
      model_label[as.character(r$Model)],
      r$k,
      fmt_aic(r$mean_AIC),
      fmt_aic(r$mean_BIC),
      r$mean_RMSE,
      r$best_n
    ))
  }
  rows
}

table_s2 <- c(
  "\\begin{table}[H]",
  "\\raggedright",
  "\\caption{Model fit scores averaged across participants.}",
  "\\label{tab:model-fits}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{l c r r r r}",
  "\\toprule",
  "Model & $k$ & Mean AIC & Mean BIC & Mean RMSE & Best-fit (\\textit{n}) \\\\",
  "\\midrule",
  sprintf("\\multicolumn{6}{l}{\\textit{Experiment 1} (\\textit{N} = %d)} \\\\", 21),
  make_aic_rows(aic_a),
  "\\addlinespace",
  sprintf("\\multicolumn{6}{l}{\\textit{Experiment 2} (\\textit{N} = %d)} \\\\", 20),
  make_aic_rows(aic_b),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste("\\item \\textit{Note.} $k$ = number of free parameters. AIC, BIC, and RMSE",
        "were computed per participant and averaged. Lower (more negative) AIC and BIC",
        "indicate better fit. Best-fit ($n$) is the number of participants for whom",
        "the model had the lowest AIC. M5 is the winning model in both experiments."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# TABLE S3: Likelihood-ratio tests

make_lrt_rows <- function(tbl) {
  rows <- character(0)
  for (i in seq_len(nrow(tbl))) {
    r <- tbl[i, ]
    rows <- c(rows, sprintf(
      "  %s & %s & %.2f & %d & %s & %d/%d \\\\",
      lrt_comp[i], lrt_param[i],
      r$group_chi2, r$group_df,
      fmt_p(r$group_p),
      r$n_sig, r$n_total
    ))
  }
  rows
}

table_s3 <- c(
  "\\begin{table}[H]",
  "\\raggedright",
  "\\caption{Likelihood-ratio tests.}",
  "\\label{tab:lrt}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{l l r r l c}",
  "\\toprule",
  "Comparison & Added parameter & $\\chi^{2}$ & $df$ & $p$ & Sig.\\ ($n$/$N$) \\\\",
  "\\midrule",
  sprintf("\\multicolumn{6}{l}{\\textit{Experiment 1} (\\textit{N} = %d)} \\\\", 21),
  make_lrt_rows(lrt_a),
  "\\addlinespace",
  sprintf("\\multicolumn{6}{l}{\\textit{Experiment 2} (\\textit{N} = %d)} \\\\", 20),
  make_lrt_rows(lrt_b),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste("\\item \\textit{Note.} Group-level $\\chi^{2}$ statistics are sums of",
        "per-participant likelihood-ratio statistics, each with $df = \\Delta k = 1$,",
        "yielding a group $df$ equal to the number of participants.",
        "Sig.\\ ($n$/$N$) indicates the number of participants for whom the alternative",
        "model significantly improved fit at $p < .05$.",
        "Each comparison adds one parameter to the simpler model."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# Assemble full .tex
preamble <- c(
  "% Supplementary tables for RM loss-and-gain manuscript.",
  "% Generated by supplementary/generate_supplementary_tables.R",
  "\\documentclass[11pt]{article}",
  "\\usepackage[a4paper,margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{amsmath}",
  "\\usepackage{float}",
  "\\usepackage[justification=raggedright,singlelinecheck=false]{caption}",
  "\\renewcommand{\\thetable}{S\\arabic{table}}",
  "\\setcounter{table}{0}",
  "\\raggedbottom",
  "\\begin{document}"
)

lines <- c(preamble, "",
           table_s1, "", "\\clearpage", "",
           table_s2, "", "\\clearpage", "",
           table_s3, "",
           "\\end{document}")

out_tex <- "supplementary/supplementary_tables.tex"
writeLines(lines, out_tex)
cat("Written:", out_tex, "\n")
