predict_null       <- function(W_act, ...) W_act
predict_perfect    <- function(W_act, pool, ...) pool * W_act
predict_partial_nc <- function(W_act, pool, gamma, ...) pool^gamma * W_act
predict_compression <- function(W_act, pool, alpha, ...) alpha * pool * W_act
predict_partial    <- function(W_act, pool, alpha, gamma, ...) alpha * pool^gamma * W_act
predict_linear     <- function(W_act, pool, a0, a1, gamma, ...) (a0 + a1 * W_act) * pool^gamma * W_act

compute_fit_stats <- function(sse, k, n) {
  sigma <- sqrt(sse / n)
  ll <- -n / 2 * log(2 * pi) - n * log(max(sigma, 1e-10)) - n / 2
  list(LL = ll,
       AIC = -2 * ll + 2 * (k + 1),
       BIC = -2 * ll + log(n) * (k + 1),
       RMSE = sqrt(sse / n))
}

fit_alpha <- function(sub) {
  W <- sub$W_rep
  obj <- function(alpha) sum((W - alpha * sub$W_act)^2)
  fit <- optim(0.95, obj, method = "Brent", lower = 0.3, upper = 1.5)
  fit$par
}

fit_alpha_gamma <- function(sub) {
  W <- sub$W_rep
  obj <- function(par) sum((W - par[1] * sub$pool^par[2] * sub$W_act)^2)
  fit <- optim(c(0.9, 0.5), obj, method = "L-BFGS-B",
               lower = c(0.3, -1), upper = c(1.5, 3))
  c(alpha = fit$par[1], gamma = fit$par[2])
}

load_and_prepare <- function(exp_name) {
  readr::read_csv(paste0(exp_name, "/data/processed.csv"),
                  show_col_types = FALSE) %>%
    dplyr::filter(number_deviation %in% c(-1, 0)) %>%
    dplyr::mutate(
      Participant = as.character(subID),
      N_act = correct_num,
      N_rep = response_num,
      W_act = correct_width,
      W_rep = response_width,
      RM    = as.integer(N_rep < N_act),
      RM_f  = factor(RM, levels = c(0, 1), labels = c("noRM", "RM")),
      pool  = N_act / N_rep
    )
}

model_order <- c("Null", "Perfect Pooling", "Partial Pooling",
                 "Compression + Pooling", "Compression + Partial Pooling",
                 "Linear Compression + Partial Pooling")

rm_conditions <- data.frame(
  N_act = c(3, 4, 5),
  N_rep = c(2, 3, 4),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(pool = N_act / N_rep,
                label = paste0(N_act, "\u2192", N_rep))

condition_colours <- c("#E69F00", "#56B4E9", "#CC79A7")
names(condition_colours) <- rm_conditions$label

rm_colours <- c("noRM" = "#2E8B57", "RM" = "#DC143C")

my_theme <- ggplot2::theme(
  axis.title.x = ggplot2::element_text(color = "black", size = 14, face = "bold",
                                       margin = ggplot2::margin(t = 10)),
  axis.title.y = ggplot2::element_text(color = "black", size = 14, face = "bold",
                                       margin = ggplot2::margin(r = 10)),
  axis.text.x  = ggplot2::element_text(size = 12, face = "bold", color = "black"),
  axis.text.y  = ggplot2::element_text(size = 12, face = "bold", color = "black"),
  axis.line    = ggplot2::element_line(colour = "black", linewidth = 0.8),
  panel.border     = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  panel.background = ggplot2::element_blank(),
  strip.text       = ggplot2::element_text(size = 12, face = "bold"),
  legend.title     = ggplot2::element_text(size = 12, face = "bold"),
  legend.text      = ggplot2::element_text(size = 10),
  plot.title       = ggplot2::element_text(size = 14, face = "bold"),
  panel.spacing    = grid::unit(1.5, "lines")
)
