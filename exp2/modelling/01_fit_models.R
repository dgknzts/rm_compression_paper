library(dplyr)
library(tidyr)

source("shared/model_functions.R")

fit_participant <- function(sub) {
  n <- nrow(sub)
  W <- sub$W_rep
  results <- list()

  pred <- predict_null(sub$W_act)
  sse <- sum((W - pred)^2)
  s <- compute_fit_stats(sse, 0, n)
  results[[1]] <- data.frame(
    Model = "Null", k = 0, alpha = NA, a1 = NA, gamma = NA,
    SSE = sse, LL = s$LL, AIC = s$AIC, BIC = s$BIC, RMSE = s$RMSE)

  pred <- predict_perfect(sub$W_act, sub$pool)
  sse <- sum((W - pred)^2)
  s <- compute_fit_stats(sse, 0, n)
  results[[2]] <- data.frame(
    Model = "Perfect Pooling", k = 0, alpha = NA, a1 = NA, gamma = NA,
    SSE = sse, LL = s$LL, AIC = s$AIC, BIC = s$BIC, RMSE = s$RMSE)

  obj <- function(gamma) sum((W - predict_partial_nc(sub$W_act, sub$pool, gamma))^2)
  fit <- optim(0.5, obj, method = "Brent", lower = -1, upper = 3)
  s <- compute_fit_stats(fit$value, 1, n)
  results[[3]] <- data.frame(
    Model = "Partial Pooling", k = 1,
    alpha = NA, a1 = NA, gamma = fit$par,
    SSE = fit$value, LL = s$LL, AIC = s$AIC, BIC = s$BIC, RMSE = s$RMSE)

  obj <- function(alpha) sum((W - predict_compression(sub$W_act, sub$pool, alpha))^2)
  fit <- optim(0.95, obj, method = "Brent", lower = 0.3, upper = 1.5)
  s <- compute_fit_stats(fit$value, 1, n)
  results[[4]] <- data.frame(
    Model = "Compression + Pooling", k = 1,
    alpha = fit$par, a1 = NA, gamma = NA,
    SSE = fit$value, LL = s$LL, AIC = s$AIC, BIC = s$BIC, RMSE = s$RMSE)

  obj <- function(par) sum((W - predict_partial(sub$W_act, sub$pool, par[1], par[2]))^2)
  fit <- optim(c(0.9, 0.5), obj, method = "L-BFGS-B",
               lower = c(0.3, -1), upper = c(1.5, 3))
  s <- compute_fit_stats(fit$value, 2, n)
  results[[5]] <- data.frame(
    Model = "Compression + Partial Pooling", k = 2,
    alpha = fit$par[1], a1 = NA, gamma = fit$par[2],
    SSE = fit$value, LL = s$LL, AIC = s$AIC, BIC = s$BIC, RMSE = s$RMSE)

  obj <- function(par) sum((W - predict_linear(sub$W_act, sub$pool, par[1], par[2], par[3]))^2)
  fit <- optim(c(0.9, 0, 0.5), obj, method = "L-BFGS-B",
               lower = c(0.1, -3, -1), upper = c(2, 3, 3))
  s <- compute_fit_stats(fit$value, 3, n)
  results[[6]] <- data.frame(
    Model = "Linear Compression + Partial Pooling", k = 3,
    alpha = fit$par[1], a1 = fit$par[2], gamma = fit$par[3],
    SSE = fit$value, LL = s$LL, AIC = s$AIC, BIC = s$BIC, RMSE = s$RMSE)

  out <- do.call(rbind, results)
  out$Participant <- sub$Participant[1]
  out$n <- n
  rownames(out) <- NULL
  out
}


data <- load_and_prepare("exp2")
all_fits <- do.call(rbind, lapply(split(data, data$Participant), fit_participant))
all_fits$Model <- factor(all_fits$Model, levels = model_order)

out_tables <- "exp2/outputs/tables"
out_figures <- "exp2/outputs"
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures, recursive = TRUE, showWarnings = FALSE)
saveRDS(all_fits, paste0(out_tables, "/model_fits.rds"))
readr::write_csv(all_fits, paste0(out_tables, "/model_fits.csv"))
