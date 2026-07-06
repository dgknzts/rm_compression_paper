library(tidyverse)
library(ggplot2)

final_data <- data.frame()

files <- list.files(path = "exp1/data/raw/", pattern = "^p[0-9]+_exp1\\.csv$")

for (i in seq_along(files)) {
  file_name <- files[i]
  full_file_path <- file.path("exp1/data/raw/", file_name)

  df <- read.csv(full_file_path, header = TRUE)

  start_point <- as.numeric(which(!is.na(df$key_resp_4.rt)))
  end_point <- as.numeric(nrow(df))

  df <- df[(start_point + 1):(end_point), ]

  df <- df %>%
    filter(key_resp.rt != "None") %>%
    mutate(key_resp.rt = as.double(key_resp.rt)) %>%
    select(
      participant,
      stim_length,
      amount,
      center_to_center,
      w,
      trial_type,
      loop.thisN,
      response,
      key_resp.rt,
      starting_width_deg,
      response_width_degree,
      next_5.rt,
      resp_group
    ) %>%
    na.omit() %>%
    mutate(response_width_degree = abs(response_width_degree)) %>%
    mutate(response = as.numeric(gsub("num_", "", response))) %>%
    mutate(exp_version = "exp1")

  final_data <- bind_rows(final_data, df)
}

col_order <- c("participant", "resp_group", "trial_type", "loop.thisN", "amount", "response",
               "key_resp.rt", "center_to_center", "w", "response_width_degree",
               "starting_width_deg", "next_5.rt", "stim_length", "exp_version")

final_data <- final_data[, col_order]

final_data$loop.thisN <- final_data$loop.thisN + 1

colnames(final_data) <- c("subID", "keyboard_condition", "trial_type", "trial_number", "correct_num",
                          "response_num", "response_rt", "correct_space", "correct_width",
                          "response_width", "probe_width", "adjustment_duration",
                          "stim_length", "exp_version")

final_data$subID <- as.factor(final_data$subID)

df <- final_data %>%
  mutate(
    number_deviation = response_num - correct_num,
    width_deviation = response_width - correct_width,
    number_deviation_ratio = number_deviation / correct_num,
    width_deviation_ratio = response_width / correct_width,
    actual_pooled_width = correct_width * correct_num,
    response_pooled_width = response_width * response_num,
    pooled_width_deviation = response_pooled_width - actual_pooled_width,
    width_deviation_relative = if_else(correct_width != 0, width_deviation / correct_width, NA_real_),
    absolute_width_deviation = abs(width_deviation)
  )

df_unfiltered <- df
initial_count <- nrow(df_unfiltered)

df_step1 <- df_unfiltered %>%
  filter(response_rt <= 10)

df_step2 <- df_step1 %>%
  filter(number_deviation >= -4, number_deviation <= 4)

df_step3 <- df_step2 %>%
  filter(adjustment_duration <= 15)

df_filtered <- df_step3

filter_steps <- tibble(
  step = c("Initial data",
           "Enumeration RT \u2264 10s",
           "Number deviation [-4,4]",
           "Adjustment RT \u2264 15s"),
  remaining_trials = c(initial_count, nrow(df_step1), nrow(df_step2), nrow(df_step3))
) %>%
  mutate(
    excluded_trials = lag(remaining_trials, default = initial_count) - remaining_trials,
    percent_excluded = round(excluded_trials / lag(remaining_trials, default = initial_count) * 100, 2)
  )

total_excluded <- initial_count - nrow(df_filtered)
total_percent <- round(total_excluded / initial_count * 100, 2)
cat(sprintf("Total trials excluded: %d of %d (%.2f%%)\n",
            total_excluded, initial_count, total_percent))
print(filter_steps)

n_after_qc  <- nrow(df_filtered)
n_rmnorm    <- sum(df_filtered$number_deviation %in% c(-1, 0))
n_other_dev <- n_after_qc - n_rmnorm
pct_other   <- round(n_other_dev / n_after_qc * 100, 2)
cat(sprintf("Trials with number deviation other than -1 or 0: %d of %d (%.2f%%)\n",
            n_other_dev, n_after_qc, pct_other))

df_final <- df_filtered %>%
  select(subID, correct_num, response_num, correct_width, response_width,
         number_deviation, width_deviation, width_deviation_relative)

write.csv(df_final, "exp1/data/processed.csv", row.names = FALSE)

one_bar_data <- data.frame()

for (i in seq_along(files)) {
  file_name <- files[i]
  full_file_path <- file.path("exp1/data/raw/", file_name)

  df_raw <- read.csv(full_file_path, header = TRUE)

  df_1bar <- df_raw %>%
    filter(!is.na(posx) & posx != "") %>%
    mutate(response_width_degree = abs(as.numeric(response_width_degree))) %>%
    select(participant, w, response_width_degree) %>%
    rename(correct_width = w) %>%
    mutate(width_deviation = response_width_degree - correct_width) %>%
    select(participant, correct_width, width_deviation)

  one_bar_data <- bind_rows(one_bar_data, df_1bar)
}

write.csv(one_bar_data, "exp1/data/one_bar_exp1.csv", row.names = FALSE)
