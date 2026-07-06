library(tidyverse)
library(ggplot2)

final_data <- data.frame()

files <- list.files(path = "exp2/data/raw/", pattern = "^[0-9]+_exp2\\.csv$")

for (i in seq_along(files)) {
  file_name <- files[i]
  full_file_path <- file.path("exp2/data/raw/", file_name)

  df <- read.csv(full_file_path, header = TRUE)

  start_point <- as.numeric(which(!is.na(df$key_resp_4.rt)))
  end_point <- ifelse(nrow(df) == 129,
                      as.numeric(which(!is.na(df$text.started))) - 1,
                      as.numeric(nrow(df)))

  df <- df[(start_point + 1):(end_point), ]

  df <- df %>%
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
      starting_space_deg,
      response_width_degree,
      response_spacing_degree,
      next_5.rt,
      resp_group
    ) %>%
    na.omit() %>%
    mutate(response_width_degree = abs(response_width_degree)) %>%
    mutate(response = as.numeric(gsub("num_", "", response))) %>%
    mutate(
      response_spacing_degree = case_when(
        response_spacing_degree == "9999" ~ 0,
        as.numeric(response_spacing_degree) >= 0 ~ as.numeric(response_spacing_degree)
      ),
      starting_space_deg = case_when(
        starting_space_deg > 3000 ~ 0,
        starting_space_deg < 3000 ~ starting_space_deg
      )
    ) %>%
    mutate(exp_version = "exp2")

  final_data <- bind_rows(final_data, df)
}

col_order <- c("participant", "resp_group", "trial_type", "loop.thisN", "amount", "response",
               "key_resp.rt", "center_to_center", "response_spacing_degree", "starting_space_deg",
               "w", "response_width_degree", "starting_width_deg", "next_5.rt", "stim_length", "exp_version")

final_data <- final_data[, col_order]

final_data$loop.thisN <- final_data$loop.thisN + 1

colnames(final_data) <- c("subID", "keyboard_condition", "trial_type", "trial_number", "correct_num",
                         "response_num", "response_rt", "correct_space", "response_space", "probe_space",
                         "correct_width", "response_width", "probe_width", "adjustment_duration",
                         "stim_length", "exp_version")

final_data$subID <- as.factor(final_data$subID)

df <- final_data %>%
  mutate(
    number_deviation = response_num - correct_num,
    width_deviation = response_width - correct_width,
    spacing_deviation = response_space - correct_space,

    number_deviation_ratio = number_deviation / correct_num,
    width_deviation_ratio = response_width / correct_width,
    spacing_deviation_ratio = response_space / correct_space,

    response_stim_length = (response_space * (response_num - 1)) + response_width,
    compression_rate = response_stim_length / stim_length,

    actual_pooled_width = correct_width * correct_num,
    response_pooled_width = response_width * response_num,
    pooled_width_deviation = response_pooled_width - actual_pooled_width,

    actual_width_density = actual_pooled_width / stim_length,
    response_width_density = response_pooled_width / response_stim_length,
    width_density_deviation = response_width_density - actual_width_density,

    actual_edge_to_edge_spacing = correct_space - correct_width,
    response_edge_to_edge_spacing = response_space - response_width,
    edge_to_edge_spacing_deviation = response_edge_to_edge_spacing - actual_edge_to_edge_spacing,

    width_deviation_relative = if_else(correct_width != 0, width_deviation / correct_width, NA_real_),
    spacing_deviation_relative = if_else(correct_space != 0, spacing_deviation / correct_space, NA_real_),

    spacing = case_when(
      correct_space <= 0.8 ~ "small",
      correct_space > 0.8 & correct_space <= 1 ~ "middle",
      correct_space > 1 ~ "large",
      TRUE ~ NA_character_
    )
  )

df_unfiltered <- df
initial_count <- nrow(df_unfiltered)

df_step1 <- df_unfiltered %>%
  filter(response_rt <= 10)

df_step2 <- df_step1 %>%
  filter(number_deviation >= -4, number_deviation <= 4)

df_step3 <- df_step2 %>%
  filter(adjustment_duration <= 15)

df_step4 <- df_step3 %>%
  filter(response_edge_to_edge_spacing > 0 | response_num == 1)

df_filtered <- df_step4

filter_steps <- tibble(
  step = c("Initial data",
           "Enumeration RT \u2264 10s",
           "Number deviation [-4,4]",
           "Adjustment RT \u2264 15s",
           "Edge-to-edge spacing > 0"),
  remaining_trials = c(initial_count, nrow(df_step1), nrow(df_step2), nrow(df_step3), nrow(df_step4))
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
         correct_space, stim_length, response_stim_length,
         number_deviation, width_deviation, width_deviation_relative,
         spacing_deviation, spacing_deviation_relative, width_density_deviation)

write.csv(df_final, "exp2/data/processed.csv", row.names = FALSE)
