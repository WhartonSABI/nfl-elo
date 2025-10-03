# Brier Scores

library(dplyr)
library(readr)

setwd("/Users/kennywatts/Documents/GitHub/nfl_elo/data/results")

elo_history <- read_csv("elo_history.csv")

head(elo_history)

# Splitting Data Set

n_total <- nrow(elo_history)
n_train <- floor(0.8 * n_total)

train_df <- elo_history[1:n_train, ]
test_df <- elo_history[(n_train + 1):n_total, ]

# Getting last known ELO for each rusher and blocker in train_df

final_rusher_elo <- train_df %>%
  dplyr::group_by(rusher_name) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(rusher_name, after_rusher_elo) %>%
  dplyr::rename(rusher_elo_pred = after_rusher_elo)

final_blocker_elo <- train_df %>%
  dplyr::group_by(blocker_name) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::select(blocker_name, after_blocker_elo) %>%
  dplyr::rename(blocker_elo_pred = after_blocker_elo)

test_df <- test_df %>%
  left_join(final_rusher_elo, by = "rusher_name") %>%
  left_join(final_blocker_elo, by = "blocker_name")

# Baseline 'dumb' predictor (win proportion observed)

baseline_mean <- mean(train_df$outcome_score != 0, na.rm = TRUE)
unique(train_df$outcome_score)
test_df$baseline_pred <- baseline_mean

# Elo Predictor

belo <- test_df$blocker_elo_pred
relo <- test_df$rusher_elo_pred
double_team <- test_df$double_team
scale <- 319

test_df$blocker_bonus <- ifelse(double_team == 1, 100, 0)
test_df$adj_belo <- belo + test_df$blocker_bonus

test_df$rusher_win_prob <- 1 / (1 + 10^((test_df$adj_belo - relo) / scale))

# Elo Predictor (Keeps updating)

belo <- test_df$before_blocker_elo
relo <- test_df$before_rusher_elo
double_team <- test_df$double_team
scale <- 319

test_df$blocker_bonus <- ifelse(double_team == 1, 100, 0)
test_df$adj_belo <- belo + test_df$blocker_bonus

test_df$updating_pred <- 1 / (1 + 10^((test_df$adj_belo - relo) / scale))

# Scale them because outcome is 0-2

test_df$baseline_pred <- test_df$baseline_pred * 2
test_df$rusher_win_prob <- test_df$rusher_win_prob * 2
test_df$updating_pred <- test_df$updating_pred * 2

# Calculating MSE

mse_base <- mean((test_df$baseline_pred - test_df$outcome_score)^2, na.rm = TRUE)
mse_elo <- mean((test_df$rusher_win_prob - test_df$outcome_score)^2, na.rm = TRUE)
mse_updating_elo <- mean((test_df$updating_pred - test_df$outcome_score)^2, na.rm = TRUE)

cat("Baseline MSE:", mse_base, "\n")
cat("ELO MSE:", mse_elo, "\n")
cat("Updating ELO MSE:", mse_updating_elo, "\n")

# Plotting

mse_df <- tibble::tibble(
  Model = factor(c("Baseline", "ELO", "Updating ELO"), 
                 levels = c("Baseline", "ELO", "Updating ELO")),
  MSE = c(mse_base, mse_elo, mse_updating_elo)
)

ggplot(mse_df, aes(x = Model, y = MSE, fill = Model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(MSE, 4)), vjust = -0.3, size = 4) +
  labs(
    title = "Model Comparison: Mean Squared Error",
    x = "Model",
    y = "Mean Squared Error (MSE)"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title       = element_text(face = "bold", size = 14),
    axis.text        = element_text(size = 12, color = "grey20"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

######

mse_df <- tibble::tibble(
  Model = factor(c("Baseline", "ELO"), 
                 levels = c("Baseline", "ELO")),
  MSE = c(mse_base, mse_elo)
)

ggplot(mse_df, aes(x = Model, y = MSE, fill = Model)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = round(MSE, 4)),
    vjust = -0.3,
    size = 4,
    fontface = "bold"
  ) +
  labs(
    title = "Naive Win Probability vs Elo",
    x = "Predictor",
    y = "Mean Squared Error (MSE)"
  ) +
  scale_fill_manual(
    values = c(
      "Baseline" = "#A8E6A3",  # light greenish
      "ELO"      = "#006D77"   # dark blue-green
    )
  ) +
  theme(
    plot.title       = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title       = element_text(face = "bold", size = 14),
    axis.text        = element_text(size = 12, color = "grey20"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),
    panel.border     = element_blank(),   # removes the square border
    legend.position  = "none"
  )





