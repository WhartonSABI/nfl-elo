library(tidyverse)
library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(nflreadr)
library(corrr)

setwd("/Users/kennywatts/Documents/GitHub/nfl_elo/data/results")

contracts <- load_contracts()

# Filtering out rookie contracts

contracts <- contracts %>%
  group_by(player) %>%
  mutate(contract_count = n()) %>%
  ungroup()

contracts <- contracts %>%
  filter(!(contract_count == 1 & year_signed == draft_year))

elo_data <- read_csv("player_elo_ratings.csv")

head(elo_data)

# Filtering for Defensive Lineman

recent_contracts <- contracts %>%
  filter(year_signed %in% c(2021, 2022))

glimpse(elo_data)

elo_data <- elo_data %>%
  left_join(recent_contracts, by = c("player_name" = "player")) %>%
  group_by(player_name) %>%
  slice_max(year_signed, with_ties = FALSE) %>%
  ungroup()

rusher_data <- elo_data %>%
  filter(role == "Rusher") %>%
  drop_na()

rusher_data <- rusher_data %>%
  group_by(position) %>%
  filter(n() >= 3) %>%
  ungroup()

blocker_data <- elo_data %>%
  filter(role == "Blocker") %>%
  drop_na()
mean(blocker_data$final_elo)
blocker_data <- blocker_data %>%
  group_by(position) %>%
  filter(n() >= 3) %>%
  ungroup()

# Regression on elo

# Rushers
ggplot(rusher_data, aes(x = final_elo, y = apy)) +  # replace with real vars
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ position) +  # use your position column name
  labs(title = "Scatter Plots by Position") +
  theme_minimal()

# Blockers
ggplot(blocker_data, aes(x = final_elo, y = apy)) +  # replace with real vars
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ position) +  # use your position column name
  labs(title = "Scatter Plots by Position") +
  theme_minimal()

# New Approcah


elo_with_num_obs <- read_csv("parallel_elo_history.csv")

last_rusher_rows <- elo_with_num_obs %>%
  filter(!is.na(rusher_name)) %>%
  group_by(rusher_name) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(player = rusher_name, after_elo = after_rusher_elo, n_obs = rusher_n)

last_blocker_rows <- elo_with_num_obs %>%
  filter(!is.na(blocker_name)) %>%
  group_by(blocker_name) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(player = blocker_name, after_elo = after_blocker_elo, n_obs = blocker_n)

player_summary <- bind_rows(last_rusher_rows, last_blocker_rows) %>%
  distinct(player, .keep_all = TRUE)

player_summary <- player_summary %>%
  filter(n_obs > 50)


player_summary <- player_summary %>%
  left_join(recent_contracts, by = c("player" = "player")) %>%
  group_by(player) %>%
  slice_max(year_signed, with_ties = FALSE) %>%
  ungroup()

rusher_data <- player_summary %>%
  filter(position %in% c("ED", "IDL", "LB")) %>%
  drop_na() %>%
  group_by(position) %>%
  filter(n() >= 3) %>%
  ungroup()

blocker_data <- player_summary %>%
  filter(position %in% c("C", "LG", "LT", "RG", "RT")) %>%
  drop_na() %>%
  group_by(position) %>%
  filter(n() >= 3) %>%
  ungroup()

# Regression on elo

# Rushers
ggplot(rusher_data, aes(x = after_elo, y = apy)) +  # replace with real vars
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ position) +  # use your position column name
  labs(
    title = "Rusher Plots by Position",
    x = "ELO Rating",
    y = "Average Per Year (APY) in Millions"
  ) +
  theme_minimal()

# Blockers
ggplot(blocker_data, aes(x = after_elo, y = apy)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ position) +
  labs(
    title = "Blocker Plots by Position",
    x = "ELO Rating",
    y = "Average Per Year (APY) in Millions"
  ) +
  theme_minimal()


# Finding biggest outliers

get_residuals <- function(df) {
  model <- lm(apy ~ final_elo, data = df)
  df$residual <- resid(model)
  df$predicted_apy <- predict(model)
  df
}

# Rushers

rusher_with_resid <- rusher_data %>%
  group_by(position) %>%
  group_modify(~ get_residuals(.x)) %>%
  ungroup()

top_rusher_outliers <- rusher_with_resid %>%
  arrange(desc(abs(residual))) %>%
  group_by(position) %>%
  slice_max(order_by = abs(residual), n = 3) %>%
  ungroup()

ggplot(rusher_with_resid, aes(x = final_elo, y = apy)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(data = top_rusher_outliers, aes(label = player_name),
            color = "red", size = 3,
            max.overlaps = Inf
  ) +
  facet_wrap(~ position) +
  labs(title = "Rusher Outliers by Position", x = "Final ELO", y = "APY") +
  theme_minimal()

ggplot(top_rusher_outliers, aes(x = reorder(player_name, residual), y = residual, fill = position)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ position, scales = "free_y") +
  labs(
    title = "Top Rusher Residuals by Position",
    x = "Player",
    y = "Residual (Actual APY - Predicted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(top_rusher_outliers, aes(x = reorder(player_name, residual), y = residual, fill = position)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ position, scales = "free_y") +
  labs(
    title = "Top Rusher Residuals by Position",
    x = "Player",
    y = "Residual (Actual APY - Predicted)"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle    = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    plot.caption     = element_text(size = 9, hjust = 1, color = "grey50"),
    axis.title       = element_text(face = "bold", size = 14),
    axis.text        = element_text(size = 12, color = "grey20"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 12),
    legend.text      = element_text(size = 10),
    legend.key       = element_rect(fill = NA),
    legend.background= element_rect(fill = NA)
  )


# Blockers

blocker_with_resid <- blocker_data %>%
  group_by(position) %>%
  group_modify(~ get_residuals(.x)) %>%
  ungroup()

top_blocker_outliers <- blocker_with_resid %>%
  arrange(desc(abs(residual))) %>%
  group_by(position) %>%
  slice_max(order_by = abs(residual), n = 3) %>%
  ungroup()

ggplot(blocker_with_resid, aes(x = final_elo, y = apy)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggrepel::geom_text_repel(
    data = top_blocker_outliers,
    aes(label = player_name),
    color = "red", size = 3,
    max.overlaps = Inf
  ) +
  facet_wrap(~ position) +
  labs(title = "Blocker Outliers by Position", x = "Final ELO", y = "APY") +
  theme_minimal()

ggplot(top_blocker_outliers, aes(x = reorder(player_name, residual), y = residual, fill = position)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ position, scales = "free_y") +
  labs(
    title = "Top Blocker Residuals by Position",
    x = "Player",
    y = "Residual (Actual APY - Predicted)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(top_blocker_outliers, aes(x = reorder(player_name, residual), y = residual, fill = position)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ position, scales = "free_y") +
  labs(
    title = "Top Blocker Residuals by Position",
    x = "Player",
    y = "Residual (Actual APY - Predicted)"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle    = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    plot.caption     = element_text(size = 9, hjust = 1, color = "grey50"),
    axis.title       = element_text(face = "bold", size = 14),
    axis.text        = element_text(size = 12, color = "grey20"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 12),
    legend.text      = element_text(size = 10),
    legend.key       = element_rect(fill = NA),
    legend.background= element_rect(fill = NA)
  )







