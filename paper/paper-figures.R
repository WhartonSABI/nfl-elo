locate_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, ".git")) && dir.exists(file.path(current, "data"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate project root.")
    }
    current <- parent
  }
}

project_root <- locate_project_root()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

paper_dir <- file.path(project_root, "paper")
fig_dir <- paper_dir
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

leaderboard_path <- file.path(project_root, "data", "output", "shared", "leaderboard_full_bt_ridge.csv")
win_path_uncertainty_path <- file.path(project_root, "data", "output", "win", "path_uncertainty_weekly_win_bt_ridge.csv")
severity_path_uncertainty_path <- file.path(project_root, "data", "output", "severity", "path_uncertainty_weekly_severity_bt_ridge.csv")

leaderboard <- read_csv(leaderboard_path, show_col_types = FALSE)
win_path <- read_csv(win_path_uncertainty_path, show_col_types = FALSE)
severity_path <- read_csv(severity_path_uncertainty_path, show_col_types = FALSE)

if (!"win_boot_q25_score" %in% names(leaderboard)) leaderboard$win_boot_q25_score <- NA_real_
if (!"win_boot_q75_score" %in% names(leaderboard)) leaderboard$win_boot_q75_score <- NA_real_
if (!"severity_boot_q25_score" %in% names(leaderboard)) leaderboard$severity_boot_q25_score <- NA_real_
if (!"severity_boot_q75_score" %in% names(leaderboard)) leaderboard$severity_boot_q75_score <- NA_real_

theme_set(theme_minimal(base_size = 12))

# Figure 1: BT score distributions by role and model.
dist_tbl <- leaderboard %>%
  select(player_name, role, win_bt_logit_score, severity_weighted_logit_score) %>%
  pivot_longer(
    cols = c(win_bt_logit_score, severity_weighted_logit_score),
    names_to = "model",
    values_to = "bt_score"
  ) %>%
  mutate(
    model = recode(
      model,
      win_bt_logit_score = "Win/Loss",
      severity_weighted_logit_score = "Severity"
    )
  )

p_dist <- ggplot(dist_tbl, aes(x = bt_score, fill = role, color = role)) +
  geom_density(alpha = 0.20, linewidth = 0.8) +
  facet_wrap(~model, ncol = 1, scales = "free_y") +
  labs(
    title = "Distribution of Raw BT Ratings",
    x = "Raw BT score",
    y = "Density",
    fill = "Role",
    color = "Role"
  ) +
  scale_fill_manual(values = c(Blocker = "#1f77b4", Rusher = "#d62728")) +
  scale_color_manual(values = c(Blocker = "#1f77b4", Rusher = "#d62728"))

ggsave(
  filename = file.path(fig_dir, "bt_rating_distributions.png"),
  plot = p_dist,
  width = 10.0,
  height = 7.2,
  dpi = 300
)

# Figure 2: Top players by model and role (interaction threshold to reduce noise).
min_interactions <- 200L
top_n <- 10L

top_win <- leaderboard %>%
  filter(role_interactions >= min_interactions) %>%
  group_by(role) %>%
  arrange(win_rank_by_role, .by_group = TRUE) %>%
  slice_head(n = top_n) %>%
  ungroup() %>%
  transmute(
    model = "Win/Loss BT (Ridge)",
    role,
    player_name,
    role_interactions,
    score = win_bt_logit_score,
    lower_score = coalesce(win_boot_q25_score, win_boot_q025_score),
    upper_score = coalesce(win_boot_q75_score, win_boot_q975_score)
  )

top_severity <- leaderboard %>%
  filter(role_interactions >= min_interactions) %>%
  group_by(role) %>%
  arrange(severity_rank_by_role, .by_group = TRUE) %>%
  slice_head(n = top_n) %>%
  ungroup() %>%
  transmute(
    model = "Severity BT (Ridge)",
    role,
    player_name,
    role_interactions,
    score = severity_weighted_logit_score,
    lower_score = coalesce(severity_boot_q25_score, severity_boot_q025_score),
    upper_score = coalesce(severity_boot_q75_score, severity_boot_q975_score)
  )

top_tbl <- bind_rows(top_win, top_severity) %>%
  mutate(
    panel = paste(model, role, sep = " | "),
    player_panel = paste(player_name, panel, sep = "___")
  ) %>%
  arrange(panel, score) %>%
  group_by(panel) %>%
  mutate(player_panel = factor(player_panel, levels = unique(player_panel))) %>%
  ungroup()

panel_labels <- function(x) sub("___.*$", "", x)

p_top <- ggplot(top_tbl, aes(y = player_panel, color = role)) +
  geom_segment(
    aes(x = lower_score, xend = upper_score, yend = player_panel),
    linewidth = 1.05,
    alpha = 0.72
  ) +
  geom_point(aes(x = score), size = 2.3, alpha = 0.95) +
  facet_wrap(~panel, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = panel_labels) +
  labs(
    title = paste0("Top ", top_n, " Players by Raw BT Score with 50% Bootstrap Intervals"),
    subtitle = paste0("Minimum ", min_interactions, " interactions per player"),
    x = "Raw BT score (point estimate with central 50% bootstrap interval)",
    y = NULL,
    color = "Role"
  ) +
  scale_color_manual(values = c(Blocker = "#1f77b4", Rusher = "#d62728")) +
  theme(
    panel.grid.minor.y = element_blank()
  )

ggsave(
  filename = file.path(fig_dir, "bt_top_players_by_role.png"),
  plot = p_top,
  width = 12.0,
  height = 8.8,
  dpi = 300
)

# Figure 3: Weekly path uncertainty for representative players.
selected_win <- c("Myles Garrett", "Joe Thuney")
selected_severity <- c("Robert Quinn", "Trent Williams")

path_tbl <- bind_rows(
  win_path %>%
    filter(player_name %in% selected_win) %>%
    mutate(model = "Win/Loss BT (Ridge)"),
  severity_path %>%
    filter(player_name %in% selected_severity) %>%
    mutate(model = "Severity BT (Ridge)")
) %>%
  mutate(
    week = as.integer(week_index),
    lower_approx95 = mean_score - 1.96 * sd_score,
    upper_approx95 = mean_score + 1.96 * sd_score
  ) %>%
  arrange(model, role, player_name, week)

p_path <- ggplot(path_tbl, aes(x = week, y = observed_score, color = player_name, fill = player_name)) +
  geom_ribbon(aes(ymin = lower_approx95, ymax = upper_approx95), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.3, alpha = 0.90) +
  facet_grid(model ~ role, scales = "free_y") +
  labs(
    title = "Weekly BT Rating Paths with Bootstrap Uncertainty",
    subtitle = "Ribbon: mean ± 1.96 SD from cumulative weekly bootstrap",
    x = "Week index (cumulative fit)",
    y = "Observed raw BT score",
    color = "Player",
    fill = "Player"
  ) +
  scale_x_continuous(breaks = pretty_breaks(8))

ggsave(
  filename = file.path(fig_dir, "bt_weekly_path_uncertainty.png"),
  plot = p_path,
  width = 12.0,
  height = 8.0,
  dpi = 300
)

message("Wrote figures to: ", fig_dir)
