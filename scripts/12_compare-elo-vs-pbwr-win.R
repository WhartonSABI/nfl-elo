locate_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, ".git")) && dir.exists(file.path(current, "scripts"))) {
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
source(file.path(project_root, "scripts", "00_config.R"))
source(file.path(project_root, "scripts", "00_utils.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})

ensure_output_directories(PIPELINE_CONFIG)

compare_output_dir <- file.path(project_root, "data", "output", "win_compare")
ensure_directory(compare_output_dir)

output_paths <- list(
  scored_rows = file.path(compare_output_dir, "rolling_scored_rows_elo_vs_pbwr.csv"),
  split_metrics = file.path(compare_output_dir, "rolling_split_metrics_elo_vs_pbwr.csv"),
  bootstrap_draws = file.path(compare_output_dir, "rolling_block_bootstrap_draws_elo_vs_pbwr.csv"),
  bootstrap_summary = file.path(compare_output_dir, "rolling_block_bootstrap_summary_elo_vs_pbwr.csv"),
  overall_metrics = file.path(compare_output_dir, "rolling_overall_metrics_elo_vs_pbwr.csv")
)

n_boot <- get_env_int("COMPARE_BOOT_ITER", 1000L)
n_splits_target <- get_env_int("COMPARE_ROLLING_SPLITS", 8L)
min_train_games <- get_env_int("COMPARE_MIN_TRAIN_GAMES", 120L)
pbwr_prior_n <- get_env_int("PBWR_PRIOR_N", 25L)
workers <- PIPELINE_CONFIG$parallel$workers
seed <- PIPELINE_CONFIG$uncertainty$seed

message("Comparing Elo vs PbWR with rolling out-of-sample splits...")
message("workers=", workers, ", n_boot=", n_boot, ", rolling_splits=", n_splits_target)

modeling_table <- read_modeling_table(PIPELINE_CONFIG) %>%
  arrange(row_index)

assert_columns(
  modeling_table,
  c("row_index", "game_id", "rusher_name", "blocker_name", "double_team", "win_target"),
  "modeling_table"
)

games_tbl <- modeling_table %>%
  group_by(game_id) %>%
  summarise(
    first_row = min(row_index),
    n_rows = n(),
    .groups = "drop"
  ) %>%
  arrange(first_row) %>%
  mutate(game_order = row_number())

n_games <- nrow(games_tbl)
if (n_games <= (min_train_games + 1L)) {
  stop("Not enough games for rolling validation with min_train_games=", min_train_games, ".")
}

train_end_points <- unique(as.integer(round(
  seq(min_train_games, n_games - 1L, length.out = max(1L, n_splits_target))
)))
train_end_points <- train_end_points[train_end_points >= min_train_games & train_end_points < n_games]
if (length(train_end_points) == 0L) {
  train_end_points <- as.integer(n_games - 1L)
}
train_end_points <- sort(unique(train_end_points))
test_end_points <- c(train_end_points[-1], n_games)

eps <- 1e-12
split_rows <- vector("list", length(train_end_points))

for (i in seq_along(train_end_points)) {
  train_end <- train_end_points[[i]]
  test_start <- train_end + 1L
  test_end <- test_end_points[[i]]

  if (test_start > test_end) {
    next
  }

  train_games <- games_tbl$game_id[seq_len(train_end)]
  test_games <- games_tbl$game_id[test_start:test_end]

  train_df <- modeling_table %>% filter(game_id %in% train_games)
  test_df <- modeling_table %>% filter(game_id %in% test_games)

  if (nrow(train_df) == 0L || nrow(test_df) == 0L) {
    next
  }

  win_cfg <- PIPELINE_CONFIG$win_model
  elo_fit <- fit_elo_model(train_df, win_cfg)

  final_rusher_elo <- elo_fit$history %>%
    group_by(rusher_name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(rusher_name, after_rusher_elo) %>%
    rename(rusher_elo_train = after_rusher_elo)

  final_blocker_elo <- elo_fit$history %>%
    group_by(blocker_name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(blocker_name, after_blocker_elo) %>%
    rename(blocker_elo_train = after_blocker_elo)

  global_win_rate <- mean(train_df$win_target, na.rm = TRUE)

  pbwr_by_blocker <- train_df %>%
    group_by(blocker_name) %>%
    summarise(
      blocker_n_train = n(),
      wins_allowed = sum(win_target),
      .groups = "drop"
    ) %>%
    mutate(
      p_rusher_win_pbwr = (wins_allowed + pbwr_prior_n * global_win_rate) / (blocker_n_train + pbwr_prior_n)
    ) %>%
    select(blocker_name, blocker_n_train, p_rusher_win_pbwr)

  scored <- test_df %>%
    left_join(final_rusher_elo, by = "rusher_name") %>%
    left_join(final_blocker_elo, by = "blocker_name") %>%
    left_join(pbwr_by_blocker, by = "blocker_name") %>%
    mutate(
      split_id = i,
      train_end_game_order = train_end,
      test_start_game_order = test_start,
      test_end_game_order = test_end,
      p_global = global_win_rate,
      rusher_elo_train = coalesce(rusher_elo_train, win_cfg$rusher_start_elo),
      blocker_elo_train = coalesce(blocker_elo_train, win_cfg$blocker_start_elo),
      holdout_bonus = if (isTRUE(win_cfg$use_double_team_bonus)) {
        if_else(coalesce(double_team, 0) == 1, win_cfg$double_team_bonus, 0)
      } else {
        0
      },
      p_elo = 1 / (1 + 10^(((blocker_elo_train + holdout_bonus) - rusher_elo_train) / win_cfg$scale)),
      p_pbwr = coalesce(p_rusher_win_pbwr, p_global),
      actual = win_target,
      p_elo_clip = pmin(pmax(p_elo, eps), 1 - eps),
      p_pbwr_clip = pmin(pmax(p_pbwr, eps), 1 - eps),
      p_global_clip = pmin(pmax(p_global, eps), 1 - eps),
      logloss_elo = -(actual * log(p_elo_clip) + (1 - actual) * log(1 - p_elo_clip)),
      logloss_pbwr = -(actual * log(p_pbwr_clip) + (1 - actual) * log(1 - p_pbwr_clip)),
      logloss_global = -(actual * log(p_global_clip) + (1 - actual) * log(1 - p_global_clip)),
      brier_elo = (p_elo - actual)^2,
      brier_pbwr = (p_pbwr - actual)^2,
      brier_global = (p_global - actual)^2,
      delta_logloss_elo_vs_pbwr = logloss_elo - logloss_pbwr,
      delta_brier_elo_vs_pbwr = brier_elo - brier_pbwr,
      delta_logloss_elo_vs_global = logloss_elo - logloss_global,
      delta_brier_elo_vs_global = brier_elo - brier_global
    ) %>%
    select(
      split_id,
      game_id,
      play_id,
      row_index,
      rusher_name,
      blocker_name,
      actual,
      p_elo,
      p_pbwr,
      p_global,
      blocker_n_train,
      train_end_game_order,
      test_start_game_order,
      test_end_game_order,
      logloss_elo,
      logloss_pbwr,
      logloss_global,
      brier_elo,
      brier_pbwr,
      brier_global,
      delta_logloss_elo_vs_pbwr,
      delta_brier_elo_vs_pbwr,
      delta_logloss_elo_vs_global,
      delta_brier_elo_vs_global
    )

  split_rows[[i]] <- scored
}

scored_rows <- bind_rows(split_rows)
if (nrow(scored_rows) == 0L) {
  stop("No scored rows produced for rolling comparison.")
}

split_metrics <- scored_rows %>%
  group_by(split_id, train_end_game_order, test_start_game_order, test_end_game_order) %>%
  summarise(
    n_rows = n(),
    win_rate = mean(actual),
    logloss_elo = mean(logloss_elo),
    logloss_pbwr = mean(logloss_pbwr),
    logloss_global = mean(logloss_global),
    brier_elo = mean(brier_elo),
    brier_pbwr = mean(brier_pbwr),
    brier_global = mean(brier_global),
    delta_logloss_elo_vs_pbwr = mean(delta_logloss_elo_vs_pbwr),
    delta_brier_elo_vs_pbwr = mean(delta_brier_elo_vs_pbwr),
    delta_logloss_elo_vs_global = mean(delta_logloss_elo_vs_global),
    delta_brier_elo_vs_global = mean(delta_brier_elo_vs_global),
    .groups = "drop"
  ) %>%
  arrange(split_id)

overall_metrics <- tibble(
  n_rows = nrow(scored_rows),
  n_games = n_distinct(scored_rows$game_id),
  n_splits = n_distinct(scored_rows$split_id),
  logloss_elo = mean(scored_rows$logloss_elo),
  logloss_pbwr = mean(scored_rows$logloss_pbwr),
  logloss_global = mean(scored_rows$logloss_global),
  brier_elo = mean(scored_rows$brier_elo),
  brier_pbwr = mean(scored_rows$brier_pbwr),
  brier_global = mean(scored_rows$brier_global),
  delta_logloss_elo_vs_pbwr = mean(scored_rows$delta_logloss_elo_vs_pbwr),
  delta_brier_elo_vs_pbwr = mean(scored_rows$delta_brier_elo_vs_pbwr),
  delta_logloss_elo_vs_global = mean(scored_rows$delta_logloss_elo_vs_global),
  delta_brier_elo_vs_global = mean(scored_rows$delta_brier_elo_vs_global)
)

game_indices <- split(seq_len(nrow(scored_rows)), scored_rows$game_id)
game_ids <- names(game_indices)

boot_draws <- parallel_map(
  iterable = seq_len(n_boot),
  workers = workers,
  seed = seed,
  worker_fn = function(iteration_id) {
    sampled_games <- sample(game_ids, size = length(game_ids), replace = TRUE)
    idx <- unlist(game_indices[sampled_games], use.names = FALSE)
    d <- scored_rows[idx, , drop = FALSE]
    data.frame(
      iteration = iteration_id,
      delta_logloss_elo_vs_pbwr = mean(d$delta_logloss_elo_vs_pbwr),
      delta_brier_elo_vs_pbwr = mean(d$delta_brier_elo_vs_pbwr),
      delta_logloss_elo_vs_global = mean(d$delta_logloss_elo_vs_global),
      delta_brier_elo_vs_global = mean(d$delta_brier_elo_vs_global),
      stringsAsFactors = FALSE
    )
  }
)

bootstrap_draws <- bind_rows(boot_draws)

bootstrap_summary <- bootstrap_draws %>%
  pivot_longer(
    cols = -iteration,
    names_to = "metric",
    values_to = "delta"
  ) %>%
  group_by(metric) %>%
  summarise(
    mean_delta = mean(delta),
    q025 = quantile(delta, 0.025),
    q50 = quantile(delta, 0.50),
    q975 = quantile(delta, 0.975),
    p_delta_gt0 = mean(delta > 0),
    iterations = n_boot,
    .groups = "drop"
  ) %>%
  mutate(
    verdict = case_when(
      q025 > 0 ~ "Elo worse than comparator",
      q975 < 0 ~ "Elo better than comparator",
      TRUE ~ "Inconclusive"
    )
  ) %>%
  arrange(metric)

write_output_csv(scored_rows, output_paths$scored_rows)
write_output_csv(split_metrics, output_paths$split_metrics)
write_output_csv(bootstrap_draws, output_paths$bootstrap_draws)
write_output_csv(bootstrap_summary, output_paths$bootstrap_summary)
write_output_csv(overall_metrics, output_paths$overall_metrics)

message("Wrote rolling scored rows: ", output_paths$scored_rows)
message("Wrote rolling split metrics: ", output_paths$split_metrics)
message("Wrote block bootstrap draws: ", output_paths$bootstrap_draws)
message("Wrote block bootstrap summary: ", output_paths$bootstrap_summary)
message("Wrote overall comparison metrics: ", output_paths$overall_metrics)

