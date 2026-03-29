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

ensure_output_directories(PIPELINE_CONFIG)

message("Building full BT leaderboard table...")

modeling_table <- read_modeling_table(PIPELINE_CONFIG)

role_counts <- bind_rows(
  modeling_table %>%
    transmute(player_name = rusher_name, role = "Rusher"),
  modeling_table %>%
    transmute(player_name = blocker_name, role = "Blocker")
) %>%
  count(player_name, role, name = "role_interactions")

win_ratings_raw <- read_csv(
  PIPELINE_CONFIG$output_paths$win_bt_player_ratings,
  show_col_types = FALSE
)
assert_columns(
  win_ratings_raw,
  c("player_name", "role", "bt_logit_score", "elo_like_score", "coefficient_source"),
  "win_bt_player_ratings"
)

severity_ratings_raw <- read_csv(
  PIPELINE_CONFIG$output_paths$severity_bt_player_ratings,
  show_col_types = FALSE
)
assert_columns(
  severity_ratings_raw,
  c("player_name", "role", "weighted_severity_logit_score", "elo_like_score", "coefficient_source"),
  "severity_bt_player_ratings"
)

win_point_tbl <- win_ratings_raw %>%
  transmute(
    player_name = player_name,
    role = role,
    win_bt_logit_score = bt_logit_score,
    win_elo_like = elo_like_score,
    win_coefficient_source = coefficient_source
  )

severity_point_tbl <- severity_ratings_raw %>%
  transmute(
    player_name = player_name,
    role = role,
    severity_weighted_logit_score = weighted_severity_logit_score,
    severity_elo_like = elo_like_score,
    severity_coefficient_source = coefficient_source
  )

empty_win_uncertainty <- tibble(
  player_name = character(0),
  role = character(0),
  win_boot_mean_score = numeric(0),
  win_boot_sd_score = numeric(0),
  win_boot_q025_score = numeric(0),
  win_boot_q25_score = numeric(0),
  win_boot_q50_score = numeric(0),
  win_boot_q75_score = numeric(0),
  win_boot_q975_score = numeric(0),
  win_boot_mean_elo = numeric(0),
  win_boot_sd_elo = numeric(0),
  win_boot_n = integer(0),
  win_boot_fixed_lambda = numeric(0),
  win_boot_scope = character(0)
)

empty_severity_uncertainty <- tibble(
  player_name = character(0),
  role = character(0),
  severity_boot_mean_score = numeric(0),
  severity_boot_sd_score = numeric(0),
  severity_boot_q025_score = numeric(0),
  severity_boot_q25_score = numeric(0),
  severity_boot_q50_score = numeric(0),
  severity_boot_q75_score = numeric(0),
  severity_boot_q975_score = numeric(0),
  severity_boot_mean_elo = numeric(0),
  severity_boot_sd_elo = numeric(0),
  severity_boot_n = integer(0),
  severity_boot_fixed_lambda = numeric(0),
  severity_boot_scope = character(0)
)

win_uncertainty_tbl <- if (file.exists(PIPELINE_CONFIG$output_paths$win_bt_rating_uncertainty)) {
  win_uncertainty_raw <- read_csv(
    PIPELINE_CONFIG$output_paths$win_bt_rating_uncertainty,
    show_col_types = FALSE
  )
  assert_columns(
    win_uncertainty_raw,
    c("player_name", "role", "mean_score", "sd_score", "q025", "q50", "q975", "mean_elo_like", "sd_elo_like", "n_boot"),
    "win_bt_rating_uncertainty"
  )
  win_uncertainty_raw %>%
    transmute(
      player_name = player_name,
      role = role,
      win_boot_mean_score = mean_score,
      win_boot_sd_score = sd_score,
      win_boot_q025_score = q025,
      win_boot_q25_score = if ("q25" %in% names(win_uncertainty_raw)) q25 else NA_real_,
      win_boot_q50_score = q50,
      win_boot_q75_score = if ("q75" %in% names(win_uncertainty_raw)) q75 else NA_real_,
      win_boot_q975_score = q975,
      win_boot_mean_elo = mean_elo_like,
      win_boot_sd_elo = sd_elo_like,
      win_boot_n = n_boot,
      win_boot_fixed_lambda = if ("fixed_lambda" %in% names(win_uncertainty_raw)) fixed_lambda else NA_real_,
      win_boot_scope = if ("bootstrap_scope" %in% names(win_uncertainty_raw)) bootstrap_scope else NA_character_
    )
} else {
  message("Win rating uncertainty file not found; leaderboard will include point estimates only for win.")
  empty_win_uncertainty
}

severity_uncertainty_tbl <- if (file.exists(PIPELINE_CONFIG$output_paths$severity_bt_rating_uncertainty)) {
  severity_uncertainty_raw <- read_csv(
    PIPELINE_CONFIG$output_paths$severity_bt_rating_uncertainty,
    show_col_types = FALSE
  )
  assert_columns(
    severity_uncertainty_raw,
    c("player_name", "role", "mean_score", "sd_score", "q025", "q50", "q975", "mean_elo_like", "sd_elo_like", "n_boot"),
    "severity_bt_rating_uncertainty"
  )
  severity_uncertainty_raw %>%
    transmute(
      player_name = player_name,
      role = role,
      severity_boot_mean_score = mean_score,
      severity_boot_sd_score = sd_score,
      severity_boot_q025_score = q025,
      severity_boot_q25_score = if ("q25" %in% names(severity_uncertainty_raw)) q25 else NA_real_,
      severity_boot_q50_score = q50,
      severity_boot_q75_score = if ("q75" %in% names(severity_uncertainty_raw)) q75 else NA_real_,
      severity_boot_q975_score = q975,
      severity_boot_mean_elo = mean_elo_like,
      severity_boot_sd_elo = sd_elo_like,
      severity_boot_n = n_boot,
      severity_boot_fixed_lambda = if ("fixed_lambda" %in% names(severity_uncertainty_raw)) fixed_lambda else NA_real_,
      severity_boot_scope = if ("bootstrap_scope" %in% names(severity_uncertainty_raw)) bootstrap_scope else NA_character_
    )
} else {
  message("Severity rating uncertainty file not found; leaderboard will include point estimates only for severity.")
  empty_severity_uncertainty
}

leaderboard <- full_join(win_point_tbl, severity_point_tbl, by = c("player_name", "role")) %>%
  left_join(role_counts, by = c("player_name", "role")) %>%
  left_join(win_uncertainty_tbl, by = c("player_name", "role")) %>%
  left_join(severity_uncertainty_tbl, by = c("player_name", "role")) %>%
  mutate(
    role_interactions = coalesce(role_interactions, 0L),
    win_rank_overall = if_else(is.na(win_elo_like), NA_integer_, dense_rank(desc(win_elo_like))),
    severity_rank_overall = if_else(is.na(severity_elo_like), NA_integer_, dense_rank(desc(severity_elo_like)))
  ) %>%
  group_by(role) %>%
  mutate(
    win_rank_by_role = if_else(is.na(win_elo_like), NA_integer_, dense_rank(desc(win_elo_like))),
    severity_rank_by_role = if_else(is.na(severity_elo_like), NA_integer_, dense_rank(desc(severity_elo_like)))
  ) %>%
  ungroup() %>%
  select(
    player_name,
    role,
    role_interactions,
    win_rank_by_role,
    win_rank_overall,
    win_elo_like,
    win_bt_logit_score,
    win_coefficient_source,
    win_boot_mean_score,
    win_boot_sd_score,
    win_boot_q025_score,
    win_boot_q25_score,
    win_boot_q50_score,
    win_boot_q75_score,
    win_boot_q975_score,
    win_boot_mean_elo,
    win_boot_sd_elo,
    win_boot_n,
    win_boot_fixed_lambda,
    win_boot_scope,
    severity_rank_by_role,
    severity_rank_overall,
    severity_elo_like,
    severity_weighted_logit_score,
    severity_coefficient_source,
    severity_boot_mean_score,
    severity_boot_sd_score,
    severity_boot_q025_score,
    severity_boot_q25_score,
    severity_boot_q50_score,
    severity_boot_q75_score,
    severity_boot_q975_score,
    severity_boot_mean_elo,
    severity_boot_sd_elo,
    severity_boot_n,
    severity_boot_fixed_lambda,
    severity_boot_scope
  ) %>%
  arrange(role, coalesce(win_rank_by_role, severity_rank_by_role), player_name)

write_output_csv(leaderboard, PIPELINE_CONFIG$output_paths$bt_full_leaderboard)

message("Wrote full BT leaderboard: ", PIPELINE_CONFIG$output_paths$bt_full_leaderboard)
message("Rows in full BT leaderboard: ", nrow(leaderboard))
