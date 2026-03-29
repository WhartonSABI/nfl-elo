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

message("Running win ridge Bradley-Terry uncertainty analyses...")
modeling_table <- read_modeling_table(PIPELINE_CONFIG) %>%
  arrange(game_id, play_id, event_game_index)
workers <- PIPELINE_CONFIG$parallel$workers
message("Using workers: ", workers, " (n_cores - 4 rule)")

win_artifact <- if (file.exists(PIPELINE_CONFIG$output_paths$win_model_artifact)) {
  readRDS(PIPELINE_CONFIG$output_paths$win_model_artifact)
} else {
  levels_tbl <- get_bt_levels(modeling_table)
  fit <- fit_bt_win_cv(
    train_df = modeling_table,
    bt_cfg = PIPELINE_CONFIG$bt_win_model,
    rusher_levels = levels_tbl$rusher_levels,
    blocker_levels = levels_tbl$blocker_levels,
    seed = PIPELINE_CONFIG$uncertainty$seed
  )
  list(lambda_min = fit$lambda_min, lambda_1se = fit$lambda_1se)
}

fixed_lambda <- select_bt_fixed_lambda(win_artifact, PIPELINE_CONFIG$bt_win_model)
message(
  "Using fixed win lambda from full-data CV: ",
  signif(fixed_lambda, 6),
  " (selection=",
  PIPELINE_CONFIG$bt_win_model$lambda_selection,
  ")"
)

boot_results <- bootstrap_bt_end_to_end_win(
  model_data = modeling_table,
  bt_cfg = PIPELINE_CONFIG$bt_win_model,
  fixed_lambda = fixed_lambda,
  train_fraction = PIPELINE_CONFIG$split$train_fraction,
  n_boot = PIPELINE_CONFIG$uncertainty$end_to_end_bootstrap_iterations,
  seed = PIPELINE_CONFIG$uncertainty$seed,
  workers = workers
)

validation_uncertainty <- boot_results$validation_summary %>%
  mutate(
    fixed_lambda = fixed_lambda,
    bootstrap_scope = "end_to_end"
  )

rating_uncertainty <- boot_results$rating_summary %>%
  mutate(
    fixed_lambda = fixed_lambda,
    bootstrap_scope = "train_fit_end_to_end"
  )

write_output_csv(validation_uncertainty, PIPELINE_CONFIG$output_paths$win_bt_validation_uncertainty)
write_output_csv(rating_uncertainty, PIPELINE_CONFIG$output_paths$win_bt_rating_uncertainty)

message("Wrote win validation uncertainty: ", PIPELINE_CONFIG$output_paths$win_bt_validation_uncertainty)
message("Wrote win rating uncertainty: ", PIPELINE_CONFIG$output_paths$win_bt_rating_uncertainty)

path_boot_iter <- as.integer(PIPELINE_CONFIG$uncertainty$path_bootstrap_iterations)
if (path_boot_iter > 0L) {
  message(
    "Running weekly cumulative win path uncertainty (bootstrap iterations=",
    path_boot_iter,
    ")..."
  )
  win_path_uncertainty <- bootstrap_bt_weekly_path_win(
    model_data = modeling_table,
    bt_cfg = PIPELINE_CONFIG$bt_win_model,
    fixed_lambda = fixed_lambda,
    n_boot = path_boot_iter,
    seed = PIPELINE_CONFIG$uncertainty$seed,
    workers = workers
  ) %>%
    mutate(
      fixed_lambda = fixed_lambda,
      bootstrap_scope = "weekly_cumulative_refit"
    )

  write_output_csv(
    win_path_uncertainty,
    PIPELINE_CONFIG$output_paths$win_bt_weekly_path_uncertainty
  )
  message("Wrote win weekly path uncertainty: ", PIPELINE_CONFIG$output_paths$win_bt_weekly_path_uncertainty)
} else {
  message("Skipping weekly win path uncertainty (PATH_BOOTSTRAP_ITER <= 0).")
}
