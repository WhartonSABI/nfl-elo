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

message("Validating win ridge Bradley-Terry model on holdout split...")
modeling_table <- read_modeling_table(PIPELINE_CONFIG)
bt_cfg <- PIPELINE_CONFIG$bt_win_model
split <- split_train_test(modeling_table, train_fraction = PIPELINE_CONFIG$split$train_fraction)
train_df <- split$train
test_df <- split$test
baseline_cfg <- bt_cfg$matchup_baseline
baseline_tbl <- build_win_baseline_predictions(
  train_df = train_df,
  test_df = test_df,
  target_col = bt_cfg$target_col,
  prior_strength = baseline_cfg$prior_strength,
  method = baseline_cfg$method
)

levels_tbl <- get_bt_levels(modeling_table)
win_fit <- fit_bt_win_cv(
  train_df = train_df,
  bt_cfg = bt_cfg,
  rusher_levels = levels_tbl$rusher_levels,
  blocker_levels = levels_tbl$blocker_levels,
  seed = PIPELINE_CONFIG$uncertainty$seed
)

x_test <- to_sparse_bt_matrix(
  df = test_df,
  rusher_levels = levels_tbl$rusher_levels,
  blocker_levels = levels_tbl$blocker_levels,
  include_double_team = isTRUE(bt_cfg$include_double_team)
)

pred_lambda_min <- predict_bt_win(win_fit$cv_fit, x_test, s = "lambda.min")
pred_lambda_1se <- predict_bt_win(win_fit$cv_fit, x_test, s = "lambda.1se")

train_rusher_counts <- train_df %>%
  count(rusher_name, name = "rusher_train_interactions")
train_blocker_counts <- train_df %>%
  count(blocker_name, name = "blocker_train_interactions")

scored <- test_df %>%
  left_join(train_rusher_counts, by = "rusher_name") %>%
  left_join(train_blocker_counts, by = "blocker_name") %>%
  bind_cols(baseline_tbl) %>%
  mutate(
    rusher_train_interactions = dplyr::coalesce(rusher_train_interactions, 0L),
    blocker_train_interactions = dplyr::coalesce(blocker_train_interactions, 0L),
    eligible_train_n50 = rusher_train_interactions >= bt_cfg$train_interaction_filter &
      blocker_train_interactions >= bt_cfg$train_interaction_filter,
    frozen_model_prediction = pred_lambda_min,
    frozen_model_prediction_lambda_min = pred_lambda_min,
    frozen_model_prediction_lambda_1se = pred_lambda_1se,
    baseline_prediction = baseline_matchup_prediction,
    actual_target = win_target,
    target_name = bt_cfg$target_col,
    model_name = bt_cfg$model_name,
    lambda_min = win_fit$lambda_min,
    lambda_1se = win_fit$lambda_1se
  )

metrics <- bind_rows(
  compute_validation_metrics(
    scored,
    mode = "win",
    baseline_col = "baseline_global_prediction",
    baseline_name = "global_mean"
  ),
  compute_validation_metrics(
    scored,
    mode = "win",
    baseline_col = "baseline_matchup_prediction",
    baseline_name = paste0("matchup_", normalize_win_baseline_method(baseline_cfg$method))
  )
) %>%
  mutate(
    coverage = mean(!is.na(scored$frozen_model_prediction)),
    n_test = nrow(test_df),
    n_scored = sum(!is.na(scored$frozen_model_prediction)),
    model_name = bt_cfg$model_name,
    baseline_matchup_method = normalize_win_baseline_method(baseline_cfg$method),
    baseline_prior_strength = as.numeric(baseline_cfg$prior_strength),
    lambda_min = win_fit$lambda_min,
    lambda_1se = win_fit$lambda_1se
  )

write_output_csv(scored, PIPELINE_CONFIG$output_paths$win_bt_holdout_scored)
write_output_csv(metrics, PIPELINE_CONFIG$output_paths$win_bt_validation_metrics)

message("Win validation coverage: ", round(100 * mean(!is.na(scored$frozen_model_prediction)), 2), "%")
message("Wrote win validation metrics: ", PIPELINE_CONFIG$output_paths$win_bt_validation_metrics)
