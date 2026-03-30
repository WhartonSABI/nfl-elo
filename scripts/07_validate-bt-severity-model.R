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

message("Validating severity ridge Bradley-Terry model on holdout split...")
modeling_table <- read_modeling_table(PIPELINE_CONFIG)
bt_cfg <- PIPELINE_CONFIG$bt_severity_model
modeling_table <- ensure_severity_outcome_column(
  modeling_table,
  bt_cfg = bt_cfg,
  severity_weights = PIPELINE_CONFIG$severity_weights
)

split <- split_train_test(modeling_table, train_fraction = PIPELINE_CONFIG$split$train_fraction)
train_df <- split$train
test_df <- split$test

levels_tbl <- get_bt_levels(modeling_table)
severity_fit <- fit_bt_severity_cv(
  train_df = train_df,
  bt_cfg = bt_cfg,
  severity_weights = PIPELINE_CONFIG$severity_weights,
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

prob_lambda_min <- predict_bt_multinomial_probs(
  cv_fit = severity_fit$cv_fit,
  x_new = x_test,
  s = "lambda.min",
  class_levels = bt_cfg$class_levels
)

prob_lambda_1se <- predict_bt_multinomial_probs(
  cv_fit = severity_fit$cv_fit,
  x_new = x_test,
  s = "lambda.1se",
  class_levels = bt_cfg$class_levels
)

expected_lambda_min <- severity_prob_to_expected(prob_lambda_min, PIPELINE_CONFIG$severity_weights)
expected_lambda_1se <- severity_prob_to_expected(prob_lambda_1se, PIPELINE_CONFIG$severity_weights)
baseline_cfg <- bt_cfg$matchup_baseline
baseline_prior_strength <- if (is.null(baseline_cfg$prior_strength)) 50 else baseline_cfg$prior_strength
baseline_reference_class <- if (is.null(baseline_cfg$reference_class)) "loss" else baseline_cfg$reference_class

baseline_tbl <- build_severity_matchup_baseline_predictions(
  train_df = train_df,
  test_df = test_df,
  target_col = bt_cfg$target_col,
  outcome_col = bt_cfg$outcome_col,
  class_levels = bt_cfg$class_levels,
  severity_weights = PIPELINE_CONFIG$severity_weights,
  prior_strength = baseline_prior_strength,
  reference_class = baseline_reference_class
)

scored <- test_df %>%
  bind_cols(
    prob_lambda_min %>% rename_with(~ paste0("prob_lambda_min_", .x)),
    prob_lambda_1se %>% rename_with(~ paste0("prob_lambda_1se_", .x)),
    baseline_tbl
  ) %>%
  mutate(
    frozen_model_prediction = expected_lambda_min,
    frozen_model_prediction_lambda_min = expected_lambda_min,
    frozen_model_prediction_lambda_1se = expected_lambda_1se,
    baseline_prediction = baseline_global_prediction,
    actual_target = severity_target,
    actual_class = as.character(.data[[bt_cfg$outcome_col]]),
    target_name = bt_cfg$target_col,
    model_name = bt_cfg$model_name,
    lambda_min = severity_fit$lambda_min,
    lambda_1se = severity_fit$lambda_1se
  )

global_prob_cols <- paste0("baseline_prob_", bt_cfg$class_levels)
matchup_prob_cols <- paste0("baseline_matchup_prob_", bt_cfg$class_levels)
baseline_global_prob_tbl <- scored %>%
  select(all_of(global_prob_cols))
colnames(baseline_global_prob_tbl) <- bt_cfg$class_levels

baseline_matchup_prob_tbl <- scored %>%
  select(all_of(matchup_prob_cols))
colnames(baseline_matchup_prob_tbl) <- bt_cfg$class_levels

multiclass_global <- compute_multiclass_validation_metrics_from_prob_tbl(
  actual_class = scored$actual_class,
  model_prob_tbl = prob_lambda_min,
  baseline_prob_tbl = baseline_global_prob_tbl,
  class_levels = bt_cfg$class_levels,
  mode = "severity_multiclass",
  baseline_name = "global_class_freq"
)

multiclass_matchup <- compute_multiclass_validation_metrics_from_prob_tbl(
  actual_class = scored$actual_class,
  model_prob_tbl = prob_lambda_min,
  baseline_prob_tbl = baseline_matchup_prob_tbl,
  class_levels = bt_cfg$class_levels,
  mode = "severity_multiclass",
  baseline_name = "matchup_multinomial_logit_mean"
)

multiclass_metrics <- bind_rows(multiclass_global, multiclass_matchup) %>%
  mutate(
    coverage = mean(!is.na(scored$frozen_model_prediction)),
    n_test = nrow(test_df),
    n_scored = sum(!is.na(scored$frozen_model_prediction)),
    model_name = bt_cfg$model_name,
    lambda_min = severity_fit$lambda_min,
    lambda_1se = severity_fit$lambda_1se,
    baseline_prior_strength = as.numeric(baseline_prior_strength),
    baseline_reference_class = as.character(baseline_reference_class)
  )

metrics <- multiclass_metrics

write_output_csv(scored, PIPELINE_CONFIG$output_paths$severity_bt_holdout_scored)
write_output_csv(metrics, PIPELINE_CONFIG$output_paths$severity_bt_validation_metrics)
write_output_csv(multiclass_metrics, PIPELINE_CONFIG$output_paths$severity_bt_multiclass_metrics)

message("Severity validation coverage: ", round(100 * mean(!is.na(scored$frozen_model_prediction)), 2), "%")
message("Wrote severity validation metrics: ", PIPELINE_CONFIG$output_paths$severity_bt_validation_metrics)
message("Wrote severity multiclass metrics: ", PIPELINE_CONFIG$output_paths$severity_bt_multiclass_metrics)
