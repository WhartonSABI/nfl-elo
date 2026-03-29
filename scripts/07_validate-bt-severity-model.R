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
baseline_prob_vec <- severity_fit$baseline_class_prob[bt_cfg$class_levels]
baseline_prob_tbl <- as_tibble(
  matrix(
    rep(as.numeric(baseline_prob_vec), each = nrow(test_df)),
    ncol = length(bt_cfg$class_levels),
    dimnames = list(NULL, bt_cfg$class_levels)
  ),
  .name_repair = "minimal"
) %>%
  rename_with(~ paste0("baseline_prob_", .x))

scored <- test_df %>%
  bind_cols(
    prob_lambda_min %>% rename_with(~ paste0("prob_lambda_min_", .x)),
    prob_lambda_1se %>% rename_with(~ paste0("prob_lambda_1se_", .x)),
    baseline_prob_tbl
  ) %>%
  mutate(
    frozen_model_prediction = expected_lambda_min,
    frozen_model_prediction_lambda_min = expected_lambda_min,
    frozen_model_prediction_lambda_1se = expected_lambda_1se,
    baseline_prediction = severity_fit$train_baseline,
    actual_target = severity_target,
    actual_class = as.character(.data[[bt_cfg$outcome_col]]),
    target_name = bt_cfg$target_col,
    model_name = bt_cfg$model_name,
    lambda_min = severity_fit$lambda_min,
    lambda_1se = severity_fit$lambda_1se
  )

metrics <- compute_validation_metrics(scored, mode = "severity") %>%
  mutate(
    coverage = mean(!is.na(scored$frozen_model_prediction)),
    n_test = nrow(test_df),
    n_scored = sum(!is.na(scored$frozen_model_prediction)),
    model_name = bt_cfg$model_name,
    lambda_min = severity_fit$lambda_min,
    lambda_1se = severity_fit$lambda_1se
  )

multiclass_metrics <- compute_multiclass_validation_metrics(
  actual_class = scored$actual_class,
  model_prob_tbl = prob_lambda_min,
  baseline_class_prob = severity_fit$baseline_class_prob,
  class_levels = bt_cfg$class_levels,
  mode = "severity_multiclass"
) %>%
  mutate(
    model_name = bt_cfg$model_name,
    lambda_min = severity_fit$lambda_min,
    lambda_1se = severity_fit$lambda_1se
  )

write_output_csv(scored, PIPELINE_CONFIG$output_paths$severity_bt_holdout_scored)
write_output_csv(metrics, PIPELINE_CONFIG$output_paths$severity_bt_validation_metrics)
write_output_csv(multiclass_metrics, PIPELINE_CONFIG$output_paths$severity_bt_multiclass_metrics)

message("Severity validation coverage: ", round(100 * mean(!is.na(scored$frozen_model_prediction)), 2), "%")
message("Wrote severity validation metrics: ", PIPELINE_CONFIG$output_paths$severity_bt_validation_metrics)
message("Wrote severity multiclass metrics: ", PIPELINE_CONFIG$output_paths$severity_bt_multiclass_metrics)
