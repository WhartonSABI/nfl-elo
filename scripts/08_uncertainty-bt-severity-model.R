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

message("Running severity ridge Bradley-Terry uncertainty analyses...")
modeling_table <- read_modeling_table(PIPELINE_CONFIG) %>%
  arrange(game_id, play_id, event_game_index) %>%
  ensure_severity_outcome_column(
    bt_cfg = PIPELINE_CONFIG$bt_severity_model,
    severity_weights = PIPELINE_CONFIG$severity_weights
  )
workers <- PIPELINE_CONFIG$parallel$workers
message("Using workers: ", workers, " (n_cores - 4 rule)")

validation_boot_iter <- as.integer(PIPELINE_CONFIG$uncertainty$end_to_end_bootstrap_iterations)

severity_holdout_scored_path <- PIPELINE_CONFIG$output_paths$severity_bt_holdout_scored
if (!file.exists(severity_holdout_scored_path)) {
  stop(
    "Missing severity holdout scored file: ",
    severity_holdout_scored_path,
    ". Run scripts/07_validate-bt-severity-model.R first (or run scripts/run-all.R)."
  )
}

severity_scored <- read_csv(severity_holdout_scored_path, show_col_types = FALSE)
class_levels <- PIPELINE_CONFIG$bt_severity_model$class_levels
baseline_method <- first_non_missing(severity_scored$baseline_matchup_method)
if (is.na(baseline_method) || baseline_method == "") {
  baseline_method <- "multinomial_logit_mean"
}
baseline_prior <- suppressWarnings(as.numeric(first_non_missing(severity_scored$baseline_prior_strength)))
if (is.na(baseline_prior) || baseline_prior <= 0) {
  baseline_prior <- as.numeric(PIPELINE_CONFIG$bt_severity_model$matchup_baseline$prior_strength)
}
baseline_reference_class <- first_non_missing(severity_scored$baseline_reference_class)
if (is.na(baseline_reference_class) || baseline_reference_class == "") {
  baseline_reference_class <- as.character(PIPELINE_CONFIG$bt_severity_model$matchup_baseline$reference_class)
}

validation_uncertainty_global <- bootstrap_multiclass_validation_uncertainty(
  scored_df = severity_scored,
  class_levels = class_levels,
  n_boot = validation_boot_iter,
  seed = PIPELINE_CONFIG$uncertainty$seed,
  workers = workers,
  bootstrap_unit = "game",
  model_prob_prefix = "prob_lambda_min_",
  baseline_prob_prefix = "baseline_prob_",
  mode = "severity_multiclass"
) %>%
  mutate(
    baseline_name = "global_class_freq",
    baseline_matchup_method = baseline_method,
    baseline_prior_strength = baseline_prior,
    baseline_reference_class = baseline_reference_class
  )

validation_uncertainty_matchup <- bootstrap_multiclass_validation_uncertainty(
  scored_df = severity_scored,
  class_levels = class_levels,
  n_boot = validation_boot_iter,
  seed = PIPELINE_CONFIG$uncertainty$seed + 1000L,
  workers = workers,
  bootstrap_unit = "game",
  model_prob_prefix = "prob_lambda_min_",
  baseline_prob_prefix = "baseline_matchup_prob_",
  mode = "severity_multiclass"
) %>%
  mutate(
    baseline_name = paste0("matchup_", baseline_method),
    baseline_matchup_method = baseline_method,
    baseline_prior_strength = baseline_prior,
    baseline_reference_class = baseline_reference_class
  )

validation_uncertainty <- bind_rows(
  validation_uncertainty_global,
  validation_uncertainty_matchup
) %>%
  mutate(
    fixed_lambda = NA_real_,
    train_rows_mean = NA_real_,
    test_rows_mean = nrow(severity_scored),
    bootstrap_scope = "temporal_holdout_game_block"
  ) %>%
  select(
    mode,
    metric,
    baseline_name,
    baseline_matchup_method,
    baseline_prior_strength,
    baseline_reference_class,
    model_value_mean,
    baseline_value_mean,
    improvement_mean,
    improvement_q025,
    improvement_q25,
    improvement_q50,
    improvement_q75,
    improvement_q975,
    train_rows_mean,
    test_rows_mean,
    iterations,
    fixed_lambda,
    bootstrap_scope
  )

write_output_csv(
  validation_uncertainty,
  PIPELINE_CONFIG$output_paths$severity_bt_validation_uncertainty
)

message("Wrote severity validation uncertainty: ", PIPELINE_CONFIG$output_paths$severity_bt_validation_uncertainty)

severity_artifact <- if (file.exists(PIPELINE_CONFIG$output_paths$severity_model_artifact)) {
  readRDS(PIPELINE_CONFIG$output_paths$severity_model_artifact)
} else {
  levels_tbl <- get_bt_levels(modeling_table)
  fit <- fit_bt_severity_cv(
    train_df = modeling_table,
    bt_cfg = PIPELINE_CONFIG$bt_severity_model,
    severity_weights = PIPELINE_CONFIG$severity_weights,
    rusher_levels = levels_tbl$rusher_levels,
    blocker_levels = levels_tbl$blocker_levels,
    seed = PIPELINE_CONFIG$uncertainty$seed
  )
  list(lambda_min = fit$lambda_min, lambda_1se = fit$lambda_1se)
}

fixed_lambda <- select_bt_fixed_lambda(severity_artifact, PIPELINE_CONFIG$bt_severity_model)
message(
  "Using fixed severity lambda from full-data CV: ",
  signif(fixed_lambda, 6),
  " (selection=",
  PIPELINE_CONFIG$bt_severity_model$lambda_selection,
  ")"
)

boot_results <- bootstrap_bt_end_to_end_severity(
  model_data = modeling_table,
  bt_cfg = PIPELINE_CONFIG$bt_severity_model,
  severity_weights = PIPELINE_CONFIG$severity_weights,
  fixed_lambda = fixed_lambda,
  train_fraction = PIPELINE_CONFIG$split$train_fraction,
  n_boot = validation_boot_iter,
  seed = PIPELINE_CONFIG$uncertainty$seed,
  workers = workers
)

rating_uncertainty <- boot_results$rating_summary %>%
  mutate(
    fixed_lambda = fixed_lambda,
    bootstrap_scope = "train_fit_end_to_end"
  )

write_output_csv(rating_uncertainty, PIPELINE_CONFIG$output_paths$severity_bt_rating_uncertainty)

message("Wrote severity rating uncertainty: ", PIPELINE_CONFIG$output_paths$severity_bt_rating_uncertainty)

path_boot_iter <- as.integer(PIPELINE_CONFIG$uncertainty$path_bootstrap_iterations)
if (path_boot_iter > 0L) {
  message(
    "Running weekly cumulative severity path uncertainty (bootstrap iterations=",
    path_boot_iter,
    ")..."
  )
  severity_path_uncertainty <- bootstrap_bt_weekly_path_severity(
    model_data = modeling_table,
    bt_cfg = PIPELINE_CONFIG$bt_severity_model,
    severity_weights = PIPELINE_CONFIG$severity_weights,
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
    severity_path_uncertainty,
    PIPELINE_CONFIG$output_paths$severity_bt_weekly_path_uncertainty
  )
  message("Wrote severity weekly path uncertainty: ", PIPELINE_CONFIG$output_paths$severity_bt_weekly_path_uncertainty)
} else {
  message("Skipping weekly severity path uncertainty (PATH_BOOTSTRAP_ITER <= 0).")
}
