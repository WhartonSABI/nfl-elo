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

parse_prior_grid <- function(env_name, default_values = c(10, 25, 50, 100)) {
  raw <- trimws(Sys.getenv(env_name, unset = ""))
  if (identical(raw, "")) {
    return(sort(unique(as.numeric(default_values))))
  }

  parts <- unlist(strsplit(raw, split = "[,[:space:];]+"))
  vals <- suppressWarnings(as.numeric(parts))
  vals <- vals[is.finite(vals) & vals > 0]

  if (length(vals) == 0L) {
    warning("No valid prior strengths parsed from ", env_name, "; using defaults.")
    return(sort(unique(as.numeric(default_values))))
  }

  sort(unique(vals))
}

build_model_prob_tbl <- function(scored_df, class_levels, prefix = "prob_lambda_min_") {
  prob_cols <- paste0(prefix, class_levels)
  assert_columns(scored_df, prob_cols, "scored_df")
  out <- scored_df %>% select(all_of(prob_cols))
  colnames(out) <- class_levels
  out
}

locate_scored_predictions <- function(test_df, scored_df, pred_col, join_name) {
  assert_columns(test_df, c("row_index"), "test_df")
  assert_columns(scored_df, c("row_index", pred_col), "scored_df")

  out <- test_df %>%
    select(row_index) %>%
    left_join(scored_df %>% select(row_index, !!sym(pred_col)), by = "row_index")

  if (any(is.na(out[[pred_col]]))) {
    stop("Missing ", join_name, " predictions after row_index join.")
  }

  out[[pred_col]]
}

project_root <- locate_project_root()
source(file.path(project_root, "scripts", "00_config.R"))
source(file.path(project_root, "scripts", "00_utils.R"))

ensure_output_directories(PIPELINE_CONFIG)

message("Running prior-strength sensitivity for matchup baselines...")

shared_grid <- parse_prior_grid("BASELINE_PRIOR_SENSITIVITY_GRID", default_values = c(10, 25, 50, 100))
win_grid <- parse_prior_grid("BASELINE_PRIOR_SENSITIVITY_GRID_WIN", default_values = shared_grid)
severity_grid <- parse_prior_grid("BASELINE_PRIOR_SENSITIVITY_GRID_SEVERITY", default_values = shared_grid)

modeling_table <- read_modeling_table(PIPELINE_CONFIG)
bt_win_cfg <- PIPELINE_CONFIG$bt_win_model
bt_sev_cfg <- PIPELINE_CONFIG$bt_severity_model

modeling_table <- ensure_severity_outcome_column(
  modeling_table,
  bt_cfg = bt_sev_cfg,
  severity_weights = PIPELINE_CONFIG$severity_weights
)

split <- split_train_test(modeling_table, train_fraction = PIPELINE_CONFIG$split$train_fraction)
train_df <- split$train
test_df <- split$test

win_scored <- read_csv(PIPELINE_CONFIG$output_paths$win_bt_holdout_scored, show_col_types = FALSE)
sev_scored <- read_csv(PIPELINE_CONFIG$output_paths$severity_bt_holdout_scored, show_col_types = FALSE)

win_model_pred <- locate_scored_predictions(
  test_df = test_df,
  scored_df = win_scored,
  pred_col = "frozen_model_prediction_lambda_min",
  join_name = "win"
)

severity_model_prob <- build_model_prob_tbl(
  scored_df = sev_scored,
  class_levels = bt_sev_cfg$class_levels,
  prefix = "prob_lambda_min_"
)

severity_actual_class <- test_df[[bt_sev_cfg$outcome_col]]
if (any(is.na(severity_actual_class))) {
  stop("Missing severity outcome labels in holdout test set.")
}

win_rows <- bind_rows(lapply(win_grid, function(prior_strength) {
  baseline_tbl <- build_win_baseline_predictions(
    train_df = train_df,
    test_df = test_df,
    target_col = bt_win_cfg$target_col,
    prior_strength = prior_strength,
    method = bt_win_cfg$matchup_baseline$method
  )

  scored_tmp <- tibble(
    actual_target = as.numeric(test_df[[bt_win_cfg$target_col]]),
    frozen_model_prediction = as.numeric(win_model_pred),
    baseline_prediction = as.numeric(baseline_tbl$baseline_matchup_prediction)
  )

  compute_validation_metrics(
    scored_df = scored_tmp,
    mode = "win",
    baseline_col = "baseline_prediction",
    baseline_name = paste0("matchup_", normalize_win_baseline_method(bt_win_cfg$matchup_baseline$method))
  ) %>%
    mutate(
      task = "win",
      prior_strength = as.numeric(prior_strength),
      baseline_method = normalize_win_baseline_method(bt_win_cfg$matchup_baseline$method),
      n_test = nrow(test_df)
    )
}))

severity_rows <- bind_rows(lapply(severity_grid, function(prior_strength) {
  baseline_tbl <- build_severity_matchup_baseline_predictions(
    train_df = train_df,
    test_df = test_df,
    target_col = bt_sev_cfg$target_col,
    outcome_col = bt_sev_cfg$outcome_col,
    class_levels = bt_sev_cfg$class_levels,
    severity_weights = PIPELINE_CONFIG$severity_weights,
    prior_strength = prior_strength,
    reference_class = bt_sev_cfg$matchup_baseline$reference_class
  )

  baseline_prob_cols <- paste0("baseline_matchup_prob_", bt_sev_cfg$class_levels)
  assert_columns(baseline_tbl, baseline_prob_cols, "severity baseline table")
  baseline_prob_tbl <- baseline_tbl %>% select(all_of(baseline_prob_cols))
  colnames(baseline_prob_tbl) <- bt_sev_cfg$class_levels

  compute_multiclass_validation_metrics_from_prob_tbl(
    actual_class = severity_actual_class,
    model_prob_tbl = severity_model_prob,
    baseline_prob_tbl = baseline_prob_tbl,
    class_levels = bt_sev_cfg$class_levels,
    mode = "severity_multiclass",
    baseline_name = "matchup_multinomial_logit_mean"
  ) %>%
    mutate(
      task = "severity",
      prior_strength = as.numeric(prior_strength),
      baseline_method = "multinomial_logit_mean",
      baseline_reference_class = as.character(bt_sev_cfg$matchup_baseline$reference_class),
      n_test = nrow(test_df)
    )
}))

sensitivity <- bind_rows(win_rows, severity_rows) %>%
  relocate(task, prior_strength, baseline_method, baseline_name, metric, mode)

write_output_csv(sensitivity, PIPELINE_CONFIG$output_paths$bt_baseline_prior_sensitivity)

message("Wrote baseline-prior sensitivity table: ", PIPELINE_CONFIG$output_paths$bt_baseline_prior_sensitivity)
message("Rows in sensitivity table: ", nrow(sensitivity))
