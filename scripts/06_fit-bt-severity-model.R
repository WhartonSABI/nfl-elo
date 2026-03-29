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

message("Fitting full-sample severity ridge Bradley-Terry model...")
modeling_table <- read_modeling_table(PIPELINE_CONFIG)
bt_cfg <- PIPELINE_CONFIG$bt_severity_model
modeling_table <- ensure_severity_outcome_column(
  modeling_table,
  bt_cfg = bt_cfg,
  severity_weights = PIPELINE_CONFIG$severity_weights
)
levels_tbl <- get_bt_levels(modeling_table)

severity_fit <- fit_bt_severity_cv(
  train_df = modeling_table,
  bt_cfg = bt_cfg,
  severity_weights = PIPELINE_CONFIG$severity_weights,
  rusher_levels = levels_tbl$rusher_levels,
  blocker_levels = levels_tbl$blocker_levels,
  seed = PIPELINE_CONFIG$uncertainty$seed
)

coef_tbl <- extract_glmnet_multinomial_coefficients(
  fit = severity_fit$cv_fit,
  s_value = severity_fit$lambda_min,
  class_levels = bt_cfg$class_levels
)

weighted_term_scores <- severity_weighted_term_scores(
  multinomial_coef_tbl = coef_tbl,
  severity_weights = PIPELINE_CONFIG$severity_weights
)

player_ratings <- build_player_ratings_from_term_scores(
  term_scores = weighted_term_scores,
  rating_scale = bt_cfg$rating_scale,
  score_col_name = "weighted_severity_logit_score",
  source_label = "lambda.min"
)

class_counts <- modeling_table %>%
  count(.data[[bt_cfg$outcome_col]], name = "n") %>%
  rename(outcome = all_of(bt_cfg$outcome_col))
if (nrow(class_counts) == 0L) {
  class_counts <- tibble(outcome = bt_cfg$class_levels, n = 0L)
}

model_diagnostics <- tibble(
  rows_fit = nrow(modeling_table),
  train_severity_mean = mean(modeling_table$severity_target, na.rm = TRUE),
  lambda_min = severity_fit$lambda_min,
  lambda_1se = severity_fit$lambda_1se,
  cv_min_cvm = min(severity_fit$cv_fit$cvm, na.rm = TRUE),
  rusher_levels = length(levels_tbl$rusher_levels),
  blocker_levels = length(levels_tbl$blocker_levels)
) %>%
  bind_cols(
    class_counts %>%
      tidyr::pivot_wider(names_from = outcome, values_from = n, names_prefix = "outcome_n_", values_fill = 0)
  )

artifact <- list(
  model_name = bt_cfg$model_name,
  fit_scope = "full_sample",
  lambda_min = severity_fit$lambda_min,
  lambda_1se = severity_fit$lambda_1se,
  baseline = severity_fit$train_baseline,
  baseline_class_prob = severity_fit$baseline_class_prob,
  class_levels = severity_fit$class_levels,
  rusher_levels = levels_tbl$rusher_levels,
  blocker_levels = levels_tbl$blocker_levels,
  cv_fit = severity_fit$cv_fit
)

saveRDS(artifact, PIPELINE_CONFIG$output_paths$severity_model_artifact)
write_output_csv(coef_tbl, PIPELINE_CONFIG$output_paths$severity_model_coefficients)
write_output_csv(player_ratings, PIPELINE_CONFIG$output_paths$severity_bt_player_ratings)
write_output_csv(model_diagnostics, PIPELINE_CONFIG$output_paths$severity_model_diagnostics)

message("Wrote severity model artifact: ", PIPELINE_CONFIG$output_paths$severity_model_artifact)
message("Wrote severity model coefficients: ", PIPELINE_CONFIG$output_paths$severity_model_coefficients)
message("Wrote severity player ratings: ", PIPELINE_CONFIG$output_paths$severity_bt_player_ratings)
message("Wrote severity model diagnostics: ", PIPELINE_CONFIG$output_paths$severity_model_diagnostics)
