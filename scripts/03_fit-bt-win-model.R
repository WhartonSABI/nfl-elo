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

message("Fitting full-sample win ridge Bradley-Terry model...")
modeling_table <- read_modeling_table(PIPELINE_CONFIG)
bt_cfg <- PIPELINE_CONFIG$bt_win_model
levels_tbl <- get_bt_levels(modeling_table)

win_fit <- fit_bt_win_cv(
  train_df = modeling_table,
  bt_cfg = bt_cfg,
  rusher_levels = levels_tbl$rusher_levels,
  blocker_levels = levels_tbl$blocker_levels,
  seed = PIPELINE_CONFIG$uncertainty$seed
)

coef_tbl <- extract_glmnet_binomial_coefficients(
  fit = win_fit$cv_fit,
  s_value = win_fit$lambda_min
)

player_ratings <- coef_tbl %>%
  filter(term != "(Intercept)") %>%
  transmute(term = term, term_score = coef) %>%
  build_player_ratings_from_term_scores(
    score_col_name = "bt_logit_score",
    source_label = "lambda.min"
  )

double_team_effect <- coef_tbl %>%
  filter(term == "double_team") %>%
  pull(coef)
if (length(double_team_effect) == 0L) {
  double_team_effect <- 0
}

model_diagnostics <- tibble(
  rows_fit = nrow(modeling_table),
  train_win_rate = mean(modeling_table$win_target, na.rm = TRUE),
  lambda_min = win_fit$lambda_min,
  lambda_1se = win_fit$lambda_1se,
  cv_min_cvm = min(win_fit$cv_fit$cvm, na.rm = TRUE),
  nonzero_terms_lambda_min = sum(abs(as.numeric(coef(win_fit$cv_fit, s = "lambda.min"))[-1]) > 0),
  nonzero_terms_lambda_1se = sum(abs(as.numeric(coef(win_fit$cv_fit, s = "lambda.1se"))[-1]) > 0),
  rusher_levels = length(levels_tbl$rusher_levels),
  blocker_levels = length(levels_tbl$blocker_levels),
  double_team_logit_effect_lambda_min = double_team_effect
)

artifact <- list(
  model_name = bt_cfg$model_name,
  fit_scope = "full_sample",
  lambda_min = win_fit$lambda_min,
  lambda_1se = win_fit$lambda_1se,
  baseline = win_fit$train_baseline,
  rusher_levels = levels_tbl$rusher_levels,
  blocker_levels = levels_tbl$blocker_levels,
  cv_fit = win_fit$cv_fit
)

saveRDS(artifact, PIPELINE_CONFIG$output_paths$win_model_artifact)
write_output_csv(coef_tbl, PIPELINE_CONFIG$output_paths$win_model_coefficients)
write_output_csv(player_ratings, PIPELINE_CONFIG$output_paths$win_bt_player_ratings)
write_output_csv(model_diagnostics, PIPELINE_CONFIG$output_paths$win_model_diagnostics)

message("Wrote win model artifact: ", PIPELINE_CONFIG$output_paths$win_model_artifact)
message("Wrote win model coefficients: ", PIPELINE_CONFIG$output_paths$win_model_coefficients)
message("Wrote win player ratings: ", PIPELINE_CONFIG$output_paths$win_bt_player_ratings)
message("Wrote win model diagnostics: ", PIPELINE_CONFIG$output_paths$win_model_diagnostics)
