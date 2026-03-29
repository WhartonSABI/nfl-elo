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
  library(tibble)
  library(Matrix)
  library(glmnet)
})

bt_output_dir <- file.path(project_root, "data", "output", "win_bt")
bt_output_paths <- list(
  holdout_scored = file.path(bt_output_dir, "validation_holdout_scored_win_bt_ridge.csv"),
  validation_metrics = file.path(bt_output_dir, "validation_metrics_win_bt_ridge.csv"),
  player_ratings = file.path(bt_output_dir, "player_ratings_win_bt_ridge.csv"),
  model_diagnostics = file.path(bt_output_dir, "model_diagnostics_win_bt_ridge.csv"),
  filtered_holdout_scored = file.path(bt_output_dir, "validation_holdout_scored_win_bt_ridge_train_n50.csv")
)

ensure_directory(bt_output_dir)
ensure_output_directories(PIPELINE_CONFIG)

if (!file.exists(PIPELINE_CONFIG$output_paths$modeling_table)) {
  message("Modeling table not found; building now...")
  modeling_table <- build_modeling_table(PIPELINE_CONFIG)
  modeling_summary <- tibble(
    rows = nrow(modeling_table),
    unique_games = dplyr::n_distinct(modeling_table$game_id),
    unique_rushers = dplyr::n_distinct(modeling_table$rusher_name),
    unique_blockers = dplyr::n_distinct(modeling_table$blocker_name),
    win_target_mean = mean(modeling_table$win_target, na.rm = TRUE),
    severity_target_mean = mean(modeling_table$severity_target, na.rm = TRUE)
  )
  write_output_csv(modeling_table, PIPELINE_CONFIG$output_paths$modeling_table)
  write_output_csv(modeling_summary, PIPELINE_CONFIG$output_paths$modeling_summary)
} else {
  modeling_table <- read_modeling_table(PIPELINE_CONFIG)
}

required_cols <- c("row_index", "rusher_name", "blocker_name", "win_target", "double_team")
assert_columns(modeling_table, required_cols, "modeling_table")

modeling_table <- modeling_table %>%
  arrange(row_index) %>%
  mutate(double_team = dplyr::coalesce(double_team, 0))

split <- split_train_test(modeling_table, train_fraction = PIPELINE_CONFIG$split$train_fraction)
train_df <- split$train
test_df <- split$test

train_rusher_counts <- train_df %>%
  count(rusher_name, name = "rusher_train_interactions")
train_blocker_counts <- train_df %>%
  count(blocker_name, name = "blocker_train_interactions")

message("Preparing sparse design matrix for regularized Bradley-Terry...")
rusher_levels <- sort(unique(modeling_table$rusher_name))
blocker_levels <- sort(unique(modeling_table$blocker_name))

to_sparse_bt_matrix <- function(df, rusher_levels, blocker_levels) {
  n <- nrow(df)
  r_idx <- match(df$rusher_name, rusher_levels)
  b_idx <- match(df$blocker_name, blocker_levels)

  if (anyNA(r_idx) || anyNA(b_idx)) {
    stop("Found unknown rusher/blocker names while building BT matrix.")
  }

  n_r <- length(rusher_levels)
  n_b <- length(blocker_levels)

  i <- c(seq_len(n), seq_len(n), seq_len(n))
  j <- c(
    r_idx,
    n_r + b_idx,
    n_r + n_b + rep(1L, n)
  )
  x <- c(
    rep(1, n),
    rep(-1, n),
    as.numeric(dplyr::coalesce(df$double_team, 0))
  )

  col_names <- c(
    paste0("rusher::", rusher_levels),
    paste0("blocker::", blocker_levels),
    "double_team"
  )

  X <- sparseMatrix(i = i, j = j, x = x, dims = c(n, length(col_names)))
  dimnames(X) <- list(NULL, col_names)
  X
}

x_train <- to_sparse_bt_matrix(train_df, rusher_levels, blocker_levels)
x_test <- to_sparse_bt_matrix(test_df, rusher_levels, blocker_levels)
y_train <- train_df$win_target
y_test <- test_df$win_target

seed <- PIPELINE_CONFIG$uncertainty$seed
set.seed(seed)

message("Fitting ridge-logit Bradley-Terry via cross-validated glmnet...")
cv_fit <- cv.glmnet(
  x = x_train,
  y = y_train,
  family = "binomial",
  alpha = 0,
  type.measure = "deviance",
  nfolds = 5,
  standardize = FALSE
)

lambda_min <- cv_fit$lambda.min
lambda_1se <- cv_fit$lambda.1se

pred_train_base <- mean(y_train, na.rm = TRUE)
pred_test_min <- as.numeric(predict(cv_fit, newx = x_test, s = "lambda.min", type = "response"))
pred_test_1se <- as.numeric(predict(cv_fit, newx = x_test, s = "lambda.1se", type = "response"))

scored <- test_df %>%
  left_join(train_rusher_counts, by = "rusher_name") %>%
  left_join(train_blocker_counts, by = "blocker_name") %>%
  mutate(
    rusher_train_interactions = dplyr::coalesce(rusher_train_interactions, 0L),
    blocker_train_interactions = dplyr::coalesce(blocker_train_interactions, 0L),
    eligible_train_n50 = rusher_train_interactions >= 50L & blocker_train_interactions >= 50L
  ) %>%
  mutate(
    frozen_model_prediction = pred_test_min,
    frozen_model_prediction_lambda_min = pred_test_min,
    frozen_model_prediction_lambda_1se = pred_test_1se,
    baseline_prediction = pred_train_base,
    actual_target = y_test,
    target_name = "win_target",
    model_name = "bt_ridge_lambda_min",
    lambda_min = lambda_min,
    lambda_1se = lambda_1se
  )

scored_n50 <- scored %>% filter(eligible_train_n50)

build_metrics <- function(df, pred_col, model_name, segment_name) {
  eval_df <- df %>%
    mutate(frozen_model_prediction = .data[[pred_col]])

  compute_validation_metrics(eval_df, mode = "win") %>%
    mutate(
      model_name = model_name,
      segment = segment_name,
      coverage = mean(!is.na(eval_df$frozen_model_prediction)),
      n_test = nrow(eval_df),
      n_scored = sum(!is.na(eval_df$frozen_model_prediction))
    )
}

metrics <- bind_rows(
  build_metrics(scored, "frozen_model_prediction_lambda_min", "bt_ridge_lambda_min", "all_holdout"),
  build_metrics(scored_n50, "frozen_model_prediction_lambda_min", "bt_ridge_lambda_min", "train_n_interactions_ge_50"),
  build_metrics(scored, "frozen_model_prediction_lambda_1se", "bt_ridge_lambda_1se", "all_holdout"),
  build_metrics(scored_n50, "frozen_model_prediction_lambda_1se", "bt_ridge_lambda_1se", "train_n_interactions_ge_50")
) %>%
  mutate(
    lambda_min = lambda_min,
    lambda_1se = lambda_1se
  ) %>%
  select(
    model_name,
    segment,
    mode,
    metric,
    model_value,
    baseline_value,
    improvement,
    n_rows,
    coverage,
    n_test,
    n_scored,
    lambda_min,
    lambda_1se
  )

coef_tbl <- as.matrix(coef(cv_fit, s = "lambda.1se")) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  rename(coef = `s1`) %>%
  filter(term != "(Intercept)")

coef_tbl_min <- as.matrix(coef(cv_fit, s = "lambda.min")) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  rename(coef = `s1`) %>%
  filter(term != "(Intercept)")

elo_scale <- 400 / log(10)

player_ratings <- coef_tbl_min %>%
  filter(term != "double_team") %>%
  mutate(
    role = if_else(grepl("^rusher::", term), "Rusher", "Blocker"),
    player_name = sub("^(rusher::|blocker::)", "", term),
    bt_logit_score = coef,
    elo_like_score = 1000 + elo_scale * (bt_logit_score - mean(bt_logit_score, na.rm = TRUE)),
    coefficient_source = "lambda.min"
  ) %>%
  select(player_name, role, bt_logit_score, elo_like_score, coefficient_source) %>%
  arrange(desc(elo_like_score))

double_team_effect <- coef_tbl_min %>%
  filter(term == "double_team") %>%
  pull(coef)
if (length(double_team_effect) == 0L) {
  double_team_effect <- 0
}

double_team_effect_1se <- coef_tbl %>%
  filter(term == "double_team") %>%
  pull(coef)
if (length(double_team_effect_1se) == 0L) {
  double_team_effect_1se <- 0
}

model_diagnostics <- tibble(
  train_rows = nrow(train_df),
  test_rows = nrow(test_df),
  train_win_rate = mean(y_train, na.rm = TRUE),
  test_win_rate = mean(y_test, na.rm = TRUE),
  lambda_min = lambda_min,
  lambda_1se = lambda_1se,
  cv_min_cvm = min(cv_fit$cvm, na.rm = TRUE),
  nonzero_terms_lambda_min = sum(abs(as.numeric(coef(cv_fit, s = "lambda.min"))[-1]) > 0),
  nonzero_terms_lambda_1se = sum(abs(as.numeric(coef(cv_fit, s = "lambda.1se"))[-1]) > 0),
  prediction_sd_lambda_min = sd(pred_test_min),
  prediction_sd_lambda_1se = sd(pred_test_1se),
  rusher_levels = length(rusher_levels),
  blocker_levels = length(blocker_levels),
  double_team_logit_effect_lambda_min = double_team_effect,
  double_team_logit_effect_lambda_1se = double_team_effect_1se
)

write_output_csv(scored, bt_output_paths$holdout_scored)
write_output_csv(scored_n50, bt_output_paths$filtered_holdout_scored)
write_output_csv(metrics, bt_output_paths$validation_metrics)
write_output_csv(player_ratings, bt_output_paths$player_ratings)
write_output_csv(model_diagnostics, bt_output_paths$model_diagnostics)

message("Wrote BT holdout scoring: ", bt_output_paths$holdout_scored)
message("Wrote BT holdout scoring (train n>=50 filter): ", bt_output_paths$filtered_holdout_scored)
message("Wrote BT metrics: ", bt_output_paths$validation_metrics)
message("Wrote BT player ratings: ", bt_output_paths$player_ratings)
message("Wrote BT diagnostics: ", bt_output_paths$model_diagnostics)
