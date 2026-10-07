# Run after 06_bootstrap.R; no player-model fitting is repeated here.
source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
expected <- config$bootstrap_iterations
controls <- checkpoint_settings(state)
sample_games <- function(d, game_draws) {
  index <- split(seq_len(nrow(d)), as.character(d$game_id))
  d[unlist(index[as.character(game_draws)], use.names = FALSE), , drop = FALSE]
}
loss_records <- function(metrics) {
  unique(as.data.frame(metrics)[c("model", "model_loss", "test_rows", "test_games")])
}
split <- split_season(state$data, config)
point <- baseline_validation_draw(split$train, split$test,
  loss_records(read_result("validation")$metrics), state$folds, config, 0L)
draws <- lapply(seq_len(expected), function(id) {
  path <- file.path(settings$output_dir, "bootstrap/validation", sprintf("%04d.rds", id))
  check(file.exists(path), "Run 06_bootstrap.R before baseline validation")
  saved <- readRDS(path)
  check(identical(saved$controls, controls), "Baseline reuse inputs or original fit settings changed")
  draw <- saved$result
  check(draw$scope == "validation" && draw$replicate_id == id && draw$status == "complete",
    "Invalid saved validation draw")
  train <- sample_games(split$train, draw$train_game_draws)
  test <- sample_games(split$test, draw$test_game_draws)
  message("Historical-baseline validation ", id, "/", expected)
  baseline_validation_draw(train, test, loss_records(draw$result), state$folds, config, id)
})
values <- as.data.frame(rbindlist(lapply(draws, `[[`, "result")))
summaries <- baseline_validation_summary(point$result, values, expected)
result <- list(point = point, draws = values, summary = summaries$summary,
  selected_frequencies = summaries$selected_frequencies,
  reuse = list(expected_draws = expected, completed_draws = length(draws),
    exact_saved_game_draws = TRUE, fitted_model_losses_unchanged = TRUE,
    fitted_models_refit = FALSE, game_folds = state$folds,
    fixed_season_ranking_strengths = config$season_baseline_strength))
save_result(result, "tuned_baseline_validation")
write_table(point$result, "tuned_baseline_validation_point")
write_table(values, "tuned_baseline_validation_draws")
write_table(result$summary, "tuned_baseline_validation_summary")
write_table(result$selected_frequencies, "tuned_baseline_selected_frequencies")
print(result$summary[result$summary$baseline == "training_cv_selected", ])
