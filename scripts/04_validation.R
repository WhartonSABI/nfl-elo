source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
fits <- read_result("models")$training
split <- split_season(state$data, config)
metrics <- rbindlist(lapply(fits, validation_metrics, train = split$train, test = split$test, config = config))
predictions <- rbindlist(lapply(fits, function(fit) {
  sample <- model_sample(split$test, fit$model)
  p <- predict_model(fit, sample)
  probabilities <- if (fit$model == "win") data.frame(prob_win = p) else as.data.frame(p)
  cbind(sample[c("game_id", "play_id", "rusher_id", "win_target", "severity_outcome")],
        model = fit$model, probabilities)
}), fill = TRUE)
calibration <- rbindlist(lapply(fits, function(fit) {
  sample <- model_sample(split$test, fit$model)
  p <- predict_model(fit, sample)
  classes <- if (fit$model == "win") "win" else config$classes
  rbindlist(lapply(classes, function(outcome) {
    probability <- if (fit$model == "win") p else p[, outcome]
    observed <- if (fit$model == "win") sample$win_target else as.integer(sample$severity_outcome == outcome)
    z <- data.table(bin = pmin(9L, floor(probability * 10)), probability = probability, observed = observed)
    cbind(model = fit$model, outcome = outcome,
          z[, .(rows = .N, predicted = mean(probability), observed = mean(observed)), by = bin])
  }))
}))
save_result(list(metrics = metrics, predictions = predictions, calibration = calibration), "validation")
write_table(metrics, "validation")
write_table(predictions, "holdout_predictions")
write_table(calibration, "calibration")
print(metrics)
