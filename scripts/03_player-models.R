source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
split <- split_season(state$data, config)
training <- full <- list()
for (model in c("win", "severity")) {
  message("Fitting ", model, ": training and full season")
  training[[model]] <- fit_model(split$train, model, state$vocabulary, state$folds, config)
  full[[model]] <- fit_model(state$data, model, state$vocabulary, state$folds, config)
}
save_result(list(training = training, full = full), "models")
curves <- rbindlist(lapply(c("training", "full"), function(scope) {
  fits <- if (scope == "training") training else full
  rbindlist(lapply(fits, function(fit) cbind(scope = scope, fit$curves)))
}))
coefficients <- rbindlist(lapply(full, function(fit) {
  cbind(model = fit$model, term = rownames(fit$coefficients), as.data.frame(fit$coefficients))
}), fill = TRUE)
summary <- rbindlist(lapply(c("training", "full"), function(scope) {
  fits <- if (scope == "training") training else full
  rbindlist(lapply(fits, function(fit) data.frame(scope = scope, model = fit$model,
    lambda = fit$lambda, rows = fit$n_rows, games = fit$n_games, grid_boundary = fit$boundary)))
}))
write_table(curves, "cv_curves")
write_table(coefficients, "player_coefficients")
write_table(summary, "model_summary")
print(summary)
