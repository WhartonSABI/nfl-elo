source("scripts/00_config.R")
state <- read_result("data")
epa <- state$epa_state
point <- fit_epa_weights(epa)
check(point$weights$defined && point$weights$denominator_negative, "EPA outcome weights need an estimable negative sack coefficient")
config <- state$config
config$severity_weights <- point$weights$normalized
config$conditional_sack_share <- sack_share(state$data)

test <- epa$data[epa$data$eligible_primary & epa$data$split == "test", ]
context_fit <- epa_fit(epa_matrix(epa$train, epa$basis, epa$config, "context"), epa$train$epa, cfg = epa$config)
validation <- epa_metrics(
  epa_predict(point$fit, epa_matrix(test, epa$basis, epa$config)),
  epa_predict(context_fit, epa_matrix(test, epa$basis, epa$config, "context")),
  test$epa, rep(1, nrow(test)))
coefficients <- data.frame(term = names(point$fit$coefficients), coefficient = point$fit$coefficients,
                           estimable = point$fit$estimable)
weights <- data.frame(outcome = names(point$weights$normalized), weight = point$weights$normalized)
save_result(list(point = point, config = config, context_fit = context_fit, validation = validation), "epa")
write_table(coefficients, "epa_coefficients")
write_table(weights, "epa_weights")
write_table(validation, "epa_validation")
print(coefficients[coefficients$term %in% c("N_W", "N_P", "N_S", "B", "E"), ], row.names = FALSE)
print(weights, row.names = FALSE)
print(validation, row.names = FALSE)
