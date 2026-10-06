source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
fits <- read_result("models")
split <- split_season(state$data, config)

baseline_state <- state
baseline_state$config <- config
baseline_state$training_fits <- fits$training
baseline_state$full_fits <- fits$full
baseline_state$point_scores <- read_result("rankings")$scores
baseline <- baseline_analysis(baseline_state)

ordinal <- ordinal_fit(state$data, state$vocabulary, state$folds, config)
ordinal_players <- ordinal_scores(ordinal, state$reference$severity, state$data, config)
multinomial_scores <- player_scores(fits$full$severity, state$reference$severity, state$data, config = config)
agreement <- ordinal_rank_comparison(ordinal_players, multinomial_scores)
ordinal_comparison <- data.frame(model = c("multinomial", "ordinal"),
  lambda = c(fits$full$severity$lambda, ordinal$lambda),
  cv_loss = c(fits$full$severity$curves$cv_loss[fits$full$severity$curves$selected],
              ordinal$curves$cv_loss[ordinal$curves$selected]))

epa <- state$epa_state
epa_sensitivity <- lapply(c("primary", "protected", "no_penalty"), function(kind) {
  sample <- epa$train
  if (kind == "no_penalty") sample <- sample[!sample$accepted_penalty, ]
  design <- epa_matrix(sample, epa$basis, epa$config, if (kind == "protected") "protected" else "primary")
  fit <- epa_fit(design, sample$epa, cfg = epa$config)
  weights <- epa_weights(fit)
  list(fit = fit, summary = data.frame(analysis = kind, outcome = names(weights$normalized),
                                     weight = weights$normalized, plays = nrow(sample)))
})
save_result(list(baseline = baseline, ordinal = ordinal, ordinal_scores = ordinal_players,
  ordinal_agreement = agreement, epa = epa_sensitivity), "sensitivity")
for (name in names(baseline)) write_table(baseline[[name]], name)
write_table(ordinal_players, "ordinal_scores")
write_table(agreement$agreement, "ordinal_agreement")
write_table(ordinal_comparison, "ordinal_cv_comparison")
write_table(rbindlist(lapply(epa_sensitivity, `[[`, "summary")), "epa_sensitivity")
print(baseline$smoothing_holdout)
print(ordinal_comparison, row.names = FALSE)
print(agreement$agreement)
print(rbindlist(lapply(epa_sensitivity, `[[`, "summary")))
