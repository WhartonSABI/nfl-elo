options(nfl.settings = list(output_dir = tempfile("baseline-validation-tests-")))
source("scripts/00_config.R")
set.seed(615)
config <- model_config()
config$nfolds <- 3L
config$severity_weights <- c(loss = 0, win = .02, pressure = .1, sack = 1)
n <- 180L
d <- data.frame(game_id = as.character(rep(1:9, each = 20)),
  week = rep(c(rep(1, 6), rep(16, 3)), each = 20),
  rusher_key = sample(paste0("id:", 1:5), n, TRUE),
  win_target = rbinom(n, 1, .3), severity_outcome = sample(config$classes, n, TRUE),
  early_model_eligible = 1L)
d$blocker_keys <- I(lapply(seq_len(n), function(i) sample(paste0("id:", 11:16), 1L + (i %% 2L))))
train <- d[d$week == 1, ]
test <- d[d$week == 16, ]
# An opponent absent from training must receive the fold's league profile.
test$rusher_key[1] <- "id:unseen"
test$blocker_keys[[2]] <- c("id:unseen", "id:11")
folds <- data.frame(game_id = as.character(1:6), fold = rep(1:3, 2))
for (model in c("win", "severity")) {
  fast <- baseline_loss_grid(train, test, model, config)
  profile <- outcome_profiles(train, model, config, 0)
  slow <- vapply(baseline_grid(), function(strength) log_loss(
    baseline_predict(profile, test, model, strength), test, model, config), numeric(1))
  stopifnot(max(abs(fast$baseline_loss - slow)) < 1e-12)
  duplicate_train <- train[c(seq_len(nrow(train)), which(train$game_id == "1")), ]
  fast_cv <- baseline_validation_cv(duplicate_train, model, folds, config)
  slow_cv <- baseline_cv(duplicate_train, model, folds, config)
  stopifnot(identical(fast_cv$selected, slow_cv$selected),
    max(abs(fast_cv$summary$cv_loss - slow_cv$summary$cv_loss)) < 1e-12)
}
saved <- data.frame(model = c("win", "severity"), model_loss = c(.51, 1.2),
  test_rows = nrow(test), test_games = 3L)
point <- baseline_validation_draw(train, test, saved, folds, config, 0L)
altered <- test
altered$win_target <- 1 - altered$win_target
altered$severity_outcome <- rev(altered$severity_outcome)
changed_test <- baseline_validation_draw(train, altered, saved, folds, config, 1L)
stopifnot(identical(point$cv, transform(changed_test$cv, replicate_id = 0L)),
  all(point$result$model_loss == saved$model_loss[match(point$result$model, saved$model)]))
draw1 <- baseline_validation_draw(train, test, saved, folds, config, 1L)$result
draw2 <- baseline_validation_draw(train, test, saved, folds, config, 2L)$result
summary <- baseline_validation_summary(point$result, rbind(draw1, draw2), 2L)
stopifnot(nrow(summary$summary) == 26L, all(summary$summary$n_boot == 2L),
  all(summary$selected_frequencies$frequency == 1))
misassigned <- rbind(draw1, draw2)
wrong <- which(misassigned$model == "win" & misassigned$baseline == "training_cv_selected" &
  misassigned$replicate_id == 1L)
misassigned$replicate_id[wrong] <- 2L
misassigned$strength[wrong] <- 5
stopifnot(inherits(try(baseline_validation_summary(point$result, misassigned, 2L), silent = TRUE), "try-error"))
cat("Historical-baseline reuse tests passed: grouped duplicates, unseen players, all-grid equivalence, training-only tuning, paired losses, completeness.\n")
