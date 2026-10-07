# Retune historical profiles while reusing the player models' validation draws.
# These functions do not change the fixed strengths used for season rankings.

baseline_loss_grid <- function(train, test, model, config, strengths = baseline_grid()) {
    z <- model_sample(as.data.frame(test), model)
    check(nrow(z) > 0L, "No evaluable historical-baseline test rows")
    profile <- outcome_profiles(as.data.frame(train), model, config, 0)
    group <- vapply(z$blocker_keys, function(keys) paste(sort(keys), collapse = "\034"), character(1))
    first <- !duplicated(group)
    groups <- z$blocker_keys[first]
    group_index <- match(group, group[first])
    blockers <- sort(unique(unlist(groups)))
    allocation <- Matrix::sparseMatrix(i = rep(seq_along(groups), lengths(groups)),
        j = match(unlist(groups), blockers), x = rep(1 / lengths(groups), lengths(groups)),
        dims = c(length(groups), length(blockers)))
    pick <- function(keys, probabilities) {
        out <- matrix(profile$global, length(keys), length(profile$global), byrow = TRUE)
        index <- match(keys, rownames(probabilities))
        present <- !is.na(index)
        out[present, ] <- probabilities[index[present], , drop = FALSE]
        out
    }
    loss <- vapply(strengths, function(strength) {
        if (is.infinite(strength)) {
            p <- matrix(profile$global, nrow(z), length(profile$global), byrow = TRUE)
        } else {
            rusher <- pick(z$rusher_key, baseline_smooth(profile$raw_rusher, profile$global, strength))
            blocker <- pick(blockers, baseline_smooth(profile$raw_blocker, profile$global, strength))
            group_profile <- exp(as.matrix(allocation %*% log(pmax(blocker, 1e-15))))
            p <- sqrt(rusher * group_profile[group_index, , drop = FALSE])
            p <- p / rowSums(p)
        }
        check(all(is.finite(p)) && max(abs(rowSums(p) - 1)) < 1e-10, "Invalid historical-baseline probabilities")
        log_loss(if (model == "win") p[, 2] else p, z, model, config)
    }, numeric(1))
    data.frame(model = model, strength = strengths, strength_label = baseline_label(strengths),
        baseline_loss = loss, test_rows = nrow(z), test_games = length(unique(z$game_id)))
}

baseline_validation_cv <- function(train, model, folds, config, strengths = baseline_grid()) {
    z <- model_sample(as.data.frame(train), model)
    check(all(z$week <= config$train_last_week), "Baseline tuning contains holdout weeks")
    check(!anyDuplicated(folds$game_id), "Duplicate original-game fold assignment")
    fold <- folds$fold[match(z$game_id, folds$game_id)]
    check(!anyNA(fold) && setequal(unique(fold), seq_len(config$nfolds)), "Incomplete saved game folds")
    curves <- data.table::rbindlist(lapply(seq_len(config$nfolds), function(k) {
        a <- z[fold != k, , drop = FALSE]
        b <- z[fold == k, , drop = FALSE]
        check(!length(intersect(a$game_id, b$game_id)), "Historical-baseline CV leaked an original game")
        result <- baseline_loss_grid(a, b, model, config, strengths)
        result$fold <- k
        result$training_rows <- nrow(a)
        result$training_games <- length(unique(a$game_id))
        result
    }))
    summary <- curves[, .(cv_loss = weighted.mean(baseline_loss, test_rows),
        test_rows = sum(test_rows)), by = .(model, strength, strength_label)]
    data.table::setorder(summary, strength)
    selected <- max(summary$strength[summary$cv_loss <= min(summary$cv_loss) + 1e-12])
    summary$selected <- summary$strength == selected
    list(selected = selected, summary = as.data.frame(summary), folds = as.data.frame(curves))
}

baseline_validation_draw <- function(train, test, models, folds, config, replicate_id,
                                     strengths = baseline_grid()) {
    check(!length(intersect(train$game_id, test$game_id)), "Validation training/test game overlap")
    check(setequal(models$model, c("win", "severity")) && !anyDuplicated(models$model),
        "Need one saved model loss for each validation model")
    output <- lapply(c("win", "severity"), function(model) {
        tuned <- baseline_validation_cv(train, model, folds, config, strengths)
        fixed <- baseline_loss_grid(train, test, model, config, strengths)
        saved <- models[models$model == model, , drop = FALSE]
        check(all(fixed$test_rows == saved$test_rows) && all(fixed$test_games == saved$test_games),
            "Baseline and saved fitted model use different test samples")
        check(is.finite(saved$model_loss) && saved$model_loss >= 0, "Invalid saved fitted-model loss")
        fixed$model_loss <- saved$model_loss
        fixed$baseline <- "fixed_grid"
        selected <- fixed[fixed$strength == tuned$selected, , drop = FALSE]
        selected$baseline <- "training_cv_selected"
        result <- rbind(fixed, selected)
        result$replicate_id <- as.integer(replicate_id)
        result$selected_strength <- tuned$selected
        result$improvement <- result$baseline_loss - result$model_loss
        result$percent_improvement <- 100 * result$improvement / result$baseline_loss
        curves <- tuned$summary
        curves$replicate_id <- as.integer(replicate_id)
        fold_curves <- tuned$folds
        fold_curves$replicate_id <- as.integer(replicate_id)
        list(result = result, cv = curves, cv_folds = fold_curves)
    })
    list(result = as.data.frame(data.table::rbindlist(lapply(output, `[[`, "result"))),
        cv = as.data.frame(data.table::rbindlist(lapply(output, `[[`, "cv"))),
        cv_folds = as.data.frame(data.table::rbindlist(lapply(output, `[[`, "cv_folds"))))
}

baseline_validation_summary <- function(point, draws, expected, strengths = baseline_grid()) {
    draws <- as.data.frame(draws)
    check(!anyDuplicated(draws[c("model", "baseline", "strength", "replicate_id")]),
        "Duplicate paired historical-baseline draws")
    check(setequal(unique(draws$replicate_id), seq_len(expected)), "Missing paired validation draw IDs")
    check(nrow(draws) == 2L * (length(strengths) + 1L) * expected &&
        all(is.finite(draws$baseline_loss)) && all(is.finite(draws$model_loss)) &&
        all(is.finite(draws$percent_improvement)), "Incomplete or undefined paired baseline comparison")
    check(all(draws$model %in% c("win", "severity")) &&
        all(draws$baseline %in% c("fixed_grid", "training_cv_selected")), "Unexpected baseline comparison")
    for (model in c("win", "severity")) for (id in seq_len(expected)) {
        fixed <- draws[draws$model == model & draws$replicate_id == id & draws$baseline == "fixed_grid", ]
        tuned <- draws[draws$model == model & draws$replicate_id == id & draws$baseline == "training_cv_selected", ]
        check(nrow(fixed) == length(strengths) && setequal(fixed$strength, strengths) && nrow(tuned) == 1L,
            "A model/draw does not contain the exact fixed grid and one tuned comparison")
        check(tuned$strength %in% strengths && tuned$selected_strength == tuned$strength,
            "A tuned comparison does not use its training-selected strength")
    }
    draws$comparison <- ifelse(draws$baseline == "training_cv_selected", "training_cv_selected", draws$strength_label)
    z <- data.table::as.data.table(draws)
    intervals <- z[, .(n_boot = .N, improvement_mean = mean(improvement),
        improvement_q025 = unname(quantile(improvement, .025)),
        improvement_q975 = unname(quantile(improvement, .975)),
        percent_improvement_mean = mean(percent_improvement),
        percent_improvement_q025 = unname(quantile(percent_improvement, .025)),
        percent_improvement_q975 = unname(quantile(percent_improvement, .975))),
        by = .(model, baseline, comparison)]
    check(all(intervals$n_boot == expected), "A baseline comparison is missing planned draws")
    point$comparison <- ifelse(point$baseline == "training_cv_selected", "training_cv_selected", point$strength_label)
    summary <- merge(point, as.data.frame(intervals), by = c("model", "baseline", "comparison"), all.x = TRUE)
    check(nrow(summary) == nrow(point) && !anyNA(summary$n_boot), "Missing point-to-bootstrap comparison")
    frequencies <- z[baseline == "training_cv_selected", .(draws = .N), by = .(model, strength, strength_label)]
    frequencies$frequency <- frequencies$draws / expected
    list(summary = summary, selected_frequencies = as.data.frame(frequencies))
}
