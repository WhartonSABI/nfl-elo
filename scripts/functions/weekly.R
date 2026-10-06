# Cumulative weekly paths reuse the season penalty, reference matchups, and EPA weights.

weekly_condition <- function (message)
{
    stop(structure(list(message = message, call = NULL), class = c("weekly_nonestimable", "error",
        "condition")))
}

weekly_fixed_fit <- function (d, model, vocabulary, config, lambda)
{
    d <- model_sample(d, model)
    expected_classes <- if (model == "win")
        c("0", "1")
    else config$classes
    target <- if (model == "win")
        as.character(d$win_target)
    else d$severity_outcome
    if (!nrow(d) || !setequal(unique(target), expected_classes))
        weekly_condition(paste("Unavailable outcome class in weekly", model, "sample"))
    check(length(lambda) == 1L && is.finite(lambda) && lambda > 0, "Invalid fixed season penalty")
    x <- matchup_matrix(d, vocabulary)
    y <- if (model == "win")
        d$win_target
    else factor(target, levels = config$classes)
    args <- list(x = x, y = y, family = if (model == "win") "binomial" else "multinomial", alpha = 0,
        standardize = FALSE, lambda = lambda)
    control <- list(thresh = config$solver_tolerance, maxit = 1000000L)
    if (packageVersion("glmnet") >= "5.0")
        args$control <- control
    else args <- c(args, control)
    warnings <- character()
    raw <- withCallingHandlers(do.call(glmnet::glmnet, args), warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    check(raw$jerr == 0L && !any(grepl("converg|numerical|overflow|underflow|error", warnings,
        ignore.case = TRUE)), paste("Unsuccessful weekly fixed-penalty fit:", paste(unique(warnings),
        collapse = "; ")))
    raw_coef <- coef(raw, s = lambda)
    cf <- if (model == "win")
        as.matrix(raw_coef)
    else do.call(cbind, lapply(raw_coef[config$classes], as.matrix))
    if (model != "win")
        colnames(cf) <- config$classes
    fit <- list(model = model, vocabulary = vocabulary, roles = c("rusher", "blocker"), coefficients = center_coefficients(cf,
        model != "win"), lambda = lambda, curves = NULL, warnings = unique(warnings), boundary = FALSE,
        n_rows = nrow(d), n_games = length(unique(d$game_id)), tuning = "fixed corresponding full-season point/draw penalty",
        loss_normalization = "glmnet mean observation loss plus ridge penalty")
    check <- seq_len(min(1000L, nrow(d)))
    expected <- predict(raw, newx = x[check, , drop = FALSE], s = lambda, type = "response")
    if (model != "win") {
        labels <- dimnames(expected)[[2L]]
        check(setequal(labels, config$classes), "Weekly glmnet prediction class mismatch")
        expected <- matrix(expected, nrow = length(check), ncol = length(labels), dimnames = list(NULL,
            labels))[, config$classes, drop = FALSE]
    }
    actual <- predict_model(fit, d[check, , drop = FALSE])
    fit$centering_max_error <- max(abs(as.numeric(expected) - as.numeric(actual)))
    check(is.finite(fit$centering_max_error) && fit$centering_max_error < 1e-07, "Weekly centering changed fitted probabilities")
    fit
}

weekly_key <- function (z)
paste(z$model, z$role, z$player_key, sep = "|")

weekly_availability <- function (observed, sampled, vocabulary)
{
    out <- list()
    for (model in c("win", "severity")) {
        original <- model_sample(observed, model)
        draw <- model_sample(sampled, model)
        for (role in c("Rusher", "Blocker")) {
            players <- vocabulary[[tolower(role)]]
            keys <- function(d) if (role == "Rusher")
                d$rusher_key
            else unlist(d$blocker_keys)
            count <- as.integer(table(factor(keys(original), levels = players)))
            sampled_count <- as.integer(table(factor(keys(draw), levels = players)))
            keep <- count > 0L
            out[[length(out) + 1L]] <- data.frame(model = rep(model, sum(keep)), role = rep(role, sum(keep)),
                player_key = players[keep], observed_interactions = count[keep], sampled_interactions = sampled_count[keep],
                present_in_draw = sampled_count[keep] > 0L)
        }
    }
    as.data.frame(data.table::rbindlist(out))
}

weekly_scores <- function (fit, reference, observed, sampled, config)
{
    z <- as.data.frame(player_scores(fit, reference, observed, sampled, config))
    z <- z[z$score_type %in% c("standardized_probability", "expected_severity") & z$role_interactions >
        0, , drop = FALSE]
    check(!anyDuplicated(weekly_key(z)), "Duplicate weekly player scores")
    z
}

weekly_raw <- function (d, vocabulary)
{
    z <- model_sample(d, "win")
    out <- lapply(c("Rusher", "Blocker"), function(role) {
        if (role == "Rusher") {
            keys <- z$rusher_key
            ii <- seq_len(nrow(z))
            mass <- rep(1, nrow(z))
        }
        else {
            ii <- rep(seq_len(nrow(z)), lengths(z$blocker_keys))
            keys <- unlist(z$blocker_keys)
            mass <- rep(1/lengths(z$blocker_keys), lengths(z$blocker_keys))
        }
        a <- data.table::data.table(player_key = keys, game_id = z$game_id[ii], outcome = z$win_target[ii],
            mass = mass)
        if (!nrow(a))
            return(NULL)
        a <- a[, .(interactions = .N, games = data.table::uniqueN(game_id), rusher_wins = sum(outcome),
            rusher_losses = sum(1 - outcome), allocated_rusher_wins = sum(mass * outcome), allocated_rusher_losses = sum(mass *
                (1 - outcome))), by = player_key]
        a[, `:=`(raw_score, if (role == "Rusher")
            rusher_wins/interactions
        else rusher_losses/interactions)]
        a[, `:=`(allocated_raw_score, if (role == "Rusher")
            allocated_rusher_wins/(allocated_rusher_wins + allocated_rusher_losses)
        else allocated_rusher_losses/(allocated_rusher_wins + allocated_rusher_losses))]
        a[, `:=`(role, role)]
        a
    })
    result <- as.data.frame(data.table::rbindlist(out, fill = TRUE))
    if (!nrow(result))
        return(weekly_empty_raw())
    result
}

weekly_intervals <- function (values, expected)
{
    ok <- is.finite(values)
    n <- sum(ok)
    q <- if (n)
        unname(quantile(values[ok], c(0.025, 0.5, 0.975)))
    else rep(NA_real_, 3L)
    list(defined_draws = n, defined_fraction = n/expected, lower = if (n == expected) q[1] else NA_real_,
        median = if (n == expected) q[2] else NA_real_, upper = if (n == expected) q[3] else NA_real_,
        conditional_lower = q[1], conditional_upper = q[3], interval_status = if (n == expected) "complete" else "nonestimable_draws_retained")
}

weekly_empty_raw <- function() {
  data.frame(player_key = character(), interactions = integer(), games = integer(), rusher_wins = numeric(),
    rusher_losses = numeric(), allocated_rusher_wins = numeric(), allocated_rusher_losses = numeric(),
    raw_score = numeric(), allocated_raw_score = numeric(), role = character())
}

weekly_estimates <- function(state, config, fits, week, draw = NULL) {
  pool <- state$epa_state$pool
  multiplicity <- if (is.null(draw)) rep(1L, nrow(pool)) else as.integer(table(factor(draw$train_game_draws, levels = pool$game_id)))
  multiplicity[pool$week > week] <- 0L
  observed <- state$data[state$data$week <= week, , drop = FALSE]
  copies <- multiplicity[match(observed$game_id, pool$game_id)]
  sampled <- observed[rep(seq_len(nrow(observed)), copies), , drop = FALSE]
  if (!is.null(draw)) {
    config$severity_weights <- draw$calibration$weights$normalized
    config$conditional_sack_share <- draw$calibration$conditional_sack_share
  }
  rows <- diagnostics <- list()
  for (model in c("win", "severity")) {
    lambda <- if (is.null(draw)) fits[[model]]$lambda else draw$selected_lambda[[model]]
    result <- tryCatch({
      if (model == "severity" && !all(is.finite(config$severity_weights))) weekly_condition("Undefined season-draw EPA weights")
      if (week == 18 && !is.null(draw)) {
        scores <- as.data.frame(draw$result)
        scores <- scores[scores$model == model & scores$score_type %in% c("standardized_probability", "expected_severity") &
                           scores$role_interactions > 0, , drop = FALSE]
      } else {
        fit <- if (week == 18) fits[[model]] else weekly_fixed_fit(sampled, model, state$vocabulary, config, lambda)
        scores <- weekly_scores(fit, state$reference[[model]], observed, sampled, config)
      }
      scores$week <- rep(week, nrow(scores))
      scores$penalty <- rep(lambda, nrow(scores))
      list(scores = scores, status = "complete", message = "")
    }, weekly_nonestimable = function(e) list(scores = NULL, status = "nonestimable", message = conditionMessage(e)))
    rows[[model]] <- result$scores
    diagnostics[[model]] <- data.frame(week = week, model = model, replicate_id = if (is.null(draw)) 0L else draw$replicate_id,
                                       status = result$status, message = result$message)
  }
  scores <- as.data.frame(rbindlist(rows, fill = TRUE))
  if (!nrow(scores)) scores <- data.frame(model = character(), role = character(), player_key = character(),
    player_id = character(), player_name = character(), role_interactions = integer(), role_games = integer(),
    effective_credit_exposure = numeric(), present_in_draw = logical(), score_type = character(), score = numeric(),
    expected_outcome = numeric(), week = integer(), penalty = numeric())
  list(scores = scores, diagnostics = as.data.frame(rbindlist(diagnostics)),
       availability = weekly_availability(observed, sampled, state$vocabulary), raw = weekly_raw(sampled, state$vocabulary))
}
