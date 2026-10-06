# Resample whole games; the player models and EPA regression use the same draws.

bootstrap_stream <- function (seed, scope, replicate_id)
{
    check(scope %in% c("validation", "ratings"), "Unknown bootstrap scope")
    check(replicate_id >= 1L && replicate_id == as.integer(replicate_id), "Invalid replicate ID")
    RNGkind("L'Ecuyer-CMRG")
    set.seed(as.integer(seed) + if (scope == "ratings")
        100000L
    else 0L)
    stream <- .Random.seed
    if (replicate_id > 1L)
        for (i in seq_len(replicate_id - 1L)) stream <- parallel::nextRNGStream(stream)
    stream
}

resample_games <- function (d, pool)
{
    games <- as.character(pool$game_id)
    draws <- sample(games, length(games), replace = TRUE)
    ix <- split(seq_len(nrow(d)), as.character(d$game_id))
    rows <- unlist(ix[draws], use.names = FALSE)
    list(data = d[rows, , drop = FALSE], game_draws = draws)
}

bootstrap_refit <- function (data, config, folds, vocabulary, reference, scope, replicate_id, epa_state)
{
    started <- Sys.time()
    stream <- bootstrap_stream(config$seed, scope, replicate_id)
    assign(".Random.seed", stream, envir = .GlobalEnv)
    if (scope == "validation") {
        sp <- split_season(data, config)
        train <- resample_games(sp$train, epa_state$pool[epa_state$pool$week <= 15, ])
        test <- resample_games(sp$test, epa_state$pool[epa_state$pool$week >= 16, ])
        check(!length(intersect(train$game_draws, test$game_draws)), "Bootstrap leaked games")
    }
    else {
        train <- resample_games(data, epa_state$pool)
        test <- NULL
    }
    calibration <- fit_epa_weights(epa_state, train$game_draws)
    config$severity_weights <- calibration$weights$normalized
    config$conditional_sack_share <- sack_share(train$data)
    calibration$conditional_sack_share <- config$conditional_sack_share
    fits <- lapply(setNames(c("win", "severity"), c("win", "severity")), function(m) fit_model(train$data,
        m, vocabulary, folds, config))
    result <- if (scope == "validation")
        rbindlist(lapply(fits, validation_metrics, train = train$data, test = test$data, config = config))
    else rbindlist(lapply(fits, function(f) player_scores(f, reference[[f$model]], data, train$data,
        config)))
    result$replicate_id <- as.integer(replicate_id)
    list(scope = scope, replicate_id = as.integer(replicate_id), rng_stream = stream, train_game_draws = train$game_draws,
        test_game_draws = test$game_draws, result = result, calibration = calibration, selected_lambda = vapply(fits,
            `[[`, numeric(1), "lambda"), boundary = vapply(fits, `[[`, logical(1), "boundary"), coefficients = lapply(fits,
            `[[`, "coefficients"), fit_diagnostics = lapply(fits, function(f) f[c("n_rows", "n_games",
            "centering_max_error")]), warnings = lapply(fits, `[[`, "warnings"), elapsed_seconds = as.numeric(difftime(Sys.time(),
            started, units = "secs")), status = "complete")
}
