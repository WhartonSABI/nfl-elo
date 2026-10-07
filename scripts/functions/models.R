# A matchup compares one rusher with the mean effect of his initial blockers.

matchup_matrix <- function (d, vocabulary, roles = c("rusher", "blocker"))
{
    n <- nrow(d)
    cols <- c(if ("rusher" %in% roles) paste0("rusher::", vocabulary$rusher), if ("blocker" %in% roles) paste0("blocker::",
        vocabulary$blocker), "double_team", "double_team_unknown")
    i <- j <- integer()
    v <- numeric()
    if ("rusher" %in% roles) {
        i <- c(i, seq_len(n))
        j <- c(j, match(paste0("rusher::", d$rusher_key), cols))
        v <- c(v, rep(1, n))
    }
    if ("blocker" %in% roles) {
        i <- c(i, rep(seq_len(n), lengths(d$blocker_keys)))
        j <- c(j, match(paste0("blocker::", unlist(d$blocker_keys)), cols))
        v <- c(v, rep(-1/lengths(d$blocker_keys), lengths(d$blocker_keys)))
    }
    for (term in c("double_team", "double_team_unknown")) {
        i <- c(i, seq_len(n))
        j <- c(j, rep(match(term, cols), n))
        v <- c(v, d[[term]])
    }
    check(!anyNA(j), "Player missing from fixed vocabulary")
    sparseMatrix(i = i, j = j, x = v, dims = c(n, length(cols)), dimnames = list(NULL, cols))
}

center_coefficients <- function (coef, multinomial = ncol(coef) > 1L)
{
    out <- as.matrix(coef)
    if (multinomial)
        out <- out - rowMeans(out)
    for (role in c("rusher", "blocker")) {
        rows <- startsWith(rownames(out), paste0(role, "::"))
        if (!any(rows))
            next
        means <- colMeans(out[rows, , drop = FALSE])
        out[rows, ] <- sweep(out[rows, , drop = FALSE], 2, means, "-")
        out["(Intercept)", ] <- out["(Intercept)", ] + if (role == "rusher")
            means
        else -means
    }
    out
}

softmax <- function (eta)
{
    z <- exp(eta - apply(eta, 1, max))
    z/rowSums(z)
}

predict_model <- function (fit, d)
{
    x <- matchup_matrix(d, fit$vocabulary, fit$roles)
    eta <- sweep(as.matrix(x %*% fit$coefficients[-1, , drop = FALSE]), 2, fit$coefficients[1, ], "+")
    if (fit$model == "win")
        as.numeric(plogis(eta[, 1]))
    else softmax(eta)
}

fit_model <- function (d, model, vocabulary, folds, config, fixed_lambda = NULL, roles = c("rusher", "blocker"))
{
    d <- model_sample(d, model)
    check(nrow(d) > 0, paste("No evaluable outcomes for", model))
    x <- matchup_matrix(d, vocabulary, roles)
    y <- if (model == "win")
        d$win_target
    else factor(d$severity_outcome, levels = config$classes)
    check(length(unique(y)) == if (model == "win")
        2L
    else length(config$classes), paste("A response class is absent for", model, "; do not silently replace this draw"))
    foldid <- folds$fold[match(d$game_id, folds$game_id)]
    check(!anyNA(foldid) && length(unique(foldid)) == config$nfolds, "Missing game-fold assignments")
    warnings <- character()
    args <- list(x = x, y = y, family = if (model == "win") "binomial" else "multinomial", alpha = 0,
        standardize = FALSE)
    if (packageVersion("glmnet") >= "5.0") {
        args$control <- list(thresh = config$solver_tolerance, maxit = 1000000L)
    }
    else args <- c(args, list(thresh = config$solver_tolerance, maxit = 1000000L))
    if (is.null(fixed_lambda)) {
        for (k in unique(foldid)) {
            check(length(unique(y[foldid != k])) == length(unique(y)), paste("A training fold is missing a class for",
                model))
        }
        args <- c(args, list(lambda = config$lambda, foldid = foldid, type.measure = "deviance", parallel = FALSE))
        raw <- withCallingHandlers(do.call(glmnet::cv.glmnet, args), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        check(!any(grepl("converg|numerical|overflow|underflow|error", warnings, ignore.case = TRUE)),
            paste("Numerical warning in grouped CV:", paste(unique(warnings), collapse = "; ")))
        lambda <- raw$lambda.min
        check(all(is.finite(raw$cvm)) && raw$glmnet.fit$jerr == 0, "Unsuccessful CV fit")
        check(length(raw$lambda) == length(config$lambda) && max(abs(log(raw$lambda/config$lambda))) <
            1e-10, "CV did not evaluate the complete prespecified penalty grid")
        curves <- data.frame(model = model, lambda = raw$lambda, cv_loss = raw$cvm/2, cv_se = raw$cvsd/2,
            selected = raw$lambda == lambda, lambda_1se_selected = raw$lambda == raw$lambda.1se)
        raw_coef <- coef(raw, s = lambda)
    }
    else {
        args$lambda <- fixed_lambda
        raw <- do.call(glmnet::glmnet, args)
        check(raw$jerr == 0, "Unsuccessful fixed-penalty diagnostic fit")
        lambda <- fixed_lambda
        curves <- NULL
        raw_coef <- coef(raw, s = lambda)
    }
    cf <- if (model == "win")
        as.matrix(raw_coef)
    else do.call(cbind, lapply(raw_coef[config$classes], as.matrix))
    if (model != "win")
        colnames(cf) <- config$classes
    centered <- center_coefficients(cf, model != "win")
    fit <- list(model = model, vocabulary = vocabulary, roles = roles, coefficients = centered, lambda = lambda,
        curves = curves, warnings = unique(warnings), boundary = lambda %in% range(config$lambda), n_rows = nrow(d),
        n_games = length(unique(d$game_id)))
    check <- seq_len(min(1000L, nrow(d)))
    expected <- predict(raw, newx = x[check, , drop = FALSE], s = lambda, type = "response")
    if (model != "win")
        expected <- expected[, config$classes, , drop = FALSE]
    actual <- predict_model(fit, d[check, , drop = FALSE])
    fit$centering_max_error <- max(abs(as.numeric(expected) - as.numeric(actual)))
    check(fit$centering_max_error < 1e-07, "Centering changed fitted probabilities")
    fit
}

observation_weights <- function (d, model)
rep(1, nrow(d))

sack_share <- function (d)
{
    z <- model_sample(d, "severity")
    v <- z$sack_credit[z$severity_outcome == "sack"]
    check(length(v) > 0 && all(is.finite(v) & v > 0 & v <= 1), "No valid observed sack shares")
    mean(v)
}

reference_matchups <- function (d, model)
{
    z <- model_sample(d, model)
    r <- z[c("rusher_key", "blocker_keys", "double_team", "double_team_unknown")]
    r$weight <- rep(1/nrow(r), nrow(r))
    ix <- rep(seq_len(nrow(z)), lengths(z$blocker_keys))
    b <- z[ix, c("rusher_key", "double_team", "double_team_unknown"), drop = FALSE]
    b$focal_key <- unlist(z$blocker_keys)
    b$group_size <- rep(lengths(z$blocker_keys), lengths(z$blocker_keys))
    b$co_blocker_keys <- I(unlist(lapply(z$blocker_keys, function(ks) lapply(seq_along(ks), function(i) ks[-i])),
        recursive = FALSE))
    b$weight <- 1/(nrow(z) * b$group_size)
    collapse_ref <- function(x, sig) {
        first <- !duplicated(sig)
        mass <- tapply(x$weight, sig, sum)
        y <- x[first, , drop = FALSE]
        y$weight <- as.numeric(mass[sig[first]])
        y
    }
    r <- collapse_ref(r, paste(vapply(r$blocker_keys, paste, character(1), collapse = "|"), r$double_team,
        r$double_team_unknown, sep = ";"))
    b <- collapse_ref(b, paste(b$rusher_key, vapply(b$co_blocker_keys, paste, character(1), collapse = "|"),
        b$group_size, b$double_team, b$double_team_unknown, sep = ";"))
    list(Rusher = r, Blocker = b)
}

player_exposure <- function (d, model, role, players)
{
    z <- model_sample(d, model)
    if (role == "Rusher") {
        keys <- z$rusher_key
        games <- z$game_id
        mass <- observation_weights(z, model)
    }
    else {
        keys <- unlist(z$blocker_keys)
        games <- rep(z$game_id, lengths(z$blocker_keys))
        mass <- rep(observation_weights(z, model)/lengths(z$blocker_keys), lengths(z$blocker_keys))
    }
    list(count = as.integer(table(factor(keys, levels = players))), games = vapply(players, function(k) length(unique(games[keys ==
        k])), integer(1)), mass = vapply(players, function(k) sum(mass[keys == k]), numeric(1)))
}

player_scores <- function (fit, reference, exposure_data, draw_data = exposure_data, config)
{
    cf <- fit$coefficients
    out <- list()
    severity <- fit$model != "win"
    for (role in c("Rusher", "Blocker")) {
        prefix <- tolower(role)
        players <- fit$vocabulary[[prefix]]
        own <- cf[paste0(prefix, "::", players), , drop = FALSE]
        coef_score <- if (!severity)
            own[, 1]
        else as.numeric(own %*% (config$severity_weights - mean(config$severity_weights)))
        ref <- reference[[role]]
        if (role == "Rusher") {
            base <- do.call(rbind, lapply(ref$blocker_keys, function(ks) -colMeans(cf[paste0("blocker::",
                ks), , drop = FALSE])))
            own_scale <- rep(1, nrow(ref))
        }
        else {
            base <- cf[paste0("rusher::", ref$rusher_key), , drop = FALSE]
            co <- do.call(rbind, lapply(seq_len(nrow(ref)), function(i) {
                ks <- ref$co_blocker_keys[[i]]
                if (!length(ks))
                  return(rep(0, ncol(cf)))
                colSums(cf[paste0("blocker::", ks), , drop = FALSE])/ref$group_size[i]
            }))
            base <- base - co
            own_scale <- -1/ref$group_size
        }
        base <- sweep(base, 2, cf["(Intercept)", ], "+") + ref$double_team %o% cf["double_team", ] +
            ref$double_team_unknown %o% cf["double_team_unknown", ]
        expected_prob <- t(vapply(seq_along(players), function(i) {
            eta <- base + own_scale %o% own[i, ]
            if (!severity)
                return(c(1 - sum(ref$weight * plogis(eta[, 1])), sum(ref$weight * plogis(eta[, 1]))))
            colSums(softmax(eta) * ref$weight)
        }, numeric(if (severity)
            length(config$classes)
        else 2L)))
        expected <- if (severity)
            as.numeric(expected_prob %*% config$severity_weights)
        else expected_prob[, 2]
        ex <- player_exposure(exposure_data, fit$model, role, players)
        dr <- player_exposure(draw_data, fit$model, role, players)
        labels <- as.data.frame(fit$vocabulary$labels)
        labels <- labels[labels$role == role, ]
        labels <- labels[match(players, labels$player_key), ]
        base_df <- data.frame(model = fit$model, role = role, player_key = players, player_id = labels$player_id,
            player_name = labels$player_name, role_interactions = ex$count, role_games = ex$games, effective_credit_exposure = ex$mass,
            present_in_draw = dr$count > 0)
        out[[length(out) + 1L]] <- cbind(base_df, score_type = "coefficient", score = coef_score, expected_outcome = NA_real_)
        out[[length(out) + 1L]] <- cbind(base_df, score_type = if (severity)
            "expected_severity"
        else "standardized_probability", score = if (role == "Rusher")
            expected
        else if (severity)
            -expected
        else 1 - expected, expected_outcome = expected)
        if (severity) {
            credited_weights <- config$severity_weights
            credited_weights["sack"] <- credited_weights["sack"] * config$conditional_sack_share
            credited <- as.numeric(expected_prob %*% credited_weights)
            credited_index <- as.numeric(own %*% (credited_weights - mean(credited_weights)))
            out[[length(out) + 1L]] <- cbind(base_df, score_type = "credited_coefficient", score = credited_index,
                expected_outcome = NA_real_)
            out[[length(out) + 1L]] <- cbind(base_df, score_type = "expected_credited_severity", score = if (role ==
                "Rusher")
                credited
            else -credited, expected_outcome = credited)
        }
    }
    rbindlist(out)
}

outcome_profiles <- function (d, model, config, strength)
{
    z <- model_sample(d, model)
    classes <- if (model == "win")
        c("0", "1")
    else config$classes
    target <- if (model == "win")
        as.character(z$win_target)
    else z$severity_outcome
    w <- observation_weights(z, model)
    counts_for <- function(keys, ii, mass) {
        players <- sort(unique(keys))
        m <- matrix(0, length(players), length(classes), dimnames = list(players, classes))
        a <- data.table(player = match(keys, players), outcome = match(target[ii], classes), mass = mass)[,
            .(mass = sum(mass)), by = .(player, outcome)]
        m[cbind(a$player, a$outcome)] <- a$mass
        m
    }
    global <- vapply(classes, function(k) sum(w[target == k]), numeric(1))/sum(w)
    r <- counts_for(z$rusher_key, seq_len(nrow(z)), w)
    ii <- rep(seq_len(nrow(z)), lengths(z$blocker_keys))
    b <- counts_for(unlist(z$blocker_keys), ii, rep(w/lengths(z$blocker_keys), lengths(z$blocker_keys)))
    smooth <- function(m) (m + matrix(strength * global, nrow(m), length(global), byrow = TRUE))/(rowSums(m) +
        strength)
    list(global = global, rusher = smooth(r), blocker = smooth(b), raw_rusher = r, raw_blocker = b, classes = classes)
}

predict_baselines <- function (train, test, model, config, strength = config$baseline_strength[[model]])
{
    a <- outcome_profiles(train, model, config, strength)
    pick <- function(keys, profile) {
        m <- matrix(a$global, length(keys), length(a$global), byrow = TRUE)
        ix <- match(keys, rownames(profile))
        ok <- !is.na(ix)
        m[ok, ] <- profile[ix[ok], , drop = FALSE]
        m
    }
    r <- pick(test$rusher_key, a$rusher)
    b <- t(vapply(test$blocker_keys, function(ks) exp(colMeans(log(pmax(pick(ks, a$blocker), 1e-15)))),
        numeric(length(a$global))))
    p <- sqrt(r * b)
    p <- p/rowSums(p)
    g <- matrix(a$global, nrow(test), length(a$global), byrow = TRUE)
    if (model == "win")
        list(global = g[, 2], smoothed_matchup = p[, 2])
    else list(global = g, smoothed_matchup = p)
}

log_loss <- function (prediction, data, model, config)
{
    if (model == "win") {
        p <- pmin(pmax(prediction, 1e-15), 1 - 1e-15)
        loss <- -(data$win_target * log(p) + (1 - data$win_target) * log1p(-p))
    }
    else {
        check(all(is.finite(prediction)) && all(prediction >= -1e-12) && max(abs(rowSums(prediction) -
            1)) < 1e-07, "Invalid class probabilities")
        loss <- -log(pmax(prediction[cbind(seq_len(nrow(data)), match(data$severity_outcome, config$classes))],
            1e-15))
    }
    weighted.mean(loss, observation_weights(data, model))
}

validation_metrics <- function (fit, train, test, config)
{
    test <- model_sample(test, fit$model)
    check(nrow(test) > 0, "No evaluable test rows")
    loss <- log_loss(predict_model(fit, test), test, fit$model, config)
    baselines <- predict_baselines(train, test, fit$model, config)
    data.table::rbindlist(lapply(names(baselines), function(b) {
        baseline_loss <- log_loss(baselines[[b]], test, fit$model, config)
        data.frame(model = fit$model, baseline = b, model_loss = loss, baseline_loss = baseline_loss,
            improvement = baseline_loss - loss, percent_improvement = 100 * (baseline_loss - loss)/baseline_loss,
            test_rows = nrow(test), test_games = length(unique(test$game_id)), lambda = fit$lambda)
    }))
}

baseline_scores <- function (d, model, vocabulary, config)
{
    p <- outcome_profiles(d, model, config, config$season_baseline_strength[[model]])
    out <- list()
    for (role in c("Rusher", "Blocker")) {
        prefix <- tolower(role)
        players <- vocabulary[[prefix]]
        m <- p[[paste0("raw_", prefix)]]
        ii <- match(players, rownames(m))
        counts <- matrix(0, length(players), ncol(m))
        ok <- !is.na(ii)
        counts[ok, ] <- m[ii[ok], , drop = FALSE]
        raw <- counts/rowSums(counts)
        smooth <- (counts + matrix(config$season_baseline_strength[[model]] * p$global, nrow(counts), ncol(counts),
            byrow = TRUE))/(rowSums(counts) + config$season_baseline_strength[[model]])
        weights <- if (model == "win")
            c(0, 1)
        else config$severity_weights
        ex <- player_exposure(d, model, role, players)
        labels <- as.data.frame(vocabulary$labels)
        labels <- labels[labels$role == role, ]
        labels <- labels[match(players, labels$player_key), ]
        for (method in c("raw", "shrunken")) {
            probs <- if (method == "raw")
                raw
            else smooth
            value <- as.numeric(probs %*% weights)
            base <- data.frame(model = model, role = role, player_key = players, player_id = labels$player_id,
                player_name = labels$player_name, role_interactions = ex$count, role_games = ex$games,
                effective_credit_exposure = ex$mass, method = method, score_type = if (model == "win")
                  "rate"
                else "expected_severity", score = if (role == "Rusher")
                  value
                else if (model == "win")
                  1 - value
                else -value, expected_outcome = value)
            out[[length(out) + 1L]] <- base
            if (model == "severity") {
                w <- weights
                w["sack"] <- w["sack"] * config$conditional_sack_share
                ev <- as.numeric(probs %*% w)
                base$score_type <- "expected_credited_severity"
                base$score <- if (role == "Rusher")
                  ev
                else -ev
                base$expected_outcome <- ev
                out[[length(out) + 1L]] <- base
            }
        }
    }
    rbindlist(out)
}
