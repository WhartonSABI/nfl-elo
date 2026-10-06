# Compare baseline smoothing choices and a proportional-odds alternative.

baseline_grid <- function ()
c(0, 1, 2, 5, 10, 25, 50, 100, 200, 400, 1000, Inf)

baseline_label <- function (m)
ifelse(is.infinite(m), "league_only", format(m, trim = TRUE, scientific = FALSE))

baseline_smooth <- function (counts, global, strength)
{
    check(length(strength) == 1L && !is.na(strength) && strength >= 0, "Invalid smoothing strength")
    prior <- matrix(global, nrow(counts), length(global), byrow = TRUE, dimnames = dimnames(counts))
    if (is.infinite(strength))
        return(prior)
    out <- (counts + strength * prior)/(rowSums(counts) + strength)
    out[rowSums(counts) == 0, ] <- prior[rowSums(counts) == 0, , drop = FALSE]
    out
}

baseline_predict <- function (profile, test, model, strength)
{
    rprof <- baseline_smooth(profile$raw_rusher, profile$global, strength)
    bprof <- baseline_smooth(profile$raw_blocker, profile$global, strength)
    pick <- function(keys, p) {
        out <- matrix(profile$global, length(keys), length(profile$global), byrow = TRUE)
        ix <- match(keys, rownames(p))
        ok <- !is.na(ix)
        out[ok, ] <- p[ix[ok], , drop = FALSE]
        out
    }
    if (is.infinite(strength)) {
        p <- matrix(profile$global, nrow(test), length(profile$global), byrow = TRUE)
    }
    else {
        r <- pick(test$rusher_key, rprof)
        b <- t(vapply(test$blocker_keys, function(ks) exp(colMeans(log(pmax(pick(ks, bprof), 1e-15)))),
            numeric(length(profile$global))))
        p <- sqrt(r * b)
        p <- p/rowSums(p)
    }
    check(all(is.finite(p)) && max(abs(rowSums(p) - 1)) < 1e-10, "Invalid baseline probabilities")
    if (model == "win")
        p[, 2]
    else p
}

baseline_cv <- function (d, model, folds, config, strengths = baseline_grid())
{
    z <- model_sample(as.data.frame(d), model)
    check(all(z$week <= config$train_last_week), "Baseline CV contains holdout weeks")
    check(!anyDuplicated(folds$game_id), "Duplicate game fold assignment")
    ff <- folds$fold[match(z$game_id, folds$game_id)]
    check(!anyNA(ff) && setequal(unique(ff), seq_len(config$nfolds)), "Invalid game folds")
    rows <- list()
    profiles <- list()
    for (k in seq_len(config$nfolds)) {
        train <- z[ff != k, , drop = FALSE]
        test <- z[ff == k, , drop = FALSE]
        check(!length(intersect(train$game_id, test$game_id)), "CV game leakage")
        a <- outcome_profiles(train, model, config, 0)
        profiles[[k]] <- list(fold = k, train_games = sort(unique(train$game_id)), test_games = sort(unique(test$game_id)),
            global = a$global)
        for (m in strengths) rows[[length(rows) + 1L]] <- data.frame(model = model, fold = k, strength = m,
            strength_label = baseline_label(m), cv_loss = log_loss(baseline_predict(a, test,
                model, m), test, model, config), training_rows = nrow(train), test_rows = nrow(test),
            training_games = length(unique(train$game_id)), test_games = length(unique(test$game_id)))
    }
    losses <- as.data.frame(data.table::rbindlist(rows))
    means <- as.data.frame(data.table::as.data.table(losses)[, .(cv_loss = weighted.mean(cv_loss, test_rows),
        test_rows = sum(test_rows)), by = .(model, strength, strength_label)])
    means <- means[order(means$strength), ]
    best <- min(means$cv_loss)
    selected <- max(means$strength[means$cv_loss <= best + 1e-12])
    means$selected <- means$strength == selected
    list(folds = losses, summary = means, selected = selected, profiles = profiles)
}

baseline_model_cv <- function (d, model, folds, config, vocabulary, training_fit, tolerance = 1e-06)
{
    z <- model_sample(as.data.frame(d), model)
    check(all(z$week <= config$train_last_week), "BT CV audit contains holdout weeks")
    ff <- folds$fold[match(z$game_id, folds$game_id)]
    check(!anyDuplicated(folds$game_id) && !anyNA(ff) && setequal(unique(ff), seq_len(config$nfolds)),
        "Invalid BT audit game folds")
    selected <- training_fit$lambda
    curve <- training_fit$curves
    check(sum(curve$selected) == 1L && any(abs(log(config$lambda/selected)) < 1e-12), "Missing original selected CV penalty")
    saved <- curve$cv_loss[curve$selected]
    out <- list()
    for (k in seq_len(config$nfolds)) {
        train <- z[ff != k, , drop = FALSE]
        test <- z[ff == k, , drop = FALSE]
        check(!length(intersect(train$game_id, test$game_id)), "BT audit game leakage")
        y <- if (model == "win")
            train$win_target
        else factor(train$severity_outcome, levels = config$classes)
        check(length(unique(y)) == if (model == "win")
            2L
        else length(config$classes), "BT audit training fold missing a response class")
        args <- list(x = matchup_matrix(train, vocabulary), y = y, family = if (model == "win") "binomial" else "multinomial",
            alpha = 0, standardize = FALSE, lambda = config$lambda)
        if (packageVersion("glmnet") >= "5.0")
            args$control <- list(thresh = config$solver_tolerance, maxit = 1000000L)
        else args <- c(args, list(thresh = config$solver_tolerance, maxit = 1000000L))
        warnings <- character()
        raw <- withCallingHandlers(do.call(glmnet::glmnet, args), warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
        check(raw$jerr == 0 && !any(grepl("converg|numerical|overflow|underflow|error", warnings,
            ignore.case = TRUE)), "BT audit path failed numerically")
        check(length(raw$lambda) == length(config$lambda) && max(abs(log(raw$lambda/config$lambda))) <
            1e-10, "BT audit did not fit the full frozen penalty path")
        p <- predict(raw, newx = matchup_matrix(test, vocabulary), s = selected, type = "response")
        if (model == "win")
            p <- as.numeric(p)
        else {
            labels <- dimnames(p)[[2L]]
            p <- matrix(p, nrow = nrow(test), ncol = length(config$classes), dimnames = list(NULL, labels))
            check(setequal(labels, config$classes), "BT audit class mismatch")
            p <- p[, config$classes, drop = FALSE]
        }
        out[[k]] <- data.frame(model = model, fold = k, lambda = selected, training_rows = nrow(train),
            test_rows = nrow(test), training_games = length(unique(train$game_id)), test_games = length(unique(test$game_id)),
            as.list(baseline_loss_audit(p, test, model, config)), warnings = paste(warnings, collapse = "; "))
    }
    out <- as.data.frame(data.table::rbindlist(out))
    original <- weighted.mean(out$glmnet_cv_loss_1e5, out$test_rows)
    out$saved_glmnet_cv_loss <- saved
    out$reconstructed_glmnet_cv_loss <- original
    out$reconciliation_difference <- original - saved
    out$reconciliation_tolerance <- tolerance
    check(abs(original - saved) <= tolerance, "Reconstructed BT CV loss differs from saved curve; review runtime/path compatibility")
    list(folds = out, cv_loss = weighted.mean(out$cv_loss_1e15, out$test_rows), saved_cv_loss = saved,
        reconstructed_glmnet_cv_loss = original, clipped_probability_count = sum(out$clipped_probability_count))
}

baseline_loss_audit <- function (prediction, d, model, config)
{
    clipped <- pmin(pmax(prediction, 1e-05), 1 - 1e-05)
    affected <- prediction < 1e-05 | prediction > 1 - 1e-05
    if (model == "win") {
        original <- mean(-(d$win_target * log(clipped) + (1 - d$win_target) * log1p(-clipped)))
        affected_outcome <- sum(affected)
    }
    else {
        ix <- cbind(seq_len(nrow(d)), match(d$severity_outcome, config$classes))
        original <- mean(-log(clipped[ix]))
        affected_outcome <- sum(affected[ix])
    }
    list(cv_loss_1e15 = log_loss(prediction, d, model, config), glmnet_cv_loss_1e5 = original, clipped_probability_count = sum(affected),
        clipped_observed_outcome_count = affected_outcome, probability_count = length(prediction), minimum_probability = min(prediction),
        maximum_probability = max(prediction))
}

baseline_literal <- function (d, vocabulary)
{
    z <- model_sample(as.data.frame(d), "win")
    ans <- list()
    for (role in c("Rusher", "Blocker")) {
        players <- vocabulary[[tolower(role)]]
        if (role == "Rusher") {
            keys <- z$rusher_key
            ix <- seq_len(nrow(z))
        }
        else {
            keys <- unlist(z$blocker_keys)
            ix <- rep(seq_len(nrow(z)), lengths(z$blocker_keys))
        }
        counts <- as.integer(table(factor(keys, levels = players)))
        wins <- vapply(players, function(k) sum(z$win_target[ix[keys == k]]), numeric(1))
        p <- wins/counts
        p[counts == 0] <- NA_real_
        labels <- as.data.frame(vocabulary$labels)
        labels <- labels[labels$role == role, ]
        labels <- labels[match(players, labels$player_key), ]
        ans[[role]] <- data.frame(role = role, player_key = players, player_id = labels$player_id, player_name = labels$player_name,
            win_interactions = counts, win_games = vapply(players, function(k) length(unique(z$game_id[ix[keys ==
                k]])), integer(1)), geometric_rusher_wins = wins, literal_raw_win = if (role == "Rusher")
                p
            else 1 - p)
    }
    as.data.frame(data.table::rbindlist(ans))
}

baseline_profile_scores <- function (d, model, vocabulary, config, strength, profile = NULL)
{
    if (is.null(profile))
        profile <- outcome_profiles(d, model, config, 0)
    ans <- list()
    for (role in c("Rusher", "Blocker")) {
        players <- vocabulary[[tolower(role)]]
        raw <- profile[[paste0("raw_", tolower(role))]]
        counts <- matrix(0, length(players), ncol(raw), dimnames = list(players, colnames(raw)))
        ix <- match(players, rownames(raw))
        ok <- !is.na(ix)
        counts[ok, ] <- raw[ix[ok], , drop = FALSE]
        probs <- baseline_smooth(counts, profile$global, strength)
        if (strength == 0)
            probs[rowSums(counts) == 0, ] <- NA_real_
        value <- as.numeric(probs %*% if (model == "win") c(0, 1) else config$severity_weights)
        ex <- player_exposure(d, model, role, players)
        ans[[role]] <- data.frame(model = model, role = role, player_key = players, strength = strength,
            strength_label = baseline_label(strength), interactions = ex$count, games = ex$games,
            allocated_mass = ex$mass, score = if (role == "Rusher")
                value
            else if (model == "win")
                1 - value
            else -value)
    }
    as.data.frame(data.table::rbindlist(ans))
}

baseline_topk <- function (score, k = 10L)
{
    lo <- rank(-score, ties.method = "min")
    hi <- rank(-score, ties.method = "max")
    k <- min(k, length(score))
    pmin(1, pmax(0, (k - lo + 1)/(hi - lo + 1)))
}

baseline_auc <- function (y, score)
{
    ok <- !is.na(y) & is.finite(score)
    y <- y[ok]
    score <- score[ok]
    np <- sum(y)
    nn <- length(y) - np
    if (!np || !nn)
        return(NA_real_)
    (sum(rank(score, ties.method = "average")[as.logical(y)]) - np * (np + 1)/2)/(np * nn)
}

baseline_pair <- function (a, b)
{
    ra <- rank(-a, ties.method = "average")
    rb <- rank(-b, ties.method = "average")
    c(spearman = if (length(unique(ra)) < 2 || length(unique(rb)) < 2) NA_real_ else cor(ra, rb), mean_absolute_rank_change = mean(abs(ra -
        rb)), max_absolute_rank_change = max(abs(ra - rb)), top10_overlap_credit = sum(pmin(baseline_topk(a),
        baseline_topk(b))))
}

baseline_honors <- function (players, d)
{
    ol <- unique(unlist(lapply(seq_len(nrow(d)), function(i) {
        flags <- as.logical(jsonlite::fromJSON(d$blocker_rated_ol[i]))
        d$blocker_keys[[i]][flags]
    })))
    players$rated_ol <- players$role == "Blocker" & players$player_key %in% ol
    first <- c("Trent Williams", "Tristan Wirfs", "Joel Bitonio", "Zack Martin", "Jason Kelce", "T.J. Watt",
        "Myles Garrett", "Aaron Donald", "Cameron Heyward", "Micah Parsons", "Darius Leonard", "De'Vondre Campbell")
    second <- c("Rashawn Slater", "Lane Johnson", "Quenton Nelson", "Wyatt Teller", "Corey Linsley",
        "Robert Quinn", "Maxx Crosby", "Chris Jones", "Jeffery Simmons", "Demario Davis", "Roquan Smith",
        "Bobby Wagner")
    names_norm <- function(x) gsub(" +", " ", gsub("[^a-z0-9 ]", "", tolower(trimws(x))))
    ref <- data.frame(role = rep(c(rep("Blocker", 5), rep("Rusher", 7)), 2), name = names_norm(c(first,
        second)), first = rep(c(TRUE, FALSE), each = 12))
    name <- names_norm(players$player_name)
    key <- paste(players$role, name)
    ambiguous <- duplicated(key) | duplicated(key, fromLast = TRUE)
    idx <- match(key, paste(ref$role, ref$name))
    ambiguous_honor <- ambiguous & !is.na(idx)
    players$ambiguous_display_name <- ambiguous
    players$all_pro_match_status <- ifelse(is.na(idx), "unmatched", ifelse(ambiguous_honor, "ambiguous_name",
        "matched"))
    players$all_pro_first_team <- !is.na(idx) & ref$first[idx]
    players$all_pro_any_team <- !is.na(idx)
    players$all_pro_first_team[ambiguous_honor] <- NA
    players$all_pro_any_team[ambiguous_honor] <- NA
    players
}

baseline_rank_views <- function (players, methods, exposure_fields, prefix = "comparison")
{
    rows <- pairs <- metrics <- list()
    for (role in c("Rusher", "Blocker")) for (nmin in c(0L, 100L, 200L, 400L)) {
        z <- players[players$role == role, , drop = FALSE]
        keep <- apply(z[, methods, drop = FALSE], 1, function(x) all(is.finite(x)))
        for (nm in exposure_fields) keep <- keep & z[[nm]] > 0 & z[[nm]] >= nmin
        z <- z[keep, , drop = FALSE]
        if (!nrow(z))
            next
        z <- z[order(z$player_key), ]
        for (method in methods) rows[[length(rows) + 1L]] <- data.frame(comparison = prefix, role = role,
            min_interactions = nmin, player_key = z$player_key, player_name = z$player_name, method = method,
            score = z[[method]], rank = rank(-z[[method]], ties.method = "average"), n_players = nrow(z))
        for (ab in combn(methods, 2, simplify = FALSE)) pairs[[length(pairs) + 1L]] <- data.frame(comparison = prefix,
            role = role, min_interactions = nmin, method_a = ab[1], method_b = ab[2], n_players = nrow(z),
            as.list(baseline_pair(z[[ab[1]]], z[[ab[2]]])))
        h <- if (role == "Blocker")
            z[z$rated_ol, , drop = FALSE]
        else z
        for (method in methods) for (accolade in c("first_team", "any_team")) {
            y <- h[[paste0("all_pro_", accolade)]]
            ok <- !is.na(y)
            if (!sum(ok))
                next
            metrics[[length(metrics) + 1L]] <- data.frame(comparison = prefix, role = role, player_scope = if (role ==
                "Blocker")
                "offensive_line"
            else "all_rushers", min_interactions = nmin, method = method, accolade = accolade, n_players = sum(ok),
                n_positive = sum(y[ok]), auc = baseline_auc(y, h[[method]]), all_pro_among_top10 = sum(baseline_topk(h[[method]][ok]) *
                  y[ok]))
        }
    }
    list(scores = as.data.frame(data.table::rbindlist(rows)), pairs = as.data.frame(data.table::rbindlist(pairs)),
        metrics = as.data.frame(data.table::rbindlist(metrics)))
}

ordinal_logprob <- function (eta, cuts, classes)
{
    m <- length(cuts)
    check(m == length(classes) - 1L && m >= 1L && all(is.finite(cuts)) && all(diff(cuts) >
        0), "Invalid ordinal thresholds")
    a <- matrix(cuts, length(eta), m, byrow = TRUE) - eta
    softplus <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
    out <- matrix(NA_real_, length(eta), m + 1L, dimnames = list(NULL, classes))
    out[, 1L] <- -softplus(-a[, 1L])
    if (m > 1L)
        for (j in seq_len(m - 1L)) {
            out[, j + 1L] <- -softplus(-a[, j + 1L]) - softplus(a[, j]) + log(-expm1(-(cuts[j + 1L] -
                cuts[j])))
        }
    out[, m + 1L] <- -softplus(a[, m])
    out
}

ordinal_unpack <- function (par, nclasses)
{
    m <- nclasses - 1L
    gaps <- if (m > 1L)
        exp(par[2L:m])
    else numeric()
    list(cuts = par[1L] + c(0, cumsum(gaps)), gaps = gaps, beta = par[-seq_len(m)])
}

ordinal_objective <- function (par, x, y, lambda, gradient = FALSE)
{
    u <- ordinal_unpack(par, ncol(y))
    m <- length(u$cuts)
    eta <- as.numeric(x %*% u$beta)
    if (!gradient) {
        return(-sum(y * ordinal_logprob(eta, u$cuts, colnames(y)))/sum(y) + lambda * sum(u$beta^2)/2)
    }
    f <- plogis(matrix(u$cuts, nrow(x), m, byrow = TRUE) - eta)
    h <- 1/expm1(u$gaps)
    cut_gradient <- matrix(0, nrow(x), m)
    for (j in seq_len(m)) {
        cut_gradient[, j] <- -y[, j] * (1 - f[, j] + if (j > 1L)
            h[j - 1L]
        else 0) + y[, j + 1L] * (f[, j] + if (j < m)
            h[j]
        else 0)
    }
    cut_gradient <- cut_gradient/sum(y)
    gc <- colSums(cut_gradient)
    gap_gradient <- if (m > 1L)
        u$gaps * vapply(2L:m, function(j) sum(gc[j:m]), numeric(1))
    else numeric()
    c(sum(gc), gap_gradient, as.numeric(-crossprod(x, rowSums(cut_gradient))) + lambda * u$beta)
}

ordinal_group <- function (d, vocabulary, config)
{
    d <- model_sample(d, "severity")
    check(nrow(d) > 0L, "No eligible ordinal observations")
    signature <- vapply(d$blocker_keys, function(x) {
        as.character(jsonlite::toJSON(sort(x), auto_unbox = FALSE))
    }, character(1))
    z <- data.table::data.table(rusher_key = d$rusher_key, blocker_signature = signature, double_team = d$double_team,
        double_team_unknown = d$double_team_unknown, severity_outcome = d$severity_outcome)
    features <- c("rusher_key", "blocker_signature", "double_team", "double_team_unknown")
    a <- unique(z[, ..features])
    data.table::setorderv(a, features)
    a[, `:=`(group, .I)]
    counts <- z[, .N, by = c(features, "severity_outcome")]
    counts <- merge(counts, a, by = features, sort = FALSE)
    y <- matrix(0, nrow(a), length(config$classes), dimnames = list(NULL, config$classes))
    y[cbind(counts$group, match(counts$severity_outcome, config$classes))] <- counts$N
    a <- as.data.frame(a)
    a$blocker_keys <- I(lapply(a$blocker_signature, function(x) as.character(jsonlite::fromJSON(x))))
    list(x = matchup_matrix(a, vocabulary), y = y, n = sum(y), features = a)
}

ordinal_path <- function (a, lambda, initial = NULL)
{
    check(all(is.finite(lambda) & lambda > 0) && all(diff(lambda) < 0), "Ordinal penalties must be positive and strictly decreasing")
    check(all(colSums(a$y) > 0), "Ordinal fitting sample is missing a category")
    m <- ncol(a$y) - 1L
    cuts <- qlogis(cumsum(colSums(a$y)/a$n)[seq_len(m)])
    par <- if (is.null(initial))
        c(cuts[1L], log(diff(cuts)), rep(0, ncol(a$x)))
    else initial
    lower <- c(-30, rep(-15, m - 1L), rep(-30, ncol(a$x)))
    upper <- c(30, rep(5, m - 1L), rep(30, ncol(a$x)))
    parameters <- vector("list", length(lambda))
    diagnostics <- vector("list", length(lambda))
    for (i in seq_along(lambda)) {
        started <- proc.time()[["elapsed"]]
        opt <- optim(par, ordinal_objective, gr = function(...) ordinal_objective(..., gradient = TRUE),
            x = a$x, y = a$y, lambda = lambda[i], method = "L-BFGS-B", lower = lower, upper = upper,
            control = list(maxit = 3000L, factr = 1e+05, pgtol = 1e-07))
        check(opt$convergence == 0L && is.finite(opt$value), paste("Ordinal numerical failure at penalty",
            lambda[i], ": optimizer did not converge"))
        check(all(opt$par > lower + 0.1 & opt$par < upper - 0.1), paste("Ordinal numerical failure at penalty",
            lambda[i], ": a guard bound is active"))
        par <- opt$par
        max_gradient <- max(abs(ordinal_objective(par, a$x, a$y, lambda[i], TRUE)))
        check(is.finite(max_gradient) && max_gradient < 2e-05, paste("Ordinal numerical failure at penalty",
            lambda[i], ": gradient tolerance not met"))
        parameters[[i]] <- par
        diagnostics[[i]] <- data.frame(lambda = lambda[i], objective = opt$value, max_gradient = max_gradient,
            evaluations = unname(opt$counts[1L]), elapsed_seconds = proc.time()[["elapsed"]] - started)
    }
    list(parameters = parameters, diagnostics = data.table::rbindlist(diagnostics), columns = colnames(a$x),
        lambda = lambda)
}

ordinal_parameters <- function (par, columns, vocabulary, classes)
{
    u <- ordinal_unpack(par, length(classes))
    beta <- setNames(u$beta, columns)
    intercepts <- -u$cuts
    for (role in c("rusher", "blocker")) {
        rows <- startsWith(names(beta), paste0(role, "::"))
        mu <- mean(beta[rows])
        beta[rows] <- beta[rows] - mu
        intercepts <- intercepts + if (role == "rusher")
            mu
        else -mu
    }
    list(model = "ordinal", coefficients = beta, intercepts = intercepts, vocabulary = vocabulary, classes = classes)
}

ordinal_probabilities <- function (eta, intercepts, classes)
{
    p <- exp(ordinal_logprob(eta, -intercepts, classes))
    check(all(is.finite(p)) && all(p >= 0) && max(abs(rowSums(p) - 1)) < 1e-10, "Invalid ordinal probabilities")
    p
}

ordinal_predict <- function (fit, d)
{
    ordinal_probabilities(as.numeric(matchup_matrix(d, fit$vocabulary) %*% fit$coefficients), fit$intercepts,
        fit$classes)
}

ordinal_extend_grid <- function (lambda, selected)
{
    spacing <- median(-diff(log(lambda)))
    n <- max(2L, as.integer(ceiling(log(10)/spacing)))
    if (selected == 1L) {
        extra <- exp(seq(log(max(lambda) * 10), log(max(lambda)), length.out = n + 1L))[-(n + 1L)]
        c(extra, lambda)
    }
    else {
        extra <- exp(seq(log(min(lambda)), log(min(lambda)/10), length.out = n + 1L))[-1L]
        c(lambda, extra)
    }
}

ordinal_fit <- function (d, vocabulary, folds, config, max_expansions = 12L)
{
    d <- model_sample(d, "severity")
    check(identical(config$classes, c("loss", "win", "pressure", "sack")), "Ordinal category order differs from the prespecified four-category order")
    check(!anyDuplicated(folds$game_id), "Duplicate ordinal game-fold assignment")
    foldid <- folds$fold[match(d$game_id, folds$game_id)]
    check(!anyNA(foldid) && identical(sort(unique(foldid)), seq_len(config$nfolds)), "Ordinal folds are missing or incompatible")
    lambda <- config$lambda
    training <- lapply(seq_len(config$nfolds), function(k) ordinal_group(d[foldid != k, ], vocabulary,
        config))
    testing <- lapply(seq_len(config$nfolds), function(k) d[foldid == k, , drop = FALSE])
    counts <- vapply(testing, nrow, integer(1))
    paths <- vector("list", config$nfolds)
    losses <- matrix(numeric(), config$nfolds, 0L)
    expansions <- list()
    attempt <- 0L
    repeat {
        new_lambda <- if (attempt == 0L)
            lambda
        else lambda[!lambda %in% previous_lambda]
        new_losses <- matrix(NA_real_, config$nfolds, length(new_lambda))
        for (k in seq_len(config$nfolds)) {
            message("Ordinal full-season CV fold ", k, "/", config$nfolds, "; ", length(new_lambda),
                " penalties; expansion ", attempt)
            initial <- if (attempt == 0L)
                NULL
            else paths[[k]]$parameters[[if (max(new_lambda) > max(previous_lambda))
                1L
            else length(previous_lambda)]]
            path <- ordinal_path(training[[k]], new_lambda, initial)
            for (i in seq_along(new_lambda)) {
                fit <- ordinal_parameters(path$parameters[[i]], path$columns, vocabulary, config$classes)
                new_losses[k, i] <- log_loss(ordinal_predict(fit, testing[[k]]), testing[[k]], "severity",
                  config)
            }
            if (attempt == 0L)
                paths[[k]] <- path
            else {
                combined <- c(paths[[k]]$lambda, new_lambda)
                order <- order(combined, decreasing = TRUE)
                paths[[k]] <- list(lambda = combined[order], parameters = c(paths[[k]]$parameters, path$parameters)[order],
                  diagnostics = data.table::rbindlist(list(paths[[k]]$diagnostics, path$diagnostics))[order,
                    ], columns = path$columns)
            }
        }
        if (attempt == 0L)
            losses <- new_losses
        else {
            losses <- cbind(losses, new_losses)[, order(c(previous_lambda, new_lambda), decreasing = TRUE),
                drop = FALSE]
        }
        cv <- colSums(losses * counts)/sum(counts)
        selected <- which.min(cv)
        curves <- data.frame(model = "ordinal", fit_scope = "full", lambda = lambda, cv_loss = cv, cv_se = sqrt(colSums(sweep(losses,
            2L, cv, "-")^2 * counts)/sum(counts)/(config$nfolds - 1L)), selected = seq_along(lambda) ==
            selected)

        if (!selected %in% c(1L, length(lambda)))
            break
        check(attempt < max_expansions, "Ordinal penalty optimum remains on a boundary after the expansion limit; no accepted fit")
        previous_lambda <- lambda
        lambda <- ordinal_extend_grid(lambda, selected)
        check(all(is.finite(lambda) & lambda > 0) && all(diff(lambda) < 0), "Ordinal numerical failure while expanding the penalty grid")
        attempt <- attempt + 1L
        expansions[[attempt]] <- data.frame(expansion = attempt, direction = if (selected == 1L)
            "larger"
        else "smaller", lambda_min = min(lambda), lambda_max = max(lambda), n_penalties = length(lambda))
    }
    full <- ordinal_group(d, vocabulary, config)
    message("Ordinal full-season final path: ", length(lambda), " penalties")
    path <- ordinal_path(full, lambda)
    fit <- ordinal_parameters(path$parameters[[selected]], path$columns, vocabulary, config$classes)
    fit$lambda <- lambda[selected]
    fit$boundary <- FALSE
    fit$curves <- curves
    fit$diagnostics <- data.table::rbindlist(c(lapply(seq_along(paths), function(k) {
        cbind(paths[[k]]$diagnostics, fold = k)
    }), list(cbind(path$diagnostics, fold = 0L))))
    fit$fold_losses <- losses
    fit$fold_counts <- counts
    fit$folds <- folds
    fit$grid_expansions <- data.table::rbindlist(expansions)
    fit$n_rows <- nrow(d)
    fit$n_games <- length(unique(d$game_id))
    before <- ordinal_unpack(path$parameters[[selected]], length(config$classes))
    raw <- ordinal_probabilities(as.numeric(full$x %*% before$beta), -before$cuts, config$classes)
    centered <- ordinal_probabilities(as.numeric(full$x %*% fit$coefficients), fit$intercepts, config$classes)
    fit$centering_max_error <- max(abs(raw - centered))
    check(fit$centering_max_error < 1e-10, "Ordinal centering changed predictions")
    fit
}

ordinal_scores <- function (fit, reference, d, config)
{
    check(all(is.finite(config$severity_weights[fit$classes])), "Undefined ordinal EPA scoring weights")
    out <- list()
    cf <- fit$coefficients
    for (role in c("Rusher", "Blocker")) {
        prefix <- tolower(role)
        players <- fit$vocabulary[[prefix]]
        ref <- reference[[role]]
        if (role == "Rusher") {
            base <- vapply(ref$blocker_keys, function(ks) -mean(cf[paste0("blocker::", ks)]), numeric(1))
            own_scale <- rep(1, nrow(ref))
        }
        else {
            co <- vapply(seq_len(nrow(ref)), function(i) {
                keys <- ref$co_blocker_keys[[i]]
                if (!length(keys))
                  return(0)
                sum(cf[paste0("blocker::", keys)])/ref$group_size[i]
            }, numeric(1))
            base <- cf[paste0("rusher::", ref$rusher_key)] - co
            own_scale <- -1/ref$group_size
        }
        base <- base + ref$double_team * cf["double_team"] + ref$double_team_unknown * cf["double_team_unknown"]
        probabilities <- t(vapply(players, function(key) {
            p <- ordinal_probabilities(base + own_scale * cf[paste0(prefix, "::", key)], fit$intercepts,
                fit$classes)
            colSums(p * ref$weight)
        }, setNames(numeric(length(fit$classes)), fit$classes)))
        expected <- as.numeric(probabilities %*% config$severity_weights[fit$classes])
        labels <- as.data.frame(fit$vocabulary$labels)
        labels <- labels[labels$role == role, ]
        labels <- labels[match(players, labels$player_key), ]
        ex <- player_exposure(d, "severity", role, players)
        out[[role]] <- cbind(data.frame(model = "ordinal", role = role, player_key = players, player_id = labels$player_id,
            player_name = labels$player_name, role_interactions = ex$count, role_games = ex$games, effective_credit_exposure = ex$mass,
            score_type = "expected_severity", expected_outcome = expected, score = if (role == "Rusher")
                expected
            else -expected, latent_effect = unname(cf[paste0(prefix, "::", players)])), setNames(as.data.frame(probabilities),
            paste0("probability_", fit$classes)))
    }
    data.table::rbindlist(out)
}

ordinal_rank_comparison <- function (ordinal, multinomial)
{
    multinomial <- multinomial[multinomial$score_type == "expected_severity", ]
    cols <- c("role", "player_key", "player_name", "role_interactions", "role_games", "score")
    pairs <- merge(as.data.frame(ordinal)[, cols], as.data.frame(multinomial)[, c("role", "player_key",
        "score")], by = c("role", "player_key"), suffixes = c("_ordinal", "_multinomial"), sort = TRUE)
    check(nrow(pairs) == nrow(ordinal), "Ordinal and multinomial player populations differ")
    ranks <- summaries <- list()
    for (role in c("Rusher", "Blocker")) for (minimum in c(0L, 100L, 200L, 400L)) {
        z <- pairs[pairs$role == role & pairs$role_interactions >= minimum & pairs$role_interactions >
            0L, ]
        z$min_interactions <- rep(minimum, nrow(z))
        z$rank_ordinal <- rank(-z$score_ordinal, ties.method = "average")
        z$rank_multinomial <- rank(-z$score_multinomial, ties.method = "average")
        z$rank_change <- z$rank_multinomial - z$rank_ordinal
        ranks[[length(ranks) + 1L]] <- z
        summaries[[length(summaries) + 1L]] <- data.frame(role = role, min_interactions = minimum, n_players = nrow(z),
            spearman = if (nrow(z) > 1L)
                suppressWarnings(cor(z$score_ordinal, z$score_multinomial, method = "spearman"))
            else NA_real_, median_absolute_rank_change = if (nrow(z))
                median(abs(z$rank_change))
            else NA_real_, maximum_absolute_rank_change = if (nrow(z))
                max(abs(z$rank_change))
            else NA_real_)
    }
    list(players = data.table::rbindlist(ranks), agreement = data.table::rbindlist(summaries))
}
baseline_analysis <- function(s) {
    d <- as.data.frame(s$data)
    cfg <- s$config
    folds <- as.data.frame(s$folds)
    check(!length(intersect(d$game_id[d$week <= 15], d$game_id[d$week >= 16])), "Training/test game overlap")
    train <- d[d$week <= 15, , drop = FALSE]
    test <- d[d$week >= 16, , drop = FALSE]
    cv <- holdout <- allprofiles <- audit <- model_audit <- list()
    for (model in c("win", "severity")) {
        cv[[model]] <- baseline_cv(train, model, folds, cfg)
        model_audit[[model]] <- baseline_model_cv(train, model, folds, cfg, s$vocabulary, s$training_fits[[model]])
        cv[[model]]$summary$bt_cv_loss <- model_audit[[model]]$cv_loss
        cv[[model]]$summary$bt_original_glmnet_cv_loss <- model_audit[[model]]$saved_cv_loss
        cv[[model]]$summary$bt_reconstructed_glmnet_cv_loss <- model_audit[[model]]$reconstructed_glmnet_cv_loss
        cv[[model]]$summary$bt_clipped_probability_count <- model_audit[[model]]$clipped_probability_count
        cv[[model]]$summary$is_original_strength <- cv[[model]]$summary$strength == cfg$baseline_strength[[model]]
        cv[[model]]$summary$baseline_beats_bt_cv <- cv[[model]]$summary$cv_loss < cv[[model]]$summary$bt_cv_loss
        te <- model_sample(test, model)
        a <- outcome_profiles(train, model, cfg, 0)
        mloss <- log_loss(predict_model(s$training_fits[[model]], te), te, model, cfg)
        for (method in c("training_cv_selected", "original_fixed")) {
            m <- if (method == "training_cv_selected")
                cv[[model]]$selected
            else cfg$baseline_strength[[model]]
            loss <- log_loss(baseline_predict(a, te, model, m), te, model, cfg)
            holdout[[length(holdout) + 1L]] <- data.frame(model = model, baseline = method, strength = m,
                strength_label = baseline_label(m), baseline_loss = loss, bt_loss = mloss, bt_absolute_improvement = loss -
                  mloss, bt_percent_improvement = 100 * (loss - mloss)/loss, baseline_beats_bt = loss <
                  mloss, test_rows = nrow(te), test_games = length(unique(te$game_id)), interval_status = "point_estimate_only")
        }
        a <- outcome_profiles(d, model, cfg, 0)
        allprofiles[[model]] <- as.data.frame(data.table::rbindlist(lapply(baseline_grid(), function(m) baseline_profile_scores(d,
            model, s$vocabulary, cfg, m, a))))
        audit[[model]] <- cv[[model]]$profiles
    }
    summary <- as.data.frame(data.table::rbindlist(lapply(cv, `[[`, "summary")))
    folds_out <- as.data.frame(data.table::rbindlist(lapply(cv, `[[`, "folds")))
    holdout <- as.data.frame(data.table::rbindlist(holdout))
    profiles <- as.data.frame(data.table::rbindlist(allprofiles))
    point <- s$point_scores
    if (is.null(point))
        point <- as.data.frame(data.table::rbindlist(lapply(s$full_fits, function(f) player_scores(f,
            s$reference[[f$model]], d, d, cfg))))
    point <- as.data.frame(point)
    players <- baseline_literal(d, s$vocabulary)
    for (model in c("win", "severity")) {
        z <- profiles[profiles$model == model & profiles$strength == 0, ]
        idx <- match(paste(players$role, players$player_key), paste(z$role, z$player_key))
        players[[paste0(model, "_interactions")]] <- z$interactions[idx]
        players[[paste0(model, "_games")]] <- z$games[idx]
        players[[paste0(model, "_allocated_mass")]] <- z$allocated_mass[idx]
        for (m in c(0, cfg$baseline_strength[[model]], cv[[model]]$selected)) {
            z <- profiles[profiles$model == model & profiles$strength == m, ]
            idx <- match(paste(players$role, players$player_key), paste(z$role, z$player_key))
            nm <- paste0("allocated_", if (m == 0)
                "raw_"
            else paste0("m", baseline_label(m), "_"), model)
            players[[nm]] <- z$score[idx]
        }
        z <- point[point$model == model & point$score_type == if (model == "win")
            "standardized_probability"
        else "expected_severity", ]
        check(!anyDuplicated(paste(z$role, z$player_key)), "Duplicated point player score")
        players[[paste0("bt_", model)]] <- z$score[match(paste(players$role, players$player_key), paste(z$role,
            z$player_key))]
    }
    players <- baseline_honors(players, d)
    views <- list(baseline_rank_views(players, c("literal_raw_win", "allocated_raw_win", paste0("allocated_m",
        cfg$baseline_strength[["win"]], "_win"), "bt_win"), "win_interactions", "binary_methods"), baseline_rank_views(players,
        c("literal_raw_win", "bt_win", "bt_severity"), c("win_interactions", "severity_interactions"),
        "binary_and_severity"))
    stability <- list()
    for (model in c("win", "severity")) for (role in c("Rusher", "Blocker")) for (nmin in c(0, 100, 200,
        400)) {
        z <- profiles[profiles$model == model & profiles$role == role & profiles$interactions > 0 & profiles$interactions >=
            nmin, ]
        original <- z[z$strength == cfg$baseline_strength[[model]], ]
        if (!nrow(original))
            next
        bt <- players[[paste0("bt_", model)]][match(paste(original$role, original$player_key), paste(players$role,
            players$player_key))]
        for (m in baseline_grid()) {
            a <- z[z$strength == m, ]
            a <- a[match(original$player_key, a$player_key), ]
            for (comparator in c("original_shrunken", "bt")) stability[[length(stability) + 1L]] <- data.frame(model = model,
                role = role, min_interactions = nmin, strength = m, strength_label = baseline_label(m),
                comparator = comparator, n_players = nrow(a), as.list(baseline_pair(a$score, if (comparator ==
                  "bt")
                  bt
                else original$score)))
        }
    }
    outputs <- list(smoothing_cv_folds = folds_out, smoothing_cv_summary = summary, smoothing_bt_cv_audit = as.data.frame(data.table::rbindlist(lapply(model_audit,
        `[[`, "folds"))), smoothing_holdout = holdout, smoothing_player_scores = profiles, smoothing_ranking_stability = as.data.frame(data.table::rbindlist(stability)),
        raw_bt_severity_players = players, raw_bt_severity_ranks = as.data.frame(data.table::rbindlist(lapply(views,
            `[[`, "scores"))), raw_bt_severity_rank_agreement = as.data.frame(data.table::rbindlist(lapply(views,
            `[[`, "pairs"))), raw_bt_severity_all_pro = as.data.frame(data.table::rbindlist(lapply(views,
            `[[`, "metrics"))))
    outputs
}
