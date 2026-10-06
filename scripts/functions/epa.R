# Conditional EPA associations use complete plays and training games only.

epa_require <- function (ok, message)
if (!isTRUE(ok)) stop(message, call. = FALSE)

epa_bool <- function (x, label)
{
    x <- as.character(x)
    epa_require(all(x %in% c("0", "1", "TRUE", "FALSE", "True", "False", "true", "false")), paste("Invalid",
        label))
    x %in% c("1", "TRUE", "True", "true")
}

epa_config <- function(seed = 20260916L, expected_train_games = 219L, expected_test_games = 47L,
                       bootstrap_iterations = 1000L) {
  list(seed = as.integer(seed), train_last_week = 15L, test_weeks = 16:18,
       expected_train_games = as.integer(expected_train_games), expected_test_games = as.integer(expected_test_games),
       bootstrap_iterations = as.integer(bootstrap_iterations),
       primary_terms = c("N_W", "N_P", "N_S", "B", "E"),
       quarterback_unknown = "__UNKNOWN_QB__", rank_tolerance = 1e-10, estimability_tolerance = 1e-7)
}

epa_read <- function (input, pool_path, cfg)
{
    chars <- c(game_id = "character", play_id = "character", nflfast_game_id = "character", quarterback_id = "character",
        defteam = "character", game_half = "character")
    d <- read.csv(input, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"), colClasses = chars)
    required <- c(names(chars), "week", "split", "eligible_primary", "exclusion_reason", "epa", "early_protection_complete",
        "early_protection_participants", "early_protection_ungraded_actors", "N_W", "N_P", "N_H", "N_S",
        "B", "U", "E", "P_B", "H_B", "S_B", "P_A", "H_A", "S_A", "K_B", "accepted_penalty", "down", "ydstogo",
        "yardline_100", "half_seconds_remaining", "score_differential", "qtr")
    epa_require(all(required %in% names(d)), paste("Missing calibration columns:", paste(setdiff(required,
        names(d)), collapse = ", ")))
    epa_require(!anyNA(d[c("game_id", "play_id", "week", "split")]), "Missing play identity or partition")
    epa_require(!anyDuplicated(paste(d$game_id, d$play_id)), "Duplicate EPA play rows")
    epa_require(all(d$week %in% 1:18), "Only regular-season weeks 1--18 are supported")
    epa_require(all(d$split == ifelse(d$week <= 15, "train", "test")), "Chronological partition mismatch")
    d$eligible_primary <- epa_bool(d$eligible_primary, "eligible_primary")
    d$early_protection_complete <- epa_bool(d$early_protection_complete, "early_protection_complete")
    completeness_counts <- d[c("early_protection_participants", "early_protection_ungraded_actors")]
    epa_require(all(vapply(completeness_counts, function(x) is.numeric(x) && all(is.finite(x) & x >=
        0 & x == round(x)), logical(1))), "Early-protection completeness counts must be nonnegative integers")
    epa_require(all(d$early_protection_ungraded_actors <= d$early_protection_participants) && all(d$early_protection_complete ==
        (d$early_protection_ungraded_actors == 0)), "Early-protection completeness flag and participant counts disagree")
    epa_require(all(d$early_protection_complete[d$eligible_primary]), "Eligible EPA plays require an early grade for every observed protection actor")
    penalty <- as.character(d$accepted_penalty)
    d$accepted_penalty <- rep(NA, nrow(d))
    valid_penalty <- penalty %in% c("0", "1", "TRUE", "FALSE", "True", "False", "true", "false")
    d$accepted_penalty[valid_penalty] <- epa_bool(penalty[valid_penalty], "accepted_penalty")
    d$accepted_penalty[d$eligible_primary] <- epa_bool(penalty[d$eligible_primary], "accepted_penalty for eligible plays")
    epa_require(all(!is.na(d$exclusion_reason[!d$eligible_primary]) & nzchar(d$exclusion_reason[!d$eligible_primary])),
        "Excluded plays need explicit reasons")
    pool <- read.csv(pool_path, stringsAsFactors = FALSE, colClasses = c(game_id = "character"))
    epa_require(all(c("game_id", "week") %in% names(pool)) && !anyNA(pool[c("game_id", "week")]), "Game pool needs game_id and week")
    epa_require(!anyDuplicated(pool$game_id) && all(pool$week %in% 1:18), "Invalid original-game pool")
    pool$split <- ifelse(pool$week <= 15, "train", "test")
    epa_require(sum(pool$split == "train") == cfg$expected_train_games && sum(pool$split == "test") ==
        cfg$expected_test_games, "Fixed game-pool sizes changed")
    index <- match(d$game_id, pool$game_id)
    epa_require(!anyNA(index) && all(d$week == pool$week[index]), "Calibration play absent from or conflicts with game pool")
    z <- d[d$eligible_primary, , drop = FALSE]
    numeric <- c("epa", "N_W", "N_P", "N_H", "N_S", "B", "U", "E", "P_B", "H_B", "S_B", "P_A", "H_A",
        "S_A", "K_B", "down", "ydstogo", "yardline_100", "half_seconds_remaining", "score_differential",
        "qtr")
    epa_require(all(vapply(z[numeric], function(x) is.numeric(x) && all(is.finite(x)), logical(1))),
        "Eligible plays have missing/nonfinite numeric context or counts; exclusions must be explicit upstream")
    epa_require(all(as.matrix(z[c("N_H", "H_B", "H_A")]) == 0), "Hit counts must be zero in strict four-category calibration")
    epa_require(all(z$U == 0), "Eligible complete EPA plays must have U = 0; unresolved counts remain audit-only")
    epa_require(all(z$B == z$early_protection_participants), "EPA protection count differs from early-grade participant accounting")
    epa_require(all(z$down %in% 1:4) && all(z$qtr >= 1 & z$qtr == as.integer(z$qtr)) && all(z$ydstogo >=
        0), "Invalid presnap context")
    epa_require(all(!is.na(z$defteam) & nzchar(z$defteam)), "Eligible plays need defense identity")
    counts <- c("N_W", "N_P", "N_H", "N_S", "B", "U", "E", "P_B", "H_B", "S_B", "P_A", "H_A", "S_A",
        "K_B")
    epa_require(all(as.matrix(z[counts]) >= -1e-09), "Negative class counts")
    integer_counts <- c("N_W", "N_P", "N_H", "B", "U", "P_B", "H_B", "P_A", "H_A", "K_B")
    epa_require(max(abs(as.matrix(z[integer_counts]) - round(as.matrix(z[integer_counts])))) < 1e-08,
        "Final-category counts must count distinct people")
    epa_require(all(abs(z$N_S) < 1e-08 | abs(z$N_S - 1) < 1e-08), "Shared-sack credits must total zero or one per play")
    epa_require(max(abs(z$N_P - z$P_B - z$P_A)) < 1e-08 && max(abs(z$N_H - z$H_B - z$H_A)) < 1e-08 &&
        max(abs(z$N_S - z$S_B - z$S_A)) < 1e-08 && max(abs(z$E - z$K_B + z$S_B)) < 1e-08, "Class/source/shared-credit accounting mismatch")
    loss <- z$B - z$N_W - z$P_B - z$H_B - z$K_B - z$U
    epa_require(all(loss >= -1e-08), "Protected final categories exceed observed participants")
    epa_require(all(c("train", "test") %in% z$split), "Empty eligible training or test sample")
    d <- d[order(d$week, d$game_id, d$play_id), ]
    rownames(d) <- NULL
    pool <- pool[order(pool$week, pool$game_id), ]
    rownames(pool) <- NULL
    list(data = d, pool = pool)
}

epa_qb <- function (d, cfg)
{
    q <- as.character(d$quarterback_id)
    q[is.na(q) | !nzchar(q)] <- cfg$quarterback_unknown
    q
}

epa_basis <- function (train, cfg)
{
    num <- cbind(log_ydstogo = log1p(train$ydstogo), yardline_100 = train$yardline_100, half_seconds_remaining = train$half_seconds_remaining,
        score_differential = train$score_differential)
    spread <- apply(num, 2, sd)
    spread[!is.finite(spread) | spread == 0] <- 1
    list(schema_version = 1L, centers = colMeans(num), scales = spread, qb_levels = sort(unique(c(epa_qb(train,
        cfg), cfg$quarterback_unknown))), defense_levels = sort(unique(train$defteam)), down_levels = 1:4,
        half_rule = "qtr 1--2 first half; 3--4 second half; qtr>=5 overtime; game_half retained for source audit",
        terms = cfg$primary_terms)
}

epa_matrix <- function (d, basis, cfg, kind = "primary")
{
    counts <- as.matrix(d[cfg$primary_terms])
    storage.mode(counts) <- "double"
    if (kind == "protected") {
        counts[, "N_P"] <- d$P_B
        counts[, "N_S"] <- d$S_B
        counts <- cbind(counts, P_A = d$P_A, S_A = d$S_A)
    }
    num <- cbind(log_ydstogo = log1p(d$ydstogo), yardline_100 = d$yardline_100, half_seconds_remaining = d$half_seconds_remaining,
        score_differential = d$score_differential)
    num <- sweep(sweep(num, 2, basis$centers, "-"), 2, basis$scales, "/")
    context <- cbind(num, down_2 = as.integer(d$down == 2), down_3 = as.integer(d$down == 3), down_4 = as.integer(d$down ==
        4), half_2 = as.integer(d$qtr %in% 3:4), overtime = as.integer(d$qtr >= 5))
    hot <- function(value, levels, prefix) {
        m <- matrix(0, nrow(d), length(levels), dimnames = list(NULL, paste0(prefix, levels)))
        j <- match(value, levels)
        ok <- !is.na(j)
        m[cbind(which(ok), j[ok])] <- 1
        m
    }
    x <- cbind(`(Intercept)` = rep(1, nrow(d)), if (kind != "context")
        counts, context, hot(epa_qb(d, cfg), basis$qb_levels, "QB::"), hot(d$defteam, basis$defense_levels,
        "DEF::"))
    storage.mode(x) <- "double"
    x
}

epa_fit <- function (x, y, weight = rep(1, length(y)), cfg = epa_config())
{
    epa_require(length(weight) == nrow(x) && all(is.finite(weight) & weight >= 0) && sum(weight) > 0,
        "Invalid or empty sampled fitting weights")
    keep <- weight > 0
    x <- x[keep, , drop = FALSE]
    y <- y[keep]
    weight <- weight[keep]
    groups <- lapply(c(QB = "QB::", DEF = "DEF::"), function(prefix) which(startsWith(colnames(x), prefix)))
    absent <- references <- integer()
    represented <- list()
    for (group in names(groups)) {
        ix <- groups[[group]]
        mass <- colSums(x[, ix, drop = FALSE] * weight)
        represented[[group]] <- ix[mass > 0]
        absent <- c(absent, ix[mass == 0])
        epa_require(any(mass > 0), "No represented fixed-effect level")
        references <- c(references, ix[which.max(mass)])
    }
    reduced <- setdiff(seq_len(ncol(x)), c(absent, references))
    fit <- lm.wfit(x[, reduced, drop = FALSE], y, w = weight, tol = cfg$rank_tolerance)
    cf <- setNames(rep(0, ncol(x)), colnames(x))
    values <- fit$coefficients
    values[is.na(values)] <- 0
    cf[reduced] <- values
    estimable <- setNames(rep(TRUE, ncol(x)), colnames(x))
    estimable[absent] <- FALSE
    full_null <- matrix(0, ncol(x), 0L)
    alias <- character()
    condition <- NA_real_
    if (fit$rank < length(reduced)) {
        p <- length(reduced)
        r <- fit$rank
        qr_r <- qr.R(fit$qr)
        null <- rbind(-backsolve(qr_r[seq_len(r), seq_len(r), drop = FALSE], qr_r[seq_len(r), (r + 1L):p,
            drop = FALSE]), diag(p - r))
        full_null <- matrix(0, ncol(x), p - r)
        full_null[reduced[fit$qr$pivot], ] <- null
        alias <- colnames(x)[reduced[fit$qr$pivot[(r + 1L):p]]]
    }
    if (fit$rank > 0)
        condition <- kappa(qr.R(fit$qr)[seq_len(fit$rank), seq_len(fit$rank), drop = FALSE], exact = FALSE)
    before <- as.numeric(x %*% cf)
    for (group in names(groups)) {
        ix <- represented[[group]]
        shift <- mean(cf[ix])
        cf[ix] <- cf[ix] - shift
        cf["(Intercept)"] <- cf["(Intercept)"] + shift
        if (ncol(full_null)) {
            null_shift <- colMeans(full_null[ix, , drop = FALSE])
            full_null[ix, ] <- sweep(full_null[ix, , drop = FALSE], 2L, null_shift, "-")
            full_null[1L, ] <- full_null[1L, ] + null_shift
        }
    }
    if (ncol(full_null))
        estimable[sqrt(rowSums(full_null^2)) > cfg$estimability_tolerance] <- FALSE
    error <- max(abs(before - as.numeric(x %*% cf)))
    epa_require(error < 1e-08, "Fixed-effect centering changed learned predictions")
    rownames(full_null) <- colnames(x)
    list(coefficients = cf, estimable = estimable, null_directions = full_null, rank = fit$rank, columns = ncol(x),
        reduced_columns = length(reduced), aliased = alias, absent_levels = colnames(x)[absent], represented_levels = lapply(represented,
            function(ix) colnames(x)[ix]), references = colnames(x)[references], condition_estimate = condition,
        centering_max_error = error, rows = nrow(x), weighted_rows = sum(weight), residual_df = nrow(x) -
            fit$rank)
}

epa_predict <- function (fit, x)
{
    epa_require(identical(names(fit$coefficients), colnames(x)), "Prediction design differs from training design")
    as.numeric(x %*% fit$coefficients)
}

epa_weights <- function (fit)
{
    cols <- c(win = "N_W", pressure = "N_P", sack = "N_S")
    b <- fit$coefficients[cols]
    names(b) <- names(cols)
    ok <- fit$estimable[cols]
    b[!ok] <- NA_real_
    denominator <- unname(b["sack"])
    defined <- is.finite(denominator) && denominator != 0
    w <- if (defined)
        b/denominator
    else b * NA_real_
    list(beta = b, raw = c(loss = 0, -b), normalized = c(loss = 0, w), denominator = denominator, denominator_negative = is.finite(denominator) &&
        denominator < 0, defined = defined && all(is.finite(w)), estimable = setNames(as.logical(ok),
        names(cols)))
}

epa_metrics <- function (primary, baseline, y, weight)
{
    epa_require(sum(weight) > 0, "Sampled holdout has no eligible EPA plays")
    measure <- function(p) c(MSE = weighted.mean((y - p)^2, weight), MAE = weighted.mean(abs(y - p),
        weight))
    a <- measure(primary)
    b <- measure(baseline)
    data.frame(metric = names(a), model_loss = unname(a), baseline_loss = unname(b), improvement = unname(b -
        a), percent_improvement = unname(ifelse(b > 0, 100 * (b - a)/b, NA_real_)), weighted_test_plays = sum(weight),
        unique_test_plays = sum(weight > 0))
}

epa_coefficient_uncertainty <- function (fits, point)
{
    terms <- names(point$coefficients)
    n <- length(fits)
    epa_require(n >= 2L && all(vapply(fits, function(f) identical(names(f$coefficients), terms), logical(1))),
        "Incompatible full coefficient vectors")
    values <- do.call(rbind, lapply(fits, function(f) f$coefficients))
    estimable <- do.call(rbind, lapply(fits, function(f) f$estimable))
    absent <- do.call(rbind, lapply(fits, function(f) terms %in% f$absent_levels))
    finite <- is.finite(values)
    summary <- do.call(rbind, lapply(seq_along(terms), function(j) {
        defined <- all(finite[, j])
        identified <- defined && all(estimable[, j]) && isTRUE(point$estimable[j])
        q <- if (defined)
            quantile(values[, j], c(0.025, 0.5, 0.975))
        else rep(NA_real_, 3)
        data.frame(term = terms[j], point = point$coefficients[j], point_estimable = point$estimable[j],
            convention_mean = if (defined)
                mean(values[, j])
            else NA_real_, convention_q025 = q[1], convention_median = q[2], convention_q975 = q[3],
            identified_q025 = if (identified)
                q[1]
            else NA_real_, identified_q975 = if (identified)
                q[3]
            else NA_real_, draws = n, defined_fraction = mean(finite[, j]), estimable_fraction = mean(estimable[,
                j]), nonestimable_fraction = mean(!estimable[, j]), absent_level_fraction = mean(absent[,
                j]), identified_interval_defined = identified)
    }))
    covariance <- if (all(finite))
        cov(values)
    else matrix(NA_real_, length(terms), length(terms), dimnames = list(terms, terms))
    list(values = values, estimable = estimable, absent = absent, summary = summary, covariance = covariance)
}

read_epa_data <- function (input, pool_path, cfg = epa_config(), basis = NULL)
{
    parsed <- epa_read(input, pool_path, cfg)
    train <- parsed$data[parsed$data$eligible_primary & parsed$data$split == "train", ]
    if (is.null(basis)) basis <- epa_basis(train, cfg)
    list(config = cfg, data = parsed$data, train = train, pool = parsed$pool, basis = basis, x = epa_matrix(train,
        basis, cfg))
}

fit_epa_weights <- function (s, game_draws = NULL)
{
    w <- if (is.null(game_draws))
        rep(1, nrow(s$train))
    else as.numeric(table(factor(game_draws, levels = s$pool$game_id)))[match(s$train$game_id, s$pool$game_id)]
    f <- epa_fit(s$x, s$train$epa, w, s$config)
    list(fit = f, weights = epa_weights(f), training_game_multiplicities = data.frame(game_id = s$pool$game_id[s$pool$week <=
        15], multiplicity = if (is.null(game_draws)) 1L else as.integer(table(factor(game_draws, levels = s$pool$game_id[s$pool$week <=
        15])))))
}
