# Compare rankings within common exposure cohorts, with fractional credit for ties.

scoped_rank_intervals <- function(draws, ranked, iterations) {
  point <- ranked[ranked$method == "bt" & ranked$comparison_eligible, ]
  group_apply(point, c("model", "score_type", "role", "min_interactions"), function(cohort) {
    z <- copy(draws[model == cohort$model[1] & score_type == cohort$score_type[1] &
                     role == cohort$role[1] & player_key %in% cohort$player_key])
    check(nrow(z) == nrow(cohort) * iterations, "Incomplete bootstrap rank cohort")
    z[, scoped_rank := rank(-score, ties.method = "average", na.last = "keep"), by = replicate_id]
    z[, top_credit := if (all(is.finite(score))) topk_credit(score) else rep(NA_real_, .N), by = replicate_id]
    summary <- z[, .(rank_q025 = if (all(is.finite(scoped_rank))) quantile(scoped_rank, .025) else NA_real_,
      rank_q975 = if (all(is.finite(scoped_rank))) quantile(scoped_rank, .975) else NA_real_,
      top10_probability = mean(top_credit)), by = player_key]
    merge(cohort[c("model", "score_type", "role", "player_key", "player_name", "min_interactions", "comparison_rank")],
          summary, by = "player_key", sort = FALSE)
  })
}

require_columns <- function (x, cols, name = "input")
{
    missing <- setdiff(cols, names(x))
    if (length(missing))
        stop(name, " is missing: ", paste(missing, collapse = ", "))
}

name_key <- function (x)
gsub(" +", " ", gsub("[^a-z0-9 ]", "", tolower(trimws(x))))

group_apply <- function (x, cols, fn)
{
    if (!nrow(x))
        return(data.frame())
    groups <- split(seq_len(nrow(x)), interaction(x[cols], drop = TRUE, lex.order = TRUE))
    out <- lapply(groups, function(i) fn(x[i, , drop = FALSE]))
    ans <- do.call(rbind, out)
    rownames(ans) <- NULL
    ans
}

topk_credit <- function (scores, k = 10L)
{
    ans <- rep(NA_real_, length(scores))
    ok <- is.finite(scores)
    if (!any(ok))
        return(ans)
    s <- scores[ok]
    k <- min(max(as.integer(k), 0L), length(s))
    first <- rank(-s, ties.method = "min")
    last <- rank(-s, ties.method = "max")
    ans[ok] <- pmin(1, pmax(0, (k - first + 1)/(last - first + 1)))
    ans
}

rank_auc <- function (labels, scores)
{
    ok <- !is.na(labels) & is.finite(scores)
    y <- as.integer(labels[ok])
    s <- scores[ok]
    if (!all(y %in% 0:1))
        stop("AUC labels must be binary.")
    np <- sum(y == 1L)
    nn <- sum(y == 0L)
    if (!np || !nn)
        return(NA_real_)
    (sum(rank(s, ties.method = "average")[y == 1L]) - np * (np + 1)/2)/(np * nn)
}

all_pro_reference <- function ()
{
    name <- c("Trent Williams", "Tristan Wirfs", "Joel Bitonio", "Zack Martin", "Jason Kelce", "T.J. Watt",
        "Myles Garrett", "Aaron Donald", "Cameron Heyward", "Micah Parsons", "Darius Leonard", "De'Vondre Campbell",
        "Rashawn Slater", "Lane Johnson", "Quenton Nelson", "Wyatt Teller", "Corey Linsley", "Robert Quinn",
        "Maxx Crosby", "Chris Jones", "Jeffery Simmons", "Demario Davis", "Roquan Smith", "Bobby Wagner")
    data.frame(player_name_norm = name_key(name), all_pro_player_name = name, role = c(rep("Blocker",
        5), rep("Rusher", 7), rep("Blocker", 5), rep("Rusher", 7)), is_all_pro_first_team = seq_along(name) <=
        12L, is_all_pro_any_team = TRUE, stringsAsFactors = FALSE)
}

add_honors <- function (x)
{
    require_columns(x, c("player_key", "player_name", "role"))
    x$player_name_norm <- name_key(x$player_name)
    ref <- all_pro_reference()
    idx <- match(paste(x$role, x$player_name_norm), paste(ref$role, ref$player_name_norm))
    identity <- unique(x[c("role", "player_key", "player_name_norm")])
    display <- paste(identity$role, identity$player_name_norm)
    ambiguous <- unique(display[duplicated(display) | duplicated(display, fromLast = TRUE)])
    x$ambiguous_display_name <- paste(x$role, x$player_name_norm) %in% ambiguous
    x$all_pro_match_status <- ifelse(is.na(idx), "unmatched", ifelse(x$ambiguous_display_name, "ambiguous_all_pro_name",
        "matched"))
    x$is_all_pro_first_team <- !is.na(idx) & ref$is_all_pro_first_team[idx]
    x$is_all_pro_any_team <- !is.na(idx)
    bad <- x$all_pro_match_status == "ambiguous_all_pro_name"
    x$is_all_pro_first_team[bad] <- NA
    x$is_all_pro_any_team[bad] <- NA
    x
}

bind_rows <- function (a, b)
{
    cols <- union(names(a), names(b))
    for (n in setdiff(cols, names(a))) a[[n]] <- NA
    for (n in setdiff(cols, names(b))) b[[n]] <- NA
    rbind(a[cols], b[cols])
}

rank_cohorts <- function (scores, thresholds = c(0L, 100L, 200L, 400L))
{
    require_columns(scores, c("model", "score_type", "role", "player_key", "player_name", "score",
        "role_interactions", "method"))
    scores$comparison_id <- paste(scores$method, scores$score_type, sep = ":")
    key <- paste(scores$model, scores$comparison_id, scores$role, scores$player_key, sep = "\034")
    if (anyDuplicated(key))
        stop("Duplicate player scores within a method/model/role.")
    group_apply(scores, c("model", "role"), function(x) {
        methods <- unique(x$comparison_id)
        players <- sort(unique(x$player_key))
        exposures <- vapply(players, function(p) {
            z <- x[x$player_key == p, ]
            if (length(unique(z$role_interactions)) != 1L)
                stop("Inconsistent exposure for ", p, " in ", x$model[1])
            z$role_interactions[1]
        }, numeric(1))
        complete <- vapply(players, function(p) {
            z <- x[x$player_key == p, ]
            setequal(z$comparison_id, methods) && all(is.finite(z$score))
        }, logical(1))
        do.call(rbind, lapply(thresholds, function(min_n) {
            eligible <- players[exposures >= min_n]
            common <- players[exposures >= min_n & complete]
            z <- x[x$player_key %in% eligible, ]
            if (!nrow(z))
                return(NULL)
            z$min_interactions <- min_n
            z$comparison_eligible <- z$player_key %in% common
            z$cohort_n_observed <- length(eligible)
            z$cohort_n_comparable <- length(common)
            if (!nrow(z))
                return(NULL)
            group_apply(z, "comparison_id", function(a) {
                a$rank_by_role <- rank(-a$score, ties.method = "average", na.last = "keep")
                a$comparison_rank <- NA_real_
                ok <- a$comparison_eligible
                a$comparison_rank[ok] <- rank(-a$score[ok], ties.method = "average")
                a$top10_credit <- topk_credit(a$score)
                a
            })
        }))
    })
}

honors_metrics <- function (ranked)
{
    x <- ranked[ranked$comparison_eligible, ]
    group_apply(x, c("model", "role", "min_interactions", "comparison_id"), function(z) {
        do.call(rbind, lapply(c("first_team", "any_team"), function(accolade) {
            y <- z[[paste0("is_all_pro_", accolade)]]
            labeled <- !is.na(y)
            data.frame(model = z$model[1], role = z$role[1], min_interactions = z$min_interactions[1],
                comparison_id = z$comparison_id[1], accolade = accolade, n_players = nrow(z), n_positive = sum(y,
                  na.rm = TRUE), n_unresolved_labels = sum(!labeled), k = min(10L, sum(labeled)), all_pro_among_top10 = sum(topk_credit(z$score[labeled]) *
                  y[labeled]), auc = rank_auc(y, z$score), stringsAsFactors = FALSE)
        }))
    })
}

rank_pairs <- function (ranked)
{
    x <- ranked[ranked$comparison_eligible, ]
    group_apply(x, c("model", "role", "min_interactions"), function(z) {
        methods <- sort(unique(z$comparison_id))
        if (length(methods) < 2L)
            return(NULL)
        pairs <- combn(methods, 2L, simplify = FALSE)
        do.call(rbind, lapply(pairs, function(pair) {
            a <- z[z$comparison_id == pair[1], ]
            b <- z[z$comparison_id == pair[2], ]
            b <- b[match(a$player_key, b$player_key), ]
            data.frame(model = a$model, role = a$role, min_interactions = a$min_interactions, method_a = pair[1],
                method_b = pair[2], player_key = a$player_key, player_id = a$player_id, player_name = a$player_name,
                role_interactions = a$role_interactions, role_games = a$role_games, score_a = a$score,
                score_b = b$score, rank_a = a$comparison_rank, rank_b = b$comparison_rank, rank_improvement_b = a$comparison_rank -
                  b$comparison_rank, stringsAsFactors = FALSE)
        }))
    })
}

rank_correlations <- function (pairs)
{
    group_apply(pairs, c("model", "role", "min_interactions", "method_a", "method_b"), function(z) {
        constant <- length(unique(z$rank_a)) < 2L || length(unique(z$rank_b)) < 2L
        data.frame(z[1, c("model", "role", "min_interactions", "method_a", "method_b")], n_players = nrow(z),
            spearman = if (constant)
                NA_real_
            else cor(z$rank_a, z$rank_b), mean_absolute_rank_change = mean(abs(z$rank_improvement_b)),
            stringsAsFactors = FALSE)
    })
}

paired_rank_uncertainty <- function (pairs, draws, expected = 1000L)
{
    require_columns(pairs, c("model", "role", "min_interactions", "method_a", "method_b", "player_key",
        "rank_a", "rank_b"))
    require_columns(draws, c("model", "score_type", "role", "player_key", "replicate_id", "score",
        "present_in_draw"))
    if (length(expected) != 1L || !is.finite(expected) || expected < 1L || expected != as.integer(expected))
        stop("Invalid paired-draw count.")
    point <- pairs[pairs$model == "severity" & pairs$method_a == "bt:coefficient" & pairs$method_b ==
        "bt:expected_severity", ]
    if (!nrow(point))
        return(data.frame())
    draws <- draws[draws$model == "severity" & draws$score_type %in% c("coefficient", "expected_severity"),
        ]
    if (anyNA(draws$replicate_id) || !setequal(unique(draws$replicate_id), seq_len(expected)))
        stop("Paired severity draws must contain exactly the expected replicate IDs.")
    group_apply(point, c("role", "min_interactions"), function(p) {
        p <- p[order(p$player_key), ]
        n <- nrow(p)
        if (anyDuplicated(p$player_key))
            stop("Duplicate players in a fixed severity comparison cohort.")
        d <- draws[draws$role == p$role[1] & draws$player_key %in% p$player_key, ]
        matrices <- lapply(c("coefficient", "expected_severity"), function(type) {
            z <- d[d$score_type == type, ]
            indices <- (z$replicate_id - 1L) * n + match(z$player_key, p$player_key)
            if (nrow(z) != n * expected || anyDuplicated(indices) || any(!is.finite(z$score)) || anyNA(z$present_in_draw)) {
                stop("Missing, duplicate, or invalid paired severity scores in a fixed cohort.")
            }
            score <- matrix(NA_real_, nrow = n, ncol = expected)
            present <- matrix(NA, nrow = n, ncol = expected)
            score[indices] <- z$score
            present[indices] <- z$present_in_draw
            list(rank = matrix(vapply(seq_len(expected), function(j) rank(-score[, j], ties.method = "average"),
                numeric(n)), nrow = n), present = present)
        })
        if (!identical(matrices[[1]]$present, matrices[[2]]$present))
            stop("Paired severity score types disagree about player presence.")
        delta <- matrices[[1]]$rank - matrices[[2]]$rank
        q <- t(apply(delta, 1L, quantile, probs = c(0.025, 0.5, 0.975), names = FALSE))
        p$rank_delta_point <- p$rank_a - p$rank_b
        p$rank_delta_mean <- rowMeans(delta)
        p$rank_delta_median <- q[, 2L]
        p$rank_delta_q025 <- q[, 1L]
        p$rank_delta_q975 <- q[, 3L]
        p$rank_delta_n_boot <- expected
        p$rank_delta_positive_proportion <- rowMeans(delta > 0)
        p$rank_delta_zero_proportion <- rowMeans(delta == 0)
        p$rank_delta_negative_proportion <- rowMeans(delta < 0)
        p$rank_delta_same_sign_proportion <- rowMeans(sign(delta) == sign(p$rank_delta_point))
        p$paired_presence_rate <- rowMeans(matrices[[1]]$present)
        p$rank_delta_cohort_size <- n
        p$rank_delta_scope <- "fixed_point_common_method_cohort"
        p
    })
}
