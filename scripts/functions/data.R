# Read graded matchups; early and final models retain their own eligibility masks.

check <- function (ok, message)
{
    if (!isTRUE(ok))
        stop(message, call. = FALSE)
}

parse_blockers <- function (ids, names = NULL)
{
    out <- lapply(ids, function(s) {
        v <- as.character(jsonlite::fromJSON(s))
        check(length(v) > 0 && !anyNA(v) && all(nzchar(v)) && !anyDuplicated(v), "Invalid first-contest blocker members")
        v
    })
    if (!is.null(names)) {
        labels <- lapply(names, function(s) as.character(jsonlite::fromJSON(s)))
        check(all(lengths(labels) == lengths(out)), "Blocker member/name lengths differ")
    }
    out
}

read_matchups <- function (path, config = model_config())
{
    d <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("NA", "", "NaN"), colClasses = c(game_id = "character",
        play_id = "character", rusher_id = "character", blocker_ids = "character", blocker_names = "character"))
    required <- c("game_id", "play_id", "game_index", "week", "kickoff_utc", "rusher_name", "rusher_id",
        "blocker_ids", "blocker_names", "double_team", "double_team_unknown", "complete_timing", "win_target",
        "severity_outcome", "sack_credit", "blocker_rated_ol", "early_model_eligible", "strict_recorded_pressure")
    check(all(required %in% names(d)), paste("Missing columns:", paste(setdiff(required, names(d)),
        collapse = ", ")))
    check(nrow(d) > 0 && !anyNA(d[c("game_id", "play_id", "game_index", "week", "kickoff_utc",
        "rusher_name", "rusher_id", "blocker_ids", "blocker_names")]), "Missing contest identity/chronology")
    d$blocker_members <- I(parse_blockers(d$blocker_ids, d$blocker_names))
    d$blocker_member_names <- I(lapply(d$blocker_names, function(s) as.character(jsonlite::fromJSON(s))))
    d$blocker_keys <- I(lapply(d$blocker_members, function(v) paste0("id:", v)))
    d$rusher_key <- paste0("id:", d$rusher_id)
    if ("group_size" %in% names(d))
        check(all(d$group_size == lengths(d$blocker_members)), "Declared group size differs from member count")
    d$group_size <- lengths(d$blocker_members)
    ol_flags <- lapply(d$blocker_rated_ol, function(s) jsonlite::fromJSON(s))
    check(all(lengths(ol_flags) == d$group_size) && all(unlist(ol_flags) %in% 0:1), "Invalid protector OL display flags")
    check(all(d$double_team %in% 0:1) && all(d$double_team_unknown %in% 0:1) && all(d$double_team +
        d$double_team_unknown <= 1), "Invalid help indicators")
    check(all(na.omit(d$win_target) %in% 0:1) && all(na.omit(d$severity_outcome) %in% config$classes),
        "Invalid outcome")
    d$win_target <- as.numeric(d$win_target)
    check(!anyNA(d$early_model_eligible) && all(d$early_model_eligible %in% 0:1), "Invalid binary eligibility mask")
    check(!any(d$early_model_eligible == 1 & is.na(d$win_target)), "Eligible binary rows need an observed early grade")
    check(!anyNA(d$strict_recorded_pressure) && all(d$strict_recorded_pressure %in% 0:1),
        "Invalid recorded-pressure flag")
    final <- !is.na(d$severity_outcome)
    expected <- ifelse(d$sack_credit > 0, "sack", ifelse(d$strict_recorded_pressure == 1, "pressure",
        ifelse(d$win_target == 1, "win", "loss")))
    check(all(d$severity_outcome[final] == expected[final]), "Final category differs from sack/recorded-pressure/early-grade hierarchy")
    check(!any(!is.na(d$severity_outcome) & is.na(d$win_target)), "Final-model outcomes require an observed early grade for every category")
    check(all(is.finite(d$sack_credit)) && all(d$sack_credit >= 0 & d$sack_credit <= 1) &&
        all(d$sack_credit[d$severity_outcome %in% "sack"] > 0), "Invalid sack credits")
    d$complete_timing <- as.character(d$complete_timing) %in% c("TRUE", "True", "true", "1")
    check(!anyDuplicated(paste(d$game_id, d$play_id, d$rusher_id)), "Duplicate rusher/first-contest rows")
    games <- unique(d[c("game_id", "game_index", "week", "kickoff_utc")])
    check(!anyDuplicated(games$game_id) && !anyDuplicated(games$game_index), "Conflicting chronology")
    games <- games[order(games$game_index), ]
    tm <- as.POSIXct(games$kickoff_utc, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    check(!anyNA(tm) && all(diff(as.numeric(tm)) >= 0) && all(d$week %in% 1:18), "Invalid chronological games")
    d[order(d$game_index, d$play_id, d$rusher_id), , drop = FALSE]
}

model_sample <- function (d, model)
{
    valid <- !is.na(d$win_target)
    if (model == "win") {
        check("early_model_eligible" %in% names(d), "Missing binary eligibility mask")
        valid <- valid & d$early_model_eligible == 1
    }
    else valid <- valid & !is.na(d$severity_outcome)
    d[valid, , drop = FALSE]
}

player_vocabulary <- function (d)
{
    r <- unique(data.table(role = "Rusher", player_key = d$rusher_key, player_id = d$rusher_id, player_name = d$rusher_name))
    b <- unique(data.table(role = "Blocker", player_key = unlist(d$blocker_keys), player_id = unlist(d$blocker_members),
        player_name = unlist(d$blocker_member_names)))
    check(!anyDuplicated(r$player_key) && !anyDuplicated(b$player_key), "Canonical identity has conflicting display labels")
    list(rusher = sort(r$player_key), blocker = sort(b$player_key), labels = rbind(r, b))
}

split_season <- function (d, config)
{
    train <- d[d$week <= config$train_last_week, , drop = FALSE]
    test <- d[d$week %in% config$test_weeks, , drop = FALSE]
    check(nrow(train) > 0 && nrow(test) > 0, "Empty prespecified training or test period")
    check(!length(intersect(train$game_id, test$game_id)), "Game leakage at holdout split")
    check(!length(intersect(paste(train$game_id, train$play_id), paste(test$game_id, test$play_id))),
        "Play leakage at holdout split")
    check(max(train$game_index) < min(test$game_index), "Holdout is not later than training")
    list(train = train, test = test)
}

game_folds <- function (d, config)
{
    games <- sort(unique(as.character(d$game_id)))
    check(length(games) >= config$nfolds, "Insufficient games for five folds")
    RNGkind("Mersenne-Twister")
    set.seed(config$seed)
    data.frame(game_id = games, fold = sample(rep(seq_len(config$nfolds), length.out = length(games))))
}
