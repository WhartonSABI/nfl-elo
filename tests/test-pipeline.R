test_pipeline <- function() {
  directory <- tempfile("nfl-pipeline-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  previous <- getOption("nfl.settings")
  on.exit(options(nfl.settings = previous), add = TRUE)
  input <- file.path(directory, "input")
  output <- file.path(directory, "results")
  dir.create(input)
  options(nfl.settings = list(input_dir = input, output_dir = output,
                              bootstrap_iterations = 2L, lambda_min = .001, lambda_length = 6L))
  source("tests/test-models.R")
  d$game_index <- rep(1:40, each = 30L)
  d$game_id <- as.character(d$game_index)
  d$week <- c(rep(1:15, length.out = 35L), rep(16:18, length.out = 5L))[d$game_index]
  dates <- seq(as.Date("2021-09-01"), by = "day", length.out = 40L)
  d$kickoff_utc <- paste0(dates[d$game_index], "T12:00:00Z")
  set.seed(8461)
  blocker_effect <- vapply(d$blocker_ids, function(ids) mean(as.numeric(jsonlite::fromJSON(ids))), numeric(1))
  eta <- as.numeric(scale(as.numeric(d$rusher_id))) - .5 * as.numeric(scale(blocker_effect)) - .4 * d$double_team
  cumulative <- plogis(matrix(c(0, 1.2, 2.2), nrow(d), 3L, byrow = TRUE) - eta)
  u <- runif(nrow(d))
  d$severity_outcome <- c("loss", "win", "pressure", "sack")[1L + rowSums(u > cumulative)]
  d$win_target <- rbinom(nrow(d), 1, plogis(eta))
  d$win_target[d$severity_outcome == "loss"] <- 0L
  d$win_target[d$severity_outcome == "win"] <- 1L
  d$strict_recorded_pressure <- as.integer(d$severity_outcome == "pressure")
  d$sack_credit <- ifelse(d$severity_outcome == "sack", .5, 0)
  d$win_target[1:5] <- NA
  d$severity_outcome[1:10] <- NA
  d$blocker_rated_ol <- vapply(d$blocker_ids, function(ids) {
    as.character(jsonlite::toJSON(as.integer(as.numeric(jsonlite::fromJSON(ids)) %% 2 == 0)))
  }, character(1))
  fwrite(d, file.path(input, "modeling_table.csv"))
  sample <- read_matchups(file.path(input, "modeling_table.csv"))
  pool <- unique(sample[c("game_id", "week")])
  pool <- rbind(pool, data.frame(game_id = "41", week = 18))
  fwrite(pool, file.path(input, "game_pool.csv"))
  fwrite(game_folds(sample, model_config()), file.path(input, "folds.csv"))
  saveRDS(lapply(setNames(c("win", "severity"), c("win", "severity")), function(model) reference_matchups(sample, model)),
          file.path(input, "reference.rds"))

  set.seed(24681)
  game <- rep(1:40, each = 60L)
  size <- length(game)
  plays <- data.frame(game_id = as.character(game), play_id = paste0("p", seq_len(size)),
    nflfast_game_id = paste0("2021_", game), week = pool$week[match(game, pool$game_id)],
    split = ifelse(game <= 35, "train", "test"), eligible_primary = game != 2,
    exclusion_reason = ifelse(game == 2, "incomplete_early_protection_grades", ""),
    epa = 0, B = sample(3:5, size, TRUE), accepted_penalty = runif(size) < .08,
    down = sample(1:4, size, TRUE), ydstogo = sample(1:20, size, TRUE),
    yardline_100 = sample(1:99, size, TRUE), half_seconds_remaining = sample(1:1800, size, TRUE),
    score_differential = sample(-21:21, size, TRUE), qtr = sample(1:5, size, TRUE),
    quarterback_id = sample(c(paste0("QB", 1:6), NA_character_), size, TRUE),
    defteam = sample(c("A", "B", "C", "D"), size, TRUE))
  plays$game_half <- ifelse(plays$qtr <= 2, "Half1", ifelse(plays$qtr <= 4, "Half2", "Overtime"))
  categories <- lapply(plays$B, function(k) sample(c("loss", "win", "pressure", "sack"), k, TRUE, c(.6, .22, .13, .05)))
  for (i in which(game == 2)) categories[[i]][1:2] <- c("unknown", "pressure")
  count <- function(label) vapply(categories, function(x) sum(x == label), integer(1))
  plays$N_W <- count("win"); plays$P_B <- count("pressure"); plays$H_B <- 0L
  plays$K_B <- count("sack"); plays$U <- count("unknown")
  plays$P_A <- rbinom(size, 1, .12); plays$H_A <- 0L
  outside_sacks <- rbinom(size, 1, .03)
  sacks <- plays$K_B + outside_sacks
  plays$S_B <- ifelse(sacks > 0, plays$K_B / sacks, 0)
  plays$S_A <- ifelse(sacks > 0, outside_sacks / sacks, 0)
  plays$N_P <- plays$P_B + plays$P_A; plays$N_H <- 0L
  plays$N_S <- as.numeric(sacks > 0); plays$E <- plays$K_B - plays$S_B
  plays$early_protection_participants <- plays$B
  plays$early_protection_ungraded_actors <- plays$U
  plays$early_protection_complete <- plays$U == 0
  plays$quarterback_id[game == 40] <- "NEW_TEST_QB"
  cfg <- epa_config(expected_train_games = 35L, expected_test_games = 6L)
  basis <- epa_basis(plays[plays$eligible_primary & plays$split == "train", ], cfg)
  design <- epa_matrix(plays, basis, cfg)
  beta <- setNames(rep(0, ncol(design)), colnames(design))
  beta[c("(Intercept)", "N_W", "N_P", "N_S", "B", "E")] <- c(.3, -.12, -.35, -2, .08, .1)
  plays$epa <- as.numeric(design %*% beta) + rnorm(size, sd = .02)
  fwrite(plays, file.path(input, "calibration_plays.csv"))
  saveRDS(basis, file.path(input, "epa_basis.rds"))

  source("scripts/run_all.R")
  prepared <- readRDS(file.path(output, "data.rds"))
  point <- readRDS(file.path(output, "epa.rds"))$point
  stopifnot(!"2" %in% prepared$epa_state$train$game_id, "41" %in% prepared$epa_state$pool$game_id)
  stopifnot(max(abs(point$fit$coefficients[c("N_W", "N_P", "N_S")] - beta[c("N_W", "N_P", "N_S")])) < .035)
  stopifnot(point$weights$defined, all(is.finite(point$weights$normalized)))
  weekly <- readRDS(file.path(output, "weekly_point.rds"))
  ranked <- readRDS(file.path(output, "rankings.rds"))$scores
  endpoint <- ranked[ranked$score_type %in% c("standardized_probability", "expected_severity") & ranked$role_interactions > 0, ]
  final_week <- weekly[[18]]$scores
  index <- match(weekly_key(endpoint), weekly_key(final_week))
  stopifnot(!anyNA(index), max(abs(endpoint$score - final_week$score[index])) < 1e-12)
  scoped <- fread(file.path(output, "rank_uncertainty.csv"))
  stopifnot(length(unique(scoped$player_key[scoped$role == "Blocker"])) < length(prepared$vocabulary$blocker))
  stopifnot(all(is.finite(scoped$rank_q025)), all(scoped$rank_q025 <= scoped$rank_q975))
  draw_path <- file.path(output, "bootstrap/ratings/0001.rds")
  original_draw <- readRDS(draw_path)
  source("scripts/06_bootstrap.R")
  stopifnot(identical(original_draw, readRDS(draw_path)))
  stopifnot(length(list.files(file.path(output, "bootstrap/weekly"), pattern = "rds$", recursive = TRUE)) == 36L)
  stopifnot(all(file.exists(file.path(output, c("cv_plot.png", "calibration_plot.png", "weekly_plot.png")))))
  cat("Full pipeline passed with synthetic inputs, including resumption and weekly endpoint checks.\n")
}
test_pipeline()
