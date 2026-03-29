find_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, ".git")) && dir.exists(file.path(current, "data"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate project root from: ", start_dir)
    }
    current <- parent
  }
}

detect_raw_data_dir <- function(project_root) {
  candidates <- c(
    file.path(project_root, "data", "raw"),
    file.path(project_root, "archived", "data", "raw")
  )
  existing <- candidates[dir.exists(candidates)]
  if (length(existing) == 0L) {
    stop(
      "Could not locate raw data directory. Checked: ",
      paste(candidates, collapse = ", ")
    )
  }
  existing[[1]]
}

get_env_int <- function(name, default_value) {
  raw <- Sys.getenv(name, unset = "")
  if (identical(raw, "")) {
    return(as.integer(default_value))
  }
  value <- suppressWarnings(as.integer(raw))
  if (is.na(value) || value <= 0) {
    warning("Invalid value for ", name, "='", raw, "'. Using default ", default_value, ".")
    return(as.integer(default_value))
  }
  value
}

get_env_num <- function(name, default_value) {
  raw <- Sys.getenv(name, unset = "")
  if (identical(raw, "")) {
    return(as.numeric(default_value))
  }
  value <- suppressWarnings(as.numeric(raw))
  if (is.na(value) || value <= 0) {
    warning("Invalid value for ", name, "='", raw, "'. Using default ", default_value, ".")
    return(as.numeric(default_value))
  }
  value
}

detect_worker_count <- function(reserve_cores = 4L) {
  try_get_int <- function(value) {
    parsed <- suppressWarnings(as.integer(value))
    if (length(parsed) == 0 || is.na(parsed) || parsed <= 0L) {
      return(NA_integer_)
    }
    parsed
  }

  override <- try_get_int(Sys.getenv("PIPELINE_WORKERS", unset = NA_character_))
  if (!is.na(override)) {
    return(override)
  }

  available <- try_get_int(parallel::detectCores(logical = TRUE))
  if (is.na(available)) {
    available <- try_get_int(parallel::detectCores(logical = FALSE))
  }
  if (is.na(available)) {
    available <- try_get_int(
      tryCatch(
        suppressWarnings(system("sysctl -n hw.logicalcpu", intern = TRUE, ignore.stderr = TRUE)),
        error = function(e) NA_character_
      )
    )
  }
  if (is.na(available)) {
    available <- try_get_int(
      tryCatch(
        suppressWarnings(system("nproc", intern = TRUE, ignore.stderr = TRUE)),
        error = function(e) NA_character_
      )
    )
  }

  if (is.na(available) || available <= 1L) {
    return(1L)
  }
  max(1L, as.integer(available) - as.integer(reserve_cores))
}

PROJECT_ROOT <- find_project_root()
SCRIPTS_DIR <- file.path(PROJECT_ROOT, "scripts")
DATA_DIR <- file.path(PROJECT_ROOT, "data")
OUTPUT_DIR <- file.path(DATA_DIR, "output")
INPUT_DIR <- file.path(DATA_DIR, "input")
RAW_DATA_DIR <- detect_raw_data_dir(PROJECT_ROOT)
PARALLEL_RESERVED_CORES <- 4L
PARALLEL_WORKERS <- detect_worker_count(PARALLEL_RESERVED_CORES)
UNCERTAINTY_SEED <- get_env_int("PIPELINE_SEED", 20260328L)
VALIDATION_BOOTSTRAP_ITER <- get_env_int("VALIDATION_BOOTSTRAP_ITER", 400L)
RATING_BOOTSTRAP_ITER <- get_env_int("RATING_BOOTSTRAP_ITER", 80L)
END_TO_END_BOOTSTRAP_ITER <- get_env_int("END_TO_END_BOOTSTRAP_ITER", VALIDATION_BOOTSTRAP_ITER)
PATH_BOOTSTRAP_ITER <- get_env_int("PATH_BOOTSTRAP_ITER", 0L)
WIN_BASELINE_PRIOR_STRENGTH <- get_env_num("WIN_BASELINE_PRIOR_STRENGTH", 25)
WIN_BASELINE_MATCHUP_METHOD <- tolower(Sys.getenv("WIN_BASELINE_MATCHUP_METHOD", unset = "logit_mean"))

SHARED_OUTPUT_DIR <- file.path(OUTPUT_DIR, "shared")
WIN_OUTPUT_DIR <- file.path(OUTPUT_DIR, "win")
SEVERITY_OUTPUT_DIR <- file.path(OUTPUT_DIR, "severity")

PIPELINE_CONFIG <- list(
  input_paths = list(
    results_table = file.path(INPUT_DIR, "results2.csv"),
    sacks_table = file.path(INPUT_DIR, "sacks.csv"),
    hits_table = file.path(INPUT_DIR, "hits.csv")
  ),
  raw_input_paths = list(
    freeze_frames = file.path(RAW_DATA_DIR, "Hudl IQ 2021 NFL freeze frames.csv"),
    events_freeze_frames = file.path(RAW_DATA_DIR, "Hudl IQ 2021 NFL Events + Freeze Frame.csv"),
    roster = file.path(RAW_DATA_DIR, "Hudl IQ 2021 player roster.csv")
  ),
  output_paths = list(
    modeling_table = file.path(SHARED_OUTPUT_DIR, "modeling_table.csv"),
    modeling_summary = file.path(SHARED_OUTPUT_DIR, "modeling_summary.csv"),
    bt_full_leaderboard = file.path(SHARED_OUTPUT_DIR, "leaderboard_full_bt_ridge.csv"),
    win_model_artifact = file.path(WIN_OUTPUT_DIR, "model_win_bt_ridge.rds"),
    win_model_diagnostics = file.path(WIN_OUTPUT_DIR, "model_diagnostics_win_bt_ridge.csv"),
    win_model_coefficients = file.path(WIN_OUTPUT_DIR, "model_coefficients_win_bt_ridge.csv"),
    win_bt_player_ratings = file.path(WIN_OUTPUT_DIR, "player_ratings_win_bt_ridge.csv"),
    win_bt_holdout_scored = file.path(WIN_OUTPUT_DIR, "validation_holdout_scored_win_bt_ridge.csv"),
    win_bt_validation_metrics = file.path(WIN_OUTPUT_DIR, "validation_metrics_win_bt_ridge.csv"),
    win_bt_validation_uncertainty = file.path(WIN_OUTPUT_DIR, "validation_uncertainty_win_bt_ridge.csv"),
    win_bt_rating_uncertainty = file.path(WIN_OUTPUT_DIR, "rating_uncertainty_win_bt_ridge.csv"),
    win_bt_weekly_path_uncertainty = file.path(WIN_OUTPUT_DIR, "path_uncertainty_weekly_win_bt_ridge.csv"),
    severity_model_artifact = file.path(SEVERITY_OUTPUT_DIR, "model_severity_bt_ridge.rds"),
    severity_model_diagnostics = file.path(SEVERITY_OUTPUT_DIR, "model_diagnostics_severity_bt_ridge.csv"),
    severity_model_coefficients = file.path(SEVERITY_OUTPUT_DIR, "model_coefficients_severity_bt_ridge.csv"),
    severity_bt_player_ratings = file.path(SEVERITY_OUTPUT_DIR, "player_ratings_severity_bt_ridge.csv"),
    severity_bt_holdout_scored = file.path(SEVERITY_OUTPUT_DIR, "validation_holdout_scored_severity_bt_ridge.csv"),
    severity_bt_validation_metrics = file.path(SEVERITY_OUTPUT_DIR, "validation_metrics_severity_bt_ridge.csv"),
    severity_bt_multiclass_metrics = file.path(SEVERITY_OUTPUT_DIR, "validation_metrics_severity_multiclass_bt_ridge.csv"),
    severity_bt_validation_uncertainty = file.path(SEVERITY_OUTPUT_DIR, "validation_uncertainty_severity_bt_ridge.csv"),
    severity_bt_rating_uncertainty = file.path(SEVERITY_OUTPUT_DIR, "rating_uncertainty_severity_bt_ridge.csv"),
    severity_bt_weekly_path_uncertainty = file.path(SEVERITY_OUTPUT_DIR, "path_uncertainty_weekly_severity_bt_ridge.csv")
  ),
  win_definition = list(
    max_win_seconds = as.numeric(Sys.getenv("WIN_SECONDS_THRESHOLD", unset = "2.5"))
  ),
  split = list(
    train_fraction = 0.80
  ),
  severity_weights = list(
    sack = 1.0,
    hit = 0.4,
    win = 0.2,
    loss = 0.0
  ),
  uncertainty = list(
    seed = UNCERTAINTY_SEED,
    validation_bootstrap_iterations = VALIDATION_BOOTSTRAP_ITER,
    rating_bootstrap_iterations = RATING_BOOTSTRAP_ITER,
    end_to_end_bootstrap_iterations = END_TO_END_BOOTSTRAP_ITER,
    path_bootstrap_iterations = PATH_BOOTSTRAP_ITER
  ),
  parallel = list(
    reserve_cores = PARALLEL_RESERVED_CORES,
    workers = PARALLEL_WORKERS
  ),
  bt_win_model = list(
    model_name = "win_bt_ridge",
    target_col = "win_target",
    alpha = 0.0,
    nfolds = 5L,
    lambda_grid = list(
      scale = "log",
      max = 1.0,
      min = 1e-6,
      length = 120L
    ),
    standardize = FALSE,
    include_double_team = TRUE,
    lambda_selection = "lambda.min",
    rating_scale = 400 / log(10),
    train_interaction_filter = 50L,
    matchup_baseline = list(
      method = WIN_BASELINE_MATCHUP_METHOD,
      prior_strength = WIN_BASELINE_PRIOR_STRENGTH
    )
  ),
  bt_severity_model = list(
    model_name = "severity_bt_ridge",
    outcome_col = "severity_outcome",
    target_col = "severity_target",
    class_levels = c("loss", "win", "hit", "sack"),
    class_weights = c(
      loss = 1.0,
      win = 1.0,
      hit = 1.0,
      sack = 1.0
    ),
    alpha = 0.0,
    nfolds = 5L,
    lambda_grid = list(
      scale = "log",
      max = 1.0,
      min = 1e-6,
      length = 120L
    ),
    standardize = FALSE,
    include_double_team = TRUE,
    lambda_selection = "lambda.min",
    rating_scale = 400 / log(10)
  ),
  win_model = list(
    model_name = "win",
    target_col = "win_target",
    scale = 400,
    use_double_team_bonus = FALSE,
    double_team_bonus = 0,
    rusher_start_elo = 900,
    blocker_start_elo = 1100,
    use_adaptive_k = FALSE,
    constant_k = 32,
    adaptive_k = list(
      k_start = 32,
      k_min = 16,
      n_provisional = 100,
      n_decay = 300
    )
  ),
  severity_model = list(
    model_name = "severity",
    target_col = "severity_target",
    scale = 319,
    use_double_team_bonus = TRUE,
    double_team_bonus = 100,
    rusher_start_elo = 750,
    blocker_start_elo = 1125,
    use_adaptive_k = TRUE,
    constant_k = 20,
    adaptive_k = list(
      k_start = 20,
      k_min = 10,
      n_provisional = 87,
      n_decay = 200
    )
  )
)
