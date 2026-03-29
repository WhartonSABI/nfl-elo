suppressPackageStartupMessages({
  library(dplyr)
  library(glmnet)
  library(Matrix)
  library(readr)
  library(tidyr)
  library(tibble)
})

parallel_map <- function(iterable, worker_fn, workers = 1L, seed = NULL) {
  n_items <- length(iterable)
  workers <- max(1L, as.integer(workers))

  if (n_items == 0L) {
    return(list())
  }
  if (workers <= 1L || n_items == 1L) {
    return(lapply(iterable, worker_fn))
  }

  workers <- min(workers, n_items)

  # On macOS/Linux, forked workers keep function scope naturally and avoid
  # explicit symbol export issues that can happen with socket clusters.
  if (.Platform$OS.type != "windows") {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    return(
      parallel::mclapply(
        iterable,
        worker_fn,
        mc.cores = workers,
        mc.set.seed = TRUE
      )
    )
  }

  cl <- tryCatch(
    parallel::makeCluster(workers),
    error = function(e) NULL
  )
  if (is.null(cl)) {
    warning("Could not start parallel worker cluster; falling back to sequential execution.")
    return(lapply(iterable, worker_fn))
  }
  on.exit(parallel::stopCluster(cl), add = TRUE)

  if (!is.null(seed)) {
    parallel::clusterSetRNGStream(cl, seed)
  }

  fn_env <- environment(worker_fn)
  if (!is.null(fn_env)) {
    fn_exports <- ls(fn_env, all.names = TRUE)
    if (length(fn_exports) > 0) {
      parallel::clusterExport(cl, varlist = fn_exports, envir = fn_env)
    }
  }

  parallel::clusterEvalQ(
    cl,
    suppressPackageStartupMessages({
      library(dplyr)
      library(tibble)
      library(tidyr)
    })
  )

  parallel::parLapply(cl, iterable, worker_fn)
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

ensure_output_directories <- function(config) {
  dirs <- c(
    dirname(config$output_paths$modeling_table),
    dirname(config$output_paths$win_model_artifact),
    dirname(config$output_paths$severity_model_artifact)
  )
  invisible(lapply(dirs, ensure_directory))
}

drop_index_columns <- function(df) {
  index_cols <- grep("^(Unnamed: 0|X|\\.\\.\\.\\d+)$", names(df), value = TRUE)
  if (length(index_cols) > 0) {
    df <- df %>% select(-all_of(index_cols))
  }
  df
}

assert_columns <- function(df, required, object_name = "data frame") {
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ", object_name, ": ", paste(missing_cols, collapse = ", "))
  }
}

add_interaction_indices <- function(df) {
  df %>%
    group_by(rusher_name) %>%
    mutate(rusher_row = row_number()) %>%
    ungroup() %>%
    group_by(blocker_name) %>%
    mutate(blocker_row = row_number()) %>%
    ungroup()
}

build_modeling_table <- function(config) {
  results_tbl <- read_csv(config$input_paths$results_table, show_col_types = FALSE, name_repair = "unique_quiet")
  sacks_tbl <- read_csv(config$input_paths$sacks_table, show_col_types = FALSE, name_repair = "unique_quiet")
  hits_tbl <- read_csv(config$input_paths$hits_table, show_col_types = FALSE, name_repair = "unique_quiet")

  results_tbl <- drop_index_columns(results_tbl)
  sacks_tbl <- drop_index_columns(sacks_tbl)
  hits_tbl <- drop_index_columns(hits_tbl)

  assert_columns(
    results_tbl,
    c("game_id", "play_id", "event_game_index", "rusher_name", "blocker_name", "rusher_won", "double_team"),
    "results table"
  )
  assert_columns(sacks_tbl, c("game_id", "play_uuid", "sack_player", "sack"), "sacks table")
  assert_columns(hits_tbl, c("game_id", "play_uuid", "hit_player", "hit"), "hits table")

  out <- results_tbl %>%
    left_join(
      sacks_tbl %>% select(game_id, play_uuid, sack_player, sack),
      by = c("game_id", "play_id" = "play_uuid", "rusher_name" = "sack_player")
    ) %>%
    mutate(sack = dplyr::coalesce(sack, 0L)) %>%
    left_join(
      hits_tbl %>% select(game_id, play_uuid, hit_player, hit),
      by = c("game_id", "play_id" = "play_uuid", "rusher_name" = "hit_player")
    ) %>%
    mutate(hit = dplyr::coalesce(hit, 0L)) %>%
    mutate(
      win_target = as.numeric(rusher_won == 1L),
      severity_target = case_when(
        sack == 1L ~ config$severity_weights$sack,
        hit == 1L ~ config$severity_weights$hit,
        rusher_won == 1L ~ config$severity_weights$win,
        TRUE ~ config$severity_weights$loss
      ),
      severity_outcome = case_when(
        sack == 1L ~ "sack",
        hit == 1L ~ "hit",
        rusher_won == 1L ~ "win",
        TRUE ~ "loss"
      )
    ) %>%
    drop_na(game_id, play_id, rusher_name, blocker_name, win_target, severity_target) %>%
    arrange(game_id, play_id, event_game_index) %>%
    add_interaction_indices() %>%
    mutate(row_index = row_number()) %>%
    select(
      row_index,
      game_id,
      play_id,
      event_game_index,
      rusher_name,
      blocker_name,
      double_team,
      sack,
      hit,
      rusher_won,
      win_target,
      severity_outcome,
      severity_target,
      rusher_row,
      blocker_row
    )

  out
}

severity_class_weight_vector <- function(severity_weights) {
  c(
    loss = as.numeric(severity_weights$loss),
    win = as.numeric(severity_weights$win),
    hit = as.numeric(severity_weights$hit),
    sack = as.numeric(severity_weights$sack)
  )
}

ensure_severity_outcome_column <- function(df, bt_cfg, severity_weights = NULL) {
  outcome_col <- bt_cfg$outcome_col
  class_levels <- bt_cfg$class_levels

  if (!outcome_col %in% names(df)) {
    if (all(c("sack", "hit", "rusher_won") %in% names(df))) {
      df[[outcome_col]] <- dplyr::case_when(
        dplyr::coalesce(df$sack, 0L) == 1L ~ "sack",
        dplyr::coalesce(df$hit, 0L) == 1L ~ "hit",
        dplyr::coalesce(df$rusher_won, 0L) == 1L ~ "win",
        TRUE ~ "loss"
      )
    } else if (!is.null(severity_weights) && "severity_target" %in% names(df)) {
      class_weights <- severity_class_weight_vector(severity_weights)
      eps <- 1e-10
      df[[outcome_col]] <- dplyr::case_when(
        abs(df$severity_target - class_weights[["sack"]]) <= eps ~ "sack",
        abs(df$severity_target - class_weights[["hit"]]) <= eps ~ "hit",
        abs(df$severity_target - class_weights[["win"]]) <= eps ~ "win",
        abs(df$severity_target - class_weights[["loss"]]) <= eps ~ "loss",
        TRUE ~ NA_character_
      )
    } else {
      stop("Could not derive severity outcome labels for column '", outcome_col, "'.")
    }
  }

  df[[outcome_col]] <- factor(df[[outcome_col]], levels = class_levels)
  if (anyNA(df[[outcome_col]])) {
    stop("Found NA severity outcome labels after coercion.")
  }

  df
}

get_bt_levels <- function(df) {
  assert_columns(df, c("rusher_name", "blocker_name"), "bt data")
  list(
    rusher_levels = sort(unique(df$rusher_name)),
    blocker_levels = sort(unique(df$blocker_name))
  )
}

to_sparse_bt_matrix <- function(df, rusher_levels, blocker_levels, include_double_team = TRUE) {
  assert_columns(df, c("rusher_name", "blocker_name"), "bt matrix input")

  n <- nrow(df)
  n_r <- length(rusher_levels)
  n_b <- length(blocker_levels)

  if (n == 0L) {
    base_cols <- c(
      paste0("rusher::", rusher_levels),
      paste0("blocker::", blocker_levels)
    )
    if (isTRUE(include_double_team)) {
      base_cols <- c(base_cols, "double_team")
    }
    out <- sparseMatrix(
      i = integer(0),
      j = integer(0),
      x = numeric(0),
      dims = c(0L, length(base_cols))
    )
    dimnames(out) <- list(NULL, base_cols)
    return(out)
  }

  r_idx <- match(df$rusher_name, rusher_levels)
  b_idx <- match(df$blocker_name, blocker_levels)

  if (anyNA(r_idx) || anyNA(b_idx)) {
    stop("Found unknown rusher/blocker names while building BT design matrix.")
  }

  i <- c(seq_len(n), seq_len(n))
  j <- c(r_idx, n_r + b_idx)
  x <- c(rep(1, n), rep(-1, n))

  col_names <- c(
    paste0("rusher::", rusher_levels),
    paste0("blocker::", blocker_levels)
  )

  if (isTRUE(include_double_team)) {
    if (!"double_team" %in% names(df)) {
      stop("double_team column is required when include_double_team = TRUE.")
    }
    i <- c(i, seq_len(n))
    j <- c(j, n_r + n_b + rep(1L, n))
    x <- c(x, as.numeric(dplyr::coalesce(df$double_team, 0)))
    col_names <- c(col_names, "double_team")
  }

  out <- sparseMatrix(i = i, j = j, x = x, dims = c(n, length(col_names)))
  dimnames(out) <- list(NULL, col_names)
  out
}

build_lambda_grid <- function(bt_cfg, default_min = 1e-4, default_max = 1.0, default_length = 80L) {
  grid_cfg <- bt_cfg$lambda_grid
  if (is.null(grid_cfg)) {
    return(NULL)
  }

  grid_min <- suppressWarnings(as.numeric(grid_cfg$min))
  grid_max <- suppressWarnings(as.numeric(grid_cfg$max))
  grid_length <- suppressWarnings(as.integer(grid_cfg$length))
  grid_scale <- if (is.null(grid_cfg$scale)) "log" else tolower(as.character(grid_cfg$scale))

  if (is.na(grid_min) || grid_min <= 0) {
    grid_min <- default_min
  }
  if (is.na(grid_max) || grid_max <= 0) {
    grid_max <- default_max
  }
  if (is.na(grid_length) || grid_length < 2L) {
    grid_length <- default_length
  }
  if (grid_max < grid_min) {
    tmp <- grid_max
    grid_max <- grid_min
    grid_min <- tmp
  }

  if (identical(grid_scale, "linear")) {
    lambda <- seq(from = grid_max, to = grid_min, length.out = grid_length)
  } else {
    lambda <- exp(seq(from = log(grid_max), to = log(grid_min), length.out = grid_length))
  }

  sort(unique(as.numeric(lambda)), decreasing = TRUE)
}

fit_bt_win_cv <- function(train_df, bt_cfg, rusher_levels, blocker_levels, seed = 42L) {
  target_col <- bt_cfg$target_col
  assert_columns(train_df, c("rusher_name", "blocker_name", target_col, "double_team"), "bt win train_df")

  x_train <- to_sparse_bt_matrix(
    df = train_df,
    rusher_levels = rusher_levels,
    blocker_levels = blocker_levels,
    include_double_team = isTRUE(bt_cfg$include_double_team)
  )
  y_train <- as.numeric(train_df[[target_col]])
  lambda_grid <- build_lambda_grid(bt_cfg)

  set.seed(seed)
  cv_fit <- cv.glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = as.numeric(bt_cfg$alpha),
    type.measure = "deviance",
    nfolds = as.integer(bt_cfg$nfolds),
    standardize = isTRUE(bt_cfg$standardize),
    lambda = lambda_grid
  )

  list(
    cv_fit = cv_fit,
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    train_baseline = mean(y_train, na.rm = TRUE)
  )
}

get_class_observation_weights <- function(y_factor, class_weights = NULL) {
  if (is.null(class_weights)) {
    return(rep(1, length(y_factor)))
  }
  if (is.null(names(class_weights))) {
    stop("class_weights must be a named vector keyed by class label.")
  }
  weight_lookup <- class_weights[levels(y_factor)]
  if (any(is.na(weight_lookup))) {
    stop("Missing class_weights for levels: ", paste(levels(y_factor)[is.na(weight_lookup)], collapse = ", "))
  }
  as.numeric(weight_lookup[as.character(y_factor)])
}

fit_bt_severity_cv <- function(train_df, bt_cfg, severity_weights, rusher_levels, blocker_levels, seed = 42L) {
  target_col <- bt_cfg$target_col
  outcome_col <- bt_cfg$outcome_col
  class_levels <- bt_cfg$class_levels

  train_df <- ensure_severity_outcome_column(train_df, bt_cfg, severity_weights)
  assert_columns(train_df, c("rusher_name", "blocker_name", target_col, outcome_col, "double_team"), "bt severity train_df")

  x_train <- to_sparse_bt_matrix(
    df = train_df,
    rusher_levels = rusher_levels,
    blocker_levels = blocker_levels,
    include_double_team = isTRUE(bt_cfg$include_double_team)
  )
  y_train <- factor(train_df[[outcome_col]], levels = class_levels)
  obs_weights <- get_class_observation_weights(y_train, bt_cfg$class_weights)
  lambda_grid <- build_lambda_grid(bt_cfg)

  set.seed(seed)
  cv_fit <- cv.glmnet(
    x = x_train,
    y = y_train,
    family = "multinomial",
    alpha = as.numeric(bt_cfg$alpha),
    type.measure = "deviance",
    nfolds = as.integer(bt_cfg$nfolds),
    standardize = isTRUE(bt_cfg$standardize),
    weights = obs_weights,
    lambda = lambda_grid
  )

  class_counts <- table(y_train)
  baseline_class_prob <- setNames(rep(0, length(class_levels)), class_levels)
  baseline_class_prob[names(class_counts)] <- as.numeric(class_counts) / length(y_train)

  list(
    cv_fit = cv_fit,
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    train_baseline = mean(train_df[[target_col]], na.rm = TRUE),
    baseline_class_prob = baseline_class_prob,
    class_levels = class_levels
  )
}

predict_bt_win <- function(cv_fit, x_new, s = "lambda.min") {
  as.numeric(predict(cv_fit, newx = x_new, s = s, type = "response"))
}

predict_bt_multinomial_probs <- function(cv_fit, x_new, s = "lambda.min", class_levels = NULL) {
  raw <- predict(cv_fit, newx = x_new, s = s, type = "response")

  if (length(dim(raw)) == 3L) {
    probs <- raw[, , 1, drop = TRUE]
  } else {
    probs <- raw
  }

  probs <- as.matrix(probs)
  if (nrow(probs) == 0L) {
    if (is.null(class_levels)) {
      return(as_tibble(probs))
    }
    empty <- matrix(0, nrow = 0, ncol = length(class_levels))
    colnames(empty) <- class_levels
    return(as_tibble(empty))
  }

  if (is.null(colnames(probs)) && !is.null(class_levels) && ncol(probs) == length(class_levels)) {
    colnames(probs) <- class_levels
  }

  if (!is.null(class_levels)) {
    missing_cols <- setdiff(class_levels, colnames(probs))
    if (length(missing_cols) > 0) {
      missing_mat <- matrix(0, nrow = nrow(probs), ncol = length(missing_cols))
      colnames(missing_mat) <- missing_cols
      probs <- cbind(probs, missing_mat)
    }
    probs <- probs[, class_levels, drop = FALSE]
  }

  row_totals <- rowSums(probs)
  row_totals[row_totals <= 0 | is.na(row_totals)] <- 1
  probs <- probs / row_totals

  as_tibble(probs, .name_repair = "unique")
}

severity_prob_to_expected <- function(prob_tbl, severity_weights) {
  class_weights <- severity_class_weight_vector(severity_weights)
  class_names <- names(class_weights)
  assert_columns(prob_tbl, class_names, "severity probability table")
  as.numeric(as.matrix(prob_tbl[, class_names, drop = FALSE]) %*% class_weights)
}

extract_glmnet_binomial_coefficients <- function(fit, s_value) {
  coef_mat <- as.matrix(coef(fit, s = s_value))
  tibble(
    term = rownames(coef_mat),
    coef = as.numeric(coef_mat[, 1])
  )
}

extract_glmnet_multinomial_coefficients <- function(fit, s_value, class_levels = NULL) {
  coef_list <- coef(fit, s = s_value)
  classes <- names(coef_list)

  if (!is.null(class_levels)) {
    classes <- intersect(class_levels, classes)
  }

  bind_rows(lapply(classes, function(cls) {
    mat <- as.matrix(coef_list[[cls]])
    tibble(
      class = cls,
      term = rownames(mat),
      coef = as.numeric(mat[, 1])
    )
  }))
}

severity_weighted_term_scores <- function(multinomial_coef_tbl, severity_weights) {
  class_weights <- severity_class_weight_vector(severity_weights)

  multinomial_coef_tbl %>%
    filter(term != "(Intercept)") %>%
    mutate(weight = unname(class_weights[class])) %>%
    mutate(weighted_component = weight * coef) %>%
    group_by(term) %>%
    summarise(term_score = sum(weighted_component), .groups = "drop")
}

build_player_ratings_from_term_scores <- function(term_scores, rating_scale = 400 / log(10), score_col_name = "bt_score", source_label = "lambda.min") {
  assert_columns(term_scores, c("term", "term_score"), "term_scores")

  player_terms <- term_scores %>%
    filter(grepl("^(rusher::|blocker::)", term)) %>%
    mutate(
      role = if_else(grepl("^rusher::", term), "Rusher", "Blocker"),
      player_name = sub("^(rusher::|blocker::)", "", term)
    )

  if (nrow(player_terms) == 0L) {
    empty <- tibble(
      player_name = character(0),
      role = character(0),
      elo_like_score = numeric(0),
      coefficient_source = character(0)
    )
    empty[[score_col_name]] <- numeric(0)
    return(empty[, c("player_name", "role", score_col_name, "elo_like_score", "coefficient_source")])
  }

  centered <- player_terms$term_score - mean(player_terms$term_score, na.rm = TRUE)
  out <- player_terms %>%
    transmute(
      player_name = player_name,
      role = role,
      term_score = term_score,
      elo_like_score = 1000 + rating_scale * centered,
      coefficient_source = source_label
    )

  out[[score_col_name]] <- out$term_score
  out <- out %>%
    select(player_name, role, all_of(score_col_name), elo_like_score, coefficient_source) %>%
    arrange(desc(elo_like_score))

  out
}

compute_multiclass_logloss_scalar <- function(actual_class, prob_tbl, class_levels) {
  eps <- 1e-12
  y <- factor(actual_class, levels = class_levels)
  if (anyNA(y)) {
    stop("actual_class contains unknown labels.")
  }
  assert_columns(prob_tbl, class_levels, "multiclass probability table")
  p <- as.matrix(prob_tbl[, class_levels, drop = FALSE])
  p <- pmin(pmax(p, eps), 1 - eps)
  idx <- cbind(seq_len(nrow(p)), as.integer(y))
  -mean(log(p[idx]))
}

compute_multiclass_validation_metrics <- function(actual_class, model_prob_tbl, baseline_class_prob, class_levels, mode = "severity_multiclass") {
  assert_columns(model_prob_tbl, class_levels, "model_prob_tbl")
  baseline_vec <- baseline_class_prob[class_levels]
  if (any(is.na(baseline_vec))) {
    stop("baseline_class_prob is missing one or more class levels.")
  }
  baseline_raw <- matrix(
    rep(baseline_vec, each = nrow(model_prob_tbl)),
    ncol = length(class_levels),
    dimnames = list(NULL, class_levels)
  )
  baseline_mat <- as_tibble(baseline_raw, .name_repair = "minimal")

  model_ll <- compute_multiclass_logloss_scalar(actual_class, model_prob_tbl, class_levels)
  baseline_ll <- compute_multiclass_logloss_scalar(actual_class, baseline_mat, class_levels)

  tibble(
    mode = mode,
    metric = "multiclass_logloss",
    model_value = model_ll,
    baseline_value = baseline_ll,
    improvement = baseline_ll - model_ll,
    n_rows = nrow(model_prob_tbl)
  )
}

select_bt_fixed_lambda <- function(model_artifact, bt_cfg) {
  if (is.null(model_artifact) || is.null(model_artifact$lambda_min) || is.null(model_artifact$lambda_1se)) {
    stop("Model artifact is missing lambda_min/lambda_1se.")
  }

  selection <- if (is.null(bt_cfg$lambda_selection)) "lambda.min" else as.character(bt_cfg$lambda_selection)
  lambda_value <- if (identical(selection, "lambda.1se")) model_artifact$lambda_1se else model_artifact$lambda_min
  lambda_value <- suppressWarnings(as.numeric(lambda_value))

  if (is.na(lambda_value) || lambda_value <= 0) {
    stop("Selected fixed lambda is invalid: ", lambda_value)
  }

  lambda_value
}

sample_game_block_rows <- function(df, games, game_chunks) {
  sampled_games <- sample(games, size = length(games), replace = TRUE)
  sampled_chunks <- vector("list", length(sampled_games))

  for (i in seq_along(sampled_games)) {
    g <- sampled_games[[i]]
    chunk <- game_chunks[[as.character(g)]]
    chunk$.bootstrap_order <- i
    sampled_chunks[[i]] <- chunk
  }

  bind_rows(sampled_chunks) %>%
    arrange(.bootstrap_order, event_game_index) %>%
    select(-.bootstrap_order)
}

locate_game_week_lookup_file <- function(explicit_path = NULL) {
  if (!is.null(explicit_path) && !is.na(explicit_path) && file.exists(explicit_path)) {
    return(explicit_path)
  }

  project_root <- if (exists("PROJECT_ROOT", inherits = TRUE)) get("PROJECT_ROOT", inherits = TRUE) else getwd()
  input_dir <- if (exists("INPUT_DIR", inherits = TRUE)) get("INPUT_DIR", inherits = TRUE) else file.path(project_root, "data", "input")

  candidates <- c(
    file.path(input_dir, "hudl_iq_game_ids.csv"),
    file.path(project_root, "data", "processed", "hudl_iq_game_ids.csv"),
    file.path(project_root, "archived", "data", "processed", "hudl_iq_game_ids.csv")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    return(NA_character_)
  }
  existing[[1]]
}

extract_nflfast_week <- function(nflfast_game_id) {
  x <- as.character(nflfast_game_id)
  ok <- grepl("^[0-9]{4}_[0-9]{2}_", x)
  out <- rep(NA_integer_, length(x))
  out[ok] <- suppressWarnings(as.integer(sub("^[0-9]{4}_([0-9]{2})_.*$", "\\1", x[ok])))
  out
}

build_game_week_lookup <- function(model_data, week_lookup_path = NULL) {
  assert_columns(model_data, c("game_id"), "model_data")

  row_col <- if ("row_index" %in% names(model_data)) "row_index" else NULL
  games_base <- if (!is.null(row_col)) {
    model_data %>%
      group_by(game_id) %>%
      summarise(first_row = min(.data[[row_col]], na.rm = TRUE), .groups = "drop")
  } else {
    model_data %>%
      mutate(.tmp_row = row_number()) %>%
      group_by(game_id) %>%
      summarise(first_row = min(.tmp_row), .groups = "drop")
  }

  games_base <- games_base %>%
    arrange(first_row, game_id) %>%
    mutate(game_key = as.character(game_id))

  lookup_path <- locate_game_week_lookup_file(week_lookup_path)
  has_lookup <- !is.na(lookup_path)

  if (!has_lookup) {
    week_tbl <- games_base %>%
      mutate(
        week_num = row_number(),
        week_index = row_number(),
        week_label = paste0("G", formatC(week_index, width = 3, flag = "0")),
        week_source = "game_sequence"
      ) %>%
      select(game_id, first_row, week_num, week_index, week_label, week_source)
    return(week_tbl)
  }

  lookup_raw <- read_csv(lookup_path, show_col_types = FALSE, name_repair = "unique_quiet") %>%
    drop_index_columns()
  if (!"game_id" %in% names(lookup_raw)) {
    warning("Week lookup file has no game_id column (", lookup_path, "); falling back to game sequence.")
    week_tbl <- games_base %>%
      mutate(
        week_num = row_number(),
        week_index = row_number(),
        week_label = paste0("G", formatC(week_index, width = 3, flag = "0")),
        week_source = "game_sequence"
      ) %>%
      select(game_id, first_row, week_num, week_index, week_label, week_source)
    return(week_tbl)
  }

  lookup_tbl <- lookup_raw %>%
    mutate(game_key = as.character(game_id))

  week_num_vec <- if ("week" %in% names(lookup_tbl)) {
    suppressWarnings(as.integer(as.character(lookup_tbl$week)))
  } else if ("nflfast_game_id" %in% names(lookup_tbl)) {
    extract_nflfast_week(lookup_tbl$nflfast_game_id)
  } else {
    rep(NA_integer_, nrow(lookup_tbl))
  }

  lookup_tbl <- lookup_tbl %>%
    mutate(week_num = week_num_vec) %>%
    group_by(game_key) %>%
    summarise(week_num = first(week_num[!is.na(week_num)]), .groups = "drop")

  merged <- games_base %>%
    left_join(lookup_tbl, by = "game_key") %>%
    arrange(first_row, game_id)

  if (all(is.na(merged$week_num))) {
    warning("Could not derive week numbers from ", lookup_path, "; falling back to game sequence.")
    week_tbl <- merged %>%
      mutate(
        week_num = row_number(),
        week_index = row_number(),
        week_label = paste0("G", formatC(week_index, width = 3, flag = "0")),
        week_source = "game_sequence"
      ) %>%
      select(game_id, first_row, week_num, week_index, week_label, week_source)
    return(week_tbl)
  }

  max_week <- suppressWarnings(max(merged$week_num, na.rm = TRUE))
  if (!is.finite(max_week)) {
    max_week <- 0L
  }
  missing_idx <- which(is.na(merged$week_num))
  if (length(missing_idx) > 0L) {
    merged$week_num[missing_idx] <- as.integer(max_week) + seq_along(missing_idx)
  }

  merged %>%
    mutate(
      week_num = as.integer(week_num),
      week_index = dense_rank(week_num),
      week_label = sprintf("%02d", week_num),
      week_source = "lookup"
    ) %>%
    select(game_id, first_row, week_num, week_index, week_label, week_source)
}

bootstrap_bt_weekly_path_win <- function(
  model_data,
  bt_cfg,
  fixed_lambda,
  n_boot,
  seed = 42L,
  workers = 1L,
  week_lookup_path = NULL
) {
  if (n_boot <= 0L) {
    return(tibble())
  }

  target_col <- bt_cfg$target_col
  assert_columns(
    model_data,
    c("game_id", "event_game_index", "rusher_name", "blocker_name", "double_team", target_col),
    "model_data"
  )

  model_data <- model_data %>%
    arrange(game_id, play_id, event_game_index)
  week_lookup <- build_game_week_lookup(model_data, week_lookup_path)
  model_data <- model_data %>%
    left_join(week_lookup %>% select(game_id, week_index, week_label, week_source), by = "game_id")

  levels_tbl <- get_bt_levels(model_data)
  rusher_levels <- levels_tbl$rusher_levels
  blocker_levels <- levels_tbl$blocker_levels

  alpha_local <- as.numeric(bt_cfg$alpha)
  include_double_team_local <- isTRUE(bt_cfg$include_double_team)
  standardize_local <- isTRUE(bt_cfg$standardize)
  fixed_lambda_local <- as.numeric(fixed_lambda)
  rating_scale_local <- as.numeric(bt_cfg$rating_scale)
  target_col_local <- target_col

  build_x_fn <- to_sparse_bt_matrix
  sample_fn <- sample_game_block_rows
  extract_coef_fn <- extract_glmnet_binomial_coefficients
  build_ratings_fn <- build_player_ratings_from_term_scores

  week_meta <- week_lookup %>%
    distinct(week_index, week_label, week_source, week_num) %>%
    arrange(week_index)

  out_list <- vector("list", nrow(week_meta))

  for (i in seq_len(nrow(week_meta))) {
    w <- week_meta$week_index[[i]]
    w_label <- week_meta$week_label[[i]]
    w_source <- week_meta$week_source[[i]]
    w_num <- week_meta$week_num[[i]]

    games_w <- week_lookup %>%
      filter(week_index <= w) %>%
      arrange(first_row) %>%
      pull(game_id)
    data_w <- model_data %>%
      filter(game_id %in% games_w)
    game_chunks_w <- split(data_w, data_w$game_id)

    x_obs <- build_x_fn(
      df = data_w,
      rusher_levels = rusher_levels,
      blocker_levels = blocker_levels,
      include_double_team = include_double_team_local
    )
    y_obs <- as.numeric(data_w[[target_col_local]])
    fit_obs <- glmnet(
      x = x_obs,
      y = y_obs,
      family = "binomial",
      alpha = alpha_local,
      lambda = fixed_lambda_local,
      standardize = standardize_local
    )
    observed_ratings <- extract_coef_fn(fit_obs, fixed_lambda_local) %>%
      filter(term != "(Intercept)") %>%
      transmute(term = term, term_score = coef) %>%
      build_ratings_fn(
        rating_scale = rating_scale_local,
        score_col_name = "bt_logit_score",
        source_label = "weekly_cumulative_observed_fit"
      ) %>%
      transmute(
        player_name = player_name,
        role = role,
        observed_score = bt_logit_score,
        observed_elo_like = elo_like_score
      )

    players_this_week <- model_data %>%
      filter(week_index == w) %>%
      transmute(player_name = rusher_name, role = "Rusher") %>%
      bind_rows(
        model_data %>%
          filter(week_index == w) %>%
          transmute(player_name = blocker_name, role = "Blocker")
      ) %>%
      distinct() %>%
      mutate(played_this_week = TRUE)

    draw_tbl <- parallel_map(
      iterable = seq_len(n_boot),
      workers = workers,
      seed = seed + as.integer(w) * 1000L,
      worker_fn = function(b) {
        boot_df <- sample_fn(data_w, games_w, game_chunks_w)
        x_boot <- build_x_fn(
          df = boot_df,
          rusher_levels = rusher_levels,
          blocker_levels = blocker_levels,
          include_double_team = include_double_team_local
        )
        y_boot <- as.numeric(boot_df[[target_col_local]])
        fit_boot <- glmnet(
          x = x_boot,
          y = y_boot,
          family = "binomial",
          alpha = alpha_local,
          lambda = fixed_lambda_local,
          standardize = standardize_local
        )

        extract_coef_fn(fit_boot, fixed_lambda_local) %>%
          filter(term != "(Intercept)") %>%
          transmute(term = term, term_score = coef) %>%
          build_ratings_fn(
            rating_scale = rating_scale_local,
            score_col_name = "bt_logit_score",
            source_label = "weekly_cumulative_bootstrap_fit"
          ) %>%
          transmute(
            player_name = player_name,
            role = role,
            bt_logit_score = bt_logit_score,
            elo_like_score = elo_like_score,
            iteration = b
          )
      }
    ) %>%
      bind_rows()

    out_list[[i]] <- draw_tbl %>%
      group_by(player_name, role) %>%
      summarise(
        mean_score = mean(bt_logit_score),
        sd_score = sd(bt_logit_score),
        q025 = quantile(bt_logit_score, 0.025),
        q50 = quantile(bt_logit_score, 0.50),
        q975 = quantile(bt_logit_score, 0.975),
        mean_elo_like = mean(elo_like_score),
        sd_elo_like = sd(elo_like_score),
        n_boot = n(),
        .groups = "drop"
      ) %>%
      left_join(observed_ratings, by = c("player_name", "role")) %>%
      left_join(players_this_week, by = c("player_name", "role")) %>%
      mutate(
        played_this_week = coalesce(played_this_week, FALSE),
        week_index = w,
        week_label = w_label,
        week_num = w_num,
        week_source = w_source,
        cumulative_games = length(games_w),
        cumulative_rows = nrow(data_w),
        iterations = n_boot
      )
  }

  bind_rows(out_list) %>%
    arrange(week_index, role, desc(mean_elo_like), player_name)
}

bootstrap_bt_weekly_path_severity <- function(
  model_data,
  bt_cfg,
  severity_weights,
  fixed_lambda,
  n_boot,
  seed = 42L,
  workers = 1L,
  week_lookup_path = NULL
) {
  if (n_boot <= 0L) {
    return(tibble())
  }

  target_col <- bt_cfg$target_col
  outcome_col <- bt_cfg$outcome_col
  class_levels <- bt_cfg$class_levels

  model_data <- ensure_severity_outcome_column(model_data, bt_cfg, severity_weights) %>%
    arrange(game_id, play_id, event_game_index)
  assert_columns(
    model_data,
    c("game_id", "event_game_index", "rusher_name", "blocker_name", "double_team", target_col, outcome_col),
    "model_data"
  )

  week_lookup <- build_game_week_lookup(model_data, week_lookup_path)
  model_data <- model_data %>%
    left_join(week_lookup %>% select(game_id, week_index, week_label, week_source), by = "game_id")

  levels_tbl <- get_bt_levels(model_data)
  rusher_levels <- levels_tbl$rusher_levels
  blocker_levels <- levels_tbl$blocker_levels

  alpha_local <- as.numeric(bt_cfg$alpha)
  include_double_team_local <- isTRUE(bt_cfg$include_double_team)
  standardize_local <- isTRUE(bt_cfg$standardize)
  fixed_lambda_local <- as.numeric(fixed_lambda)
  rating_scale_local <- as.numeric(bt_cfg$rating_scale)
  class_weights_local <- bt_cfg$class_weights
  outcome_col_local <- outcome_col
  class_levels_local <- class_levels

  build_x_fn <- to_sparse_bt_matrix
  sample_fn <- sample_game_block_rows
  get_weights_fn <- get_class_observation_weights
  extract_coef_fn <- extract_glmnet_multinomial_coefficients
  weighted_scores_fn <- severity_weighted_term_scores
  build_ratings_fn <- build_player_ratings_from_term_scores

  zero_term_scores <- tibble(
    term = c(paste0("rusher::", rusher_levels), paste0("blocker::", blocker_levels)),
    term_score = 0
  )

  fit_severity_ratings <- function(df, source_label) {
    y_chr <- as.character(df[[outcome_col_local]])
    present_classes <- intersect(class_levels_local, unique(y_chr[!is.na(y_chr)]))

    # Multinomial requires at least two observed classes in the sample.
    if (length(present_classes) < 2L) {
      return(
        build_ratings_fn(
          term_scores = zero_term_scores,
          rating_scale = rating_scale_local,
          score_col_name = "weighted_severity_logit_score",
          source_label = source_label
        )
      )
    }

    x_fit <- build_x_fn(
      df = df,
      rusher_levels = rusher_levels,
      blocker_levels = blocker_levels,
      include_double_team = include_double_team_local
    )
    y_fit <- factor(y_chr, levels = present_classes)
    fit_weights <- get_weights_fn(y_fit, class_weights_local[present_classes])

    fit <- tryCatch(
      glmnet(
        x = x_fit,
        y = y_fit,
        family = "multinomial",
        alpha = alpha_local,
        lambda = fixed_lambda_local,
        standardize = standardize_local,
        weights = fit_weights
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      return(
        build_ratings_fn(
          term_scores = zero_term_scores,
          rating_scale = rating_scale_local,
          score_col_name = "weighted_severity_logit_score",
          source_label = source_label
        )
      )
    }

    term_scores <- extract_coef_fn(fit, fixed_lambda_local, class_levels = present_classes) %>%
      weighted_scores_fn(severity_weights) %>%
      transmute(term = term, term_score = term_score)

    if (nrow(term_scores) == 0L) {
      term_scores <- zero_term_scores
    }

    build_ratings_fn(
      term_scores = term_scores,
      rating_scale = rating_scale_local,
      score_col_name = "weighted_severity_logit_score",
      source_label = source_label
    )
  }

  week_meta <- week_lookup %>%
    distinct(week_index, week_label, week_source, week_num) %>%
    arrange(week_index)

  out_list <- vector("list", nrow(week_meta))

  for (i in seq_len(nrow(week_meta))) {
    w <- week_meta$week_index[[i]]
    w_label <- week_meta$week_label[[i]]
    w_source <- week_meta$week_source[[i]]
    w_num <- week_meta$week_num[[i]]

    games_w <- week_lookup %>%
      filter(week_index <= w) %>%
      arrange(first_row) %>%
      pull(game_id)
    data_w <- model_data %>%
      filter(game_id %in% games_w)
    game_chunks_w <- split(data_w, data_w$game_id)

    observed_ratings <- fit_severity_ratings(
      df = data_w,
      source_label = "weekly_cumulative_observed_fit"
    ) %>%
      transmute(
        player_name = player_name,
        role = role,
        observed_score = weighted_severity_logit_score,
        observed_elo_like = elo_like_score
      )

    players_this_week <- model_data %>%
      filter(week_index == w) %>%
      transmute(player_name = rusher_name, role = "Rusher") %>%
      bind_rows(
        model_data %>%
          filter(week_index == w) %>%
          transmute(player_name = blocker_name, role = "Blocker")
      ) %>%
      distinct() %>%
      mutate(played_this_week = TRUE)

    draw_tbl <- parallel_map(
      iterable = seq_len(n_boot),
      workers = workers,
      seed = seed + as.integer(w) * 1000L,
      worker_fn = function(b) {
        boot_df <- sample_fn(data_w, games_w, game_chunks_w)
        fit_severity_ratings(
          df = boot_df,
          source_label = "weekly_cumulative_bootstrap_fit"
        ) %>%
          transmute(
            player_name = player_name,
            role = role,
            weighted_severity_logit_score = weighted_severity_logit_score,
            elo_like_score = elo_like_score,
            iteration = b
          )
      }
    ) %>%
      bind_rows()

    out_list[[i]] <- draw_tbl %>%
      group_by(player_name, role) %>%
      summarise(
        mean_score = mean(weighted_severity_logit_score),
        sd_score = sd(weighted_severity_logit_score),
        q025 = quantile(weighted_severity_logit_score, 0.025),
        q50 = quantile(weighted_severity_logit_score, 0.50),
        q975 = quantile(weighted_severity_logit_score, 0.975),
        mean_elo_like = mean(elo_like_score),
        sd_elo_like = sd(elo_like_score),
        n_boot = n(),
        .groups = "drop"
      ) %>%
      left_join(observed_ratings, by = c("player_name", "role")) %>%
      left_join(players_this_week, by = c("player_name", "role")) %>%
      mutate(
        played_this_week = coalesce(played_this_week, FALSE),
        week_index = w,
        week_label = w_label,
        week_num = w_num,
        week_source = w_source,
        cumulative_games = length(games_w),
        cumulative_rows = nrow(data_w),
        iterations = n_boot
      )
  }

  bind_rows(out_list) %>%
    arrange(week_index, role, desc(mean_elo_like), player_name)
}

bootstrap_bt_end_to_end_win <- function(
  model_data,
  bt_cfg,
  fixed_lambda,
  train_fraction,
  n_boot,
  seed = 42L,
  workers = 1L
) {
  if (n_boot <= 0L) {
    return(list(validation_summary = tibble(), rating_summary = tibble()))
  }

  target_col <- bt_cfg$target_col
  assert_columns(
    model_data,
    c("game_id", "event_game_index", "rusher_name", "blocker_name", "double_team", target_col),
    "model_data"
  )

  levels_tbl <- get_bt_levels(model_data)
  rusher_levels <- levels_tbl$rusher_levels
  blocker_levels <- levels_tbl$blocker_levels
  games <- unique(model_data$game_id)
  game_chunks <- split(model_data, model_data$game_id)

  target_col_local <- target_col
  alpha_local <- as.numeric(bt_cfg$alpha)
  include_double_team_local <- isTRUE(bt_cfg$include_double_team)
  standardize_local <- isTRUE(bt_cfg$standardize)
  fixed_lambda_local <- as.numeric(fixed_lambda)
  rating_scale_local <- as.numeric(bt_cfg$rating_scale)
  train_fraction_local <- as.numeric(train_fraction)
  baseline_cfg <- bt_cfg$matchup_baseline
  baseline_method_local <- if (is.null(baseline_cfg$method)) "logit_mean" else baseline_cfg$method
  baseline_prior_local <- if (is.null(baseline_cfg$prior_strength)) 25 else baseline_cfg$prior_strength

  split_fn <- split_train_test
  build_x_fn <- to_sparse_bt_matrix
  metric_fn <- compute_win_scalar_metrics
  sample_fn <- sample_game_block_rows
  baseline_fn <- build_win_baseline_predictions
  extract_coef_fn <- extract_glmnet_binomial_coefficients
  build_ratings_fn <- build_player_ratings_from_term_scores

  draws <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      boot_df <- sample_fn(model_data, games, game_chunks)
      split <- split_fn(boot_df, train_fraction = train_fraction_local)
      train_df <- split$train
      test_df <- split$test

      x_train <- build_x_fn(
        df = train_df,
        rusher_levels = rusher_levels,
        blocker_levels = blocker_levels,
        include_double_team = include_double_team_local
      )
      y_train <- as.numeric(train_df[[target_col_local]])

      fit <- glmnet(
        x = x_train,
        y = y_train,
        family = "binomial",
        alpha = alpha_local,
        lambda = fixed_lambda_local,
        standardize = standardize_local
      )

      x_test <- build_x_fn(
        df = test_df,
        rusher_levels = rusher_levels,
        blocker_levels = blocker_levels,
        include_double_team = include_double_team_local
      )
      pred <- as.numeric(predict(fit, newx = x_test, s = fixed_lambda_local, type = "response"))
      baseline_tbl <- baseline_fn(
        train_df = train_df,
        test_df = test_df,
        target_col = target_col_local,
        prior_strength = baseline_prior_local,
        method = baseline_method_local
      )

      metrics_global <- metric_fn(
        actual = as.numeric(test_df[[target_col_local]]),
        model_pred = pred,
        baseline_pred = baseline_tbl$baseline_global_prediction
      ) %>%
        mutate(
          baseline_name = "global_mean",
          baseline_matchup_method = baseline_tbl$baseline_matchup_method[[1]],
          baseline_prior_strength = baseline_tbl$baseline_prior_strength[[1]]
        )

      metrics_matchup <- metric_fn(
        actual = as.numeric(test_df[[target_col_local]]),
        model_pred = pred,
        baseline_pred = baseline_tbl$baseline_matchup_prediction
      ) %>%
        mutate(
          baseline_name = paste0("matchup_", baseline_tbl$baseline_matchup_method[[1]]),
          baseline_matchup_method = baseline_tbl$baseline_matchup_method[[1]],
          baseline_prior_strength = baseline_tbl$baseline_prior_strength[[1]]
        )

      metrics <- bind_rows(metrics_global, metrics_matchup) %>%
        mutate(
          iteration = b,
          n_train = nrow(train_df),
          n_test = nrow(test_df)
        )

      term_scores <- extract_coef_fn(fit, fixed_lambda_local) %>%
        filter(term != "(Intercept)") %>%
        transmute(term = term, term_score = coef)

      ratings <- build_ratings_fn(
        term_scores = term_scores,
        rating_scale = rating_scale_local,
        score_col_name = "bt_logit_score",
        source_label = "bootstrap_fixed_lambda_train_fit"
      ) %>%
        select(player_name, role, bt_logit_score, elo_like_score) %>%
        mutate(
          iteration = b,
          n_train = nrow(train_df),
          n_test = nrow(test_df)
        )

      list(metrics = metrics, ratings = ratings)
    }
  )

  metric_draws <- bind_rows(lapply(draws, function(x) x$metrics))
  rating_draws <- bind_rows(lapply(draws, function(x) x$ratings))

  validation_summary <- metric_draws %>%
    group_by(metric, baseline_name, baseline_matchup_method, baseline_prior_strength) %>%
    summarise(
      mode = "win",
      model_value_mean = mean(model_value),
      baseline_value_mean = mean(baseline_value),
      improvement_mean = mean(improvement),
      improvement_q025 = quantile(improvement, 0.025),
      improvement_q50 = quantile(improvement, 0.50),
      improvement_q975 = quantile(improvement, 0.975),
      train_rows_mean = mean(n_train),
      test_rows_mean = mean(n_test),
      iterations = n_boot,
      .groups = "drop"
    ) %>%
    select(
      mode,
      metric,
      baseline_name,
      baseline_matchup_method,
      baseline_prior_strength,
      model_value_mean,
      baseline_value_mean,
      improvement_mean,
      improvement_q025,
      improvement_q50,
      improvement_q975,
      train_rows_mean,
      test_rows_mean,
      iterations
    )

  rating_summary <- rating_draws %>%
    group_by(player_name, role) %>%
    summarise(
      mean_score = mean(bt_logit_score),
      sd_score = sd(bt_logit_score),
      q025 = quantile(bt_logit_score, 0.025),
      q50 = quantile(bt_logit_score, 0.50),
      q975 = quantile(bt_logit_score, 0.975),
      mean_elo_like = mean(elo_like_score),
      sd_elo_like = sd(elo_like_score),
      n_boot = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_elo_like))

  list(
    validation_summary = validation_summary,
    rating_summary = rating_summary
  )
}

bootstrap_bt_end_to_end_severity <- function(
  model_data,
  bt_cfg,
  severity_weights,
  fixed_lambda,
  train_fraction,
  n_boot,
  seed = 42L,
  workers = 1L
) {
  if (n_boot <= 0L) {
    return(list(validation_summary = tibble(), rating_summary = tibble()))
  }

  target_col <- bt_cfg$target_col
  outcome_col <- bt_cfg$outcome_col
  class_levels <- bt_cfg$class_levels

  model_data <- ensure_severity_outcome_column(model_data, bt_cfg, severity_weights)
  assert_columns(
    model_data,
    c("game_id", "event_game_index", "rusher_name", "blocker_name", "double_team", target_col, outcome_col),
    "model_data"
  )

  levels_tbl <- get_bt_levels(model_data)
  rusher_levels <- levels_tbl$rusher_levels
  blocker_levels <- levels_tbl$blocker_levels
  games <- unique(model_data$game_id)
  game_chunks <- split(model_data, model_data$game_id)

  alpha_local <- as.numeric(bt_cfg$alpha)
  include_double_team_local <- isTRUE(bt_cfg$include_double_team)
  standardize_local <- isTRUE(bt_cfg$standardize)
  fixed_lambda_local <- as.numeric(fixed_lambda)
  rating_scale_local <- as.numeric(bt_cfg$rating_scale)
  class_weights_local <- bt_cfg$class_weights
  train_fraction_local <- as.numeric(train_fraction)
  target_col_local <- target_col
  outcome_col_local <- outcome_col
  class_levels_local <- class_levels

  split_fn <- split_train_test
  build_x_fn <- to_sparse_bt_matrix
  sample_fn <- sample_game_block_rows
  get_weights_fn <- get_class_observation_weights
  prob_fn <- predict_bt_multinomial_probs
  expected_fn <- severity_prob_to_expected
  scalar_metric_fn <- compute_severity_scalar_metrics
  multiclass_metric_fn <- compute_multiclass_logloss_scalar
  extract_coef_fn <- extract_glmnet_multinomial_coefficients
  weighted_scores_fn <- severity_weighted_term_scores
  build_ratings_fn <- build_player_ratings_from_term_scores
  ensure_outcome_fn <- ensure_severity_outcome_column

  draws <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      boot_df <- sample_fn(model_data, games, game_chunks) %>%
        ensure_outcome_fn(bt_cfg, severity_weights)
      split <- split_fn(boot_df, train_fraction = train_fraction_local)
      train_df <- split$train
      test_df <- split$test

      x_train <- build_x_fn(
        df = train_df,
        rusher_levels = rusher_levels,
        blocker_levels = blocker_levels,
        include_double_team = include_double_team_local
      )
      y_train <- factor(train_df[[outcome_col_local]], levels = class_levels_local)
      obs_weights <- get_weights_fn(y_train, class_weights_local)

      fit <- glmnet(
        x = x_train,
        y = y_train,
        family = "multinomial",
        alpha = alpha_local,
        lambda = fixed_lambda_local,
        standardize = standardize_local,
        weights = obs_weights
      )

      x_test <- build_x_fn(
        df = test_df,
        rusher_levels = rusher_levels,
        blocker_levels = blocker_levels,
        include_double_team = include_double_team_local
      )
      prob_tbl <- prob_fn(
        cv_fit = fit,
        x_new = x_test,
        s = fixed_lambda_local,
        class_levels = class_levels_local
      )
      expected_pred <- expected_fn(prob_tbl, severity_weights)
      baseline_pred <- mean(train_df[[target_col_local]], na.rm = TRUE)

      scalar_metrics <- scalar_metric_fn(
        actual = as.numeric(test_df[[target_col_local]]),
        model_pred = expected_pred,
        baseline_pred = rep(baseline_pred, nrow(test_df))
      )

      train_class_counts <- table(y_train)
      baseline_class_prob <- setNames(rep(0, length(class_levels_local)), class_levels_local)
      baseline_class_prob[names(train_class_counts)] <- as.numeric(train_class_counts) / length(y_train)
      baseline_prob_tbl <- as_tibble(
        matrix(
          rep(as.numeric(baseline_class_prob[class_levels_local]), each = nrow(test_df)),
          ncol = length(class_levels_local),
          dimnames = list(NULL, class_levels_local)
        ),
        .name_repair = "minimal"
      )

      actual_class <- as.character(test_df[[outcome_col_local]])
      model_ll <- multiclass_metric_fn(actual_class, prob_tbl, class_levels_local)
      baseline_ll <- multiclass_metric_fn(actual_class, baseline_prob_tbl, class_levels_local)
      multiclass_metrics <- tibble(
        metric = "multiclass_logloss",
        model_value = model_ll,
        baseline_value = baseline_ll,
        improvement = baseline_ll - model_ll
      )

      metrics <- bind_rows(scalar_metrics, multiclass_metrics) %>%
        mutate(
          iteration = b,
          n_train = nrow(train_df),
          n_test = nrow(test_df)
        )

      term_scores <- extract_coef_fn(fit, fixed_lambda_local, class_levels = class_levels_local) %>%
        weighted_scores_fn(severity_weights) %>%
        transmute(term = term, term_score = term_score)

      ratings <- build_ratings_fn(
        term_scores = term_scores,
        rating_scale = rating_scale_local,
        score_col_name = "weighted_severity_logit_score",
        source_label = "bootstrap_fixed_lambda_train_fit"
      ) %>%
        select(player_name, role, weighted_severity_logit_score, elo_like_score) %>%
        mutate(
          iteration = b,
          n_train = nrow(train_df),
          n_test = nrow(test_df)
        )

      list(metrics = metrics, ratings = ratings)
    }
  )

  metric_draws <- bind_rows(lapply(draws, function(x) x$metrics))
  rating_draws <- bind_rows(lapply(draws, function(x) x$ratings))

  validation_summary <- metric_draws %>%
    mutate(mode = if_else(metric == "multiclass_logloss", "severity_multiclass", "severity")) %>%
    group_by(mode, metric) %>%
    summarise(
      model_value_mean = mean(model_value),
      baseline_value_mean = mean(baseline_value),
      improvement_mean = mean(improvement),
      improvement_q025 = quantile(improvement, 0.025),
      improvement_q50 = quantile(improvement, 0.50),
      improvement_q975 = quantile(improvement, 0.975),
      train_rows_mean = mean(n_train),
      test_rows_mean = mean(n_test),
      iterations = n_boot,
      .groups = "drop"
    ) %>%
    select(
      mode,
      metric,
      model_value_mean,
      baseline_value_mean,
      improvement_mean,
      improvement_q025,
      improvement_q50,
      improvement_q975,
      train_rows_mean,
      test_rows_mean,
      iterations
    )

  rating_summary <- rating_draws %>%
    group_by(player_name, role) %>%
    summarise(
      mean_score = mean(weighted_severity_logit_score),
      sd_score = sd(weighted_severity_logit_score),
      q025 = quantile(weighted_severity_logit_score, 0.025),
      q50 = quantile(weighted_severity_logit_score, 0.50),
      q975 = quantile(weighted_severity_logit_score, 0.975),
      mean_elo_like = mean(elo_like_score),
      sd_elo_like = sd(elo_like_score),
      n_boot = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_elo_like))

  list(
    validation_summary = validation_summary,
    rating_summary = rating_summary
  )
}

bootstrap_bt_player_rating_uncertainty_win <- function(model_data, bt_cfg, n_boot, seed = 42L, workers = 1L) {
  if (n_boot <= 0L) {
    return(tibble())
  }

  target_col <- bt_cfg$target_col
  assert_columns(model_data, c("game_id", "event_game_index", "rusher_name", "blocker_name", "double_team", target_col), "model_data")

  levels_tbl <- get_bt_levels(model_data)
  rusher_levels <- levels_tbl$rusher_levels
  blocker_levels <- levels_tbl$blocker_levels
  fit_full <- fit_bt_win_cv(
    train_df = model_data,
    bt_cfg = bt_cfg,
    rusher_levels = rusher_levels,
    blocker_levels = blocker_levels,
    seed = seed
  )
  lambda_choice <- if (identical(bt_cfg$lambda_selection, "lambda.1se")) fit_full$lambda_1se else fit_full$lambda_min

  games <- unique(model_data$game_id)
  game_chunks <- split(model_data, model_data$game_id)
  target_col_local <- target_col
  alpha_local <- as.numeric(bt_cfg$alpha)
  include_double_team_local <- isTRUE(bt_cfg$include_double_team)
  standardize_local <- isTRUE(bt_cfg$standardize)
  lambda_local <- lambda_choice
  rating_scale_local <- as.numeric(bt_cfg$rating_scale)

  build_x_fn <- to_sparse_bt_matrix
  extract_coef_fn <- extract_glmnet_binomial_coefficients
  build_ratings_fn <- build_player_ratings_from_term_scores

  draws <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      sampled_games <- sample(games, size = length(games), replace = TRUE)
      sampled_chunks <- vector("list", length(sampled_games))

      for (i in seq_along(sampled_games)) {
        g <- sampled_games[[i]]
        chunk <- game_chunks[[as.character(g)]]
        chunk$.bootstrap_order <- i
        sampled_chunks[[i]] <- chunk
      }

      boot_df <- bind_rows(sampled_chunks) %>%
        arrange(.bootstrap_order, event_game_index) %>%
        select(-.bootstrap_order)

      x_boot <- build_x_fn(
        df = boot_df,
        rusher_levels = rusher_levels,
        blocker_levels = blocker_levels,
        include_double_team = include_double_team_local
      )
      y_boot <- as.numeric(boot_df[[target_col_local]])

      fit <- glmnet(
        x = x_boot,
        y = y_boot,
        family = "binomial",
        alpha = alpha_local,
        lambda = lambda_local,
        standardize = standardize_local
      )

      term_scores <- extract_coef_fn(fit, lambda_local) %>%
        filter(term != "(Intercept)") %>%
        transmute(term = term, term_score = coef)

      build_ratings_fn(
        term_scores = term_scores,
        rating_scale = rating_scale_local,
        score_col_name = "bt_logit_score",
        source_label = "bootstrap_fixed_lambda"
      ) %>%
        select(player_name, role, bt_logit_score, elo_like_score) %>%
        mutate(iteration = b)
    }
  )

  bind_rows(draws) %>%
    group_by(player_name, role) %>%
    summarise(
      mean_score = mean(bt_logit_score),
      sd_score = sd(bt_logit_score),
      q025 = quantile(bt_logit_score, 0.025),
      q50 = quantile(bt_logit_score, 0.50),
      q975 = quantile(bt_logit_score, 0.975),
      mean_elo_like = mean(elo_like_score),
      sd_elo_like = sd(elo_like_score),
      n_boot = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_elo_like))
}

bootstrap_bt_player_rating_uncertainty_severity <- function(model_data, bt_cfg, severity_weights, n_boot, seed = 42L, workers = 1L) {
  if (n_boot <= 0L) {
    return(tibble())
  }

  target_col <- bt_cfg$target_col
  outcome_col <- bt_cfg$outcome_col
  class_levels <- bt_cfg$class_levels

  model_data <- ensure_severity_outcome_column(model_data, bt_cfg, severity_weights)
  assert_columns(model_data, c("game_id", "event_game_index", "rusher_name", "blocker_name", "double_team", target_col, outcome_col), "model_data")

  levels_tbl <- get_bt_levels(model_data)
  rusher_levels <- levels_tbl$rusher_levels
  blocker_levels <- levels_tbl$blocker_levels

  fit_full <- fit_bt_severity_cv(
    train_df = model_data,
    bt_cfg = bt_cfg,
    severity_weights = severity_weights,
    rusher_levels = rusher_levels,
    blocker_levels = blocker_levels,
    seed = seed
  )
  lambda_choice <- if (identical(bt_cfg$lambda_selection, "lambda.1se")) fit_full$lambda_1se else fit_full$lambda_min

  games <- unique(model_data$game_id)
  game_chunks <- split(model_data, model_data$game_id)
  alpha_local <- as.numeric(bt_cfg$alpha)
  include_double_team_local <- isTRUE(bt_cfg$include_double_team)
  standardize_local <- isTRUE(bt_cfg$standardize)
  lambda_local <- lambda_choice
  rating_scale_local <- as.numeric(bt_cfg$rating_scale)
  class_weights_local <- bt_cfg$class_weights

  build_x_fn <- to_sparse_bt_matrix
  get_obs_weights_fn <- get_class_observation_weights
  extract_multi_coef_fn <- extract_glmnet_multinomial_coefficients
  weighted_scores_fn <- severity_weighted_term_scores
  build_ratings_fn <- build_player_ratings_from_term_scores
  ensure_outcome_fn <- ensure_severity_outcome_column

  draws <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      sampled_games <- sample(games, size = length(games), replace = TRUE)
      sampled_chunks <- vector("list", length(sampled_games))

      for (i in seq_along(sampled_games)) {
        g <- sampled_games[[i]]
        chunk <- game_chunks[[as.character(g)]]
        chunk$.bootstrap_order <- i
        sampled_chunks[[i]] <- chunk
      }

      boot_df <- bind_rows(sampled_chunks) %>%
        arrange(.bootstrap_order, event_game_index) %>%
        select(-.bootstrap_order)

      boot_df <- ensure_outcome_fn(boot_df, bt_cfg, severity_weights)
      y_boot <- factor(boot_df[[outcome_col]], levels = class_levels)
      obs_weights <- get_obs_weights_fn(y_boot, class_weights_local)

      x_boot <- build_x_fn(
        df = boot_df,
        rusher_levels = rusher_levels,
        blocker_levels = blocker_levels,
        include_double_team = include_double_team_local
      )

      fit <- glmnet(
        x = x_boot,
        y = y_boot,
        family = "multinomial",
        alpha = alpha_local,
        lambda = lambda_local,
        standardize = standardize_local,
        weights = obs_weights
      )

      term_scores <- extract_multi_coef_fn(fit, lambda_local, class_levels = class_levels) %>%
        weighted_scores_fn(severity_weights) %>%
        transmute(term = term, term_score = term_score)

      build_ratings_fn(
        term_scores = term_scores,
        rating_scale = rating_scale_local,
        score_col_name = "weighted_severity_logit_score",
        source_label = "bootstrap_fixed_lambda"
      ) %>%
        select(player_name, role, weighted_severity_logit_score, elo_like_score) %>%
        mutate(iteration = b)
    }
  )

  bind_rows(draws) %>%
    group_by(player_name, role) %>%
    summarise(
      mean_score = mean(weighted_severity_logit_score),
      sd_score = sd(weighted_severity_logit_score),
      q025 = quantile(weighted_severity_logit_score, 0.025),
      q50 = quantile(weighted_severity_logit_score, 0.50),
      q975 = quantile(weighted_severity_logit_score, 0.975),
      mean_elo_like = mean(elo_like_score),
      sd_elo_like = sd(elo_like_score),
      n_boot = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_elo_like))
}

adaptive_k_provisional <- function(n, k_start, k_min, n_provisional, n_decay) {
  ifelse(
    n <= n_provisional,
    k_start,
    k_min + (k_start - k_min) * exp(-(n - n_provisional) / n_decay)
  )
}

compute_model_k <- function(interaction_count, model_cfg) {
  if (isTRUE(model_cfg$use_adaptive_k)) {
    adaptive_k_provisional(
      n = interaction_count,
      k_start = model_cfg$adaptive_k$k_start,
      k_min = model_cfg$adaptive_k$k_min,
      n_provisional = model_cfg$adaptive_k$n_provisional,
      n_decay = model_cfg$adaptive_k$n_decay
    )
  } else {
    rep(model_cfg$constant_k, length(interaction_count))
  }
}

fit_elo_model <- function(model_data, model_cfg) {
  target_col <- model_cfg$target_col
  assert_columns(model_data, c("rusher_name", "blocker_name", target_col, "rusher_row", "blocker_row"), "model_data")

  history <- model_data %>%
    arrange(game_id, play_id, event_game_index) %>%
    mutate(
      rusher_k = compute_model_k(rusher_row, model_cfg),
      blocker_k = compute_model_k(blocker_row, model_cfg),
      before_rusher_elo = NA_real_,
      before_blocker_elo = NA_real_,
      after_rusher_elo = NA_real_,
      after_blocker_elo = NA_real_,
      model_prediction = NA_real_
    )

  elo_vec <- numeric(0)

  get_elo <- function(player_id, default_elo) {
    key <- as.character(player_id)
    if (!key %in% names(elo_vec)) {
      elo_vec[[key]] <<- default_elo
    }
    unname(elo_vec[[key]])
  }

  for (i in seq_len(nrow(history))) {
    r_id <- history$rusher_name[[i]]
    b_id <- history$blocker_name[[i]]
    y <- history[[target_col]][[i]]
    if (is.na(r_id) || is.na(b_id) || is.na(y)) {
      next
    }

    r_elo <- get_elo(r_id, model_cfg$rusher_start_elo)
    b_elo <- get_elo(b_id, model_cfg$blocker_start_elo)

    bonus <- 0
    if (isTRUE(model_cfg$use_double_team_bonus) && "double_team" %in% names(history)) {
      is_double <- !is.na(history$double_team[[i]]) && history$double_team[[i]] == 1
      bonus <- ifelse(is_double, model_cfg$double_team_bonus, 0)
    }

    expected_rusher <- 1 / (1 + 10^(((b_elo + bonus) - r_elo) / model_cfg$scale))

    r_k <- history$rusher_k[[i]]
    b_k <- history$blocker_k[[i]]

    new_r <- r_elo + r_k * (y - expected_rusher)
    new_b <- b_elo + b_k * ((1 - y) - (1 - expected_rusher))

    history$before_rusher_elo[[i]] <- r_elo
    history$before_blocker_elo[[i]] <- b_elo
    history$model_prediction[[i]] <- expected_rusher
    history$after_rusher_elo[[i]] <- new_r
    history$after_blocker_elo[[i]] <- new_b

    elo_vec[[as.character(r_id)]] <- new_r
    elo_vec[[as.character(b_id)]] <- new_b
  }

  player_roles <- bind_rows(
    history %>% transmute(player_name = rusher_name, role = "Rusher"),
    history %>% transmute(player_name = blocker_name, role = "Blocker")
  ) %>%
    group_by(player_name) %>%
    summarise(
      role = if_else(n_distinct(role) == 1L, first(role), "Both"),
      .groups = "drop"
    )

  final_ratings <- tibble(
    player_name = names(elo_vec),
    final_elo = as.numeric(elo_vec)
  ) %>%
    left_join(player_roles, by = "player_name") %>%
    arrange(desc(final_elo))

  list(history = history, final_ratings = final_ratings)
}

split_train_test <- function(df, train_fraction = 0.80) {
  n <- nrow(df)
  n_train <- floor(train_fraction * n)
  if (n_train <= 0 || n_train >= n) {
    stop("Invalid train/test split. n=", n, ", train_fraction=", train_fraction)
  }

  list(
    n_total = n,
    n_train = n_train,
    n_test = n - n_train,
    train = df[seq_len(n_train), , drop = FALSE],
    test = df[(n_train + 1L):n, , drop = FALSE]
  )
}

score_holdout <- function(history, model_cfg, train_fraction = 0.80) {
  target_col <- model_cfg$target_col
  assert_columns(history, c("rusher_name", "blocker_name", target_col, "after_rusher_elo", "after_blocker_elo"), "history")

  split <- split_train_test(history, train_fraction)
  train_df <- split$train
  test_df <- split$test

  final_rusher_elo <- train_df %>%
    group_by(rusher_name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(rusher_name, after_rusher_elo) %>%
    rename(rusher_elo_frozen = after_rusher_elo)

  final_blocker_elo <- train_df %>%
    group_by(blocker_name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(blocker_name, after_blocker_elo) %>%
    rename(blocker_elo_frozen = after_blocker_elo)

  baseline_pred <- mean(train_df[[target_col]], na.rm = TRUE)

  scored <- test_df %>%
    left_join(final_rusher_elo, by = "rusher_name") %>%
    left_join(final_blocker_elo, by = "blocker_name") %>%
    mutate(
      holdout_bonus = if (
        isTRUE(model_cfg$use_double_team_bonus) && "double_team" %in% names(test_df)
      ) {
        if_else(dplyr::coalesce(double_team, 0) == 1, model_cfg$double_team_bonus, 0)
      } else {
        0
      },
      frozen_model_prediction = 1 / (1 + 10^(((blocker_elo_frozen + holdout_bonus) - rusher_elo_frozen) / model_cfg$scale)),
      baseline_prediction = baseline_pred,
      actual_target = .data[[target_col]],
      target_name = target_col
    )

  coverage <- mean(!is.na(scored$frozen_model_prediction))

  list(
    scored = scored,
    baseline_prediction = baseline_pred,
    coverage = coverage,
    n_test = nrow(test_df),
    n_scored = sum(!is.na(scored$frozen_model_prediction))
  )
}

clamp_probability <- function(x, eps = 1e-12) {
  pmin(pmax(as.numeric(x), eps), 1 - eps)
}

normalize_win_baseline_method <- function(method) {
  method_raw <- tolower(trimws(as.character(method)))
  if (method_raw %in% c("arithmetic", "arithmetic_mean", "mean")) {
    return("arithmetic_mean")
  }
  if (method_raw %in% c("geometric", "geometric_mean", "gmean")) {
    return("geometric_mean")
  }
  if (method_raw %in% c("logit", "logit_mean", "odds_mean")) {
    return("logit_mean")
  }
  warning("Unknown win matchup baseline method '", method, "'. Falling back to 'logit_mean'.")
  "logit_mean"
}

combine_win_probabilities <- function(p_left, p_right, method = "logit_mean") {
  method_norm <- normalize_win_baseline_method(method)
  p_left <- clamp_probability(p_left, eps = 1e-6)
  p_right <- clamp_probability(p_right, eps = 1e-6)

  if (identical(method_norm, "arithmetic_mean")) {
    return((p_left + p_right) / 2)
  }
  if (identical(method_norm, "geometric_mean")) {
    return(sqrt(p_left * p_right))
  }
  plogis((qlogis(p_left) + qlogis(p_right)) / 2)
}

build_win_baseline_predictions <- function(train_df, test_df, target_col = "win_target", prior_strength = 25, method = "logit_mean") {
  assert_columns(train_df, c("rusher_name", "blocker_name", target_col), "train_df")
  assert_columns(test_df, c("rusher_name", "blocker_name"), "test_df")

  prior_strength <- suppressWarnings(as.numeric(prior_strength))
  if (is.na(prior_strength) || prior_strength <= 0) {
    prior_strength <- 25
  }
  method_norm <- normalize_win_baseline_method(method)

  global_mean <- mean(train_df[[target_col]], na.rm = TRUE)
  if (is.na(global_mean) || global_mean <= 0 || global_mean >= 1) {
    global_mean <- clamp_probability(global_mean, eps = 1e-4)
  }

  rusher_tbl <- train_df %>%
    group_by(rusher_name) %>%
    summarise(
      n_rusher = n(),
      rusher_win_rate = mean(.data[[target_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rusher_win_rate_smoothed = (n_rusher * rusher_win_rate + prior_strength * global_mean) /
        (n_rusher + prior_strength)
    )

  blocker_tbl <- train_df %>%
    group_by(blocker_name) %>%
    summarise(
      n_blocker = n(),
      rusher_win_rate_allowed = mean(.data[[target_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rusher_win_rate_allowed_smoothed = (n_blocker * rusher_win_rate_allowed + prior_strength * global_mean) /
        (n_blocker + prior_strength)
    )

  out <- test_df %>%
    select(rusher_name, blocker_name) %>%
    left_join(
      rusher_tbl %>% select(rusher_name, n_rusher, rusher_win_rate_smoothed),
      by = "rusher_name"
    ) %>%
    left_join(
      blocker_tbl %>% select(blocker_name, n_blocker, rusher_win_rate_allowed_smoothed),
      by = "blocker_name"
    ) %>%
    mutate(
      baseline_global_prediction = global_mean,
      baseline_rusher_component = coalesce(rusher_win_rate_smoothed, global_mean),
      baseline_blocker_component = coalesce(rusher_win_rate_allowed_smoothed, global_mean),
      baseline_matchup_prediction = combine_win_probabilities(
        baseline_rusher_component,
        baseline_blocker_component,
        method = method_norm
      ),
      baseline_matchup_method = method_norm,
      baseline_prior_strength = prior_strength,
      baseline_rusher_train_interactions = coalesce(n_rusher, 0L),
      baseline_blocker_train_interactions = coalesce(n_blocker, 0L)
    ) %>%
    select(
      baseline_global_prediction,
      baseline_matchup_prediction,
      baseline_rusher_component,
      baseline_blocker_component,
      baseline_matchup_method,
      baseline_prior_strength,
      baseline_rusher_train_interactions,
      baseline_blocker_train_interactions
    )

  out
}

compute_win_scalar_metrics <- function(actual, model_pred, baseline_pred) {
  eps <- 1e-12
  model_prob <- pmin(pmax(model_pred, eps), 1 - eps)
  baseline_prob <- pmin(pmax(baseline_pred, eps), 1 - eps)

  model_brier <- mean((model_prob - actual)^2)
  baseline_brier <- mean((baseline_prob - actual)^2)

  model_logloss <- -mean(actual * log(model_prob) + (1 - actual) * log(1 - model_prob))
  baseline_logloss <- -mean(actual * log(baseline_prob) + (1 - actual) * log(1 - baseline_prob))

  data.frame(
    metric = c("brier", "logloss"),
    model_value = c(model_brier, model_logloss),
    baseline_value = c(baseline_brier, baseline_logloss),
    improvement = c(baseline_brier - model_brier, baseline_logloss - model_logloss),
    stringsAsFactors = FALSE
  )
}

compute_severity_scalar_metrics <- function(actual, model_pred, baseline_pred) {
  model_mse <- mean((model_pred - actual)^2)
  baseline_mse <- mean((baseline_pred - actual)^2)

  model_rmse <- sqrt(model_mse)
  baseline_rmse <- sqrt(baseline_mse)

  model_mae <- mean(abs(model_pred - actual))
  baseline_mae <- mean(abs(baseline_pred - actual))

  data.frame(
    metric = c("mse", "rmse", "mae"),
    model_value = c(model_mse, model_rmse, model_mae),
    baseline_value = c(baseline_mse, baseline_rmse, baseline_mae),
    improvement = c(
      baseline_mse - model_mse,
      baseline_rmse - model_rmse,
      baseline_mae - model_mae
    ),
    stringsAsFactors = FALSE
  )
}

compute_validation_metrics <- function(scored_df, mode, baseline_col = "baseline_prediction", baseline_name = NULL) {
  if (is.null(baseline_name)) {
    baseline_name <- baseline_col
  }
  if (!baseline_col %in% names(scored_df)) {
    stop("Baseline column '", baseline_col, "' not found in scored_df.")
  }

  eval_df <- scored_df %>%
    filter(!is.na(actual_target), !is.na(frozen_model_prediction), !is.na(.data[[baseline_col]]))

  if (nrow(eval_df) == 0) {
    stop("No rows available for validation after filtering missing predictions.")
  }

  if (identical(mode, "win")) {
    metrics <- compute_win_scalar_metrics(
      actual = eval_df$actual_target,
      model_pred = eval_df$frozen_model_prediction,
      baseline_pred = eval_df[[baseline_col]]
    )
  } else if (identical(mode, "severity")) {
    metrics <- compute_severity_scalar_metrics(
      actual = eval_df$actual_target,
      model_pred = eval_df$frozen_model_prediction,
      baseline_pred = eval_df[[baseline_col]]
    )
  } else {
    stop("Unknown mode '", mode, "'. Use 'win' or 'severity'.")
  }

  metrics %>%
    mutate(
      mode = mode,
      baseline_name = baseline_name,
      n_rows = nrow(eval_df)
    ) %>%
    select(mode, metric, baseline_name, model_value, baseline_value, improvement, n_rows)
}

bootstrap_validation_uncertainty <- function(scored_df, mode, n_boot, seed = 42L, workers = 1L, bootstrap_unit = "game") {
  eval_df <- scored_df %>%
    filter(!is.na(actual_target), !is.na(frozen_model_prediction), !is.na(baseline_prediction))

  if (nrow(eval_df) == 0) {
    stop("No rows available for bootstrap validation.")
  }

  n <- nrow(eval_df)
  metric_fn <- if (identical(mode, "win")) compute_win_scalar_metrics else compute_severity_scalar_metrics
  bootstrap_unit <- tolower(as.character(bootstrap_unit))
  if (!bootstrap_unit %in% c("game", "row")) {
    stop("bootstrap_unit must be 'game' or 'row'.")
  }

  use_game_blocks <- identical(bootstrap_unit, "game")
  if (use_game_blocks && !"game_id" %in% names(eval_df)) {
    warning("bootstrap_unit='game' requested but game_id is missing; falling back to row bootstrap.")
    use_game_blocks <- FALSE
  }

  if (use_game_blocks) {
    games <- unique(eval_df$game_id)
    game_row_index <- split(seq_len(nrow(eval_df)), eval_df$game_id)
  }

  boot_rows <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      idx <- if (use_game_blocks) {
        sampled_games <- sample(games, size = length(games), replace = TRUE)
        unlist(game_row_index[as.character(sampled_games)], use.names = FALSE)
      } else {
        sample.int(n = n, size = n, replace = TRUE)
      }
      boot_df <- eval_df[idx, , drop = FALSE]

      boot_metrics <- metric_fn(
        actual = boot_df$actual_target,
        model_pred = boot_df$frozen_model_prediction,
        baseline_pred = boot_df$baseline_prediction
      )

      boot_metrics %>% mutate(iteration = b)
    }
  )

  boot_tbl <- bind_rows(boot_rows)

  boot_tbl %>%
    group_by(metric) %>%
    summarise(
      mode = mode,
      model_value_mean = mean(model_value),
      baseline_value_mean = mean(baseline_value),
      improvement_mean = mean(improvement),
      improvement_q025 = quantile(improvement, 0.025),
      improvement_q50 = quantile(improvement, 0.50),
      improvement_q975 = quantile(improvement, 0.975),
      iterations = n_boot,
      .groups = "drop"
    ) %>%
    select(
      mode,
      metric,
      model_value_mean,
      baseline_value_mean,
      improvement_mean,
      improvement_q025,
      improvement_q50,
      improvement_q975,
      iterations
    )
}

bootstrap_multiclass_validation_uncertainty <- function(
  scored_df,
  class_levels,
  n_boot,
  seed = 42L,
  workers = 1L,
  bootstrap_unit = "game",
  model_prob_prefix = "prob_lambda_min_",
  baseline_prob_prefix = "baseline_prob_",
  mode = "severity_multiclass"
) {
  model_prob_cols <- paste0(model_prob_prefix, class_levels)
  baseline_prob_cols <- paste0(baseline_prob_prefix, class_levels)
  required_cols <- c("actual_class", model_prob_cols, baseline_prob_cols)
  assert_columns(scored_df, required_cols, "scored_df")

  eval_df <- scored_df %>%
    filter(!is.na(actual_class))

  if (nrow(eval_df) == 0) {
    stop("No rows available for multiclass bootstrap validation.")
  }

  keep_rows <- complete.cases(eval_df[, required_cols, drop = FALSE])
  eval_df <- eval_df[keep_rows, , drop = FALSE]

  if (nrow(eval_df) == 0) {
    stop("No complete rows available for multiclass bootstrap validation.")
  }

  bootstrap_unit <- tolower(as.character(bootstrap_unit))
  if (!bootstrap_unit %in% c("game", "row")) {
    stop("bootstrap_unit must be 'game' or 'row'.")
  }

  use_game_blocks <- identical(bootstrap_unit, "game")
  if (use_game_blocks && !"game_id" %in% names(eval_df)) {
    warning("bootstrap_unit='game' requested but game_id is missing; falling back to row bootstrap.")
    use_game_blocks <- FALSE
  }

  n <- nrow(eval_df)
  if (use_game_blocks) {
    games <- unique(eval_df$game_id)
    game_row_index <- split(seq_len(nrow(eval_df)), eval_df$game_id)
  }

  model_prob_cols_local <- model_prob_cols
  baseline_prob_cols_local <- baseline_prob_cols
  class_levels_local <- class_levels
  multiclass_fn <- compute_multiclass_logloss_scalar

  boot_rows <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      idx <- if (use_game_blocks) {
        sampled_games <- sample(games, size = length(games), replace = TRUE)
        unlist(game_row_index[as.character(sampled_games)], use.names = FALSE)
      } else {
        sample.int(n = n, size = n, replace = TRUE)
      }

      boot_df <- eval_df[idx, , drop = FALSE]
      model_prob_tbl <- boot_df[, model_prob_cols_local, drop = FALSE]
      baseline_prob_tbl <- boot_df[, baseline_prob_cols_local, drop = FALSE]
      colnames(model_prob_tbl) <- class_levels_local
      colnames(baseline_prob_tbl) <- class_levels_local

      model_ll <- multiclass_fn(
        actual_class = boot_df$actual_class,
        prob_tbl = model_prob_tbl,
        class_levels = class_levels_local
      )
      baseline_ll <- multiclass_fn(
        actual_class = boot_df$actual_class,
        prob_tbl = baseline_prob_tbl,
        class_levels = class_levels_local
      )

      tibble(
        metric = "multiclass_logloss",
        model_value = model_ll,
        baseline_value = baseline_ll,
        improvement = baseline_ll - model_ll,
        iteration = b
      )
    }
  )

  boot_tbl <- bind_rows(boot_rows)

  boot_tbl %>%
    group_by(metric) %>%
    summarise(
      mode = mode,
      model_value_mean = mean(model_value),
      baseline_value_mean = mean(baseline_value),
      improvement_mean = mean(improvement),
      improvement_q025 = quantile(improvement, 0.025),
      improvement_q50 = quantile(improvement, 0.50),
      improvement_q975 = quantile(improvement, 0.975),
      iterations = n_boot,
      .groups = "drop"
    ) %>%
    select(
      mode,
      metric,
      model_value_mean,
      baseline_value_mean,
      improvement_mean,
      improvement_q025,
      improvement_q50,
      improvement_q975,
      iterations
    )
}

bootstrap_player_rating_uncertainty <- function(model_data, model_cfg, n_boot, seed = 42L, workers = 1L) {
  if (n_boot <= 0) {
    return(tibble())
  }

  assert_columns(model_data, c("game_id", "event_game_index", "rusher_name", "blocker_name"), "model_data")

  games <- unique(model_data$game_id)
  game_chunks <- split(model_data, model_data$game_id)
  add_idx_fn <- add_interaction_indices
  fit_fn <- fit_elo_model

  draws <- parallel_map(
    iterable = seq_len(n_boot),
    workers = workers,
    seed = seed,
    worker_fn = function(b) {
      sampled_games <- sample(games, size = length(games), replace = TRUE)
      sampled_chunks <- vector("list", length(sampled_games))

      for (i in seq_along(sampled_games)) {
        g <- sampled_games[[i]]
        chunk <- game_chunks[[as.character(g)]]
        chunk$.bootstrap_order <- i
        sampled_chunks[[i]] <- chunk
      }

      boot_df <- bind_rows(sampled_chunks) %>%
        arrange(.bootstrap_order, event_game_index) %>%
        select(-.bootstrap_order) %>%
        add_idx_fn()

      fit <- fit_fn(boot_df, model_cfg)

      fit$final_ratings %>%
        select(player_name, role, final_elo) %>%
        mutate(iteration = b)
    }
  )

  bind_rows(draws) %>%
    group_by(player_name, role) %>%
    summarise(
      mean_final_elo = mean(final_elo),
      sd_final_elo = sd(final_elo),
      q025 = quantile(final_elo, 0.025),
      q50 = quantile(final_elo, 0.50),
      q975 = quantile(final_elo, 0.975),
      n_boot = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_final_elo))
}

read_modeling_table <- function(config) {
  read_csv(config$output_paths$modeling_table, show_col_types = FALSE)
}

write_output_csv <- function(df, path) {
  ensure_directory(dirname(path))
  write_csv(df, path)
  invisible(path)
}
