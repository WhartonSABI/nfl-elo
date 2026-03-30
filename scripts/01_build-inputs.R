locate_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, ".git")) && dir.exists(file.path(current, "scripts"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate project root.")
    }
    current <- parent
  }
}

project_root <- locate_project_root()
source(file.path(project_root, "scripts", "00_config.R"))
source(file.path(project_root, "scripts", "00_utils.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

ensure_directory(dirname(PIPELINE_CONFIG$input_paths$matchups_table))

resolve_raw_events_file <- function(config) {
  candidates <- c(
    config$raw_input_paths$freeze_frames,
    config$raw_input_paths$events_freeze_frames
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    stop("Could not find raw events+freeze-frame file. Checked: ", paste(candidates, collapse = ", "))
  }
  existing[[1]]
}

raw_file <- resolve_raw_events_file(PIPELINE_CONFIG)
roster_file <- PIPELINE_CONFIG$raw_input_paths$roster

if (!file.exists(roster_file)) {
  stop("Roster file not found: ", roster_file)
}

sample_rows <- suppressWarnings(as.integer(Sys.getenv("RAW_SAMPLE_ROWS", unset = NA_character_)))
n_max_rows <- if (!is.na(sample_rows) && sample_rows > 0L) sample_rows else Inf

message("Reading roster: ", roster_file)
roster <- read_csv(roster_file, show_col_types = FALSE) %>%
  transmute(
    player_id = suppressWarnings(as.numeric(player_id)),
    player_name = as.character(player_name),
    team_position_name = as.character(team_position_name)
  )

position_by_id <- roster %>%
  filter(!is.na(player_id), !is.na(team_position_name)) %>%
  group_by(player_id) %>%
  summarise(position = first(team_position_name), .groups = "drop")

position_by_name <- roster %>%
  filter(!is.na(player_name), !is.na(team_position_name), player_name != "") %>%
  group_by(player_name) %>%
  summarise(position = first(team_position_name), .groups = "drop")

message("Inspecting raw header: ", raw_file)
raw_header <- names(read_csv(raw_file, n_max = 0, show_col_types = FALSE))

required_raw_cols <- c(
  "freeze_frame_player",
  "event_player_name",
  "opponent_player_name",
  "event_uuid",
  "game_id",
  "event_game_index",
  "play_uuid",
  "event_types",
  "player_id",
  "opponent_player_id",
  "event_penalty_type",
  "time_since_snap",
  "freeze_frame_x",
  "freeze_frame_y"
)
optional_raw_cols <- c(
  "nflfast_game_id",
  "season",
  "week",
  "game_type"
)
raw_read_cols <- unique(c(required_raw_cols, optional_raw_cols))

missing_cols <- setdiff(required_raw_cols, raw_header)
if (length(missing_cols) > 0L) {
  stop("Raw file is missing required columns: ", paste(missing_cols, collapse = ", "))
}

raw_col_types <- cols(
  freeze_frame_player = col_character(),
  event_player_name = col_character(),
  opponent_player_name = col_character(),
  event_uuid = col_character(),
  game_id = col_double(),
  event_game_index = col_double(),
  play_uuid = col_character(),
  event_types = col_character(),
  player_id = col_double(),
  opponent_player_id = col_double(),
  event_penalty_type = col_character(),
  time_since_snap = col_double(),
  freeze_frame_x = col_double(),
  freeze_frame_y = col_double(),
  nflfast_game_id = col_character(),
  season = col_integer(),
  week = col_integer(),
  game_type = col_character()
)

message("Reading raw events+freeze-frame table (selected columns only)...")
raw <- read_csv(
  raw_file,
  col_select = any_of(raw_read_cols),
  col_types = raw_col_types,
  n_max = n_max_rows,
  show_col_types = FALSE
)

message("Raw rows loaded: ", nrow(raw))

extract_nflfast_week_local <- function(nflfast_game_id) {
  x <- as.character(nflfast_game_id)
  ok <- grepl("^[0-9]{4}_[0-9]{2}_", x)
  out <- rep(NA_integer_, length(x))
  out[ok] <- suppressWarnings(as.integer(sub("^[0-9]{4}_([0-9]{2})_.*$", "\\1", x[ok])))
  out
}

first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (is.character(x)) {
    x <- x[trimws(x) != ""]
  }
  if (length(x) == 0L) {
    return(NA)
  }
  x[[1]]
}

if ("nflfast_game_id" %in% names(raw)) {
  raw$nflfast_game_id <- if_else(
    is.na(raw$nflfast_game_id) | trimws(raw$nflfast_game_id) == "",
    NA_character_,
    raw$nflfast_game_id
  )
} else {
  raw$nflfast_game_id <- NA_character_
}

if (!"week" %in% names(raw)) {
  raw$week <- NA_integer_
}

if (!"game_type" %in% names(raw)) {
  raw$game_type <- NA_character_
}

if (!"season" %in% names(raw)) {
  raw$season <- NA_integer_
}

normalize_game_lookup_tbl <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(tibble(game_id = numeric(), nflfast_game_id = character(), season = integer(), week = integer(), game_type = character()))
  }
  df <- drop_index_columns(df)
  if (!"game_id" %in% names(df)) {
    return(tibble(game_id = numeric(), nflfast_game_id = character(), season = integer(), week = integer(), game_type = character()))
  }

  out <- df %>%
    transmute(
      game_id = as.numeric(game_id),
      nflfast_game_id = if ("nflfast_game_id" %in% names(df)) as.character(nflfast_game_id) else NA_character_,
      season = if ("season" %in% names(df)) suppressWarnings(as.integer(season)) else NA_integer_,
      week = if ("week" %in% names(df)) suppressWarnings(as.integer(week)) else NA_integer_,
      game_type = if ("game_type" %in% names(df)) {
        if_else(is.na(game_type) | trimws(as.character(game_type)) == "", NA_character_, str_to_upper(trimws(as.character(game_type))))
      } else {
        NA_character_
      }
    ) %>%
    group_by(game_id) %>%
    summarise(
      nflfast_game_id = first_non_missing(nflfast_game_id),
      season = suppressWarnings(as.integer(first_non_missing(season))),
      week = suppressWarnings(as.integer(first_non_missing(week))),
      game_type = first_non_missing(game_type),
      .groups = "drop"
    ) %>%
    mutate(
      week = coalesce(week, extract_nflfast_week_local(nflfast_game_id)),
      game_type = case_when(
        !is.na(game_type) ~ game_type,
        !is.na(week) & week <= 18L ~ "REG",
        !is.na(week) & week == 19L ~ "WC",
        !is.na(week) & week == 20L ~ "DIV",
        !is.na(week) & week == 21L ~ "CON",
        !is.na(week) & week == 22L ~ "SB",
        TRUE ~ NA_character_
      )
    ) %>%
    arrange(game_id)

  out
}

raw_game_lookup <- normalize_game_lookup_tbl(raw)
lookup_path <- PIPELINE_CONFIG$input_paths$game_lookup_table
existing_game_lookup <- if (file.exists(lookup_path)) {
  normalize_game_lookup_tbl(read_csv(lookup_path, show_col_types = FALSE, name_repair = "unique_quiet"))
} else {
  normalize_game_lookup_tbl(tibble())
}

raw_game_ids <- raw_game_lookup %>% distinct(game_id)
stale_existing_count <- existing_game_lookup %>%
  anti_join(raw_game_ids, by = "game_id") %>%
  nrow()
if (stale_existing_count > 0L) {
  message("Ignoring stale lookup rows not present in raw HUDL: ", stale_existing_count)
}
existing_game_lookup <- existing_game_lookup %>%
  semi_join(raw_game_ids, by = "game_id")

game_lookup <- raw_game_lookup %>%
  left_join(existing_game_lookup, by = "game_id", suffix = c("_raw", "_existing")) %>%
  transmute(
    game_id = game_id,
    nflfast_game_id = coalesce(nflfast_game_id_raw, nflfast_game_id_existing),
    season = coalesce(season_raw, season_existing),
    week = coalesce(week_raw, week_existing),
    game_type = coalesce(game_type_raw, game_type_existing)
  ) %>%
  mutate(
    week = coalesce(week, extract_nflfast_week_local(nflfast_game_id)),
    game_type = case_when(
      !is.na(game_type) ~ game_type,
      !is.na(week) & week <= 18L ~ "REG",
      !is.na(week) & week == 19L ~ "WC",
      !is.na(week) & week == 20L ~ "DIV",
      !is.na(week) & week == 21L ~ "CON",
      !is.na(week) & week == 22L ~ "SB",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(game_id)

message(
  "Built game lookup rows: ",
  nrow(game_lookup),
  " (missing week: ",
  sum(is.na(game_lookup$week)),
  ", missing game_type: ",
  sum(is.na(game_lookup$game_type)),
  ", existing lookup rows merged: ",
  nrow(existing_game_lookup),
  ")"
)

raw <- raw %>%
  left_join(
    position_by_id %>% rename(event_player_position = position),
    by = c("player_id")
  ) %>%
  left_join(
    position_by_id %>% rename(opponent_position = position),
    by = c("opponent_player_id" = "player_id")
  ) %>%
  left_join(
    position_by_name %>% rename(freeze_frame_position = position),
    by = c("freeze_frame_player" = "player_name")
  ) %>%
  left_join(
    position_by_name %>% rename(event_player_position_name = position),
    by = c("event_player_name" = "player_name")
  ) %>%
  left_join(
    position_by_name %>% rename(opponent_position_name = position),
    by = c("opponent_player_name" = "player_name")
  ) %>%
  mutate(
    event_player_position = coalesce(event_player_position, event_player_position_name),
    opponent_position = coalesce(opponent_position, opponent_position_name)
  ) %>%
  select(-event_player_position_name, -opponent_position_name)

event_rows <- raw %>%
  filter(!is.na(game_id), !is.na(play_uuid), !is.na(event_types), !is.na(time_since_snap)) %>%
  distinct(
    game_id,
    play_uuid,
    event_uuid,
    event_game_index,
    event_types,
    time_since_snap,
    event_player_name,
    opponent_player_name,
    player_id,
    opponent_player_id,
    event_player_position,
    opponent_position,
    event_penalty_type
  )

pass_times <- event_rows %>%
  filter(str_detect(event_types, "Pass")) %>%
  group_by(play_uuid) %>%
  summarise(pass_time = suppressWarnings(min(time_since_snap, na.rm = TRUE)), .groups = "drop") %>%
  mutate(pass_time = if_else(is.infinite(pass_time), NA_real_, pass_time))

message("Building sacks table...")
sacks <- event_rows %>%
  filter(str_detect(event_types, "Sack"), !is.na(event_player_name), event_player_name != "") %>%
  transmute(
    game_id,
    play_uuid,
    sack_player = event_player_name,
    sack = 1L
  ) %>%
  distinct()

qbs <- roster %>%
  filter(team_position_name == "Quarterback", !is.na(player_name), player_name != "") %>%
  pull(player_name) %>%
  unique()

message("Building hits table...")
tackles_qb <- event_rows %>%
  filter(
    str_detect(event_types, "Tackle"),
    !is.na(event_player_name),
    event_player_name != "",
    !is.na(opponent_player_name),
    opponent_player_name %in% qbs
  ) %>%
  left_join(pass_times, by = "play_uuid") %>%
  anti_join(sacks %>% select(play_uuid), by = "play_uuid") %>%
  filter(is.na(pass_time) | time_since_snap < pass_time)

hits <- tackles_qb %>%
  transmute(
    game_id,
    play_uuid,
    hit_player = event_player_name,
    hit = 1L
  ) %>%
  distinct()

message("Building penalty tables...")
block_pens <- c("Offensive Holding", "Illegal Use of Hands", "Chop Block", "Face Mask (15 Yards)", "Clipping")
rush_pens <- c("Illegal Use of Hands", "Face Mask (15 Yards)", "Defensive Holding")

penalty_flags <- event_rows %>%
  filter(!is.na(event_penalty_type), event_penalty_type != "") %>%
  mutate(event_penalty_type = str_remove_all(event_penalty_type, "[\\{\\}\"]")) %>%
  separate_rows(event_penalty_type, sep = ",\\s*") %>%
  filter(event_penalty_type != "") %>%
  distinct(game_id, play_uuid, event_player_name, event_penalty_type)

blocker_penalties <- penalty_flags %>%
  filter(event_penalty_type %in% block_pens) %>%
  transmute(
    game_id,
    play_uuid,
    blocker_name = event_player_name,
    blocker_penalty = 1L
  ) %>%
  distinct()

rusher_penalties <- penalty_flags %>%
  filter(event_penalty_type %in% rush_pens) %>%
  transmute(
    game_id,
    play_uuid,
    rusher_name = event_player_name,
    rusher_penalty = 1L
  ) %>%
  distinct()

message("Constructing engagement matchups...")
defensive_rush_positions <- c("Defensive Lineman", "Linebacker")

engagement_rows <- event_rows %>%
  filter(str_detect(event_types, "Engagement Start|Engagement End")) %>%
  mutate(
    rusher_name = case_when(
      event_player_position %in% defensive_rush_positions & opponent_position == "Offensive Lineman" ~ event_player_name,
      event_player_position == "Offensive Lineman" & opponent_position %in% defensive_rush_positions ~ opponent_player_name,
      TRUE ~ NA_character_
    ),
    blocker_name = case_when(
      event_player_position %in% defensive_rush_positions & opponent_position == "Offensive Lineman" ~ opponent_player_name,
      event_player_position == "Offensive Lineman" & opponent_position %in% defensive_rush_positions ~ event_player_name,
      TRUE ~ NA_character_
    ),
    rusher_id = case_when(
      event_player_position %in% defensive_rush_positions & opponent_position == "Offensive Lineman" ~ player_id,
      event_player_position == "Offensive Lineman" & opponent_position %in% defensive_rush_positions ~ opponent_player_id,
      TRUE ~ NA_real_
    ),
    blocker_id = case_when(
      event_player_position %in% defensive_rush_positions & opponent_position == "Offensive Lineman" ~ opponent_player_id,
      event_player_position == "Offensive Lineman" & opponent_position %in% defensive_rush_positions ~ player_id,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(rusher_name), !is.na(blocker_name))

matchups_all <- engagement_rows %>%
  arrange(game_id, play_uuid, event_game_index, time_since_snap) %>%
  group_by(game_id, play_uuid, rusher_id, blocker_id, rusher_name, blocker_name) %>%
  summarise(
    event_game_index = suppressWarnings(min(event_game_index[str_detect(event_types, "Engagement Start")], na.rm = TRUE)),
    Engage_start = suppressWarnings(min(time_since_snap[str_detect(event_types, "Engagement Start")], na.rm = TRUE)),
    Engage_end = suppressWarnings(min(time_since_snap[str_detect(event_types, "Engagement End")], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    event_game_index = if_else(is.infinite(event_game_index), NA_real_, event_game_index),
    Engage_start = if_else(is.infinite(Engage_start), NA_real_, Engage_start),
    Engage_end = if_else(is.infinite(Engage_end), NA_real_, Engage_end)
  ) %>%
  left_join(pass_times, by = "play_uuid") %>%
  add_count(game_id, play_uuid, rusher_id, name = "n_blockers_for_rusher") %>%
  mutate(double_team = if_else(n_blockers_for_rusher > 1L, 1L, 0L))

message("Preparing play tracking lookup...")
tracking_points <- raw %>%
  select(play_uuid, time_since_snap, freeze_frame_player, freeze_frame_x, freeze_frame_y, freeze_frame_position) %>%
  filter(
    !is.na(play_uuid),
    !is.na(time_since_snap),
    !is.na(freeze_frame_player),
    !is.na(freeze_frame_x),
    !is.na(freeze_frame_y)
  )

qb_tracking <- tracking_points %>%
  filter(freeze_frame_position == "Quarterback") %>%
  group_by(play_uuid, time_since_snap) %>%
  summarise(
    qb_x = mean(freeze_frame_x, na.rm = TRUE),
    qb_y = mean(freeze_frame_y, na.rm = TRUE),
    .groups = "drop"
  )

play_tracking <- tracking_points %>%
  select(play_uuid, time_since_snap, freeze_frame_player, freeze_frame_x, freeze_frame_y) %>%
  inner_join(qb_tracking, by = c("play_uuid", "time_since_snap")) %>%
  group_by(play_uuid) %>%
  nest()

matchups_eval <- matchups_all %>%
  left_join(play_tracking, by = "play_uuid")

max_win_seconds <- as.numeric(PIPELINE_CONFIG$win_definition$max_win_seconds)
if (is.na(max_win_seconds) || max_win_seconds <= 0) {
  stop("Invalid max win seconds: ", PIPELINE_CONFIG$win_definition$max_win_seconds)
}

message("Evaluating matchup outcomes with hard ", max_win_seconds, " second win threshold...")

safe_eval <- function(rusher, blocker, start, end, tracking, game_id, play_id, event_game_index, double_team, max_seconds) {
  default_row <- tibble(
    game_id = game_id,
    play_id = play_id,
    event_game_index = event_game_index,
    rusher_name = rusher,
    blocker_name = blocker,
    beat_time = NA_real_,
    beat_dist = NA_real_,
    rusher_won = 0L,
    double_team = double_team
  )

  if (is.na(start) || !is.data.frame(tracking)) {
    return(default_row)
  }

  window_end <- suppressWarnings(min(c(end, max_seconds), na.rm = TRUE))
  if (is.infinite(window_end) || is.na(window_end) || start > max_seconds || window_end < start) {
    return(default_row)
  }

  rusher_x_col <- paste0(rusher, "_x")
  rusher_y_col <- paste0(rusher, "_y")
  blocker_x_col <- paste0(blocker, "_x")
  blocker_y_col <- paste0(blocker, "_y")

  wide_rb <- tracking %>%
    filter(
      time_since_snap >= start,
      time_since_snap <= window_end,
      freeze_frame_player %in% c(rusher, blocker)
    ) %>%
    group_by(time_since_snap, freeze_frame_player) %>%
    summarise(
      x = mean(freeze_frame_x, na.rm = TRUE),
      y = mean(freeze_frame_y, na.rm = TRUE),
      qb_x = mean(qb_x, na.rm = TRUE),
      qb_y = mean(qb_y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = freeze_frame_player,
      values_from = c(x, y),
      names_glue = "{freeze_frame_player}_{.value}"
    )

  required_cols <- c(rusher_x_col, rusher_y_col, blocker_x_col, blocker_y_col, "qb_x", "qb_y")
  if (nrow(wide_rb) == 0 || !all(required_cols %in% names(wide_rb))) {
    return(default_row)
  }

  d_r <- sqrt((wide_rb[[rusher_x_col]] - wide_rb$qb_x)^2 + (wide_rb[[rusher_y_col]] - wide_rb$qb_y)^2)
  d_b <- sqrt((wide_rb[[blocker_x_col]] - wide_rb$qb_x)^2 + (wide_rb[[blocker_y_col]] - wide_rb$qb_y)^2)
  idx <- which(d_r < d_b)

  if (length(idx) == 0 || idx[[1]] > nrow(wide_rb)) {
    return(default_row)
  }

  beat_time <- as.numeric(wide_rb$time_since_snap[idx[[1]]])
  beat_dist <- as.numeric(d_r[idx[[1]]])
  rusher_won <- if_else(!is.na(beat_time) && beat_time <= max_seconds, 1L, 0L)

  tibble(
    game_id = game_id,
    play_id = play_id,
    event_game_index = event_game_index,
    rusher_name = rusher,
    blocker_name = blocker,
    beat_time = beat_time,
    beat_dist = beat_dist,
    rusher_won = rusher_won,
    double_team = double_team
  )
}

workers <- PIPELINE_CONFIG$parallel$workers
matchup_rows <- nrow(matchups_eval)
chunk_size <- suppressWarnings(as.integer(Sys.getenv("INPUT_MATCHUP_CHUNK_SIZE", unset = "2000")))
if (is.na(chunk_size) || chunk_size <= 0L) {
  chunk_size <- 2000L
}

message(
  "Running matchup evaluation in parallel with workers=",
  workers,
  ", rows=",
  matchup_rows,
  ", chunk_size=",
  chunk_size,
  "..."
)

if (matchup_rows == 0L) {
  matchups <- tibble(
    game_id = numeric(0),
    play_id = character(0),
    event_game_index = numeric(0),
    rusher_name = character(0),
    blocker_name = character(0),
    beat_time = numeric(0),
    beat_dist = numeric(0),
    rusher_won = integer(0),
    double_team = numeric(0)
  )
} else {
  row_idx <- seq_len(matchup_rows)
  idx_chunks <- split(row_idx, ceiling(row_idx / chunk_size))

  chunk_results <- parallel_map(
    iterable = idx_chunks,
    workers = workers,
    seed = PIPELINE_CONFIG$uncertainty$seed,
    worker_fn = function(idx_chunk) {
      bind_rows(lapply(idx_chunk, function(i) {
        safe_eval(
          rusher = matchups_eval$rusher_name[[i]],
          blocker = matchups_eval$blocker_name[[i]],
          start = matchups_eval$Engage_start[[i]],
          end = matchups_eval$Engage_end[[i]],
          tracking = matchups_eval$data[[i]],
          game_id = matchups_eval$game_id[[i]],
          play_id = matchups_eval$play_uuid[[i]],
          event_game_index = matchups_eval$event_game_index[[i]],
          double_team = matchups_eval$double_team[[i]],
          max_seconds = max_win_seconds
        )
      }))
    }
  )

  matchups <- bind_rows(chunk_results)
}

matchups <- matchups %>%
  left_join(
    rusher_penalties,
    by = c("game_id", "play_id" = "play_uuid", "rusher_name")
  ) %>%
  left_join(
    blocker_penalties,
    by = c("game_id", "play_id" = "play_uuid", "blocker_name")
  ) %>%
  mutate(
    rusher_penalty = replace_na(rusher_penalty, 0L),
    blocker_penalty = replace_na(blocker_penalty, 0L),
    penalty = if_else(rusher_penalty == 1L | blocker_penalty == 1L, 1L, 0L),
    # Hard definition requested: only beats at or before threshold count as wins.
    rusher_won = if_else(!is.na(beat_time) & beat_time <= max_win_seconds, 1L, 0L),
    double_team = replace_na(double_team, 0L)
  ) %>%
  arrange(game_id, play_id, event_game_index, rusher_name, blocker_name) %>%
  distinct(
    game_id,
    play_id,
    event_game_index,
    rusher_name,
    blocker_name,
    .keep_all = TRUE
  ) %>%
  select(
    game_id,
    play_id,
    event_game_index,
    rusher_name,
    blocker_name,
    beat_time,
    beat_dist,
    rusher_won,
    double_team,
    rusher_penalty,
    blocker_penalty,
    penalty
  )

message("Writing rebuilt inputs to data/input...")
write_output_csv(matchups, PIPELINE_CONFIG$input_paths$matchups_table)
write_output_csv(sacks, PIPELINE_CONFIG$input_paths$sacks_table)
write_output_csv(hits, PIPELINE_CONFIG$input_paths$hits_table)
write_output_csv(game_lookup, PIPELINE_CONFIG$input_paths$game_lookup_table)

message("Wrote rebuilt matchups table: ", PIPELINE_CONFIG$input_paths$matchups_table)
message("Wrote rebuilt sacks table: ", PIPELINE_CONFIG$input_paths$sacks_table)
message("Wrote rebuilt hits table: ", PIPELINE_CONFIG$input_paths$hits_table)
message("Wrote rebuilt game lookup table: ", PIPELINE_CONFIG$input_paths$game_lookup_table)
message("Rebuilt win rate (rusher_won == 1): ", round(mean(matchups$rusher_won, na.rm = TRUE), 4))
