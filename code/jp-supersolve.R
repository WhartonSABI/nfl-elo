#############
### SETUP ###
#############

# package load
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, dplyr, ggthemes, data.table, lubridate, glmnet,
               GGally, RColorBrewer, ggsci, plotROC, usmap,
               plotly, ggpubr, vistime, coefplot, skimr, car, ggrepel, slider, lubridate,
               tidymodels,ranger,vip,ggplot2, tune,dials,pdp, purrr, stringr, lmtest,
               sandwich, parallel, furrr, future)

# boost working memory
mem.maxVSize(81920)

# setup parallel processing early
plan(multisession, workers = parallel::detectCores() - 1)

####################
### DATA LOADING ###
####################

# read in data
new_data <- fread("../data/processed/full_hudl_data.csv")

names(new_data)
head(new_data)

##########################
### DATA PREPROCESSING ###
##########################

# filter for relevant event types for elo 

#sacks
sacks <- new_data %>%
  filter(str_detect(event_types, "Sack")) %>% 
  select(play_uuid, game_id, event_player_name) %>%
  rename(sack_player = event_player_name) %>% 
  distinct(play_uuid, game_id, sack_player) %>% 
  mutate(sack = 1L)

#hits
qbs <- new_data %>% 
  filter(freeze_frame_position == "Quarterback") %>% 
  distinct(freeze_frame_player)

tackles_qb <- new_data %>%
  filter(event_types == "{Tackle}") %>%
  filter(opponent_player_name %in% qbs$freeze_frame_player) %>% 
  filter(freeze_frame_position == "Quarterback") %>%   # Only QB getting tackled
  select(game_id, play_uuid, time_since_snap, event_player_name,
         event_x, event_y, freeze_frame_x, freeze_frame_y,
         freeze_frame_position, play_start_event, event_types, opponent_player_name)

pass_times <- new_data %>%
  filter(str_detect(event_types, "Pass")) %>%
  group_by(play_uuid) %>%
  summarise(pass_time = min(time_since_snap), .groups = "drop")

tackles_qb <- tackles_qb %>%
  left_join(pass_times, by = "play_uuid") %>%
  anti_join(sacks, by = "play_uuid") %>% 
  filter(is.na(pass_time) | time_since_snap < pass_time) %>% 
  filter(!is.na(time_since_snap)) 

hits <- tackles_qb %>% 
  select(play_uuid, game_id, event_player_name) %>%
  rename(hit_player = event_player_name) %>% 
  distinct(play_uuid, game_id, hit_player) %>% 
  mutate(hit = 1L) 

# memory cleanup: remove intermediate datasets
rm(pass_times, tackles_qb)
gc()

# filter data to select relevant columns for matchups
matchups <- new_data %>% 
  select(freeze_frame_player, event_player_name, opponent_player_name, event_uuid, event_game_index, game_id, play_uuid, player_id,  time_since_snap,opponent_player_id,  freeze_frame_player_id, event_types, event_x, event_y, start_engagement_uuid, end_engagement_uuid, play_start_event, time_since_snap, freeze_frame_x, freeze_frame_y, event_player_position, freeze_frame_player_id, freeze_frame_position, freeze_frame_player_team, nflfast_game_id)

# add the freeze frame of opponent players and qb in matchups to data 
matchups <- new_data

# join tracking data with opponent player positions and coordinates
matchup_tracking <- matchups %>%
  left_join(
    matchups %>% 
      select(game_id, play_uuid, time_since_snap, freeze_frame_player_id, freeze_frame_x, freeze_frame_y, freeze_frame_player, event_game_index) %>% 
      rename(
        opponent_player_id = freeze_frame_player_id,
        x_opp = freeze_frame_x,
        y_opp = freeze_frame_y,
        opponent_name = freeze_frame_player
      ),
    by = c("game_id", "play_uuid", "time_since_snap", "opponent_player_id"),
    # silence warning
    relationship = "many-to-many"
  )

# join tracking data with quarterback positions and coordinates
matchup_tracking_with_qb <- matchups %>%
  left_join(
    matchups %>%
      filter(freeze_frame_position == "Quarterback") %>%
      # %>%
      select(game_id, play_uuid, time_since_snap,
             qb_id = freeze_frame_player_id,
             qb_x  = freeze_frame_x,
             qb_y  = freeze_frame_y,
             qb_name = freeze_frame_player),
    by = c("game_id", "play_uuid", "time_since_snap"),
    # silence warning
    relationship = "many-to-many"
  )

# make table with engagement start and end times
engagements <- matchup_tracking_with_qb %>% 
  filter(event_types %in% c("{\"\"\"\"Engagement Start\"\"\"\"}"  , "{\"\"\"\"Engagement End\"\"\"\"}", "{Pass}" , "{Pass,Hit}"))

##############################
### ENGAGEMENT PROCESSING ###
##############################

# identify rusher and blocker and put engagement start and end in columns
# this function processes engagement data to identify rushers (defenders) and blockers (offensive linemen)
# and extracts their engagement start/end times along with quarterback information
matchups_all <- engagements %>%
  filter(
    event_types %in% c(
      "{\"\"\"\"Engagement Start\"\"\"\"}",
      "{\"\"\"\"Engagement End\"\"\"\"}",
      "{Pass}", "{Pass,Hit}"
    )
  ) %>%
  arrange(play_uuid, event_game_index, player_id, opponent_player_id, event_types) %>%
  #filter(player_position %in% c("Defensive Lineman", "Linebacker")) %>%  # only keep defenders
  group_by(play_uuid, player_id, opponent_player_id) %>%
  summarize(
  rusher_idx = which(freeze_frame_position %in% c("Defensive Lineman", "Linebacker"))[1],
  blocker_idx = which(opponent_position == "Offensive Lineman")[1],

  rusher_id = if (!is.na(rusher_idx)) player_id[rusher_idx] else NA_integer_,
  rusher_name = if (!is.na(rusher_idx)) freeze_frame_player[rusher_idx] else NA_character_,

  blocker_id = if (!is.na(blocker_idx)) opponent_player_id[blocker_idx] else NA_integer_,
  blocker_name = if (!is.na(blocker_idx)) opponent_player_name[blocker_idx] else NA_character_,

  Engage_start = time_since_snap[event_types == "{\"\"\"\"Engagement Start\"\"\"\"}"][1],
  Engage_end   = time_since_snap[event_types == "{\"\"\"\"Engagement End\"\"\"\"}"][1],

  game_id  = first(game_id),
  qb_id    = first(qb_id),
  qb_x     = first(qb_x),
  qb_y     = first(qb_y),
  qb_name  = first(qb_name),
  .groups = "drop"
)

# memory cleanup: remove intermediate datasets
rm(matchups, matchup_tracking, matchup_tracking_with_qb)
gc()

# find pass times and add to table
# get pass times
pass_times <- engagements %>%
  filter(event_types == "{Pass}") %>%
  group_by(play_uuid) %>%
  summarize(pass_time = min(time_since_snap, na.rm = TRUE), .groups = "drop")

matchups_all <- matchups_all %>%
  left_join(pass_times, by = "play_uuid")

# add a column to each engagement with each play nested in that row's entry
# prepare engagements table to iterate through frames in a play
play_tracking <- engagements %>%
  group_by(play_uuid) %>%
  nest()

# merge with matchups_all to contain each matchups and then the freeze frames for each one in a separate column
matchups_all_nested <- matchups_all %>%
  left_join(play_tracking, by = "play_uuid")

# memory cleanup: remove nested data sources
rm(engagements, play_tracking)
gc()

#############################
### PASS BLOCK EVALUATION ###
#############################

# this function calculates whether a rusher "beats" a blocker by determining if the rusher
# gets closer to the quarterback than the blocker during their engagement window
# parameters:
#   rusher: name of the defensive player
#   blocker: name of the offensive lineman
#   start: engagement start time
#   end: engagement end time  
#   pass_time: when the pass was thrown
#   tracking: tracking data for the play
#   game_id: game identifier
#   play_id: play identifier
# returns: tibble with rusher/blocker names, beat time, beat distance, and win indicator
safe_eval <- function(rusher, blocker, start, end, pass_time, tracking, game_id, play_id) {
  tryCatch({
    if (is.na(start) || is.na(end) || is.na(pass_time) || !is.data.frame(tracking)) {
      return(tibble(
        game_id = game_id,
        play_id = play_id,
        rusher_name  = rusher,
        blocker_name = blocker,
        beat_time    = NA_real_,
        beat_dist    = NA_real_,
        rusher_won   = 0L
      ))
    }

    window_end <- min(end, pass_time, na.rm = TRUE)

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
      select(time_since_snap,
             player = freeze_frame_player,
             x = freeze_frame_x,
             y = freeze_frame_y) %>%
      group_by(time_since_snap, player) %>%
      summarise(
        x = mean(x, na.rm = TRUE),
        y = mean(y, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = player,
        values_from = c(x, y),
        names_glue = "{player}_{.value}"
      ) %>%
      left_join(
        tracking %>% select(time_since_snap, qb_x, qb_y),
        by = "time_since_snap"
      ) %>%
      mutate(across(ends_with("_x"), as.numeric),
             across(ends_with("_y"), as.numeric))

    required_cols <- c(rusher_x_col, rusher_y_col,
                       blocker_x_col, blocker_y_col, "qb_x", "qb_y")

    if (nrow(wide_rb) == 0 || !all(required_cols %in% names(wide_rb))) {
      return(tibble(
        game_id = game_id,
        play_id = play_id,
        rusher_name  = rusher,
        blocker_name = blocker,
        beat_time    = NA_real_,
        beat_dist    = NA_real_,
        rusher_won   = 0L
      ))
    }

    # Distance to QB
    d_r <- sqrt((wide_rb[[rusher_x_col]] - wide_rb$qb_x)^2 +
                (wide_rb[[rusher_y_col]] - wide_rb$qb_y)^2)
    d_b <- sqrt((wide_rb[[blocker_x_col]] - wide_rb$qb_x)^2 +
                (wide_rb[[blocker_y_col]] - wide_rb$qb_y)^2)

    idx <- which(d_r < d_b)

    if (length(idx) > 0 && idx[1] <= nrow(wide_rb)) {
      tibble(
        game_id = game_id,
        play_id = play_id,
        rusher_name  = rusher,
        blocker_name = blocker,
        beat_time    = as.numeric(wide_rb$time_since_snap[idx[1]]),
        beat_dist    = as.numeric(d_r[idx[1]]),
        rusher_won   = 1L
      )
    } else {
      tibble(
        game_id = game_id,
        play_id = play_id,
        rusher_name  = rusher,
        blocker_name = blocker,
        beat_time    = NA_real_,
        beat_dist    = NA_real_,
        rusher_won   = 0L
      )
    }

  }, error = function(e) {
    tibble(
      game_id = game_id,
      play_id = play_id,
      rusher_name  = rusher,
      blocker_name = blocker,
      beat_time    = NA_real_,
      beat_dist    = NA_real_,
      rusher_won   = 0L
    )
  })
}

# apply function to get table of results using parallel processing
# this evaluates all rusher-blocker engagements in parallel for speed
cat("Starting parallel evaluation of", nrow(matchups_all_nested), "engagements...\n")
results2 <- future_pmap_dfr(
  list(
    rusher     = matchups_all_nested$rusher_name,
    blocker    = matchups_all_nested$blocker_name,
    start      = matchups_all_nested$Engage_start,
    end        = matchups_all_nested$Engage_end,
    pass_time  = matchups_all_nested$pass_time,
    tracking   = matchups_all_nested$data,
    game_id    = matchups_all_nested$game_id, 
    play_id    = matchups_all_nested$play_uuid
  ),
  safe_eval,
  .options = furrr_options(seed = TRUE)
)

cat("Parallel evaluation completed. Processing", nrow(results2), "results.\n")

# memory cleanup: remove nested data after evaluation
rm(matchups_all_nested)
gc()

#########################
### ELO RATING SYSTEM ###
#########################

# cleaning results data
results2 <- results2 %>%
  left_join(
    sacks %>% select(game_id, play_uuid, sack_player, sack),
    by = c("game_id", "play_id" = "play_uuid", "rusher_name" = "sack_player")
  ) %>%
  mutate(sack = replace_na(sack, 0L)) 


results2 <- results2 %>%
  left_join(
    hits %>% select(game_id, play_uuid, hit_player, hit),
    by = c("game_id", "play_id" = "play_uuid", "rusher_name" = "hit_player")
  ) %>%
  mutate(hit = replace_na(hit, 0L))


results2 <- results2 %>%
  mutate(
    outcome_score = case_when(
      sack == 1L         ~ 1.0,
      hit == 1L          ~ 0.5,
      rusher_won == 1L   ~ 0.25,
      TRUE               ~ 0.0
    )
  )

results_cleaned <- results2 %>% 
  drop_na(game_id, rusher_name, blocker_name) %>% 
  arrange(game_id, play_id)

# memory cleanup: remove raw results after cleaning
rm(results2)
gc()

# initialize player elo ratings - rushers start at 900, blockers at 1100
init_play_ratings <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(everything(), names_to = "type", values_to = "player_name") %>%
  mutate(Player_Elo = ifelse(type == "rusher_name", 750, 1100)) %>%  
  distinct(player_name, .keep_all = TRUE)

elo_vec <- setNames(init_play_ratings$Player_Elo, init_play_ratings$player_name)

# memory cleanup: remove initial ratings after vector creation
rm(init_play_ratings)
gc()

# elo rating system parameters and functions
K <- 32  # k-factor determines how much ratings change per game
scale <- 319  # scale factor for elo calculations

# elo step function - updates ratings based on outcome
# parameters:
#   belo: blocker's current elo rating
#   relo: rusher's current elo rating  
#   outcome: 1 if rusher wins, 0 if blocker wins
# returns: vector with updated blocker and rusher ratings
elo_step <- function(belo, relo, outcome) {
  # expectation for rusher
  E_r <- 1 / (1 + 10^((belo - relo) / scale))
  E_b <- 1 - E_r
  
  # update each player with K * (S - E) where S is actual score
  new_r <- relo + K * (outcome - E_r)      # rusher update
  new_b <- belo + K * ((1 - outcome) - E_b) # blocker update
  
  c(b = new_b, r = new_r)
}

# helper function to get current elo rating for a player
# parameters:
#   id: player name/identifier
# returns: current elo rating (defaults to 1000 if player not found)
get_elo <- function(id) {
  id <- as.character(id)  
  if (!id %in% names(elo_vec)) elo_vec[[id]] <- 1000
  return(elo_vec[[id]])
} 

# main elo rating update loop
# this processes each engagement chronologically and updates player ratings
cat("Starting sequential ELO rating updates for", nrow(results_cleaned), "engagements...\n")
results_cleaned <- as.data.table(results_cleaned)
interaction_counts <- list()  # initialize interaction count
elo_history <- results_cleaned %>%
  mutate(
    before_rusher_elo = NA_real_,
    before_blocker_elo = NA_real_,
    after_rusher_elo = NA_real_,
    after_blocker_elo = NA_real_, 
    rusher_n = NA_integer_,
    blocker_n = NA_integer_
  )

for (i in seq_len(nrow(elo_history))) {
  r_id <- elo_history$rusher_name[i]
  b_id <- elo_history$blocker_name[i]
  out  <- elo_history$rusher_won[i]

  if (is.na(r_id) || is.na(b_id) || is.na(out)) next

  r_elo <- get_elo(r_id)
  b_elo <- get_elo(b_id)

  elo_history$before_rusher_elo[i]  <- r_elo
  elo_history$before_blocker_elo[i] <- b_elo
  
  interaction_counts[[r_id]] <- if (is.null(interaction_counts[[r_id]])) 1L else interaction_counts[[r_id]] + 1L
  interaction_counts[[b_id]] <- if (is.null(interaction_counts[[b_id]])) 1L else interaction_counts[[b_id]] + 1L

  elo_history$rusher_n[i]  <- interaction_counts[[r_id]]
  elo_history$blocker_n[i] <- interaction_counts[[b_id]]

  new <- elo_step(b_elo, r_elo, out)

  elo_vec[[r_id]] <- new["r"]
  elo_vec[[b_id]] <- new["b"]

  elo_history$after_rusher_elo[i]  <- new["r"]
  elo_history$after_blocker_elo[i] <- new["b"]
}

cat("ELO rating updates completed.\n")

# memory cleanup: remove interaction counter
rm(interaction_counts)
gc()

# get final results and create player rankings
# get results
elo_df <- enframe(elo_vec, name = "player_name", value = "final_elo")

# differentiate between rushers and blockers
player_roles <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(cols = everything(), values_to = "player_name") %>%
  mutate(role = ifelse(name == "rusher_name", "Rusher", "Blocker")) %>%
  distinct(player_name, role)

# join to get elo with annotated rusher/blocker label
elo_labeled <- elo_df %>%
  left_join(player_roles, by = "player_name") %>%
  filter(!is.na(role))

# memory cleanup: remove intermediate ranking data
rm(elo_df, player_roles)
gc()

# get top 10
top_rushers <- elo_labeled %>%
  filter(role == "Rusher") %>%
  slice_max(final_elo, n = 10)

top_blockers <- elo_labeled %>%
  filter(role == "Blocker") %>%
  slice_max(final_elo, n = 10)

# create long format data for plotting elo trajectories
# pivot to get elo updates per play 
elo_long <- elo_history %>%
  select(game_id, play_id,
         rusher_name, blocker_name,
         after_rusher_elo, after_blocker_elo,
         rusher_n, blocker_n) %>%
  pivot_longer(
    cols = c(after_rusher_elo, after_blocker_elo),
    names_to = "elo_type",
    values_to = "elo"
  ) %>%
  mutate(
    player_name = ifelse(elo_type == "after_rusher_elo", rusher_name, blocker_name),
    interaction_n = ifelse(elo_type == "after_rusher_elo", rusher_n, blocker_n),
    role = ifelse(elo_type == "after_rusher_elo", "Rusher", "Blocker")
  ) %>%
  select(play_id, game_id, player_name, role, interaction_n, elo) %>%
  filter(!is.na(elo))

# filter for top players
top_players <- elo_long %>%
  group_by(player_name, role) %>%
  summarize(final_elo = max(elo, na.rm = TRUE), .groups = "drop") %>%
  group_by(role) %>%
  slice_max(final_elo, n = 10) %>%
  pull(player_name)

elo_top <- elo_long %>%
  filter(player_name %in% top_players)

# create visualization plots for top players
elo_top_rusher <- elo_top %>% filter(role == "Rusher")
elo_top_blocker <- elo_top %>% filter(role == "Blocker")

# plots for top players' elo trajectories
top_10_rushers = ggplot(elo_top_rusher, aes(x = as.numeric(interaction_n), y = elo, color = player_name)) +
  geom_line(size = 1) +
  labs(
    title = "Top 10 Rushers: Elo Trajectory (Parallel)",
    x = "Interaction Number",
    y = "Elo Rating",
    color = "Rusher"
  ) +
  theme_minimal()
top_10_blockers = ggplot(elo_top_blocker, aes(x = as.numeric(interaction_n), y = elo, color = player_name)) +
  geom_line(size = 1) +
  labs(
    title = "Top 10 Blockers: Elo Trajectory (Parallel)",
    x = "Interaction Number",
    y = "Elo Rating",
    color = "Blocker"
  ) +
  theme_minimal()

# save results to processed data
write_csv(elo_history, "../data/results/parallel_elo_history.csv")
write_csv(elo_labeled, "../data/results/parallel_player_elo_ratings.csv")
# save plots
ggsave("../data/results/parallel_top_10_rushers.png", plot = top_10_rushers, width = 8, height = 6)
ggsave("../data/results/parallel_top_10_blockers.png", plot = top_10_blockers, width = 8, height = 6)

# memory cleanup: remove plotting datasets
rm(elo_long, top_players, elo_top, elo_top_rusher, elo_top_blocker)
gc()

#############################
### LOGIC VERIFICATION ###
#############################

# Test function to verify Elo logic is working correctly
test_elo_logic <- function() {
  cat("Testing Elo logic...\n")
  
  # Test case 1: Rusher wins (outcome = 1)
  # Blocker starts at 1100, Rusher at 900
  belo <- 1100
  relo <- 900
  outcome <- 1  # Rusher wins
  
  # Expected win probability for blocker
  exp_blk <- 1 / (1 + 10^((relo - belo) / 400))
  cat("Blocker expected win probability:", round(exp_blk, 3), "\n")
  
  # Rating updates
  new_elos <- elo_step(belo, relo, outcome)
  cat("After rusher wins:\n")
  cat("  Blocker: 1100 ->", round(new_elos["b"], 1), "\n")
  cat("  Rusher:  900  ->", round(new_elos["r"], 1), "\n")
  
  # Test case 2: Blocker wins (outcome = 0)
  belo <- 1100
  relo <- 900
  outcome <- 0  # Blocker wins
  
  exp_blk <- 1 / (1 + 10^((relo - belo) / 400))
  cat("Blocker expected win probability:", round(exp_blk, 3), "\n")
  
  new_elos <- elo_step(belo, relo, outcome)
  cat("After blocker wins:\n")
  cat("  Blocker: 1100 ->", round(new_elos["b"], 1), "\n")
  cat("  Rusher:  900  ->", round(new_elos["r"], 1), "\n")
  
  cat("Logic test completed.\n")
}

# Run the test
test_elo_logic()

# memory cleanup: final cleanup
rm(new_data, results_cleaned, elo_history, elo_labeled, top_rushers, top_blockers)
gc()

cat("memory cleanup completed\n")
cat("Parallel version completed successfully!\n")