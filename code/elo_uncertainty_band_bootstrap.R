library(tidyverse)
library(data.table)
library(furrr)

# =========================================================
# User settings
# =========================================================
B <- 5000
SEED <- 5710
N_WORKERS <- max(1, parallelly::availableCores() - 1)

# If TRUE, keep every bootstrap draw for every player-week.
# This can get large quickly.
STORE_FULL_DRAWS <- FALSE

# =========================================================
# Read and build analysis data
# =========================================================
hudl <- read.csv("~/Downloads/Analytics Summer Lab/hudl_iq_game_ids.csv") %>%
  mutate(
    week = stringr::str_extract(nflfast_game_id, "(?<=^\\d{4}_)\\d{2}")
  )

results2 <- read.csv("~/Downloads/results2.csv") %>%
  left_join(hudl %>% select(game_id, week), by = "game_id")

sacks <- read.csv("~/Downloads/sacks.csv")
hits  <- read.csv("~/Downloads/hits.csv")

results2 <- results2 %>%
  left_join(
    sacks %>% select(game_id, play_uuid, sack_player, sack),
    by = c("game_id", "play_id" = "play_uuid", "rusher_name" = "sack_player")
  ) %>%
  mutate(sack = replace_na(sack, 0L)) %>%
  left_join(
    hits %>% select(game_id, play_uuid, hit_player, hit),
    by = c("game_id", "play_id" = "play_uuid", "rusher_name" = "hit_player")
  ) %>%
  mutate(hit = replace_na(hit, 0L)) %>%
  mutate(
    outcome_score = case_when(
      sack == 1L       ~ 1.0,
      hit == 1L        ~ 0.4,
      rusher_won == 1L ~ 0.2,
      TRUE             ~ 0.0
    )
  )

results_cleaned <- results2 %>%
  drop_na(game_id, rusher_name, blocker_name) %>%
  mutate(
    week = replace_na(week, "UNK"),
    double_team = replace_na(double_team, 0L)
  ) %>%
  select(
    game_id, play_id, event_game_index, week,
    rusher_name, blocker_name, double_team, outcome_score
  )

# =========================================================
# Global player list and initial Elos
# =========================================================
init_play_ratings <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(everything(), names_to = "type", values_to = "player_name") %>%
  mutate(Player_Elo = ifelse(type == "rusher_name", 750, 1125)) %>%
  distinct(player_name, .keep_all = TRUE)

initial_elo_vec <- setNames(init_play_ratings$Player_Elo, init_play_ratings$player_name)
player_names <- names(initial_elo_vec)
n_players <- length(player_names)

player_roles <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(cols = everything(), values_to = "player_name") %>%
  mutate(role = ifelse(name == "rusher_name", "Rusher", "Blocker")) %>%
  distinct(player_name, role) %>%
  arrange(match(player_name, player_names))

# =========================================================
# Week ordering
# UNK is treated as week 1 for simplicity
# =========================================================
week_lookup <- tibble(
  week = sort(unique(results_cleaned$week))
) %>%
  mutate(
    week_num = ifelse(week == "UNK", 1L, as.integer(week))
  ) %>%
  arrange(week_num, week) %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(week_index = row_number())

results_cleaned <- results_cleaned %>%
  left_join(week_lookup, by = "week") %>%
  arrange(week_index, game_id, play_id, event_game_index)

week_levels <- week_lookup$week
n_weeks <- length(week_levels)

# =========================================================
# Elo helpers
# =========================================================
adaptive_K_prov <- function(n, K_start, K_min, n_prov, n_decay) {
  k <- ifelse(
    n <= n_prov,
    K_start,
    ifelse(
      n <= n_decay,
      K_start + (K_min - K_start) * (n - n_prov) / (n_decay - n_prov),
      K_min
    )
  )
  pmax(k, K_min)
}

elo_step <- function(
    belo, relo, outcome_score, blocker_K, rusher_K,
    double_team, scale = 319
) {
  if (is.na(outcome_score)) return(c(b = belo, r = relo))
  
  blocker_bonus <- ifelse(double_team == 1, 100, 0)
  adj_belo <- belo + blocker_bonus
  
  exp_rusher <- 1 / (1 + 10 ^ ((adj_belo - relo) / scale))
  
  c(
    b = belo + blocker_K * ((1 - outcome_score) - (1 - exp_rusher)),
    r = relo + rusher_K  * ( outcome_score      -      exp_rusher)
  )
}

# =========================================================
# Core Elo engine for a given ordered dataset
# Returns a numeric vector aligned to player_names
# =========================================================
run_elo_on_ordered_data <- function(dat, initial_elo_vec, player_names) {
  dt <- as.data.table(copy(dat))
  
  dt[, rusher_row := seq_len(.N), by = rusher_name]
  dt[, blocker_row := seq_len(.N), by = blocker_name]
  
  dt[, rusher_k := adaptive_K_prov(
    n = rusher_row, K_start = 20, K_min = 10, n_prov = 87, n_decay = 200
  )]
  
  dt[, blocker_k := adaptive_K_prov(
    n = blocker_row, K_start = 20, K_min = 10, n_prov = 87, n_decay = 200
  )]
  
  elo_vec <- initial_elo_vec
  
  for (i in seq_len(nrow(dt))) {
    r_id <- dt$rusher_name[i]
    b_id <- dt$blocker_name[i]
    out  <- dt$outcome_score[i]
    r_K  <- dt$rusher_k[i]
    b_K  <- dt$blocker_k[i]
    d_t  <- dt$double_team[i]
    
    if (is.na(r_id) || is.na(b_id) || is.na(out)) next
    
    r_elo <- elo_vec[[r_id]]
    b_elo <- elo_vec[[b_id]]
    
    new <- elo_step(
      belo = b_elo,
      relo = r_elo,
      outcome_score = out,
      blocker_K = b_K,
      rusher_K = r_K,
      double_team = d_t
    )
    
    if (!any(is.na(new))) {
      elo_vec[[r_id]] <- new["r"]
      elo_vec[[b_id]] <- new["b"]
    }
  }
  
  unname(elo_vec[player_names])
}

# =========================================================
# One bootstrap replicate for a cumulative week dataset
# =========================================================
run_bootstrap_replicate <- function(cum_dat, initial_elo_vec, player_names) {
  n <- nrow(cum_dat)
  idx <- sample.int(n = n, size = n, replace = TRUE)
  boot_dat <- cum_dat[idx, ] %>%
    mutate(resample_order = seq_len(n())) %>%
    arrange(resample_order)
  
  run_elo_on_ordered_data(
    dat = boot_dat,
    initial_elo_vec = initial_elo_vec,
    player_names = player_names
  )
}

# =========================================================
# Observed Elo path by week (no bootstrap)
# This gives the actual cumulative Elo after each week
# =========================================================
observed_weekly_elos <- vector("list", n_weeks)

for (w in seq_len(n_weeks)) {
  cum_dat_w <- results_cleaned %>%
    filter(week_index <= w) %>%
    arrange(week_index, game_id, play_id, event_game_index)
  
  obs_vec <- run_elo_on_ordered_data(
    dat = cum_dat_w,
    initial_elo_vec = initial_elo_vec,
    player_names = player_names
  )
  
  observed_weekly_elos[[w]] <- tibble(
    week = week_levels[w],
    week_index = w,
    player_name = player_names,
    observed_elo = obs_vec
  )
}

observed_weekly_elos <- bind_rows(observed_weekly_elos) %>%
  left_join(player_roles, by = "player_name")

# =========================================================
# Bootstrap week-by-week uncertainty
# =========================================================
set.seed(SEED)
plan(multisession, workers = N_WORKERS)

weekly_summary_list <- vector("list", n_weeks)
if (STORE_FULL_DRAWS) {
  weekly_draws_list <- vector("list", n_weeks)
}

start_time <- Sys.time()

for (w in seq_len(n_weeks)) {
  message("Processing week ", w, " / ", n_weeks, " (", week_levels[w], ")")
  
  cum_dat_w <- results_cleaned %>%
    filter(week_index <= w) %>%
    arrange(week_index, game_id, play_id, event_game_index)
  
  # Which players actually appear this week
  players_this_week <- results_cleaned %>%
    filter(week_index == w) %>%
    select(rusher_name, blocker_name) %>%
    pivot_longer(cols = everything(), values_to = "player_name") %>%
    distinct(player_name) %>%
    pull(player_name)
  
  # B bootstrap draws; each draw is a full player Elo vector
  boot_list <- future_map(
    seq_len(B),
    ~ run_bootstrap_replicate(
      cum_dat = cum_dat_w,
      initial_elo_vec = initial_elo_vec,
      player_names = player_names
    ),
    .options = furrr_options(seed = TRUE)
  )
  
  # Efficient week-level storage: B x P matrix
  boot_mat <- do.call(rbind, boot_list)
  colnames(boot_mat) <- player_names
  
  obs_week <- observed_weekly_elos %>%
    filter(week_index == w) %>%
    arrange(match(player_name, player_names))
  
  week_summary <- tibble(
    week = week_levels[w],
    week_index = w,
    player_name = player_names,
    mean_final_elo = colMeans(boot_mat, na.rm = TRUE),
    sd_final_elo   = apply(boot_mat, 2, sd, na.rm = TRUE),
    q025           = apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE),
    q50            = apply(boot_mat, 2, quantile, probs = 0.50,  na.rm = TRUE),
    q975           = apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE),
    played_this_week = player_names %in% players_this_week,
    observed_elo = obs_week$observed_elo
  ) %>%
    left_join(player_roles, by = "player_name")
  
  weekly_summary_list[[w]] <- week_summary
  
  if (STORE_FULL_DRAWS) {
    weekly_draws_list[[w]] <- as_tibble(boot_mat) %>%
      mutate(iter = seq_len(B)) %>%
      pivot_longer(
        cols = -iter,
        names_to = "player_name",
        values_to = "elo"
      ) %>%
      mutate(
        week = week_levels[w],
        week_index = w
      ) %>%
      left_join(player_roles, by = "player_name")
  }
  
  rm(boot_list, boot_mat, week_summary, cum_dat_w, obs_week)
  gc()
}

end_time <- Sys.time()
print(end_time - start_time)

weekly_elo_uncertainty_summary <- bind_rows(weekly_summary_list)

if (STORE_FULL_DRAWS) {
  weekly_elo_boot_draws <- bind_rows(weekly_draws_list)
}

# =========================================================
# Example summaries
# =========================================================

# Top 10 blockers by mean Elo in the final week
weekly_elo_uncertainty_summary %>%
  filter(role == "Blocker", week_index == max(week_index)) %>%
  arrange(desc(mean_final_elo)) %>%
  slice_head(n = 10)

# Top 10 rushers by mean Elo in the final week
weekly_elo_uncertainty_summary %>%
  filter(role == "Rusher", week_index == max(week_index)) %>%
  arrange(desc(mean_final_elo)) %>%
  slice_head(n = 10)

# Example: uncertainty evolution for one player
weekly_elo_uncertainty_summary %>%
  filter(player_name == "Myles Garrett") %>%
  select(week, week_index, mean_final_elo, q025, q975, observed_elo)

# =========================================================
# Save outputs
# =========================================================
data.table::fwrite(
  weekly_elo_uncertainty_summary,
  "~/Downloads/weekly_elo_uncertainty_summary.csv"
)

data.table::fwrite(
  observed_weekly_elos,
  "~/Downloads/weekly_observed_elo_path.csv"
)

if (STORE_FULL_DRAWS) {
  data.table::fwrite(
    weekly_elo_boot_draws,
    "~/Downloads/weekly_elo_boot_draws.csv"
  )
}

