library(tidyverse)
library(data.table)
library(furrr)

# =========================================================
# User settings
# =========================================================
B <- 5000
SEED <- 5710
N_WORKERS <- max(1, parallelly::availableCores() - 1)

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
# Build game order
# Treat UNK as week 1
# =========================================================
game_order_df <- results_cleaned %>%
  distinct(game_id, week) %>%
  mutate(
    week_num = ifelse(week == "UNK", 1L, as.integer(week))
  ) %>%
  arrange(week_num, game_id) %>%
  mutate(game_index = row_number())

results_cleaned <- results_cleaned %>%
  left_join(game_order_df %>% select(game_id, game_index, week_num), by = "game_id") %>%
  arrange(game_index, play_id, event_game_index)

n_games <- max(results_cleaned$game_index)

# =========================================================
# Initial ratings and role map
# =========================================================
init_play_ratings <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(everything(), names_to = "type", values_to = "player_name") %>%
  mutate(Player_Elo = ifelse(type == "rusher_name", 750, 1125)) %>%
  distinct(player_name, .keep_all = TRUE)

initial_elo_vec <- setNames(init_play_ratings$Player_Elo, init_play_ratings$player_name)
player_names <- names(initial_elo_vec)

player_roles <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(cols = everything(), values_to = "player_name") %>%
  mutate(role = ifelse(name == "rusher_name", "Rusher", "Blocker")) %>%
  distinct(player_name, role)

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
# Pre-split rows by game
# =========================================================
game_data_list <- vector("list", n_games)
players_by_game <- vector("list", n_games)

for (g in seq_len(n_games)) {
  dat_g <- results_cleaned %>%
    filter(game_index == g) %>%
    arrange(play_id, event_game_index)
  
  game_data_list[[g]] <- dat_g
  
  players_by_game[[g]] <- dat_g %>%
    select(rusher_name, blocker_name) %>%
    pivot_longer(cols = everything(), values_to = "player_name") %>%
    distinct(player_name) %>%
    pull(player_name)
}

game_meta <- game_order_df %>%
  arrange(game_index) %>%
  select(game_index, game_id, week, week_num)

# =========================================================
# Elo runner over a list of ordered games
# Returns player Elo after every game
# =========================================================
run_elo_over_games <- function(game_list, initial_elo_vec, player_names) {
  elo_vec <- initial_elo_vec
  
  rusher_counts <- setNames(integer(length(player_names)), player_names)
  blocker_counts <- setNames(integer(length(player_names)), player_names)
  
  out_list <- vector("list", length(game_list))
  
  for (g in seq_along(game_list)) {
    dt <- as.data.table(copy(game_list[[g]]))
    
    for (i in seq_len(nrow(dt))) {
      r_id <- dt$rusher_name[i]
      b_id <- dt$blocker_name[i]
      out  <- dt$outcome_score[i]
      d_t  <- dt$double_team[i]
      
      if (is.na(r_id) || is.na(b_id) || is.na(out)) next
      
      rusher_counts[[r_id]] <- rusher_counts[[r_id]] + 1L
      blocker_counts[[b_id]] <- blocker_counts[[b_id]] + 1L
      
      r_K <- adaptive_K_prov(
        n = rusher_counts[[r_id]],
        K_start = 20, K_min = 10, n_prov = 87, n_decay = 200
      )
      
      b_K <- adaptive_K_prov(
        n = blocker_counts[[b_id]],
        K_start = 20, K_min = 10, n_prov = 87, n_decay = 200
      )
      
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
    
    out_list[[g]] <- unname(elo_vec[player_names])
  }
  
  out_mat <- do.call(rbind, out_list)
  colnames(out_mat) <- player_names
  out_mat
}

# =========================================================
# Bootstrap rows within each game, preserving game order
# =========================================================
bootstrap_game_rows <- function(game_df) {
  n <- nrow(game_df)
  idx <- sample.int(n = n, size = n, replace = TRUE)
  game_df[idx, ] %>%
    mutate(.boot_order = seq_len(n())) %>%
    arrange(.boot_order) %>%
    select(-.boot_order)
}

make_boot_game_list <- function(game_data_list) {
  lapply(game_data_list, bootstrap_game_rows)
}

# =========================================================
# Observed Elo path by game
# =========================================================
observed_mat <- run_elo_over_games(
  game_list = game_data_list,
  initial_elo_vec = initial_elo_vec,
  player_names = player_names
)

observed_long <- as_tibble(observed_mat) %>%
  mutate(game_index = seq_len(nrow(observed_mat))) %>%
  pivot_longer(
    cols = -game_index,
    names_to = "player_name",
    values_to = "observed_elo"
  ) %>%
  left_join(game_meta, by = "game_index") %>%
  left_join(player_roles, by = "player_name")

# =========================================================
# Bootstrap replicates
# Each replicate produces Elo for every player after every game
# =========================================================
set.seed(SEED)
plan(multisession, workers = N_WORKERS)

start_time <- Sys.time()

boot_results <- future_map(
  seq_len(B),
  function(b) {
    boot_games <- make_boot_game_list(game_data_list)
    
    boot_mat <- run_elo_over_games(
      game_list = boot_games,
      initial_elo_vec = initial_elo_vec,
      player_names = player_names
    )
    
    list(iter = b, mat = boot_mat)
  },
  .options = furrr_options(seed = TRUE)
)

end_time <- Sys.time()
print(end_time - start_time)

# =========================================================
# Summarize bootstrap distributions by player x game
# =========================================================
weekly_summary_list <- vector("list", n_games)
if (STORE_FULL_DRAWS) full_draws_list <- vector("list", n_games)

for (g in seq_len(n_games)) {
  boot_mat_g <- do.call(
    rbind,
    lapply(boot_results, function(x) x$mat[g, , drop = FALSE])
  )
  
  colnames(boot_mat_g) <- player_names
  
  players_this_game <- players_by_game[[g]]
  
  observed_g <- observed_long %>%
    filter(game_index == g) %>%
    arrange(match(player_name, player_names))
  
  weekly_summary_list[[g]] <- tibble(
    game_index = g,
    player_name = player_names,
    mean_final_elo = colMeans(boot_mat_g, na.rm = TRUE),
    sd_final_elo   = apply(boot_mat_g, 2, sd, na.rm = TRUE),
    q025           = apply(boot_mat_g, 2, quantile, probs = 0.025, na.rm = TRUE),
    q50            = apply(boot_mat_g, 2, quantile, probs = 0.50, na.rm = TRUE),
    q975           = apply(boot_mat_g, 2, quantile, probs = 0.975, na.rm = TRUE),
    played_this_game = player_names %in% players_this_game,
    observed_elo = observed_g$observed_elo
  ) %>%
    left_join(game_meta, by = "game_index") %>%
    left_join(player_roles, by = "player_name")
  
  if (STORE_FULL_DRAWS) {
    full_draws_list[[g]] <- as_tibble(boot_mat_g) %>%
      mutate(iter = seq_len(nrow(boot_mat_g))) %>%
      pivot_longer(
        cols = -iter,
        names_to = "player_name",
        values_to = "elo"
      ) %>%
      mutate(game_index = g) %>%
      left_join(game_meta, by = "game_index") %>%
      left_join(player_roles, by = "player_name")
  }
  
  rm(boot_mat_g)
  gc()
}

game_elo_uncertainty_summary <- bind_rows(weekly_summary_list)

if (STORE_FULL_DRAWS) {
  game_elo_boot_draws <- bind_rows(full_draws_list)
}

# =========================================================
# Save
# =========================================================
data.table::fwrite(
  game_elo_uncertainty_summary,
  "~/Downloads/game_elo_uncertainty_summary2.csv"
)

data.table::fwrite(
  observed_long,
  "~/Downloads/game_observed_elo_path2.csv"
)

if (STORE_FULL_DRAWS) {
  data.table::fwrite(
    game_elo_boot_draws,
    "~/Downloads/game_elo_boot_draws.csv"
  )
}

