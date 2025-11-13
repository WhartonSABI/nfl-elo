pacman::p_load(tidyverse, dplyr, ggthemes, data.table, lubridate, glmnet,
               GGally, RColorBrewer, ggsci, plotROC, usmap,
               plotly, ggpubr, vistime, coefplot, skimr, car, ggrepel, slider, lubridate,
               tidymodels,ranger,vip,ggplot2, tune,dials,pdp, purrr, stringr, lmtest,
               sandwich, parallel, furrr, future)


results2 <- data.table::fread("~/GitHub/nfl-elo/data/results/results2.csv")
sacks <- data.table::fread("~/GitHub/nfl-elo/data/results/sacks.csv")
hits <- data.table::fread("~/GitHub/nfl-elo/data/results/hits.csv")

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
      hit == 1L          ~ 0.4,
      rusher_won == 1L   ~ 0.2,
      TRUE               ~ 0.0
    )
  )

results_cleaned <- results2 %>% 
  drop_na(game_id, rusher_name, blocker_name) %>% 
  arrange(game_id, play_id, event_game_index) %>% 
  select(-sack, -hit, -rusher_won)

#results_cleaned <- results_cleaned %>%
#left_join(outcome_summary, by = c("game_id", "play_id", "rusher_name"))

init_play_ratings <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(everything(), names_to = "type", values_to = "player_name") %>%
  mutate(Player_Elo = ifelse(type == "rusher_name", 800, 1200)) %>%  
  distinct(player_name, .keep_all = TRUE)

elo_vec <- setNames(init_play_ratings$Player_Elo, init_play_ratings$player_name)

#adaptive learning rate from max
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

#updated elo function to include K and double teams
elo_step <- function(
    belo, relo, outcome_score, blocker_K, rusher_K,
    double_team, scale = 400
) {
  if (is.na(outcome_score)) return(c(b = belo, r = relo))
  
  blocker_bonus <- ifelse(double_team == 1, 100, 0)
  adj_belo <- belo + blocker_bonus
  
  #calc based on rusher win prob
  exp_rusher <- 1 / (1 + 10 ^ ((adj_belo - relo) / scale))
  
  #rusher rewarded when they win; blocker rewarded when rusher fails
  c(
    b = belo + blocker_K * ((1 - outcome_score) - (1 - exp_rusher)), 
    r = relo + rusher_K  * ( outcome_score      -      exp_rusher)   
  )
}

get_elo <- function(id) {
  id <- as.character(id)  
  if (!id %in% names(elo_vec)) elo_vec[[id]] <- 1000
  return(elo_vec[[id]])
} 

results_cleaned <- as.data.table(results_cleaned)

results_cleaned <- results_cleaned %>%
  group_by(rusher_name) %>%
  mutate(rusher_row = row_number()) %>%
  ungroup() %>%
  group_by(blocker_name) %>%
  mutate(blocker_row = row_number()) %>%
  ungroup()

#compute k for rusher and blocker
results_cleaned <- results_cleaned %>%
  mutate(
    rusher_k = adaptive_K_prov(n = rusher_row, K_start = 40, K_min = 20, n_prov = 50, n_decay = 200),
    blocker_k = adaptive_K_prov(blocker_row, K_start = 40, K_min = 20, n_prov = 50, n_decay = 200)
  )

interaction_counts <- list()  # Initialize interaction count
elo_vec <- setNames(init_play_ratings$Player_Elo, init_play_ratings$player_name)

elo_history <- results_cleaned %>%
  mutate(
    before_rusher_elo = NA_real_,
    before_blocker_elo = NA_real_,
    after_rusher_elo = NA_real_,
    after_blocker_elo = NA_real_,
    rusher_n           = rusher_row,
    blocker_n          = blocker_row
  )

for (i in seq_len(nrow(results_cleaned))) {
  r_id <- results_cleaned$rusher_name[i]
  b_id <- results_cleaned$blocker_name[i]
  out  <- results_cleaned$outcome_score[i]
  r_K  <- results_cleaned$rusher_k[i]
  b_K  <- results_cleaned$blocker_k[i]
  d_t <- results_cleaned$double_team[i]
  
  if (is.na(r_id) || is.na(b_id) || is.na(out)) next
  
  r_elo <- get_elo(r_id)
  b_elo <- get_elo(b_id)
  
  elo_history$before_rusher_elo[i]  <- r_elo
  elo_history$before_blocker_elo[i] <- b_elo
  
  new <- elo_step(b_elo, r_elo, outcome_score = out, blocker_K = b_K, rusher_K = r_K, double_team = d_t)
  
  if (!any(is.na(new))) {
    elo_vec[[r_id]] <- new["r"]
    elo_vec[[b_id]] <- new["b"]
  }
  
  elo_history$after_rusher_elo[i]  <- new["r"]
  elo_history$after_blocker_elo[i] <- new["b"]
}

#get results
elo_df <- enframe(elo_vec, name = "player_name", value = "final_elo")

#differentiate between rushers and blockers
player_roles <- results_cleaned %>%
  select(rusher_name, blocker_name) %>%
  pivot_longer(cols = everything(), values_to = "player_name") %>%
  mutate(role = ifelse(name == "rusher_name", "Rusher", "Blocker")) %>%
  distinct(player_name, role)

#join to get elo with annotated rusher/blocker label
elo_labeled <- elo_df %>%
  left_join(player_roles, by = "player_name") %>%
  filter(!is.na(role))

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

final_elos <- elo_long %>%
  group_by(player_name, role) %>%
  filter(interaction_n == max(interaction_n, na.rm = TRUE)) %>%
  ungroup()

# Select top 10 per role based on final Elo
top_players <- final_elos %>%
  group_by(role) %>%
  filter(interaction_n > 200) %>% 
  slice_max(elo, n = 10) %>%
  pull(player_name)

elo_top <- elo_long %>%
  filter(player_name %in% top_players) %>%
  group_by(player_name)%>%
  arrange(desc(interaction_n)) %>%
  slice_head(n=1)

names(elo_history)

blockers <- elo_history %>%
  pull(blocker_name)

rushers <- elo_history %>%
  pull(rusher_name)

final_elos %>%
  group_by(role) %>%
  summarise(
    mean_n = mean(interaction_n),
    sd_n = sd(interaction_n)
  )

elo_history %>%
  group_by(factor(outcome_score)) %>%
  summarise(
    N = n()
  ) %>% ungroup()%>% mutate(
    Perc = N / 94539 *100
  )

elo_history %>%
  summarise(mean_d = mean(double_team))


