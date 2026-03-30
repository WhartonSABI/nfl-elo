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

normalize_player_name <- function(x) {
  out <- trimws(as.character(x))
  out <- tolower(out)
  out <- gsub("[^a-z0-9 ]", "", out)
  out <- gsub("\\s+", " ", out)
  out
}

compute_auc_rank <- function(labels, scores) {
  labels <- as.integer(labels)
  scores <- as.numeric(scores)

  keep <- !is.na(labels) & !is.na(scores)
  labels <- labels[keep]
  scores <- scores[keep]

  n_pos <- sum(labels == 1L)
  n_neg <- sum(labels == 0L)
  if (n_pos == 0L || n_neg == 0L) {
    return(NA_real_)
  }

  ranks <- rank(scores, ties.method = "average")
  (sum(ranks[labels == 1L]) - n_pos * (n_pos + 1L) / 2) / (n_pos * n_neg)
}

summarise_rank_alignment <- function(df, label_col, accolade_set) {
  labels <- as.integer(df[[label_col]])
  scores <- as.numeric(df$score)

  keep <- !is.na(labels) & !is.na(scores)
  labels <- labels[keep]
  scores <- scores[keep]
  n_players <- length(scores)

  if (n_players == 0L) {
    return(
      tibble(
        accolade_set = accolade_set,
        n_players = 0L,
        n_positive = 0L,
        n_negative = 0L,
        base_rate = NA_real_,
        k_eval = 0L,
        hits_at_k = 0L,
        precision_at_k = NA_real_,
        recall_at_k = NA_real_,
        enrichment_at_k = NA_real_,
        hits_at_10 = 0L,
        precision_at_10 = NA_real_,
        recall_at_10 = NA_real_,
        auc = NA_real_,
        mean_rank_positive = NA_real_,
        median_rank_positive = NA_real_,
        mean_percentile_positive = NA_real_,
        median_percentile_positive = NA_real_
      )
    )
  }

  n_positive <- sum(labels == 1L)
  n_negative <- sum(labels == 0L)
  base_rate <- n_positive / n_players

  rank_desc <- rank(-scores, ties.method = "average")
  percentile_desc <- if (n_players <= 1L) rep(NA_real_, n_players) else (n_players - rank_desc) / (n_players - 1L)

  k_eval <- min(n_positive, n_players)
  hits_at_k <- 0L
  precision_at_k <- NA_real_
  recall_at_k <- NA_real_
  enrichment_at_k <- NA_real_

  if (k_eval > 0L) {
    top_k_idx <- order(scores, decreasing = TRUE)[seq_len(k_eval)]
    hits_at_k <- sum(labels[top_k_idx] == 1L)
    precision_at_k <- hits_at_k / k_eval
    recall_at_k <- hits_at_k / n_positive
    enrichment_at_k <- if (is.na(base_rate) || base_rate <= 0) NA_real_ else precision_at_k / base_rate
  }

  top10_k <- min(10L, n_players)
  hits_at_10 <- 0L
  precision_at_10 <- NA_real_
  recall_at_10 <- NA_real_
  if (top10_k > 0L) {
    top10_idx <- order(scores, decreasing = TRUE)[seq_len(top10_k)]
    hits_at_10 <- sum(labels[top10_idx] == 1L)
    precision_at_10 <- hits_at_10 / top10_k
    recall_at_10 <- if (n_positive > 0L) hits_at_10 / n_positive else NA_real_
  }

  auc <- compute_auc_rank(labels, scores)
  mean_rank_positive <- if (n_positive > 0L) mean(rank_desc[labels == 1L]) else NA_real_
  median_rank_positive <- if (n_positive > 0L) median(rank_desc[labels == 1L]) else NA_real_
  mean_percentile_positive <- if (n_positive > 0L) mean(percentile_desc[labels == 1L], na.rm = TRUE) else NA_real_
  median_percentile_positive <- if (n_positive > 0L) median(percentile_desc[labels == 1L], na.rm = TRUE) else NA_real_

  tibble(
    accolade_set = accolade_set,
    n_players = n_players,
    n_positive = n_positive,
    n_negative = n_negative,
    base_rate = base_rate,
    k_eval = k_eval,
    hits_at_k = hits_at_k,
    precision_at_k = precision_at_k,
    recall_at_k = recall_at_k,
    enrichment_at_k = enrichment_at_k,
    hits_at_10 = hits_at_10,
    precision_at_10 = precision_at_10,
    recall_at_10 = recall_at_10,
    auc = auc,
    mean_rank_positive = mean_rank_positive,
    median_rank_positive = median_rank_positive,
    mean_percentile_positive = mean_percentile_positive,
    median_percentile_positive = median_percentile_positive
  )
}

project_root <- locate_project_root()
source(file.path(project_root, "scripts", "00_config.R"))
source(file.path(project_root, "scripts", "00_utils.R"))

ensure_output_directories(PIPELINE_CONFIG)

message("Validating BT leaderboard alignment with AP All-Pro selections...")

leaderboard_path <- PIPELINE_CONFIG$output_paths$bt_full_leaderboard
if (!file.exists(leaderboard_path)) {
  stop("Leaderboard file not found: ", leaderboard_path, ". Run scripts/09_build-full-bt-leaderboard.R first.")
}

leaderboard <- read_csv(leaderboard_path, show_col_types = FALSE)
assert_columns(
  leaderboard,
  c("player_name", "role", "win_bt_logit_score", "severity_weighted_logit_score"),
  "bt_full_leaderboard"
)

if (!"role_interactions" %in% names(leaderboard)) {
  leaderboard <- leaderboard %>% mutate(role_interactions = NA_integer_)
}

modeling_table <- read_modeling_table(PIPELINE_CONFIG)
assert_columns(
  modeling_table,
  c("rusher_name", "blocker_name", "win_target"),
  "modeling_table"
)

# AP All-Pro list for 2021 validation.
all_pro_reference <- tibble(
  player_name = c(
    "Trent Williams", "Tristan Wirfs", "Joel Bitonio", "Zack Martin", "Jason Kelce",
    "T.J. Watt", "Myles Garrett", "Aaron Donald", "Cameron Heyward", "Micah Parsons",
    "Darius Leonard", "De'Vondre Campbell", "Rashawn Slater", "Lane Johnson",
    "Quenton Nelson", "Wyatt Teller", "Corey Linsley", "Robert Quinn", "Maxx Crosby",
    "Chris Jones", "Jeffery Simmons", "Demario Davis", "Roquan Smith", "Bobby Wagner"
  ),
  all_pro_team = c(
    "First Team", "First Team", "First Team", "First Team", "First Team",
    "First Team", "First Team", "First Team", "First Team", "First Team",
    "First Team", "First Team", "Second Team", "Second Team", "Second Team",
    "Second Team", "Second Team", "Second Team", "Second Team", "Second Team",
    "Second Team", "Second Team", "Second Team", "Second Team"
  ),
  season = 2021L
) %>%
  mutate(
    all_pro_team = if_else(all_pro_team == "First Team", "first_team", "second_team"),
    is_all_pro_first_team = all_pro_team == "first_team",
    is_all_pro_any_team = TRUE,
    player_name_norm = normalize_player_name(player_name)
  ) %>%
  distinct(player_name_norm, .keep_all = TRUE)

bt_player_scores <- leaderboard %>%
  mutate(player_name_norm = normalize_player_name(player_name)) %>%
  select(
    player_name,
    player_name_norm,
    role,
    role_interactions,
    win_bt_logit_score,
    severity_weighted_logit_score
  ) %>%
  pivot_longer(
    cols = c(win_bt_logit_score, severity_weighted_logit_score),
    names_to = "model_metric",
    values_to = "score"
  ) %>%
  mutate(
    model_name = "bt_ridge",
    model_metric = recode(
      model_metric,
      win_bt_logit_score = "win_bt_score",
      severity_weighted_logit_score = "severity_bt_score"
    )
  ) %>%
  filter(!is.na(score)) %>%
  select(
    model_name,
    model_metric,
    player_name,
    player_name_norm,
    role,
    role_interactions,
    score
  )

baseline_player_scores <- bind_rows(
  modeling_table %>%
    group_by(player_name = rusher_name) %>%
    summarise(
      role = "Rusher",
      role_interactions = n(),
      score = mean(win_target, na.rm = TRUE),
      .groups = "drop"
    ),
  modeling_table %>%
    group_by(player_name = blocker_name) %>%
    summarise(
      role = "Blocker",
      role_interactions = n(),
      score = mean(1 - win_target, na.rm = TRUE),
      .groups = "drop"
    )
) %>%
  mutate(
    model_name = "baseline",
    model_metric = "raw_win_rate",
    player_name_norm = normalize_player_name(player_name)
  ) %>%
  select(
    model_name,
    model_metric,
    player_name,
    player_name_norm,
    role,
    role_interactions,
    score
  )

all_player_scores <- bind_rows(bt_player_scores, baseline_player_scores)

player_scores <- all_player_scores %>%
  left_join(
    all_pro_reference %>%
      select(
        player_name_norm,
        all_pro_player_name = player_name,
        all_pro_team,
        all_pro_season = season,
        is_all_pro_first_team,
        is_all_pro_any_team
      ),
    by = "player_name_norm"
  ) %>%
  mutate(
    is_all_pro_first_team = coalesce(is_all_pro_first_team, FALSE),
    is_all_pro_any_team = coalesce(is_all_pro_any_team, FALSE)
  ) %>%
  group_by(model_name, model_metric, role) %>%
  mutate(
    rank_by_role = rank(-score, ties.method = "average"),
    percentile_by_role = case_when(
      n() <= 1L ~ NA_real_,
      TRUE ~ (n() - rank_by_role) / (n() - 1)
    )
  ) %>%
  ungroup() %>%
  select(
    model_name,
    model_metric,
    player_name,
    role,
    role_interactions,
    score,
    rank_by_role,
    percentile_by_role,
    is_all_pro_first_team,
    is_all_pro_any_team,
    all_pro_team,
    all_pro_player_name,
    all_pro_season
  ) %>%
  arrange(model_metric, role, rank_by_role, player_name)

summary_metrics <- player_scores %>%
  group_by(model_name, model_metric, role) %>%
  group_modify(~ {
    bind_rows(
      summarise_rank_alignment(.x, "is_all_pro_first_team", "first_team"),
      summarise_rank_alignment(.x, "is_all_pro_any_team", "any_team")
    )
  }) %>%
  ungroup() %>%
  mutate(
    all_pro_season = 2021L,
    metric_scope = "role_ranking"
  ) %>%
  select(
    model_name,
    model_metric,
    role,
    metric_scope,
    all_pro_season,
    accolade_set,
    n_players,
    n_positive,
    n_negative,
    base_rate,
    auc,
    k_eval,
    hits_at_k,
    precision_at_k,
    recall_at_k,
    enrichment_at_k,
    hits_at_10,
    precision_at_10,
    recall_at_10,
    mean_rank_positive,
    median_rank_positive,
    mean_percentile_positive,
    median_percentile_positive
  ) %>%
  arrange(model_metric, role, accolade_set)

positive_matches <- player_scores %>%
  filter(is_all_pro_any_team) %>%
  arrange(model_metric, role, rank_by_role, player_name)

write_output_csv(player_scores, PIPELINE_CONFIG$output_paths$bt_all_pro_player_scores)
write_output_csv(summary_metrics, PIPELINE_CONFIG$output_paths$bt_all_pro_summary_metrics)
write_output_csv(positive_matches, PIPELINE_CONFIG$output_paths$bt_all_pro_positive_matches)

message("Wrote All-Pro player-level scores: ", PIPELINE_CONFIG$output_paths$bt_all_pro_player_scores)
message("Wrote All-Pro summary metrics: ", PIPELINE_CONFIG$output_paths$bt_all_pro_summary_metrics)
message("Wrote All-Pro positive matches: ", PIPELINE_CONFIG$output_paths$bt_all_pro_positive_matches)
message("All-Pro summary rows: ", nrow(summary_metrics))
