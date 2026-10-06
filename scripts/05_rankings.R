source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
fits <- read_result("models")$full
scores <- rbindlist(lapply(fits, function(fit) {
  player_scores(fit, state$reference[[fit$model]], state$data, config = config)
}))
scores$method <- "bt"
baselines <- rbindlist(lapply(c("win", "severity"), function(model) {
  baseline_scores(state$data, model, state$vocabulary, config)
}))
labeled <- add_honors(bind_rows(as.data.frame(scores), as.data.frame(baselines)))
ol_keys <- unique(unlist(lapply(seq_len(nrow(state$data)), function(i) {
  state$data$blocker_keys[[i]][as.logical(jsonlite::fromJSON(state$data$blocker_rated_ol[i]))]
})))
labeled$rated_ol <- labeled$role == "Blocker" & labeled$player_key %in% ol_keys
ranked_all <- rank_cohorts(labeled)
# The main blocker comparisons use offensive linemen; all protectors remain fitted.
ranked <- rank_cohorts(labeled[labeled$role == "Rusher" | labeled$rated_ol, ])
pairs <- rank_pairs(ranked)
correlations <- rank_correlations(pairs)
honors <- honors_metrics(ranked)
save_result(list(scores = scores, baselines = baselines, ranked = ranked, ranked_all = ranked_all, pairs = pairs,
                correlations = correlations, honors = honors), "rankings")
write_table(ranked, "rankings")
write_table(ranked_all, "all_protector_rankings")
write_table(correlations, "rank_correlations")
write_table(honors, "all_pro_alignment")
for (model in c("win", "severity")) for (role in c("Rusher", "Blocker")) {
  kind <- if (model == "win") "coefficient" else "expected_severity"
  z <- ranked[ranked$model == model & ranked$role == role & ranked$method == "bt" &
               ranked$score_type == kind & ranked$min_interactions == 200, ]
  message(model, ": ", role, " (at least 200 matchups)")
  print(head(z[order(-z$score), c("player_name", "role_interactions", "score", "rank_by_role")], 10), row.names = FALSE)
}
