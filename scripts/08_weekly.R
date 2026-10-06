source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
fits <- read_result("models")$full
controls <- checkpoint_settings(state)
iterations <- config$bootstrap_iterations
point <- lapply(1:18, function(week) weekly_estimates(state, config, fits, week))
write_table(rbindlist(lapply(point, `[[`, "scores"), fill = TRUE), "weekly_scores")
write_table(rbindlist(lapply(point, `[[`, "diagnostics")), "weekly_point_diagnostics")
save_result(point, "weekly_point")

for (id in seq_len(iterations)) {
  message("Weekly trajectory ", id, "/", iterations)
  path <- file.path(settings$output_dir, "bootstrap/ratings", sprintf("%04d.rds", id))
  check(file.exists(path), "Run 06_bootstrap.R before the weekly trajectories")
  draw <- readRDS(path)
  check(identical(draw$controls, controls), "Rating draw inputs or settings changed")
  for (week in 1:18) {
    checkpoint(file.path(settings$output_dir, "bootstrap/weekly", sprintf("%04d", id), sprintf("week-%02d.rds", week)),
      controls, function() weekly_estimates(state, config, fits, week, draw$result))
  }
}

summaries <- diagnostics <- list()
for (week in 1:18) {
  raw_point <- point[[week]]$raw
  samples <- lapply(seq_len(iterations), function(id) {
    value <- readRDS(file.path(settings$output_dir, "bootstrap/weekly", sprintf("%04d", id), sprintf("week-%02d.rds", week)))$result
    value$scores$replicate_id <- rep(id, nrow(value$scores))
    if (nrow(value$scores)) {
      index <- match(paste(value$scores$role, value$scores$player_key), paste(value$raw$role, value$raw$player_key))
      value$scores$raw_score <- ifelse(value$scores$model == "win", value$raw$raw_score[index], NA_real_)
    }
    value
  })
  scores <- rbindlist(lapply(samples, `[[`, "scores"), fill = TRUE)
  p <- as.data.table(point[[week]]$scores)
  availability <- rbindlist(lapply(samples, `[[`, "availability"))
  diagnostics[[week]] <- rbindlist(lapply(samples, `[[`, "diagnostics"))
  if (!nrow(p)) next
  p[, key := weekly_key(p)]
  availability[, key := weekly_key(availability)]
  absence <- availability[, .(absent_draws = sum(!present_in_draw), planned_draws = .N), by = key]
  if (nrow(scores)) {
    scores[, key := weekly_key(scores)]
    intervals <- scores[, c(weekly_intervals(score, iterations), list(
      raw_lower = if (.N == iterations && all(is.finite(raw_score))) quantile(raw_score, .025) else NA_real_,
      raw_upper = if (.N == iterations && all(is.finite(raw_score))) quantile(raw_score, .975) else NA_real_)), by = key]
  } else intervals <- data.table(key = character(), defined_draws = integer(), defined_fraction = numeric(),
    lower = numeric(), median = numeric(), upper = numeric(), conditional_lower = numeric(), conditional_upper = numeric(),
    interval_status = character(), raw_lower = numeric(), raw_upper = numeric())
  summary <- merge(p, intervals, by = "key", all.x = TRUE, sort = FALSE)
  summary <- merge(summary, absence, by = "key", all.x = TRUE, sort = FALSE)
  summary[is.na(defined_draws), `:=`(defined_draws = 0L, defined_fraction = 0, interval_status = "nonestimable_draws_retained")]
  index <- match(paste(summary$role, summary$player_key), paste(raw_point$role, raw_point$player_key))
  summary[, raw_score := ifelse(model == "win", raw_point$raw_score[index], NA_real_)]
  summary[, key := NULL]
  summaries[[week]] <- summary
}
summary <- rbindlist(summaries, fill = TRUE)
write_table(summary, "weekly_uncertainty")
write_table(rbindlist(diagnostics), "weekly_bootstrap_diagnostics")
save_result(summary, "weekly_uncertainty")
print(summary[, .(players = .N, complete_intervals = sum(interval_status == "complete")), by = .(week, model, role)])
