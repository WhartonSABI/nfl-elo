source("scripts/00_config.R")
state <- read_result("data")
config <- read_result("epa")$config
controls <- checkpoint_settings(state)
iterations <- config$bootstrap_iterations
check(iterations >= 2L, "Use at least two bootstrap draws")
for (scope in c("validation", "ratings")) {
  draws <- lapply(seq_len(iterations), function(id) {
    message(scope, " bootstrap ", id, "/", iterations)
    checkpoint(file.path(settings$output_dir, "bootstrap", scope, sprintf("%04d.rds", id)), controls, function() {
      bootstrap_refit(state$data, config, state$folds, state$vocabulary, state$reference,
                      scope, id, state$epa_state)
    })
  })
  values <- rbindlist(lapply(draws, `[[`, "result"))
  if (scope == "validation") {
    intervals <- values[, .(improvement_mean = mean(improvement),
      improvement_q025 = quantile(improvement, .025), improvement_q975 = quantile(improvement, .975),
      percent_improvement_mean = mean(percent_improvement),
      percent_improvement_q025 = quantile(percent_improvement, .025),
      percent_improvement_q975 = quantile(percent_improvement, .975), n_boot = .N), by = .(model, baseline)]
  } else {
    values[, rank := rank(-score, ties.method = "average", na.last = "keep"),
           by = .(replicate_id, model, score_type, role)]
    values[, top10_credit := if (all(is.finite(score))) topk_credit(score) else rep(NA_real_, .N),
           by = .(replicate_id, model, score_type, role)]
    intervals <- values[, .(score_mean = mean(score), score_sd = sd(score),
      q025 = if (all(is.finite(score))) quantile(score, .025) else NA_real_,
      q50 = if (all(is.finite(score))) quantile(score, .5) else NA_real_,
      q975 = if (all(is.finite(score))) quantile(score, .975) else NA_real_,
      rank_q025 = if (all(is.finite(rank))) quantile(rank, .025) else NA_real_,
      rank_q975 = if (all(is.finite(rank))) quantile(rank, .975) else NA_real_,
      n_boot = .N, defined_fraction = mean(is.finite(score)), presence_rate = mean(present_in_draw),
      top10_probability = mean(top10_credit)), by = .(model, score_type, role, player_key, player_id, player_name)]
    paired <- paired_rank_uncertainty(read_result("rankings")$pairs, values, iterations)
    write_table(paired, "paired_rank_uncertainty")
    scoped <- scoped_rank_intervals(values, read_result("rankings")$ranked, iterations)
    write_table(scoped, "rank_uncertainty")
  }
  coefficient_intervals <- epa_coefficient_uncertainty(lapply(draws, function(draw) draw$calibration$fit),
                                                    read_result("epa")$point$fit)
  denominators <- vapply(draws, function(draw) draw$calibration$weights$denominator, numeric(1))
  defined <- vapply(draws, function(draw) draw$calibration$weights$defined, logical(1))
  denominator_interval <- if (all(is.finite(denominators))) quantile(denominators, c(.025, .975)) else c(NA, NA)
  normalization <- data.frame(scope = scope, defined_fraction = mean(defined),
    sack_q025 = denominator_interval[1], sack_q975 = denominator_interval[2],
    normalization_supported = all(defined) && isTRUE(denominator_interval[2] < 0))
  diagnostics <- rbindlist(lapply(draws, function(draw) {
    data.frame(replicate_id = draw$replicate_id, model = names(draw$selected_lambda),
      lambda = draw$selected_lambda, grid_boundary = draw$boundary,
      warnings = vapply(draw$warnings, paste, character(1), collapse = "; "))
  }))
  save_result(list(intervals = intervals, epa = coefficient_intervals, normalization = normalization,
                  diagnostics = diagnostics), paste0(scope, "_uncertainty"))
  write_table(intervals, paste0(scope, "_uncertainty"))
  write_table(coefficient_intervals$summary, paste0(scope, "_epa_uncertainty"))
  write_table(diagnostics, paste0(scope, "_bootstrap_diagnostics"))
  print(normalization, row.names = FALSE)
  if (scope == "validation") print(intervals)
  if (any(diagnostics$grid_boundary)) warning("Some penalties lie at a grid edge; inspect the CV curves and widen the grid if needed.")
  rm(draws, values)
  invisible(gc(FALSE))
}
