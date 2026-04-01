locate_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, ".git")) && dir.exists(file.path(current, "data"))) {
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

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

paper_dir <- file.path(project_root, "jqas")

validation_win_path <- file.path(project_root, "data", "output", "win", "validation_metrics_win_bt_ridge.csv")
validation_sev_path <- file.path(project_root, "data", "output", "severity", "validation_metrics_severity_bt_ridge.csv")
uncertainty_win_path <- file.path(project_root, "data", "output", "win", "validation_uncertainty_win_bt_ridge.csv")
uncertainty_sev_path <- file.path(project_root, "data", "output", "severity", "validation_uncertainty_severity_bt_ridge.csv")
allpro_metrics_path <- file.path(project_root, "data", "output", "shared", "validation_all_pro_metrics_bt_ridge.csv")
leaderboard_path <- file.path(project_root, "data", "output", "shared", "leaderboard_full_bt_ridge.csv")
prior_sensitivity_path <- file.path(project_root, "data", "output", "shared", "validation_baseline_prior_sensitivity_bt_ridge.csv")

latex_escape <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "\\\\textbackslash{}")
  x <- str_replace_all(x, "([#$%&_{}])", "\\\\\\1")
  x <- str_replace_all(x, "~", "\\\\textasciitilde{}")
  x <- str_replace_all(x, "\\^", "\\\\textasciicircum{}")
  x
}

fmt_num <- function(x, digits = 4L) {
  sprintf(paste0("%.", as.integer(digits), "f"), as.numeric(x))
}

write_tex_lines <- function(path, lines) {
  writeLines(as.character(lines), con = path, useBytes = TRUE)
}

as_tabular_fragment <- function(lines) {
  if (!length(lines)) {
    return(lines)
  }

  is_row <- !str_starts(lines, "\\\\")
  row_idx <- which(is_row)
  if (!length(row_idx)) {
    return(lines)
  }

  out <- lines
  last_row <- tail(row_idx, 1)
  for (i in row_idx) {
    if (i != last_row) {
      out[i] <- paste0(out[i], " \\cr")
    }
  }
  out
}

# ------------------------------------------------------------
# Validation table rows
# ------------------------------------------------------------

win_val <- read_csv(validation_win_path, show_col_types = FALSE)
sev_val <- read_csv(validation_sev_path, show_col_types = FALSE)
win_unc <- read_csv(uncertainty_win_path, show_col_types = FALSE)
sev_unc <- read_csv(uncertainty_sev_path, show_col_types = FALSE)

val_tbl <- bind_rows(
  win_val %>%
    mutate(task = "Win"),
  sev_val %>%
    mutate(task = "Severity")
) %>%
  mutate(
    baseline_label = case_when(
      baseline_name %in% c("global_mean", "global_class_freq") ~ "Global",
      TRUE ~ "Matchup"
    )
  )

unc_tbl <- bind_rows(
  win_unc %>% mutate(task = "Win"),
  sev_unc %>% mutate(task = "Severity")
) %>%
  select(task, baseline_name, improvement_q025, improvement_q975)

validation_rows <- val_tbl %>%
  left_join(unc_tbl, by = c("task", "baseline_name")) %>%
  mutate(
    sort_task = if_else(task == "Win", 1L, 2L),
    sort_base = if_else(baseline_label == "Global", 1L, 2L)
  ) %>%
  arrange(sort_task, sort_base) %>%
  transmute(
    line = paste0(
      task, " & ",
      baseline_label, " & ",
      fmt_num(model_value, 4), " & ",
      fmt_num(baseline_value, 4), " & ",
      fmt_num(improvement, 4), " & [",
      fmt_num(improvement_q025, 4), ", ",
      fmt_num(improvement_q975, 4), "]"
    )
  ) %>%
  pull(line)

write_tex_lines(
  path = file.path(paper_dir, "tab_validation_rows.tex"),
  lines = as_tabular_fragment(validation_rows)
)

# ------------------------------------------------------------
# All-Pro table rows
# ------------------------------------------------------------

allpro_metrics <- read_csv(allpro_metrics_path, show_col_types = FALSE)

allpro_base <- allpro_metrics %>%
  filter(model_name == "baseline") %>%
  select(
    model_metric,
    role,
    accolade_set,
    base_auc = auc,
    base_enrichment = enrichment_at_k
  )

allpro_models <- allpro_metrics %>%
  filter(model_name == "bt_ridge", model_metric %in% c("win_bt_score", "severity_bt_score")) %>%
  mutate(
    task = if_else(model_metric == "win_bt_score", "Win/Loss", "Severity"),
    baseline_metric = if_else(model_metric == "win_bt_score", "raw_win_rate", "raw_severity_ev")
  ) %>%
  left_join(
    allpro_base,
    by = c("baseline_metric" = "model_metric", "role", "accolade_set")
  ) %>%
  mutate(
    delta_auc = auc - base_auc,
    delta_enrichment = enrichment_at_k - base_enrichment
  )

build_allpro_rows <- function(accolade_key) {
  allpro_models %>%
    filter(accolade_set == accolade_key) %>%
    mutate(
      sort_task = if_else(task == "Win/Loss", 1L, 2L),
      sort_role = if_else(role == "Blocker", 1L, 2L)
    ) %>%
    arrange(sort_task, sort_role) %>%
    transmute(
      line = paste0(
        task, " & ",
        role, " & ",
        as.integer(k_eval), " & ",
        fmt_num(auc, 3), " & ",
        fmt_num(base_auc, 3), " & ",
        fmt_num(delta_auc, 3), " & ",
        fmt_num(enrichment_at_k, 2), " & ",
        fmt_num(base_enrichment, 2), " & ",
        fmt_num(delta_enrichment, 2)
      )
    ) %>%
    pull(line)
}

write_tex_lines(
  path = file.path(paper_dir, "tab_allpro_first_rows.tex"),
  lines = as_tabular_fragment(build_allpro_rows("first_team"))
)

write_tex_lines(
  path = file.path(paper_dir, "tab_allpro_any_rows.tex"),
  lines = as_tabular_fragment(build_allpro_rows("any_team"))
)

# ------------------------------------------------------------
# Top-5 table rows
# ------------------------------------------------------------

leaderboard <- read_csv(leaderboard_path, show_col_types = FALSE)
min_interactions <- 200L
top_n <- 5L

top_group <- function(model_label, role_label, score_col) {
  leaderboard %>%
    filter(role == role_label, role_interactions >= min_interactions, !is.na(.data[[score_col]])) %>%
    arrange(desc(.data[[score_col]]), player_name) %>%
    slice_head(n = top_n) %>%
    transmute(
      model = model_label,
      role = role_label,
      player_name = latex_escape(player_name),
      rating = fmt_num(.data[[score_col]], 3)
    )
}

top_tbl <- bind_rows(
  top_group("Win/Loss", "Rusher", "win_bt_logit_score"),
  top_group("Win/Loss", "Blocker", "win_bt_logit_score"),
  top_group("Severity", "Rusher", "severity_weighted_logit_score"),
  top_group("Severity", "Blocker", "severity_weighted_logit_score")
) %>%
  mutate(group_id = paste(model, role))

top5_lines <- character(0)
for (gid in unique(top_tbl$group_id)) {
  block <- top_tbl %>% filter(group_id == gid)
  block_lines <- block %>%
    transmute(line = paste0(model, " & ", role, " & ", player_name, " & ", rating)) %>%
    pull(line)
  top5_lines <- c(top5_lines, block_lines)
  if (!identical(gid, tail(unique(top_tbl$group_id), 1))) {
    top5_lines <- c(top5_lines, "\\addlinespace")
  }
}

write_tex_lines(
  path = file.path(paper_dir, "tab_top5_rows.tex"),
  lines = as_tabular_fragment(top5_lines)
)

# ------------------------------------------------------------
# Baseline prior-sensitivity table rows
# ------------------------------------------------------------

prior_sensitivity <- read_csv(prior_sensitivity_path, show_col_types = FALSE)

prior_sensitivity_rows <- prior_sensitivity %>%
  mutate(
    task_label = if_else(task == "win", "Win", "Severity"),
    sort_task = if_else(task == "win", 1L, 2L)
  ) %>%
  arrange(sort_task, prior_strength) %>%
  transmute(
    line = paste0(
      task_label, " & ",
      as.integer(prior_strength), " & ",
      fmt_num(model_value, 4), " & ",
      fmt_num(baseline_value, 4), " & ",
      fmt_num(improvement, 4)
    )
  ) %>%
  pull(line)

write_tex_lines(
  path = file.path(paper_dir, "tab_prior_sensitivity_rows.tex"),
  lines = as_tabular_fragment(prior_sensitivity_rows)
)

message("Wrote table row fragments to: ", paper_dir)
