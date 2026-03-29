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

ensure_output_directories(PIPELINE_CONFIG)

message("Building canonical modeling table...")
modeling_table <- build_modeling_table(PIPELINE_CONFIG)

modeling_summary <- tibble(
  rows = nrow(modeling_table),
  unique_games = dplyr::n_distinct(modeling_table$game_id),
  unique_rushers = dplyr::n_distinct(modeling_table$rusher_name),
  unique_blockers = dplyr::n_distinct(modeling_table$blocker_name),
  win_target_mean = mean(modeling_table$win_target, na.rm = TRUE),
  severity_target_mean = mean(modeling_table$severity_target, na.rm = TRUE)
)

write_output_csv(modeling_table, PIPELINE_CONFIG$output_paths$modeling_table)
write_output_csv(modeling_summary, PIPELINE_CONFIG$output_paths$modeling_summary)

message("Wrote modeling table: ", PIPELINE_CONFIG$output_paths$modeling_table)
