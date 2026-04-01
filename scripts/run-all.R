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
scripts_dir <- file.path(project_root, "scripts")
as_flag <- function(name, default = FALSE) {
  default_raw <- if (isTRUE(default)) "1" else "0"
  raw <- tolower(Sys.getenv(name, unset = default_raw))
  raw %in% c("1", "true", "yes", "y")
}

skip_build_inputs <- as_flag("SKIP_BUILD_INPUTS", FALSE)
force_rebuild_inputs <- as_flag("FORCE_REBUILD_INPUTS", FALSE)
force_rebuild_modeling <- as_flag("FORCE_REBUILD_MODELING", FALSE)

input_files <- file.path(
  project_root,
  "data",
  "input",
  c("matchups.csv", "sacks.csv", "hits.csv", "hudl_iq_game_ids.csv")
)
modeling_table_path <- file.path(project_root, "data", "output", "shared", "modeling_table.csv")

inputs_ready <- all(file.exists(input_files))
modeling_ready <- file.exists(modeling_table_path)

need_build_inputs <- !inputs_ready || isTRUE(force_rebuild_inputs)
if (isTRUE(skip_build_inputs) && !isTRUE(force_rebuild_inputs)) {
  need_build_inputs <- FALSE
}

need_build_modeling <- !modeling_ready || need_build_inputs || isTRUE(force_rebuild_modeling)

pipeline_scripts <- character(0)

if (need_build_inputs) {
  pipeline_scripts <- c(pipeline_scripts, "01_build-inputs.R")
} else {
  message("Skipping 01_build-inputs.R (cached input files already exist).")
}

if (need_build_modeling) {
  pipeline_scripts <- c(pipeline_scripts, "02_build-modeling-table.R")
} else {
  message("Skipping 02_build-modeling-table.R (cached modeling table already exists).")
}

pipeline_scripts <- c(
  pipeline_scripts,
  "03_fit-bt-win-model.R",
  "04_validate-bt-win-model.R",
  "05_uncertainty-bt-win-model.R",
  "06_fit-bt-severity-model.R",
  "07_validate-bt-severity-model.R",
  "08_uncertainty-bt-severity-model.R",
  "09_build-full-bt-leaderboard.R",
  "10_validate-bt-all-pro.R",
  "11_validate-bt-baseline-prior-sensitivity.R"
)

for (script_name in pipeline_scripts) {
  message("Running: ", script_name)
  sys.source(file.path(scripts_dir, script_name), envir = globalenv())
}

message("Pipeline complete.")
