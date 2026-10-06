suppressPackageStartupMessages({
  library(Matrix)
  library(glmnet)
  library(data.table)
  library(jsonlite)
})

settings <- modifyList(list(
  input_dir = "data/input",
  output_dir = "results",
  seed = 20260916L,
  bootstrap_iterations = 1000L,
  nfolds = 5L,
  lambda_min = 1e-6,
  lambda_max = 1,
  lambda_length = 120L
), getOption("nfl.settings", list()))

# Model IDs: win is the early model; severity is the final outcome model.
model_config <- function(seed = settings$seed, lambda_min = settings$lambda_min,
                         lambda_max = settings$lambda_max, lambda_length = settings$lambda_length) {
  list(seed = as.integer(seed), nfolds = as.integer(settings$nfolds),
       train_last_week = 15L, test_weeks = 16:18,
       lambda = exp(seq(log(lambda_max), log(lambda_min), length.out = lambda_length)),
       classes = c("loss", "win", "pressure", "sack"),
       severity_weights = c(loss = 0, win = NA_real_, pressure = NA_real_, sack = 1),
       baseline_strength = c(win = 25, severity = 50),
       bootstrap_iterations = as.integer(settings$bootstrap_iterations),
       solver_tolerance = 1e-9, conditional_sack_share = NA_real_)
}

for (file in c("data", "models", "epa", "bootstrap", "rankings", "sensitivity", "weekly")) {
  source(file.path("scripts/functions", paste0(file, ".R")))
}
dir.create(settings$output_dir, recursive = TRUE, showWarnings = FALSE)

save_result <- function(value, name) saveRDS(value, file.path(settings$output_dir, paste0(name, ".rds")))
read_result <- function(name) {
  path <- file.path(settings$output_dir, paste0(name, ".rds"))
  check(file.exists(path), paste("Run the earlier pipeline steps first; missing", path))
  readRDS(path)
}
write_table <- function(value, name) fwrite(value, file.path(settings$output_dir, paste0(name, ".csv")))

# Checkpoints are reusable only with the same inputs, functions, settings, and R runtime.
checkpoint_settings <- function(state) {
  list(inputs = state$input_md5, settings = settings,
       code = tools::md5sum(c("scripts/00_config.R", list.files("scripts/functions", full.names = TRUE))),
       R = as.character(getRversion()),
       packages = sapply(c("Matrix", "glmnet", "data.table", "jsonlite"), function(p) as.character(packageVersion(p))))
}
checkpoint <- function(path, controls, compute) {
  if (file.exists(path)) {
    value <- readRDS(path)
    check(identical(value$controls, controls), paste("Inputs or settings changed; use a fresh output directory:", path))
    return(value$result)
  }
  value <- compute()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  saveRDS(list(controls = controls, result = value), temporary)
  check(file.rename(temporary, path), paste("Cannot save", path))
  value
}
