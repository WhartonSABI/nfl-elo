source("scripts/00_config.R")

files <- file.path(settings$input_dir, c("modeling_table.csv", "calibration_plays.csv", "game_pool.csv",
                                      "folds.csv", "reference.rds", "epa_basis.rds"))
check(all(file.exists(files)), paste("Missing licensed inputs:", paste(files[!file.exists(files)], collapse = ", ")))
config <- model_config()
data <- read_matchups(files[1], config)
pool <- read.csv(files[3], colClasses = c(game_id = "character"))
epa_configurations <- epa_config(expected_train_games = sum(pool$week <= 15),
                                 expected_test_games = sum(pool$week >= 16))
epa_state <- read_epa_data(files[2], files[3], epa_configurations, readRDS(files[6]))
vocabulary <- player_vocabulary(data)
folds <- read.csv(files[4], colClasses = c(game_id = "character"))
reference <- readRDS(files[5])
check(!anyDuplicated(folds$game_id) && all(data$game_id %in% folds$game_id), "Missing or duplicated game folds")
check(setequal(folds$fold, seq_len(config$nfolds)), "Unexpected fold labels")
check(all(data$game_id %in% epa_state$pool$game_id), "Model game missing from the original game pool")
for (model in c("win", "severity")) for (role in c("Rusher", "Blocker")) {
  weights <- reference[[model]][[role]]$weight
  check(all(is.finite(weights) & weights > 0) && abs(sum(weights) - 1) < 1e-10, "Invalid reference weights")
}

state <- list(data = data, config = config, vocabulary = vocabulary, folds = folds,
              reference = reference, epa_state = epa_state, input_md5 = tools::md5sum(files))
save_result(state, "data")
sample_sizes <- rbindlist(lapply(c("win", "severity"), function(model) {
  sample <- model_sample(data, model)
  data.frame(model = model, rows = nrow(sample), plays = length(unique(paste(sample$game_id, sample$play_id))),
             games = length(unique(sample$game_id)), training_rows = sum(sample$week <= 15),
             test_rows = sum(sample$week >= 16))
}))
write_table(sample_sizes, "sample_sizes")
print(sample_sizes)
print(with(epa_state$data, table(split[eligible_primary])))
