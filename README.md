# NFL Pass Rush Ridge Bradley-Terry Pipeline

This repository runs a single pipeline with two separate ridge Bradley-Terry models:

- `win` model: binary `0/1` target from the 2.5-second win logic.
- `severity` model: multinomial outcome model (`loss/win/hit/sack`) converted to weighted expected severity (`0.0 / 0.2 / 0.4 / 1.0`).

Each model has its own:

- fit stage
- holdout validation vs its own baseline
- uncertainty stage (validation metrics + player-rating uncertainty)

Optional path uncertainty is also supported via cumulative week-by-week refits.

Pipeline output also includes a unified full leaderboard table:

- `data/output/shared/leaderboard_full_bt_ridge.csv`

Pipeline output also includes All-Pro alignment validation tables for BT ratings:

- `data/output/shared/validation_all_pro_player_scores_bt_ridge.csv`
- `data/output/shared/validation_all_pro_metrics_bt_ridge.csv`
- `data/output/shared/validation_all_pro_positive_matches_bt_ridge.csv`

Validation uncertainty uses game-level block bootstrap.  
Severity uncertainty includes both expected-severity metrics (`mse/rmse/mae`) and multiclass logloss.

## Current Layout

```text
.
├── archived/                  # legacy reference material only (not used by pipeline runtime)
├── data/
│   ├── hudl/                  # required raw Hudl files for input rebuilds
│   ├── input/
│   │   ├── matchups.csv
│   │   ├── sacks.csv
│   │   ├── hits.csv
│   │   └── hudl_iq_game_ids.csv
│   └── output/
│       ├── shared/
│       │   └── leaderboard_full_bt_ridge.csv
│       ├── win/
│       └── severity/
├── scripts/
│   ├── 00_config.R
│   ├── 00_utils.R
│   ├── 01_build-inputs.R
│   ├── 02_build-modeling-table.R
│   ├── 03_fit-bt-win-model.R
│   ├── 04_validate-bt-win-model.R
│   ├── 05_uncertainty-bt-win-model.R
│   ├── 06_fit-bt-severity-model.R
│   ├── 07_validate-bt-severity-model.R
│   ├── 08_uncertainty-bt-severity-model.R
│   ├── 09_build-full-bt-leaderboard.R
│   ├── 10_validate-bt-all-pro.R
│   └── run-all.R
├── README.md
└── .gitignore
```

## Run The Full Pipeline

From repository root:

```bash
Rscript scripts/run-all.R
```

Required raw files:

- `data/hudl/Hudl IQ 2021 NFL freeze frames.csv` (or `data/hudl/Hudl IQ 2021 NFL Events + Freeze Frame.csv`)
- `data/hudl/Hudl IQ 2021 player roster.csv`

`02_build-modeling-table.R` filters to regular-season games (`game_type == REG`, weeks 1-18) using `data/input/hudl_iq_game_ids.csv`.

## Parallel Workers

Parallelizable stages use:

- `workers = max(1, n_cores - 4)`

With 16 cores, that resolves to 12 workers. Override explicitly if needed:

```bash
PIPELINE_WORKERS=12 Rscript scripts/run-all.R
```

## Runtime Controls

Tune end-to-end bootstrap intensity (validation + player uncertainty):

```bash
END_TO_END_BOOTSTRAP_ITER=400 Rscript scripts/run-all.R
```

Quick smoke test:

```bash
END_TO_END_BOOTSTRAP_ITER=25 PIPELINE_WORKERS=12 Rscript scripts/run-all.R
```

Enable cumulative weekly path uncertainty:

```bash
PATH_BOOTSTRAP_ITER=100 PIPELINE_WORKERS=12 Rscript scripts/run-all.R
```

Path uncertainty outputs:

- `data/output/win/path_uncertainty_weekly_win_bt_ridge.csv`
- `data/output/severity/path_uncertainty_weekly_severity_bt_ridge.csv`

`run-all.R` now caches preprocessing by default:

- it skips `01_build-inputs.R` if `data/input/matchups.csv`, `sacks.csv`, `hits.csv`, and `hudl_iq_game_ids.csv` already exist
- it skips `02_build-modeling-table.R` if `data/output/shared/modeling_table.csv` already exists

Legacy explicit skip flag (still supported):

```bash
SKIP_BUILD_INPUTS=1 Rscript scripts/run-all.R
```

Force rebuild controls:

```bash
FORCE_REBUILD_INPUTS=1 Rscript scripts/run-all.R
FORCE_REBUILD_MODELING=1 Rscript scripts/run-all.R
```

Lambda grid for BT CV is now explicit and sequence-based (in [`scripts/00_config.R`](/Users/Jonathan/wsabi/lab/projects/nfl-elo/scripts/00_config.R)):

- `lambda_grid$scale` = `"log"` or `"linear"`
- `lambda_grid$max`, `lambda_grid$min`
- `lambda_grid$length`

Set the hard win threshold (seconds from snap):

```bash
WIN_SECONDS_THRESHOLD=2.5 Rscript scripts/run-all.R
```
