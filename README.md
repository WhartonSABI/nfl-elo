# NFL ELO

This repository contains the work of the NFL Pass Rushing ELO project.

## Directory Structure

```
.
├── code/
│   └── code.Rproj
├── data/
│   ├── processed/
│       ├── 10_sample_games.csv
│       ├── five_weeks_hudl_data.csv
│       ├── full_hudl_data.csv
│       └── hudl_iq_game_ids.csv
│   ├── raw/
│       ├── Hudl IQ 2021 player roster.csv
│       ├── Hudl IQ 2021 NFL Events.csv
│       ├── Hudl IQ 2021 NFL freeze frames.csv
│       └── Hudl IQ 2021 NFL Events + Freeze Frame.csv
│   └── results/
│       ├── clean_beat_data.csv
│       ├── elo_history.csv
│       ├── elo_history_optimized.csv
│       ├── player_elo_ratings.csv
│       ├── player_elo_ratings_optimized.csv
│       ├── elo_prelim_results.csv
│       ├── elo_tuning_log.csv
│       └── predict_beat_data.csv
└── papers/
```