# Analysis inputs

Place these files in `data/input/`. Hudl data and row-level derived data are licensed and are not distributed with the code. These inputs begin after player identities, initial blocking assignments, play linkage, event coverage, and sack credits have been reviewed.

| File | Contents |
| --- | --- |
| `modeling_table.csv` | One initial blocking matchup per rusher/play, including chronology, blocker arrays, help, early grade, final category, and sack share |
| `calibration_plays.csv` | One linked play per row, including outcome counts, complete-play eligibility, EPA, and pre-snap context |
| `game_pool.csv` | Original game IDs and weeks, including games with no eligible observations |
| `folds.csv` | Saved five-fold game assignments: `game_id`, `fold` |
| `reference.rds` | Saved full-season opponent/help distributions, with `win` and `severity` entries for `Rusher` and `Blocker` |
| `epa_basis.rds` | Training-set numeric centers/scales and quarterback/defense levels used by the EPA design |

The last three files retain the study's CV folds, comparison populations, and EPA coding. They contain analysis controls, without run manifests or revision identifiers. The player vocabulary is reconstructed from the matchup table.

## Matchups

`modeling_table.csv` requires:

- `game_id`, `play_id`, `game_index`, `week`, `kickoff_utc`, `rusher_id`, `rusher_name`;
- JSON arrays `blocker_ids`, `blocker_names`, and `blocker_rated_ol`;
- 0/1 indicators `double_team`, `double_team_unknown`, `complete_timing`, `early_model_eligible`, and `strict_recorded_pressure`;
- `win_target` (0, 1, or missing), `severity_outcome` (loss, win, pressure, sack, or missing), and fractional `sack_credit`.

An early grade refers to the initial matchup within 2.5 seconds of the snap, subject to the observed release, sack, or matchup endpoint. The early model uses the independent `early_model_eligible` mask and observed grades. Every final-model row needs both an early grade and a verified final category. Missing grades remain missing.

Final outcomes give priority to sack, then explicitly recorded pressure, then the early win/loss. A hit alone does not imply pressure. Shared sacks have unit weight in the categorical player model; their fractions are retained for credited summaries and the play-level EPA regression.

## EPA plays

The play table includes identity/partition fields (`game_id`, `play_id`, `nflfast_game_id`, `week`, `split`), eligibility/exclusion fields, and these counts:

- `N_W`, `N_P`, `N_S`: win-only actors, pressure actors, and summed sack credits;
- `B`, `U`: observed protection participants and unresolved final grades;
- `P_B`, `S_B`, `K_B`: protected pressure actors, sack credits, and sack actors;
- `P_A`, `S_A`: recorded pressures and sack credits outside observed protection;
- `E = K_B - S_B`: correction for shared protected sacks.

`N_H`, `H_B`, and `H_A` are retained zero columns checked by the reader. They do not enter the regression. Completeness fields are `early_protection_complete`, `early_protection_participants`, and `early_protection_ungraded_actors`. Eligible plays require an early grade for every observed protection participant, verified event coverage and credits, observed EPA, and complete required context; `U` is then zero. Known source-only pressures and sacks still contribute to the recorded counts.

Context columns are `epa`, `accepted_penalty`, `down`, `ydstogo`, `yardline_100`, `half_seconds_remaining`, `score_differential`, `qtr`, `game_half`, `quarterback_id`, and `defteam`. `eligible_primary` identifies usable plays; `exclusion_reason` explains excluded ones. The design includes outcome counts, `B`, `E`, numeric context, down/half/overtime indicators, and quarterback/defense effects.

The study input has 70,307 matchup rows: 70,304 eligible early rows and 69,114 final rows. The game pool has 266 games. Complete-play EPA uses 13,231 training plays and 2,836 testing plays. The synthetic test constructs smaller inputs with the same structure.
