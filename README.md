# NFL pass protection and pass rush

R code for *Opponent-Adjusted Evaluation of NFL Pass Protection and Pass Rush*. The early model estimates rusher and blocker strength from graded matchups. The final model uses loss, win, recorded pressure, and sack, with EPA coefficients weighting the outcomes.

## Run

From the repository root, install the packages and place the six licensed inputs in `data/input/` as described in [data/README.md](data/README.md).

```r
install.packages(c("Matrix", "glmnet", "data.table", "jsonlite", "ggplot2"))
```

Run the scripts in order, or run the entire analysis:

```sh
Rscript scripts/run_all.R
```

| Script | Calculation |
| --- | --- |
| `00_config.R` | Paths, seed, penalty grid, and bootstrap count |
| `01_data-engineering.R` | Read matchups, apply independent outcome masks, and construct model inputs |
| `02_epa-regression.R` | Learn outcome weights from complete plays in Weeks 1–15 |
| `03_player-models.R` | Fit early and final models with game-grouped cross-validation |
| `04_validation.R` | Evaluate Weeks 16–18 and calculate calibration summaries |
| `05_rankings.R` | Player ratings, raw and smoothed comparisons, and All-Pro alignment |
| `06_bootstrap.R` | Game-bootstrap validation and rating intervals, re-estimating EPA jointly |
| `07_sensitivity.R` | Baseline smoothing, ordinal outcome model, and EPA specifications |
| `08_weekly.R` | Cumulative weekly estimates and pointwise intervals |
| `09_plots.R` | CV, calibration, and weekly plots |
| `10_baseline_validation.R` | Retune historical-baseline smoothing within saved validation draws, reusing player fits |

For example, `Rscript scripts/04_validation.R` prints the validation table after the first three steps have run. Tables are printed in R and saved as CSV; models and intermediate objects are saved as RDS. Everything generated goes into ignored `results/`. There is no knitting or LaTeX reporting step.

Edit `scripts/00_config.R` to change paths or settings. From an R session, settings can also be supplied before sourcing a script:

```r
options(nfl.settings = list(input_dir = "data/input", output_dir = "results"))
source("scripts/01_data-engineering.R")
```

The full analysis uses 1,000 validation draws, 1,000 rating draws, and 1,000 weekly trajectories and takes substantial compute time. Bootstrap checkpoints resume in the same output directory; use a fresh directory after changing inputs, code, settings, or package versions. Weekly paths keep each full-season fit's penalty and EPA weights. They are retrospective paths, with pointwise intervals.

Predictive comparison with the tuned historical baseline uses the training-only game folds to select smoothing separately in each saved validation draw. Step 10 evaluates the complete fixed smoothing grid on those same test-game draws and saves paired percentile intervals, selected-strength frequencies, and the tuned point comparison. It repeats no player fitting. The earlier fixed-strength validation outputs remain available. Descriptive season ranking comparisons use smoothing strengths of 2 for early outcomes and 10 for final outcomes, selected by cross-validation on the original Weeks 1–15 sample; these are configured separately from the historical fixed validation comparator.

The published runtime used R 4.4.3, glmnet 5.0, Matrix 1.7.6, data.table 1.18.6.1, jsonlite 2.0.0, and ggplot2 4.0.3. `scripts/functions/` contains the shared fitting and scoring functions.

The models include all observed protectors. Main blocker rankings and All-Pro comparisons use offensive linemen, with rank intervals recomputed within each comparison cohort. The full protector rankings remain available separately.

## Check

```sh
Rscript tests/test-pipeline.R
Rscript tests/test-baseline-validation.R
```

This exercises the full pipeline on synthetic data, including two bootstrap draws, all weekly cutoffs, and checkpoint resumption. It needs no Hudl data.

Licensed data, results, manuscript materials, historical code, and cluster/publication utilities are ignored. Existing Git history contains older derived data; share a clean source snapshot rather than that history.
