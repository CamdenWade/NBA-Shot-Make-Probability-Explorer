# NBA Shot-Make Probability Explorer

A public NBA analytics portfolio project that estimates pre-shot make probability using reproducible R modeling, chronological validation, an untouched final holdout, and an interactive Shiny application.

## Project Overview

The goal of this project is not simply to classify shots as makes or misses. It is to estimate **well-calibrated make probabilities** from information available before the shot result is known.

The workflow uses public ESPN-derived NBA shot data distributed through `hoopR`, evaluates several model families chronologically across seasons, freezes the selected model before the final holdout reveal, and exposes the results in an employer-facing Shiny application.

## Key Results

- **Selected model:** Restrained XGBoost
- **Chronological validation log-loss:** 0.639
- **2025–26 final holdout log-loss:** 0.637
- **2025–26 final holdout Brier score:** 0.224
- **2025–26 mean predicted make probability:** 47.0%
- **2025–26 observed make rate:** 46.9%
- **Final evaluation shots:** 220,085

The model selected during development remained the best-performing model on the untouched 2025–26 holdout.

## Model Comparison

| Model | Validation Log-Loss | Final Holdout Log-Loss | Final Holdout Brier |
|---|---:|---:|---:|
| Restrained XGBoost | 0.639 | 0.637 | 0.224 |
| Spatial GAM | 0.644 | 0.642 | 0.226 |
| Logistic Regression | 0.648 | 0.645 | 0.227 |
| Constant Benchmark | 0.691 | 0.691 | 0.249 |

Lower log-loss and Brier scores are better.

## Validation Design

The project uses season-based chronological validation rather than relying on a single random split:

- **Fold 1:** train on 2022–23, validate on 2023–24
- **Fold 2:** train on 2022–23 + 2023–24, validate on 2024–25
- **Final refit:** train on all development seasons from 2022–23 through 2024–25
- **Final holdout:** evaluate once on 2025–26 after model selection was frozen

Primary model-selection metric: **log-loss**.

## Model Interpretation

TreeSHAP analysis shows that the model is driven primarily by shot geometry and shot context.

- **Distance to Rim:** 39.1% of global SHAP importance
- **Shot Type:** 21.7% of global SHAP importance

Feature importance describes how the frozen model uses inputs; it should not be interpreted as causal.

## Shiny Application

The completed application contains five functional tabs:

1. **Shot Probability Explorer** — click a court location, choose pre-shot context, and score the scenario with the frozen XGBoost model.
2. **Interactive Shot Chart** — explore the complete frozen 2025–26 evaluation population with team, player, shot-type, zone, and result filters.
3. **Model Comparison** — compare validation and final-holdout log-loss and Brier scores.
4. **Calibration & Validation** — inspect reliability, calibration slope/intercept, and subgroup calibration.
5. **Model Insights** — review TreeSHAP and XGBoost gain importance along with the project's key findings.

Player and team identity are used only for **exploration and filtering** in the Shot Chart. They are intentionally excluded from the predictive model.

## Data

Public NBA shot data are acquired through the SportsDataverse `hoopR` package. The raw shot release is ESPN-derived.

Development seasons:

- 2022–23
- 2023–24
- 2024–25

Final evaluation season:

- 2025–26

Raw source files are intentionally excluded from the public repository. The acquisition and cleaning scripts make the data pipeline reproducible.

## Modeling Features

The frozen model uses pre-shot information such as:

- shot distance and court location
- shot angle and 2PT/3PT geometry
- grouped shot type
- driving, cutting, running, alley-oop, bank, putback, turnaround, and fadeaway indicators
- period and game clock
- late-clock context
- home/away context

The project does **not** claim to incorporate unavailable tracking information such as defender distance, release mechanics, fatigue, or player-specific latent skill.

## Repository Structure

```text
NBA-Shot-Make-Probability-Explorer/
├── app.R
├── R/
│   ├── app_helpers.R
│   ├── court_helpers.R
│   └── ...
├── scripts/
│   ├── 01_acquire_data.R
│   ├── 02_clean_validate*.R
│   ├── 03*_*.R
│   ├── 04*_*.R
│   ├── 05*_*.R
│   ├── 06*_*.R
│   └── 07*_*.R
├── data/
│   └── app/
├── models/
├── docs/
├── www/
├── renv.lock
└── README.md
```

## Run Locally

Clone the repository, restore the R environment, and launch the app:

```r
renv::restore()
shiny::runApp()
```

The deployed Shiny runtime uses precomputed artifacts and does not reacquire data or retrain models when the app starts.

## Reproducibility & Deployment

- Package versions are managed with `renv`.
- The deployment bundle uses an explicit file allowlist.
- `manifest.json` is generated with strict dependency resolution.
- Raw data, local credentials, and intermediate development artifacts are excluded from the deployment bundle.
- The final holdout was revealed only after model selection was frozen.

## Limitations

- The model is population-level rather than player-specific.
- Public shot data do not include full optical tracking context.
- Extremely late-clock attempts remain the clearest calibration weakness.
- Shot-location coordinates are source-derived and may have discretization/rounding effects.
- Results should be interpreted as estimated probabilities, not causal effects.

## Author

**Camden Wade**  
B.S. Applied Statistics, Penn State University  
Sports analytics and business analytics portfolio project

Data attribution: SportsDataverse / `hoopR` and ESPN-derived public NBA shot data.
