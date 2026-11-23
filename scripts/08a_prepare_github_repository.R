# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 8A — GitHub / Portfolio Repository Preparation
# File: scripts/08a_prepare_github_repository.R
#
# PURPOSE:
#   1. Re-run Stage 7B so manifest.json reflects the corrected app.R.
#   2. Write a professional portfolio README.md.
#   3. Audit public-repository safety.
#   4. Build a safe Git staging allowlist.
#   5. Initialize Git if needed and stage ONLY approved public files.
#
# This script does NOT create or push a GitHub remote yet.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# ---- 1. Confirm Shot Chart patch is present ----------------------------------

if (!file.exists("app.R")) {
  stop("app.R was not found.")
}

app_text <- paste(
  readLines(
    "app.R",
    warn = FALSE
  ),
  collapse = "\n"
)

if (
  !grepl(
    "shiny::validate(",
    app_text,
    fixed = TRUE
  ) ||
  !grepl(
    "shiny::need(",
    app_text,
    fixed = TRUE
  )
) {
  stop(
    paste0(
      "The targeted Shot Chart validate/need patch is not present in app.R. ",
      "Run scripts/07c_fix_shot_chart_validate.R first."
    )
  )
}

# ---- 2. Refresh deployment manifest after 07C -------------------------------

if (!file.exists(
  "scripts/07b_deployment_readiness.R"
)) {
  stop(
    paste0(
      "scripts/07b_deployment_readiness.R is missing. ",
      "Restore it before Stage 8A."
    )
  )
}

cat(
  "\nRefreshing Stage 7B deployment manifest after the Shot Chart fix...\n"
)

source(
  "scripts/07b_deployment_readiness.R",
  local = FALSE
)

# ---- 3. Read frozen portfolio metrics ---------------------------------------

performance <- readRDS(
  "data/app/model_performance.rds"
)

selected <- performance %>%
  filter(
    selected_model
  )

if (nrow(selected) != 1L) {
  stop("Could not identify exactly one frozen selected model.")
}

feature_importance <- readRDS(
  "data/app/model_feature_importance.rds"
) %>%
  arrange(
    shap_rank
  )

shot_chart_data <- readRDS(
  "data/app/shot_chart_data.rds"
)

# ---- 4. Build professional README -------------------------------------------

top1 <- feature_importance %>%
  slice(1)

top2 <- feature_importance %>%
  slice(2)

readme <- c(
  "# NBA Shot-Make Probability Explorer",
  "",
  "A public NBA analytics portfolio project that estimates pre-shot make probability using reproducible R modeling, chronological validation, an untouched final holdout, and an interactive Shiny application.",
  "",
  "## Project Overview",
  "",
  "The goal of this project is not simply to classify shots as makes or misses. It is to estimate **well-calibrated make probabilities** from information available before the shot result is known.",
  "",
  "The workflow uses public ESPN-derived NBA shot data distributed through `hoopR`, evaluates several model families chronologically across seasons, freezes the selected model before the final holdout reveal, and exposes the results in an employer-facing Shiny application.",
  "",
  "## Key Results",
  "",
  paste0(
    "- **Selected model:** ",
    selected$model_label[[1]]
  ),
  paste0(
    "- **Chronological validation log-loss:** ",
    sprintf("%.3f", selected$validation_log_loss[[1]])
  ),
  paste0(
    "- **2025–26 final holdout log-loss:** ",
    sprintf("%.3f", selected$test_log_loss[[1]])
  ),
  paste0(
    "- **2025–26 final holdout Brier score:** ",
    sprintf("%.3f", selected$test_brier[[1]])
  ),
  paste0(
    "- **2025–26 mean predicted make probability:** ",
    sprintf("%.1f%%", 100 * selected$test_mean_prediction[[1]])
  ),
  paste0(
    "- **2025–26 observed make rate:** ",
    sprintf("%.1f%%", 100 * selected$test_observed_rate[[1]])
  ),
  paste0(
    "- **Final evaluation shots:** ",
    format(nrow(shot_chart_data), big.mark = ",", scientific = FALSE)
  ),
  "",
  "The model selected during development remained the best-performing model on the untouched 2025–26 holdout.",
  "",
  "## Model Comparison",
  "",
  "| Model | Validation Log-Loss | Final Holdout Log-Loss | Final Holdout Brier |",
  "|---|---:|---:|---:|",
  paste0(
    "| Restrained XGBoost | ",
    sprintf("%.3f", performance$validation_log_loss[performance$model == "xgboost_restrained"]),
    " | ",
    sprintf("%.3f", performance$test_log_loss[performance$model == "xgboost_restrained"]),
    " | ",
    sprintf("%.3f", performance$test_brier[performance$model == "xgboost_restrained"]),
    " |"
  ),
  paste0(
    "| Spatial GAM | ",
    sprintf("%.3f", performance$validation_log_loss[performance$model == "gam_spatial"]),
    " | ",
    sprintf("%.3f", performance$test_log_loss[performance$model == "gam_spatial"]),
    " | ",
    sprintf("%.3f", performance$test_brier[performance$model == "gam_spatial"]),
    " |"
  ),
  paste0(
    "| Logistic Regression | ",
    sprintf("%.3f", performance$validation_log_loss[performance$model == "logistic_baseline"]),
    " | ",
    sprintf("%.3f", performance$test_log_loss[performance$model == "logistic_baseline"]),
    " | ",
    sprintf("%.3f", performance$test_brier[performance$model == "logistic_baseline"]),
    " |"
  ),
  paste0(
    "| Constant Benchmark | ",
    sprintf("%.3f", performance$validation_log_loss[performance$model == "constant_prevalence"]),
    " | ",
    sprintf("%.3f", performance$test_log_loss[performance$model == "constant_prevalence"]),
    " | ",
    sprintf("%.3f", performance$test_brier[performance$model == "constant_prevalence"]),
    " |"
  ),
  "",
  "Lower log-loss and Brier scores are better.",
  "",
  "## Validation Design",
  "",
  "The project uses season-based chronological validation rather than relying on a single random split:",
  "",
  "- **Fold 1:** train on 2022–23, validate on 2023–24",
  "- **Fold 2:** train on 2022–23 + 2023–24, validate on 2024–25",
  "- **Final refit:** train on all development seasons from 2022–23 through 2024–25",
  "- **Final holdout:** evaluate once on 2025–26 after model selection was frozen",
  "",
  "Primary model-selection metric: **log-loss**.",
  "",
  "## Model Interpretation",
  "",
  "TreeSHAP analysis shows that the model is driven primarily by shot geometry and shot context.",
  "",
  paste0(
    "- **",
    top1$feature_label[[1]],
    ":** ",
    sprintf("%.1f%%", 100 * top1$shap_share[[1]]),
    " of global SHAP importance"
  ),
  paste0(
    "- **",
    top2$feature_label[[1]],
    ":** ",
    sprintf("%.1f%%", 100 * top2$shap_share[[1]]),
    " of global SHAP importance"
  ),
  "",
  "Feature importance describes how the frozen model uses inputs; it should not be interpreted as causal.",
  "",
  "## Shiny Application",
  "",
  "The completed application contains five functional tabs:",
  "",
  "1. **Shot Probability Explorer** — click a court location, choose pre-shot context, and score the scenario with the frozen XGBoost model.",
  "2. **Interactive Shot Chart** — explore the complete frozen 2025–26 evaluation population with team, player, shot-type, zone, and result filters.",
  "3. **Model Comparison** — compare validation and final-holdout log-loss and Brier scores.",
  "4. **Calibration & Validation** — inspect reliability, calibration slope/intercept, and subgroup calibration.",
  "5. **Model Insights** — review TreeSHAP and XGBoost gain importance along with the project's key findings.",
  "",
  "Player and team identity are used only for **exploration and filtering** in the Shot Chart. They are intentionally excluded from the predictive model.",
  "",
  "## Data",
  "",
  "Public NBA shot data are acquired through the SportsDataverse `hoopR` package. The raw shot release is ESPN-derived.",
  "",
  "Development seasons:",
  "",
  "- 2022–23",
  "- 2023–24",
  "- 2024–25",
  "",
  "Final evaluation season:",
  "",
  "- 2025–26",
  "",
  "Raw source files are intentionally excluded from the public repository. The acquisition and cleaning scripts make the data pipeline reproducible.",
  "",
  "## Modeling Features",
  "",
  "The frozen model uses pre-shot information such as:",
  "",
  "- shot distance and court location",
  "- shot angle and 2PT/3PT geometry",
  "- grouped shot type",
  "- driving, cutting, running, alley-oop, bank, putback, turnaround, and fadeaway indicators",
  "- period and game clock",
  "- late-clock context",
  "- home/away context",
  "",
  "The project does **not** claim to incorporate unavailable tracking information such as defender distance, release mechanics, fatigue, or player-specific latent skill.",
  "",
  "## Repository Structure",
  "",
  "```text",
  "NBA-Shot-Make-Probability-Explorer/",
  "├── app.R",
  "├── R/",
  "│   ├── app_helpers.R",
  "│   ├── court_helpers.R",
  "│   └── ...",
  "├── scripts/",
  "│   ├── 01_acquire_data.R",
  "│   ├── 02_clean_validate*.R",
  "│   ├── 03*_*.R",
  "│   ├── 04*_*.R",
  "│   ├── 05*_*.R",
  "│   ├── 06*_*.R",
  "│   └── 07*_*.R",
  "├── data/",
  "│   └── app/",
  "├── models/",
  "├── docs/",
  "├── www/",
  "├── renv.lock",
  "└── README.md",
  "```",
  "",
  "## Run Locally",
  "",
  "Clone the repository, restore the R environment, and launch the app:",
  "",
  "```r",
  "renv::restore()",
  "shiny::runApp()",
  "```",
  "",
  "The deployed Shiny runtime uses precomputed artifacts and does not reacquire data or retrain models when the app starts.",
  "",
  "## Reproducibility & Deployment",
  "",
  "- Package versions are managed with `renv`.",
  "- The deployment bundle uses an explicit file allowlist.",
  "- `manifest.json` is generated with strict dependency resolution.",
  "- Raw data, local credentials, and intermediate development artifacts are excluded from the deployment bundle.",
  "- The final holdout was revealed only after model selection was frozen.",
  "",
  "## Limitations",
  "",
  "- The model is population-level rather than player-specific.",
  "- Public shot data do not include full optical tracking context.",
  "- Extremely late-clock attempts remain the clearest calibration weakness.",
  "- Shot-location coordinates are source-derived and may have discretization/rounding effects.",
  "- Results should be interpreted as estimated probabilities, not causal effects.",
  "",
  "## Author",
  "",
  "**Camden Wade**  ",
  "B.S. Applied Statistics, Penn State University  ",
  "Sports analytics and business analytics portfolio project",
  "",
  "Data attribution: SportsDataverse / `hoopR` and ESPN-derived public NBA shot data."
)

writeLines(
  readme,
  "README.md"
)

cat(
  "\nREADME.md created successfully.\n"
)

# ---- 5. Public-repository allowlist -----------------------------------------

public_files <- c(
  "app.R",
  "README.md",
  "DATA_SOURCES.md",
  ".gitignore",
  "renv.lock",
  "manifest.json",
  list.files(
    "R",
    pattern = "\\.R$",
    full.names = TRUE
  ),
  list.files(
    "scripts",
    pattern = "\\.R$",
    full.names = TRUE
  ),
  "www/custom.css",
  list.files(
    "data/app",
    pattern = "\\.rds$",
    full.names = TRUE
  ),
  "data/processed/model_final_test.rds",
  "models/final_xgboost_model.rds",
  "models/final_model_selection_metadata.rds",
  list.files(
    "docs/interpretation",
    full.names = TRUE,
    recursive = TRUE
  ),
  list.files(
    "docs/validation",
    full.names = TRUE,
    recursive = TRUE
  )
)

public_files <- unique(
  public_files[
    file.exists(
      public_files
    )
  ]
)

# Never stage these.
forbidden_public_patterns <- c(
  "^data/raw/",
  "^\\.Renviron$",
  "^\\.env$",
  "^renv/library/",
  "^renv/staging/",
  "^data/processed/shots_development",
  "^data/processed/model_development",
  "^models/stage4"
)

is_forbidden <- vapply(
  public_files,
  function(path) {
    any(
      vapply(
        forbidden_public_patterns,
        function(pattern) {
          grepl(
            pattern,
            path
          )
        },
        logical(1)
      )
    )
  },
  logical(1)
)

public_files <- sort(
  public_files[
    !is_forbidden
  ]
)

writeLines(
  public_files,
  "docs/validation/stage8a_public_repo_allowlist.txt"
)

# ---- 6. Git setup and safe staging ------------------------------------------

git_available <- nzchar(
  Sys.which(
    "git"
  )
)

if (!git_available) {
  stop(
    "Git is not available on PATH. Install/configure Git before continuing."
  )
}

repo_probe <- suppressWarnings(
  system2(
    "git",
    c(
      "rev-parse",
      "--is-inside-work-tree"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
)

git_repo <- length(repo_probe) > 0 &&
  identical(
    trimws(
      repo_probe[[1]]
    ),
    "true"
  )

if (!git_repo) {
  system2(
    "git",
    "init"
  )
}

# Unstage anything that may have been previously staged, then stage ONLY the
# allowlisted public files.
suppressWarnings(
  system2(
    "git",
    c(
      "reset"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
)

for (path in public_files) {
  system2(
    "git",
    c(
      "add",
      "--",
      path
    )
  )
}

staged_files <- suppressWarnings(
  system2(
    "git",
    c(
      "diff",
      "--cached",
      "--name-only"
    ),
    stdout = TRUE,
    stderr = FALSE
  )
)

staged_forbidden <- staged_files[
  vapply(
    staged_files,
    function(path) {
      any(
        vapply(
          forbidden_public_patterns,
          function(pattern) {
            grepl(
              pattern,
              path
            )
          },
          logical(1)
        )
      )
    },
    logical(1)
  )
]

git_remote <- suppressWarnings(
  system2(
    "git",
    c(
      "remote",
      "-v"
    ),
    stdout = TRUE,
    stderr = FALSE
  )
)

# ---- 7. Verification ---------------------------------------------------------

checks <- tibble(
  check = c(
    "Shot Chart validate/need namespace fix is present",
    "Stage 7B manifest was refreshed",
    "README.md exists",
    "README contains final XGBoost holdout metrics",
    "Public repository allowlist exists",
    "Git is available",
    "Git repository is initialized",
    "At least one approved file is staged",
    "No forbidden public files are staged"
  ),
  passed = c(
    grepl(
      "shiny::validate(",
      app_text,
      fixed = TRUE
    ) &&
      grepl(
        "shiny::need(",
        app_text,
        fixed = TRUE
      ),
    file.exists(
      "manifest.json"
    ),
    file.exists(
      "README.md"
    ),
    any(
      grepl(
        sprintf(
          "%.3f",
          selected$test_log_loss[[1]]
        ),
        readme,
        fixed = TRUE
      )
    ),
    file.exists(
      "docs/validation/stage8a_public_repo_allowlist.txt"
    ),
    git_available,
    TRUE,
    length(
      staged_files
    ) >
      0L,
    length(
      staged_forbidden
    ) ==
      0L
  )
)

cat(
  "\n--- STAGE 8A GITHUB REPOSITORY PREPARATION VERIFICATION ---\n"
)

print(
  checks,
  n = Inf
)

cat(
  "\n--- STAGE 8A GIT STAGING SUMMARY ---\n"
)

print(
  tibble(
    staged_files =
      length(
        staged_files
      ),
    forbidden_staged_files =
      length(
        staged_forbidden
      ),
    git_remote_entries =
      length(
        git_remote
      )
  ),
  n = Inf
)

if (
  length(
    git_remote
  ) >
    0L
) {
  cat(
    "\nCurrent Git remote(s):\n"
  )
  cat(
    paste(
      git_remote,
      collapse = "\n"
    ),
    "\n"
  )
}

if (!all(
  checks$
    passed
)) {
  stop(
    "One or more Stage 8A repository-preparation checks failed."
  )
}

cat(
  "\nSTAGE 8A PREPARATION COMPLETE.\n",
  "The corrected app manifest is refreshed, README.md is ready, and only ",
  "approved public files are staged. Next: review the staged repository and ",
  "create/push the GitHub remote.\n",
  sep = ""
)
