# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 1 — Project Architecture Setup
# File: scripts/00_project_setup.R
#
# Purpose:
#   Create the local project directory structure and starter documentation.
#   This script does NOT download NBA data. Data acquisition begins in Stage 2.
# =============================================================================

project_name <- "NBA-Shot-Make-Probability-Explorer"

cat("\n--- NBA SHOT-MAKE PROBABILITY EXPLORER: STAGE 1 SETUP ---\n")
cat("Working directory:", getwd(), "\n")

write_if_missing <- function(path, lines) {
  if (!file.exists(path)) {
    writeLines(lines, con = path)
    cat("Created:", path, "\n")
  } else {
    cat("Exists :", path, "— left unchanged\n")
  }
}

project_dirs <- c(
  "R",
  "scripts",
  "data",
  "data/raw",
  "data/processed",
  "data/app",
  "models",
  "www",
  "www/assets",
  "docs",
  "docs/screenshots",
  "docs/validation"
)

for (dir_path in project_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    cat("Created:", dir_path, "\n")
  } else {
    cat("Exists :", dir_path, "\n")
  }
}

starter_files <- c(
  "app.R",
  "R/data_helpers.R",
  "R/feature_engineering.R",
  "R/model_helpers.R",
  "R/court_helpers.R",
  "R/plot_helpers.R",
  "R/app_helpers.R"
)

for (file_path in starter_files) {
  if (!file.exists(file_path)) {
    file.create(file_path)
    cat("Created:", file_path, "\n")
  } else {
    cat("Exists :", file_path, "\n")
  }
}

gitignore_lines <- c(
  "# R / RStudio",
  ".Rhistory",
  ".RData",
  ".Ruserdata",
  ".Rproj.user/",
  "",
  "# renv local library/cache",
  "renv/library/",
  "renv/staging/",
  "",
  "# Raw downloaded NBA data",
  "data/raw/",
  "",
  "# Large/local temporary artifacts",
  "*.tmp",
  "*.log",
  ".DS_Store",
  "",
  "# Optional local environment files",
  ".Renviron",
  ".env"
)

write_if_missing(".gitignore", gitignore_lines)

data_sources_text <- c(
  "# Data Sources",
  "",
  "## Primary source",
  "",
  "**SportsDataverse / hoopR NBA shot releases**",
  "",
  "- R package: `hoopR`",
  "- Primary loader planned for Stage 2: `hoopR::load_nba_shots()`",
  "- Source family: ESPN-derived NBA play-by-play shot releases distributed through SportsDataverse",
  "- Project data repository license: CC BY 4.0",
  "- hoopR package license: MIT",
  "",
  "## Planned season coverage",
  "",
  "| hoopR season | NBA season | Planned role |",
  "|---:|---|---|",
  "| 2023 | 2022-23 | Development / training |",
  "| 2024 | 2023-24 | Development / validation |",
  "| 2025 | 2024-25 | Development / validation |",
  "| 2026 | 2025-26 | Final untouched test season |",
  "",
  "## Planned scope",
  "",
  "- NBA regular-season field-goal attempts",
  "- Made/missed outcomes",
  "- Shot coordinates and derived court geometry",
  "- Shot/event type where available and validated",
  "- Game, team, player, period, and clock identifiers/context",
  "- No defender-tracking, contest-distance, or player-movement variables unless a separate public and redistributable source is verified",
  "",
  "## Public repository policy",
  "",
  "- Raw season downloads will be reproducible but excluded from Git.",
  "- Public code will document all acquisition and transformation steps.",
  "- Compact derived application artifacts may be committed when appropriate.",
  "- No restricted material from prior interview/technical exercises will be included.",
  "",
  "## Attribution",
  "",
  "This project will provide attribution to SportsDataverse/hoopR and the underlying source documentation in the final README and methodology documentation."
)

write_if_missing("DATA_SOURCES.md", data_sources_text)

methodology_text <- c(
  "# Methodology",
  "",
  "## Modeling objective",
  "",
  "Estimate calibrated NBA shot-make probabilities using public shot-level data.",
  "",
  "## Primary evaluation metric",
  "",
  "- Log-loss",
  "",
  "## Secondary evaluation",
  "",
  "- Brier score",
  "- Calibration / reliability analysis",
  "- Performance by shot distance and shot category",
  "- Cross-season robustness",
  "- Feature-group sensitivity / interpretation",
  "",
  "## Planned validation design",
  "",
  "### Development Fold 1",
  "- Train: 2022-23",
  "- Validate: 2023-24",
  "",
  "### Development Fold 2",
  "- Train: 2022-23 + 2023-24",
  "- Validate: 2024-25",
  "",
  "### Final untouched test",
  "- Train final model after development: 2022-23 + 2023-24 + 2024-25",
  "- Final test: 2025-26",
  "",
  "The 2025-26 season must remain untouched for model selection and tuning.",
  "",
  "## Planned model ladder",
  "",
  "1. Constant make-rate benchmark",
  "2. Logistic regression",
  "3. Generalized additive model (GAM)",
  "4. Restrained XGBoost",
  "",
  "The final model will be selected from observed validation performance and calibration rather than assumed in advance.",
  "",
  "## Leakage rules",
  "",
  "- Do not use information created after the shot outcome.",
  "- Do not use made/missed text as a predictor.",
  "- Any score-state variable must represent the state before the shot.",
  "- Player-history features, if later considered, must use only prior information.",
  "- The final 2025-26 test season is not used for feature selection, tuning, or calibration decisions."
)

write_if_missing("docs/methodology.md", methodology_text)

readme_text <- c(
  "# NBA Shot-Make Probability Explorer",
  "",
  "An interactive R Shiny basketball analytics application for estimating and exploring NBA shot-make probabilities using publicly available shot-level data.",
  "",
  "## Project goals",
  "",
  "- Estimate reliable shot-make probabilities rather than only made/missed classifications.",
  "- Model nonlinear relationships between shot location, distance, and game context.",
  "- Compare logistic regression, GAM, and XGBoost models using cross-season validation.",
  "- Evaluate probability calibration using log-loss, Brier score, and reliability plots.",
  "- Build an interactive Shiny application for shot exploration, model comparison, validation, and interpretation.",
  "",
  "## Planned data source",
  "",
  "SportsDataverse / hoopR NBA shot releases derived from ESPN play-by-play data.",
  "",
  "See `DATA_SOURCES.md` for source, licensing, attribution, and season details.",
  "",
  "## Planned seasons",
  "",
  "- 2022-23",
  "- 2023-24",
  "- 2024-25",
  "- 2025-26 (final untouched test season)",
  "",
  "## Status",
  "",
  "Stage 1: Project architecture and data strategy."
)

write_if_missing("README.md", readme_text)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!dir.exists("renv")) {
  renv::init(bare = TRUE, restart = FALSE)
  cat("Initialized renv.\n")
} else {
  cat("renv already initialized.\n")
}

renv::snapshot(prompt = FALSE)
cat("Updated: renv.lock\n")

required_dirs <- project_dirs
required_files <- c(
  "app.R",
  ".gitignore",
  "README.md",
  "DATA_SOURCES.md",
  "docs/methodology.md",
  "renv.lock",
  starter_files[-1]
)

directory_check <- all(vapply(required_dirs, dir.exists, logical(1)))
file_check <- all(vapply(required_files, file.exists, logical(1)))
raw_data_gitignored <- any(
  grepl("^data/raw/?$", readLines(".gitignore"), ignore.case = FALSE)
)

stage1_checks <- data.frame(
  check = c(
    "Required directories exist",
    "Required starter files exist",
    "data/raw is excluded by .gitignore",
    "renv.lock exists"
  ),
  passed = c(
    directory_check,
    file_check,
    raw_data_gitignored,
    file.exists("renv.lock")
  )
)

cat("\n--- STAGE 1 VERIFICATION ---\n")
print(stage1_checks, row.names = FALSE)

if (!all(stage1_checks$passed)) {
  stop("One or more Stage 1 setup checks failed. Review the table above.")
}

cat(
  "\nSTAGE 1 SETUP COMPLETE.\n",
  "No NBA data have been downloaded yet.\n",
  "Next step: scripts/01_acquire_data.R in Stage 2.\n",
  sep = ""
)
