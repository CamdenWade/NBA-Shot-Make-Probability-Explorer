# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 4B — Nonlinear GAM
# File: scripts/04b_gam_models.R
#
# Purpose:
#   Fit and evaluate a restrained nonlinear Generalized Additive Model (GAM)
#   on the same locked chronological folds used in Stage 4A.
#
# Model design:
#   - 2D smooth of folded half-court location
#   - geometry-derived three-point region
#   - grouped shot family
#   - nonlinear game-clock effect
#   - period group
#   - home/away
#   - final-5-seconds indicator
#
# Engine:
#   mgcv::bam() for efficient fitting on 200k-400k+ rows per fold.
#
# Primary metric:
#   Log-loss
#
# Secondary:
#   Brier score
#   Calibration intercept
#   Calibration slope
#   Decile calibration
#
# IMPORTANT:
#   - 2025-26 final-test outcome is never loaded or accessed.
#   - Same chronological folds as Stage 4A.
#   - No player/team identity, raw type_id/type_text, score_value, or
#     scoring_play enters the model.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "mgcv",
  "purrr",
  "readr",
  "tibble",
  "tidyr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(mgcv)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
})

# ---- 2. Inputs ---------------------------------------------------------------

model_development_path <-
  "data/processed/model_development.rds"

baseline_metrics_path <-
  "docs/validation/stage4a_baseline_fold_metrics.csv"

baseline_predictions_path <-
  "data/processed/stage4a_baseline_validation_predictions.rds"

if (!file.exists(model_development_path)) {
  stop(
    "Missing model_development.rds. ",
    "Run scripts/03c_validation_freeze.R first."
  )
}

if (!file.exists(baseline_metrics_path)) {
  stop(
    "Missing Stage 4A baseline metrics. ",
    "Run scripts/04a_baseline_models.R first."
  )
}

if (!file.exists(baseline_predictions_path)) {
  stop(
    "Missing Stage 4A validation predictions. ",
    "Run scripts/04a_baseline_models.R first."
  )
}

model_development <- readRDS(
  model_development_path
)

baseline_metrics <- readr::read_csv(
  baseline_metrics_path,
  show_col_types = FALSE
)

baseline_predictions <- readRDS(
  baseline_predictions_path
)

if (!dir.exists("models")) {
  dir.create(
    "models",
    recursive = TRUE
  )
}

if (!dir.exists("docs/validation")) {
  dir.create(
    "docs/validation",
    recursive = TRUE
  )
}

# ---- 3. Locked GAM specification --------------------------------------------
#
# Why this specification:
#   - The spatial smooth captures nonlinear location effects across the court.
#   - three_region preserves the sharp 2PT / corner-3 / above-break distinction.
#   - shot_type_group provides pre-shot action context.
#   - game clock is allowed to vary nonlinearly.
#   - remaining context terms stay interpretable and restrained.
#
# k values are deliberately moderate to avoid an unnecessarily flexible model.

gam_formula <- stats::as.formula(
  paste(
    "outcome ~",
    "s(court_x_folded, court_y_folded, k = 60, bs = 'tp') +",
    "three_region +",
    "shot_type_group +",
    "s(clock_seconds_remaining, k = 10, bs = 'cr') +",
    "period_group +",
    "is_home_shooter +",
    "is_last_5_seconds_period"
  )
)

gam_predictors <- c(
  "court_x_folded",
  "court_y_folded",
  "three_region",
  "shot_type_group",
  "clock_seconds_remaining",
  "period_group",
  "is_home_shooter",
  "is_last_5_seconds_period"
)

missing_gam_predictors <- setdiff(
  gam_predictors,
  names(
    model_development
  )
)

if (length(missing_gam_predictors) > 0) {
  stop(
    paste0(
      "GAM predictor(s) missing from model data: ",
      paste(
        missing_gam_predictors,
        collapse = ", "
      )
    )
  )
}

# Use a modest thread count for portability.
detected_cores <- parallel::detectCores(
  logical = TRUE
)

gam_threads <- if (
  is.na(
    detected_cores
  )
) {
  1L
} else {
  max(
    1L,
    min(
      4L,
      detected_cores - 1L
    )
  )
}

cat(
  "\nGAM threads: ",
  gam_threads,
  "\n",
  sep = ""
)

# ---- 4. Metric helpers -------------------------------------------------------

clip_probability <- function(
    p,
    eps = 1e-15) {

  pmin(
    pmax(
      p,
      eps
    ),
    1 - eps
  )
}

log_loss <- function(
    truth,
    probability) {

  p <- clip_probability(
    probability
  )

  -mean(
    truth *
      log(
        p
      ) +
      (
        1 -
          truth
      ) *
        log(
          1 -
            p
        )
  )
}

brier_score <- function(
    truth,
    probability) {

  mean(
    (
      probability -
        truth
    )^2
  )
}

calibration_metrics <- function(
    truth,
    probability) {

  p <- clip_probability(
    probability,
    eps = 1e-6
  )

  lp <- qlogis(
    p
  )

  intercept_fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        truth ~ 1,
        offset = lp,
        family = stats::binomial()
      )
    ),
    error = function(e) NULL
  )

  slope_fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        truth ~ lp,
        family = stats::binomial()
      )
    ),
    error = function(e) NULL
  )

  calibration_intercept <- if (
    is.null(
      intercept_fit
    )
  ) {
    NA_real_
  } else {
    unname(
      stats::coef(
        intercept_fit
      )[[1]]
    )
  }

  calibration_slope <- if (
    is.null(
      slope_fit
    ) ||
    length(
      stats::coef(
        slope_fit
      )
    ) < 2
  ) {
    NA_real_
  } else {
    unname(
      stats::coef(
        slope_fit
      )[[2]]
    )
  }

  tibble(
    calibration_intercept =
      calibration_intercept,
    calibration_slope =
      calibration_slope
  )
}

evaluate_probabilities <- function(
    truth,
    probability) {

  calibration <- calibration_metrics(
    truth,
    probability
  )

  tibble(
    log_loss =
      log_loss(
        truth,
        probability
      ),
    brier =
      brier_score(
        truth,
        probability
      ),
    mean_predicted_probability =
      mean(
        probability
      ),
    observed_make_rate =
      mean(
        truth
      ),
    calibration_intercept =
      calibration$
        calibration_intercept,
    calibration_slope =
      calibration$
        calibration_slope
  )
}

make_calibration_table <- function(
    truth,
    probability,
    bins = 10L) {

  tibble(
    truth =
      truth,
    probability =
      probability
  ) %>%
    mutate(
      calibration_bin =
        dplyr::ntile(
          probability,
          bins
        )
    ) %>%
    group_by(
      calibration_bin
    ) %>%
    summarise(
      shots =
        n(),
      mean_predicted_probability =
        mean(
          probability
        ),
      observed_make_rate =
        mean(
          truth
        ),
      absolute_gap =
        abs(
          mean_predicted_probability -
            observed_make_rate
        ),
      .groups =
        "drop"
    )
}

# ---- 5. Locked folds ---------------------------------------------------------

fold1_train <- model_development %>%
  filter(
    hoopr_season ==
      2023L
  )

fold1_validation <- model_development %>%
  filter(
    hoopr_season ==
      2024L
  )

fold2_train <- model_development %>%
  filter(
    hoopr_season %in%
      c(
        2023L,
        2024L
      )
  )

fold2_validation <- model_development %>%
  filter(
    hoopr_season ==
      2025L
  )

# ---- 6. Fit/evaluate one fold ------------------------------------------------

run_gam_fold <- function(
    train_data,
    validation_data,
    fold_name) {

  cat(
    "\nFitting GAM for ",
    fold_name,
    "...\n",
    sep = ""
  )

  gam_fit <- mgcv::bam(
    formula =
      gam_formula,
    data =
      train_data,
    family =
      stats::binomial(),
    method =
      "fREML",
    discrete =
      TRUE,
    nthreads =
      gam_threads
  )

  probability <- stats::predict(
    gam_fit,
    newdata =
      validation_data,
    type =
      "response"
  )

  metrics <- evaluate_probabilities(
    validation_data$
      outcome,
    probability
  ) %>%
    mutate(
      fold =
        fold_name,
      model =
        "gam_spatial",
      train_rows =
        nrow(
          train_data
        ),
      validation_rows =
        nrow(
          validation_data
        ),
      .before =
        1
    )

  calibration <- make_calibration_table(
    validation_data$
      outcome,
    probability
  ) %>%
    mutate(
      fold =
        fold_name,
      model =
        "gam_spatial",
      .before =
        1
    )

  predictions <- tibble(
    fold =
      fold_name,
    shot_id =
      validation_data$
        shot_id,
    hoopr_season =
      validation_data$
        hoopr_season,
    nba_season =
      validation_data$
        nba_season,
    outcome =
      validation_data$
        outcome,
    gam_probability =
      probability
  )

  gam_summary <- summary(
    gam_fit
  )

  model_summary <- tibble(
    fold =
      fold_name,
    train_rows =
      nrow(
        train_data
      ),
    validation_rows =
      nrow(
        validation_data
      ),
    deviance_explained =
      gam_summary$
        dev.expl,
    adjusted_r_squared =
      gam_summary$
        r.sq,
    scale_estimate =
      gam_summary$
        scale,
    aic =
      stats::AIC(
        gam_fit
      )
  )

  smooth_table <- as.data.frame(
    gam_summary$
      s.table
  ) %>%
    tibble::rownames_to_column(
      "smooth_term"
    ) %>%
    as_tibble() %>%
    mutate(
      fold =
        fold_name,
      .before =
        1
    )

  list(
    model =
      gam_fit,
    metrics =
      metrics,
    calibration =
      calibration,
    predictions =
      predictions,
    model_summary =
      model_summary,
    smooth_table =
      smooth_table
  )
}

# ---- 7. Run both folds -------------------------------------------------------

fold1_gam <- run_gam_fold(
  train_data =
    fold1_train,
  validation_data =
    fold1_validation,
  fold_name =
    "fold1"
)

fold2_gam <- run_gam_fold(
  train_data =
    fold2_train,
  validation_data =
    fold2_validation,
  fold_name =
    "fold2"
)

gam_metrics <- bind_rows(
  fold1_gam$
    metrics,
  fold2_gam$
    metrics
)

gam_calibration <- bind_rows(
  fold1_gam$
    calibration,
  fold2_gam$
    calibration
)

gam_predictions <- bind_rows(
  fold1_gam$
    predictions,
  fold2_gam$
    predictions
)

gam_model_summary <- bind_rows(
  fold1_gam$
    model_summary,
  fold2_gam$
    model_summary
)

gam_smooth_table <- bind_rows(
  fold1_gam$
    smooth_table,
  fold2_gam$
    smooth_table
)

# ---- 8. Aggregate GAM metrics ------------------------------------------------

gam_aggregate <- gam_metrics %>%
  summarise(
    model =
      "gam_spatial",
    weighted_log_loss =
      weighted.mean(
        log_loss,
        w =
          validation_rows
      ),
    weighted_brier =
      weighted.mean(
        brier,
        w =
          validation_rows
      ),
    weighted_mean_predicted_probability =
      weighted.mean(
        mean_predicted_probability,
        w =
          validation_rows
      ),
    weighted_observed_make_rate =
      weighted.mean(
        observed_make_rate,
        w =
          validation_rows
      ),
    validation_rows =
      sum(
        validation_rows
      )
  )

# ---- 9. Direct comparison to Stage 4A ---------------------------------------

baseline_comparison_rows <- baseline_metrics %>%
  filter(
    model %in%
      c(
        "constant_prevalence",
        "logistic_baseline"
      )
  ) %>%
  select(
    fold,
    model,
    validation_rows,
    log_loss,
    brier,
    mean_predicted_probability,
    observed_make_rate,
    calibration_intercept,
    calibration_slope
  )

stage4_model_comparison <- bind_rows(
  baseline_comparison_rows,
  gam_metrics %>%
    select(
      fold,
      model,
      validation_rows,
      log_loss,
      brier,
      mean_predicted_probability,
      observed_make_rate,
      calibration_intercept,
      calibration_slope
    )
) %>%
  arrange(
    fold,
    log_loss
  )

gam_vs_logistic <- stage4_model_comparison %>%
  filter(
    model %in%
      c(
        "logistic_baseline",
        "gam_spatial"
      )
  ) %>%
  select(
    fold,
    model,
    log_loss,
    brier
  ) %>%
  tidyr::pivot_wider(
    names_from =
      model,
    values_from =
      c(
        log_loss,
        brier
      )
  ) %>%
  mutate(
    log_loss_gain_vs_logistic =
      log_loss_logistic_baseline -
      log_loss_gam_spatial,
    brier_gain_vs_logistic =
      brier_logistic_baseline -
      brier_gam_spatial
  )

aggregate_comparison <- bind_rows(
  baseline_metrics %>%
    group_by(
      model
    ) %>%
    summarise(
      weighted_log_loss =
        weighted.mean(
          log_loss,
          w =
            validation_rows
        ),
      weighted_brier =
        weighted.mean(
          brier,
          w =
            validation_rows
        ),
      weighted_mean_predicted_probability =
        weighted.mean(
          mean_predicted_probability,
          w =
            validation_rows
        ),
      weighted_observed_make_rate =
        weighted.mean(
          observed_make_rate,
          w =
            validation_rows
        ),
      validation_rows =
        sum(
          validation_rows
        ),
      .groups =
        "drop"
    ),
  gam_aggregate
) %>%
  arrange(
    weighted_log_loss
  )

# ---- 10. Save artifacts ------------------------------------------------------

readr::write_csv(
  gam_metrics,
  "docs/validation/stage4b_gam_fold_metrics.csv"
)

readr::write_csv(
  gam_aggregate,
  "docs/validation/stage4b_gam_aggregate_metrics.csv"
)

readr::write_csv(
  gam_calibration,
  "docs/validation/stage4b_gam_calibration.csv"
)

readr::write_csv(
  gam_model_summary,
  "docs/validation/stage4b_gam_model_summary.csv"
)

readr::write_csv(
  gam_smooth_table,
  "docs/validation/stage4b_gam_smooth_terms.csv"
)

readr::write_csv(
  stage4_model_comparison,
  "docs/validation/stage4b_model_comparison.csv"
)

readr::write_csv(
  gam_vs_logistic,
  "docs/validation/stage4b_gam_vs_logistic.csv"
)

readr::write_csv(
  aggregate_comparison,
  "docs/validation/stage4b_aggregate_model_comparison.csv"
)

saveRDS(
  gam_predictions,
  "data/processed/stage4b_gam_validation_predictions.rds",
  compress =
    "xz"
)

saveRDS(
  list(
    formula =
      gam_formula,
    fold1_model =
      fold1_gam$
        model,
    fold2_model =
      fold2_gam$
        model
  ),
  "models/stage4b_gam_models.rds",
  compress =
    "xz"
)

# ---- 11. Console output ------------------------------------------------------

cat(
  "\n--- STAGE 4B GAM FOLD METRICS ---\n"
)

print(
  gam_metrics,
  n = Inf
)

cat(
  "\n--- GAM VS LOGISTIC BY FOLD ---\n"
)

print(
  gam_vs_logistic,
  n = Inf
)

cat(
  "\n--- STAGE 4B AGGREGATE MODEL COMPARISON ---\n"
)

print(
  aggregate_comparison,
  n = Inf
)

cat(
  "\n--- GAM MODEL SUMMARY ---\n"
)

print(
  gam_model_summary,
  n = Inf
)

cat(
  "\n--- GAM SMOOTH-TERM AUDIT ---\n"
)

print(
  gam_smooth_table,
  n = Inf
)

# ---- 12. Verification --------------------------------------------------------

all_probabilities_valid <- all(
  gam_predictions$
    gam_probability >
    0 &
  gam_predictions$
    gam_probability <
    1
)

expected_validation_rows <-
  nrow(
    fold1_validation
  ) +
  nrow(
    fold2_validation
  )

prediction_rows_correct <-
  nrow(
    gam_predictions
  ) ==
  expected_validation_rows

both_folds_present <- setequal(
  unique(
    gam_metrics$
      fold
  ),
  c(
    "fold1",
    "fold2"
  )
)

gam_models_saved <- file.exists(
  "models/stage4b_gam_models.rds"
)

gam_predictions_saved <- file.exists(
  "data/processed/stage4b_gam_validation_predictions.rds"
)

gam_metrics_finite <- all(
  is.finite(
    gam_metrics$
      log_loss
  )
) &&
  all(
    is.finite(
      gam_metrics$
        brier
    )
  )

stage4b_checks <- tibble(
  check = c(
    "Both locked chronological folds were evaluated",
    "All GAM validation probabilities are strictly between 0 and 1",
    "GAM validation prediction row count matches both folds",
    "GAM fold metrics are finite",
    "GAM smooth-term audit was created",
    "GAM vs logistic comparison was created",
    "Aggregate three-model comparison was created",
    "GAM validation predictions RDS exists",
    "GAM model RDS exists"
  ),
  passed = c(
    both_folds_present,
    all_probabilities_valid,
    prediction_rows_correct,
    gam_metrics_finite,
    nrow(
      gam_smooth_table
    ) >
      0,
    nrow(
      gam_vs_logistic
    ) ==
      2L,
    all(
      c(
        "constant_prevalence",
        "logistic_baseline",
        "gam_spatial"
      ) %in%
        aggregate_comparison$
          model
    ),
    gam_predictions_saved,
    gam_models_saved
  )
)

cat(
  "\n--- STAGE 4B GAM VERIFICATION ---\n"
)

print(
  stage4b_checks,
  n = Inf
)

if (!all(
  stage4b_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 4B checks failed. ",
      "Review the GAM metrics and diagnostics before continuing."
    )
  )
}

if (requireNamespace(
  "renv",
  quietly = TRUE
)) {
  renv::snapshot(
    prompt = FALSE
  )

  cat(
    "\nUpdated renv.lock after Stage 4B GAM modeling.\n"
  )
}

cat(
  "\nSTAGE 4B COMPLETE.\n",
  "Next: review GAM log-loss, Brier score, calibration, and smooth complexity ",
  "before deciding whether to proceed unchanged to restrained XGBoost in ",
  "Stage 4C.\n",
  sep = ""
)
