# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 4A — Baseline Probability Models
# File: scripts/04a_baseline_models.R
#
# Purpose:
#   Establish the locked benchmark models before GAM / XGBoost:
#     1. Constant training-prevalence benchmark
#     2. Interpretable logistic regression
#
# Validation:
#   Fold 1:
#     Train      = 2022-23
#     Validation = 2023-24
#
#   Fold 2:
#     Train      = 2022-23 + 2023-24
#     Validation = 2024-25
#
# Primary metric:
#   Log-loss
#
# Secondary:
#   Brier score
#   Calibration intercept
#   Calibration slope
#   Decile calibration table
#
# IMPORTANT:
#   - 2025-26 final-test outcome is never loaded or accessed here.
#   - No player/team identity enters the model.
#   - No raw type_id/type_text enters the model.
#   - No score_value/scoring_play leakage enters the model.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
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
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
})

# ---- 2. Input checks ---------------------------------------------------------

model_development_path <-
  "data/processed/model_development.rds"

split_assignments_path <-
  "data/processed/model_split_assignments.rds"

freeze_metadata_path <-
  "data/processed/modeling_freeze_metadata.rds"

if (!file.exists(model_development_path)) {
  stop(
    "Missing model_development.rds. ",
    "Run scripts/03c_validation_freeze.R first."
  )
}

if (!file.exists(split_assignments_path)) {
  stop(
    "Missing model_split_assignments.rds. ",
    "Run scripts/03c_validation_freeze.R first."
  )
}

if (!file.exists(freeze_metadata_path)) {
  stop(
    "Missing modeling_freeze_metadata.rds. ",
    "Run scripts/03c_validation_freeze.R first."
  )
}

model_development <- readRDS(
  model_development_path
)

split_assignments <- readRDS(
  split_assignments_path
)

freeze_metadata <- readRDS(
  freeze_metadata_path
)

if (!("outcome" %in% names(model_development))) {
  stop(
    "model_development.rds does not contain outcome."
  )
}

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

# ---- 3. Locked baseline logistic specification -------------------------------

baseline_formula <- stats::as.formula(
  paste(
    "outcome ~",
    "distance_to_rim_check_ft +",
    "I(distance_to_rim_check_ft^2) +",
    "absolute_angle_deg +",
    "three_region +",
    "shot_type_group +",
    "period_group +",
    "clock_seconds_remaining +",
    "is_home_shooter +",
    "is_last_5_seconds_period"
  )
)

baseline_predictors <- c(
  "distance_to_rim_check_ft",
  "absolute_angle_deg",
  "three_region",
  "shot_type_group",
  "period_group",
  "clock_seconds_remaining",
  "is_home_shooter",
  "is_last_5_seconds_period"
)

missing_baseline_predictors <- setdiff(
  baseline_predictors,
  names(
    model_development
  )
)

if (length(missing_baseline_predictors) > 0) {
  stop(
    paste0(
      "Baseline predictor(s) missing from model data: ",
      paste(
        missing_baseline_predictors,
        collapse = ", "
      )
    )
  )
}

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
    eps =
      1e-6
  )

  lp <- qlogis(
    p
  )

  intercept_fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        truth ~ 1,
        offset =
          lp,
        family =
          stats::binomial()
      )
    ),
    error =
      function(e) NULL
  )

  slope_fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        truth ~ lp,
        family =
          stats::binomial()
      )
    ),
    error =
      function(e) NULL
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
    ) <
      2
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

# ---- 5. Factor-level audit helper -------------------------------------------

factor_variables <- c(
  "three_region",
  "shot_type_group",
  "period_group"
)

audit_fold_levels <- function(
    train_data,
    validation_data,
    fold_name) {

  map_dfr(
    factor_variables,
    function(feature_name) {

      train_values <- unique(
        as.character(
          train_data[[
            feature_name
          ]]
        )
      )

      validation_values <- unique(
        as.character(
          validation_data[[
            feature_name
          ]]
        )
      )

      unseen <- setdiff(
        validation_values[
          !is.na(
            validation_values
          )
        ],
        train_values[
          !is.na(
            train_values
          )
        ]
      )

      if (length(unseen) == 0) {
        tibble(
          fold =
            fold_name,
          feature =
            feature_name,
          unseen_validation_level =
            NA_character_
        )
      } else {
        tibble(
          fold =
            fold_name,
          feature =
            feature_name,
          unseen_validation_level =
            unseen
        )
      }
    }
  )
}

# ---- 6. Build locked folds ---------------------------------------------------

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

fold_level_audit <- bind_rows(
  audit_fold_levels(
    fold1_train,
    fold1_validation,
    "fold1"
  ),
  audit_fold_levels(
    fold2_train,
    fold2_validation,
    "fold2"
  )
)

readr::write_csv(
  fold_level_audit,
  "docs/validation/stage4a_fold_factor_level_audit.csv"
)

cat(
  "\n--- FOLD FACTOR-LEVEL AUDIT ---\n"
)

print(
  fold_level_audit,
  n = Inf
)

problem_levels <- fold_level_audit %>%
  filter(
    !is.na(
      unseen_validation_level
    )
  )

if (nrow(problem_levels) > 0) {
  stop(
    paste0(
      "At least one validation fold contains a categorical level not observed ",
      "in its training fold. Review stage4a_fold_factor_level_audit.csv."
    )
  )
}

# ---- 7. Fit/evaluate one fold ------------------------------------------------

run_baseline_fold <- function(
    train_data,
    validation_data,
    fold_name) {

  train_prevalence <- mean(
    train_data$
      outcome
  )

  constant_probability <- rep(
    train_prevalence,
    nrow(
      validation_data
    )
  )

  constant_metrics <- evaluate_probabilities(
    validation_data$
      outcome,
    constant_probability
  ) %>%
    mutate(
      fold =
        fold_name,
      model =
        "constant_prevalence",
      train_rows =
        nrow(
          train_data
        ),
      validation_rows =
        nrow(
          validation_data
        ),
      train_make_rate =
        train_prevalence,
      .before =
        1
    )

  logistic_fit <- stats::glm(
    formula =
      baseline_formula,
    data =
      train_data,
    family =
      stats::binomial()
  )

  logistic_probability <- stats::predict(
    logistic_fit,
    newdata =
      validation_data,
    type =
      "response"
  )

  logistic_metrics <- evaluate_probabilities(
    validation_data$
      outcome,
    logistic_probability
  ) %>%
    mutate(
      fold =
        fold_name,
      model =
        "logistic_baseline",
      train_rows =
        nrow(
          train_data
        ),
      validation_rows =
        nrow(
          validation_data
        ),
      train_make_rate =
        train_prevalence,
      .before =
        1
    )

  constant_calibration <- make_calibration_table(
    validation_data$
      outcome,
    constant_probability
  ) %>%
    mutate(
      fold =
        fold_name,
      model =
        "constant_prevalence",
      .before =
        1
    )

  logistic_calibration <- make_calibration_table(
    validation_data$
      outcome,
    logistic_probability
  ) %>%
    mutate(
      fold =
        fold_name,
      model =
        "logistic_baseline",
      .before =
        1
    )

  predictions <- tibble(
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
    constant_probability =
      constant_probability,
    logistic_probability =
      logistic_probability
  )

  list(
    metrics =
      bind_rows(
        constant_metrics,
        logistic_metrics
      ),
    calibration =
      bind_rows(
        constant_calibration,
        logistic_calibration
      ),
    predictions =
      predictions,
    logistic_model =
      logistic_fit
  )
}

# ---- 8. Run both folds -------------------------------------------------------

fold1_results <- run_baseline_fold(
  train_data =
    fold1_train,
  validation_data =
    fold1_validation,
  fold_name =
    "fold1"
)

fold2_results <- run_baseline_fold(
  train_data =
    fold2_train,
  validation_data =
    fold2_validation,
  fold_name =
    "fold2"
)

baseline_metrics <- bind_rows(
  fold1_results$
    metrics,
  fold2_results$
    metrics
)

baseline_calibration <- bind_rows(
  fold1_results$
    calibration,
  fold2_results$
    calibration
)

baseline_predictions <- bind_rows(
  fold1_results$
    predictions %>%
    mutate(
      fold =
        "fold1",
      .before =
        1
    ),
  fold2_results$
    predictions %>%
    mutate(
      fold =
        "fold2",
      .before =
        1
    )
)

# ---- 9. Aggregate metrics across locked validation folds ---------------------

aggregate_metrics <- baseline_metrics %>%
  group_by(
    model
  ) %>%
  summarise(
    # IMPORTANT:
    # Calculate weighted metrics BEFORE replacing validation_rows with its
    # group total. dplyr::summarise() evaluates expressions sequentially, so
    # summing validation_rows first would overwrite the two fold-level weights
    # with a single scalar and cause weighted.mean() to fail.
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
  ) %>%
  arrange(
    weighted_log_loss
  )

constant_aggregate <- aggregate_metrics %>%
  filter(
    model ==
      "constant_prevalence"
  ) %>%
  select(
    constant_log_loss =
      weighted_log_loss,
    constant_brier =
      weighted_brier
  )

aggregate_metrics <- aggregate_metrics %>%
  mutate(
    log_loss_improvement_vs_constant =
      constant_aggregate$
        constant_log_loss -
      weighted_log_loss,
    brier_improvement_vs_constant =
      constant_aggregate$
        constant_brier -
      weighted_brier
  )

# ---- 10. Logistic coefficient audit -----------------------------------------

extract_coefficients <- function(
    fit,
    fold_name) {

  coefficient_matrix <- summary(
    fit
  )$
    coefficients

  tibble(
    fold =
      fold_name,
    term =
      rownames(
        coefficient_matrix
      ),
    estimate =
      coefficient_matrix[
        ,
        "Estimate"
      ],
    std_error =
      coefficient_matrix[
        ,
        "Std. Error"
      ],
    z_value =
      coefficient_matrix[
        ,
        "z value"
      ],
    p_value =
      coefficient_matrix[
        ,
        "Pr(>|z|)"
      ]
  )
}

logistic_coefficients <- bind_rows(
  extract_coefficients(
    fold1_results$
      logistic_model,
    "fold1"
  ),
  extract_coefficients(
    fold2_results$
      logistic_model,
    "fold2"
  )
)

# ---- 11. Save artifacts ------------------------------------------------------

readr::write_csv(
  baseline_metrics,
  "docs/validation/stage4a_baseline_fold_metrics.csv"
)

readr::write_csv(
  aggregate_metrics,
  "docs/validation/stage4a_baseline_aggregate_metrics.csv"
)

readr::write_csv(
  baseline_calibration,
  "docs/validation/stage4a_baseline_calibration.csv"
)

readr::write_csv(
  logistic_coefficients,
  "docs/validation/stage4a_logistic_coefficients.csv"
)

saveRDS(
  baseline_predictions,
  "data/processed/stage4a_baseline_validation_predictions.rds",
  compress =
    "xz"
)

saveRDS(
  list(
    formula =
      baseline_formula,
    fold1_model =
      fold1_results$
        logistic_model,
    fold2_model =
      fold2_results$
        logistic_model
  ),
  "models/stage4a_logistic_baselines.rds",
  compress =
    "xz"
)

# ---- 12. Console output ------------------------------------------------------

cat(
  "\n--- STAGE 4A BASELINE FOLD METRICS ---\n"
)

print(
  baseline_metrics,
  n = Inf
)

cat(
  "\n--- STAGE 4A AGGREGATE METRICS ---\n"
)

print(
  aggregate_metrics,
  n = Inf
)

cat(
  "\n--- LOGISTIC COEFFICIENT AUDIT ---\n"
)

print(
  logistic_coefficients,
  n = Inf
)

# ---- 13. Verification --------------------------------------------------------

all_probabilities_valid <- all(
  baseline_predictions$
    constant_probability >
    0 &
  baseline_predictions$
    constant_probability <
    1 &
  baseline_predictions$
    logistic_probability >
    0 &
  baseline_predictions$
    logistic_probability <
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
    baseline_predictions
  ) ==
  expected_validation_rows

both_models_both_folds <-
  nrow(
    baseline_metrics
  ) ==
  4L &&
  setequal(
    unique(
      baseline_metrics$
        model
    ),
    c(
      "constant_prevalence",
      "logistic_baseline"
    )
  ) &&
  setequal(
    unique(
      baseline_metrics$
        fold
    ),
    c(
      "fold1",
      "fold2"
    )
  )

logistic_beats_constant_both_folds <- baseline_metrics %>%
  select(
    fold,
    model,
    log_loss
  ) %>%
  tidyr::pivot_wider(
    names_from =
      model,
    values_from =
      log_loss
  ) %>%
  summarise(
    passed =
      all(
        logistic_baseline <
          constant_prevalence
      )
  ) %>%
  pull(
    passed
  )

stage4a_checks <- tibble(
  check = c(
    "Both locked chronological folds were evaluated",
    "Constant and logistic models were evaluated on both folds",
    "All validation probabilities are strictly between 0 and 1",
    "Validation prediction row count matches both folds",
    "No fold-level unseen categorical levels were detected",
    "Logistic baseline improves log-loss over constant benchmark in both folds",
    "Fold-level metric CSV exists",
    "Aggregate metric CSV exists",
    "Calibration CSV exists",
    "Logistic coefficient CSV exists",
    "Validation predictions RDS exists",
    "Logistic baseline model RDS exists"
  ),
  passed = c(
    setequal(
      unique(
        baseline_metrics$
          fold
      ),
      c(
        "fold1",
        "fold2"
      )
    ),
    both_models_both_folds,
    all_probabilities_valid,
    prediction_rows_correct,
    nrow(
      problem_levels
    ) ==
      0L,
    logistic_beats_constant_both_folds,
    file.exists(
      "docs/validation/stage4a_baseline_fold_metrics.csv"
    ),
    file.exists(
      "docs/validation/stage4a_baseline_aggregate_metrics.csv"
    ),
    file.exists(
      "docs/validation/stage4a_baseline_calibration.csv"
    ),
    file.exists(
      "docs/validation/stage4a_logistic_coefficients.csv"
    ),
    file.exists(
      "data/processed/stage4a_baseline_validation_predictions.rds"
    ),
    file.exists(
      "models/stage4a_logistic_baselines.rds"
    )
  )
)

cat(
  "\n--- STAGE 4A BASELINE MODEL VERIFICATION ---\n"
)

print(
  stage4a_checks,
  n = Inf
)

if (!all(
  stage4a_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 4A checks failed. ",
      "Review the fold metrics before building the GAM."
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
    "\nUpdated renv.lock after Stage 4A baseline modeling.\n"
  )
}

cat(
  "\nSTAGE 4A COMPLETE.\n",
  "Next: review fold-level log-loss, Brier score, calibration, and logistic ",
  "coefficients before building the nonlinear GAM in Stage 4B.\n",
  sep = ""
)
