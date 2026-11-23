# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 4D — Model Selection & Final Development Refits
# File: scripts/04d_model_selection_final_refit.R
#
# Purpose:
#   1. Formalize model selection using the project's locked primary metric:
#      chronological validation log-loss.
#   2. Compare constant, logistic, GAM, and restrained XGBoost performance.
#   3. Lock the champion model BEFORE any 2025-26 outcome is revealed.
#   4. Refit logistic, GAM, and XGBoost on ALL development shots.
#   5. Generate BLINDED 2025-26 predictions from each finalist model.
#
# IMPORTANT:
#   - The final-test dataset contains NO outcome.
#   - This script never loads raw 2025-26 outcome/scoring_play data.
#   - Model selection is based ONLY on development-era chronological validation.
#   - Primary selection criterion: lowest weighted log-loss.
#   - Weighted Brier score is the secondary tie-break metric.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "Matrix",
  "mgcv",
  "purrr",
  "readr",
  "tibble",
  "tidyr",
  "xgboost"
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
  library(Matrix)
  library(mgcv)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(xgboost)
})

# ---- 2. Required inputs ------------------------------------------------------

required_files <- c(
  model_development =
    "data/processed/model_development.rds",
  model_final_test =
    "data/processed/model_final_test.rds",
  stage4a_metrics =
    "docs/validation/stage4a_baseline_fold_metrics.csv",
  stage4b_metrics =
    "docs/validation/stage4b_gam_fold_metrics.csv",
  stage4c_metrics =
    "docs/validation/stage4c_xgboost_fold_metrics.csv",
  stage4a_models =
    "models/stage4a_logistic_baselines.rds",
  stage4b_models =
    "models/stage4b_gam_models.rds",
  stage4c_models =
    "models/stage4c_xgboost_models.rds"
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required Stage 4 input(s): ",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

model_development <- readRDS(
  required_files[[
    "model_development"
  ]]
)

model_final_test <- readRDS(
  required_files[[
    "model_final_test"
  ]]
)

stage4a_metrics <- readr::read_csv(
  required_files[[
    "stage4a_metrics"
  ]],
  show_col_types =
    FALSE
)

stage4b_metrics <- readr::read_csv(
  required_files[[
    "stage4b_metrics"
  ]],
  show_col_types =
    FALSE
)

stage4c_metrics <- readr::read_csv(
  required_files[[
    "stage4c_metrics"
  ]],
  show_col_types =
    FALSE
)

stage4a_models <- readRDS(
  required_files[[
    "stage4a_models"
  ]]
)

stage4b_models <- readRDS(
  required_files[[
    "stage4b_models"
  ]]
)

stage4c_models <- readRDS(
  required_files[[
    "stage4c_models"
  ]]
)

if (!("outcome" %in% names(
  model_development
))) {
  stop(
    "Development model data do not contain outcome."
  )
}

if ("outcome" %in% names(
  model_final_test
)) {
  stop(
    paste0(
      "FINAL TEST OUTCOME IS PRESENT. ",
      "Stop immediately and restore the blinded Stage 3C final-test dataset."
    )
  )
}

if (!dir.exists("models")) {
  dir.create(
    "models",
    recursive =
      TRUE
  )
}

if (!dir.exists("docs/validation")) {
  dir.create(
    "docs/validation",
    recursive =
      TRUE
  )
}

# ---- 3. Rebuild complete Stage 4 fold comparison ----------------------------

comparison_rows <- bind_rows(
  stage4a_metrics %>%
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
    ),

  stage4b_metrics %>%
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
    ),

  stage4c_metrics %>%
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

# IMPORTANT:
# Calculate weighted metrics before collapsing validation_rows, because
# dplyr::summarise() evaluates expressions sequentially.
aggregate_comparison <- comparison_rows %>%
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

    fold1_log_loss =
      log_loss[
        fold ==
          "fold1"
      ][[1]],

    fold2_log_loss =
      log_loss[
        fold ==
          "fold2"
      ][[1]],

    validation_rows =
      sum(
        validation_rows
      ),

    .groups =
      "drop"
  ) %>%
  arrange(
    weighted_log_loss,
    weighted_brier
  ) %>%
  mutate(
    rank =
      row_number(),
    log_loss_gap_to_best =
      weighted_log_loss -
      min(
        weighted_log_loss
      )
  )

readr::write_csv(
  comparison_rows,
  "docs/validation/stage4d_fold_model_comparison.csv"
)

readr::write_csv(
  aggregate_comparison,
  "docs/validation/stage4d_aggregate_model_comparison.csv"
)

cat(
  "\n--- STAGE 4D AGGREGATE MODEL COMPARISON ---\n"
)

print(
  aggregate_comparison,
  n = Inf
)

# ---- 4. Locked model-selection rule -----------------------------------------
#
# Stage 1 established log-loss as the primary model-selection metric.
#
# Selection rule:
#   1. Lowest validation-row-weighted log-loss across the two chronological
#      folds wins.
#   2. Weighted Brier score is used only as a tie-break if weighted log-loss is
#      equal to machine precision.
#   3. Fold-by-fold performance is retained as a stability diagnostic.
#
# We do NOT use 2025-26 results to choose the model.

winner_row <- aggregate_comparison %>%
  arrange(
    weighted_log_loss,
    weighted_brier
  ) %>%
  slice(
    1
  )

runner_up_row <- aggregate_comparison %>%
  arrange(
    weighted_log_loss,
    weighted_brier
  ) %>%
  slice(
    2
  )

selected_model <- winner_row$
  model[[1]]

selection_summary <- tibble(
  selected_model =
    selected_model,

  primary_metric =
    "weighted chronological validation log-loss",

  selected_weighted_log_loss =
    winner_row$
      weighted_log_loss[[1]],

  runner_up_model =
    runner_up_row$
      model[[1]],

  runner_up_weighted_log_loss =
    runner_up_row$
      weighted_log_loss[[1]],

  absolute_log_loss_gain_vs_runner_up =
    runner_up_row$
      weighted_log_loss[[1]] -
    winner_row$
      weighted_log_loss[[1]],

  relative_log_loss_gain_vs_runner_up =
    (
      runner_up_row$
        weighted_log_loss[[1]] -
      winner_row$
        weighted_log_loss[[1]]
    ) /
    runner_up_row$
      weighted_log_loss[[1]],

  selected_weighted_brier =
    winner_row$
      weighted_brier[[1]],

  selected_best_fold1 =
    winner_row$
      fold1_log_loss[[1]] ==
    min(
      aggregate_comparison$
        fold1_log_loss
    ),

  selected_best_fold2 =
    winner_row$
      fold2_log_loss[[1]] ==
    min(
      aggregate_comparison$
        fold2_log_loss
    ),

  test_outcome_used_for_selection =
    FALSE
)

readr::write_csv(
  selection_summary,
  "docs/validation/stage4d_model_selection_summary.csv"
)

cat(
  "\n--- LOCKED MODEL SELECTION ---\n"
)

print(
  selection_summary,
  n = Inf
)

# ---- 5. Locked formulas / parameters ----------------------------------------

logistic_formula <- stage4a_models$
  formula

gam_formula <- stage4b_models$
  formula

xgb_params <- stage4c_models$
  params

xgb_feature_formula <- stage4c_models$
  formula

if (
  is.null(
    logistic_formula
  ) ||
  is.null(
    gam_formula
  ) ||
  is.null(
    xgb_params
  ) ||
  is.null(
    xgb_feature_formula
  )
) {
  stop(
    "One or more locked Stage 4 model specifications are missing."
  )
}

# Keep final-refit thread counts portable.
detected_cores <- parallel::detectCores(
  logical =
    TRUE
)

model_threads <- if (
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
      detected_cores -
        1L
    )
  )
}

xgb_params$
  nthread <- model_threads

# ---- 6. Final logistic refit -------------------------------------------------

cat(
  "\nRefitting logistic model on ALL development shots...\n"
)

final_logistic_model <- stats::glm(
  formula =
    logistic_formula,
  data =
    model_development,
  family =
    stats::binomial()
)

final_test_logistic_probability <- stats::predict(
  final_logistic_model,
  newdata =
    model_final_test,
  type =
    "response"
)

# ---- 7. Final GAM refit ------------------------------------------------------

cat(
  "Refitting GAM on ALL development shots...\n"
)

final_gam_model <- mgcv::bam(
  formula =
    gam_formula,
  data =
    model_development,
  family =
    stats::binomial(),
  method =
    "fREML",
  discrete =
    TRUE,
  nthreads =
    model_threads
)

final_test_gam_probability <- stats::predict(
  final_gam_model,
  newdata =
    model_final_test,
  type =
    "response"
)

# ---- 8. XGBoost helpers ------------------------------------------------------

make_sparse_matrix <- function(
    df) {

  Matrix::sparse.model.matrix(
    object =
      xgb_feature_formula,
    data =
      df
  )
}

make_dmatrix <- function(
    x,
    label = NULL) {

  xgboost::xgb.DMatrix(
    data =
      x,
    label =
      label,
    nthread =
      model_threads
  )
}

split_internal_early_stopping <- function(
    train_data) {

  unique_dates <- sort(
    unique(
      train_data$
        game_date
    )
  )

  if (length(
    unique_dates
  ) < 20L) {
    stop(
      "Too few unique game dates for final-refit XGBoost early stopping."
    )
  }

  cutoff_index <- floor(
    0.90 *
      length(
        unique_dates
      )
  )

  cutoff_index <- max(
    1L,
    min(
      cutoff_index,
      length(
        unique_dates
      ) -
        1L
    )
  )

  cutoff_date <- unique_dates[
    cutoff_index
  ]

  fit_data <- train_data %>%
    filter(
      game_date <=
        cutoff_date
    )

  early_stop_data <- train_data %>%
    filter(
      game_date >
        cutoff_date
    )

  if (
    nrow(
      fit_data
    ) ==
      0L ||
    nrow(
      early_stop_data
    ) ==
      0L
  ) {
    stop(
      "Final XGBoost internal split produced an empty partition."
    )
  }

  list(
    fit =
      fit_data,
    early_stop =
      early_stop_data,
    cutoff_date =
      cutoff_date
  )
}

get_best_round_from_model <- function(
    model) {

  best_iteration_c <- tryCatch(
    xgboost::xgb.attr(
      model,
      "best_iteration"
    ),
    error =
      function(e) NULL
  )

  best_score_c <- tryCatch(
    xgboost::xgb.attr(
      model,
      "best_score"
    ),
    error =
      function(e) NULL
  )

  if (
    !is.null(
      best_iteration_c
    ) &&
    length(
      best_iteration_c
    ) ==
      1L &&
    !is.na(
      suppressWarnings(
        as.integer(
          best_iteration_c
        )
      )
    )
  ) {

    best_round <-
      suppressWarnings(
        as.integer(
          best_iteration_c
        )
      ) +
      1L

    best_score <- if (
      !is.null(
        best_score_c
      ) &&
      length(
        best_score_c
      ) ==
        1L
    ) {
      suppressWarnings(
        as.numeric(
          best_score_c
        )
      )
    } else {
      NA_real_
    }

    return(
      tibble(
        best_round =
          best_round,
        best_internal_logloss =
          best_score,
        selection_source =
          "xgb.attr"
      )
    )
  }

  early_stop_attr <- tryCatch(
    attr(
      model,
      "early_stop",
      exact =
        TRUE
    ),
    error =
      function(e) NULL
  )

  if (
    !is.null(
      early_stop_attr
    ) &&
    !is.null(
      early_stop_attr$
        best_iteration
    )
  ) {

    return(
      tibble(
        best_round =
          as.integer(
            early_stop_attr$
              best_iteration
          ),
        best_internal_logloss =
          if (
            !is.null(
              early_stop_attr$
                best_score
            )
          ) {
            as.numeric(
              early_stop_attr$
                best_score
            )
          } else {
            NA_real_
          },
        selection_source =
          "R early_stop attribute"
      )
    )
  }

  evaluation_log <- tryCatch(
    attr(
      model,
      "evaluation_log",
      exact =
        TRUE
    ),
    error =
      function(e) NULL
  )

  if (is.null(
    evaluation_log
  )) {
    evaluation_log <- tryCatch(
      model$
        evaluation_log,
      error =
        function(e) NULL
    )
  }

  if (
    is.null(
      evaluation_log
    ) ||
    nrow(
      evaluation_log
    ) ==
      0L
  ) {
    stop(
      "Unable to retrieve final-refit XGBoost early-stopping information."
    )
  }

  metric_candidates <- grep(
    "internal.validation.*logloss|internal_validation.*logloss",
    names(
      evaluation_log
    ),
    value =
      TRUE
  )

  if (length(
    metric_candidates
  ) != 1L) {
    stop(
      "Could not identify final-refit internal validation log-loss."
    )
  }

  metric_column <- metric_candidates[[1]]

  best_row <- which.min(
    evaluation_log[[
      metric_column
    ]]
  )

  tibble(
    best_round =
      as.integer(
        best_row
      ),
    best_internal_logloss =
      as.numeric(
        evaluation_log[[
          metric_column
        ]][
          best_row
        ]
      ),
    selection_source =
      "evaluation_log fallback"
  )
}

# ---- 9. Final XGBoost round selection using DEVELOPMENT ONLY -----------------

cat(
  "Selecting final XGBoost tree count using DEVELOPMENT data only...\n"
)

final_xgb_internal_split <- split_internal_early_stopping(
  model_development
)

x_internal_fit <- make_sparse_matrix(
  final_xgb_internal_split$
    fit
)

x_internal_validation <- make_sparse_matrix(
  final_xgb_internal_split$
    early_stop
)

if (!identical(
  colnames(
    x_internal_fit
  ),
  colnames(
    x_internal_validation
  )
)) {
  stop(
    "Final XGBoost internal matrices have mismatched columns."
  )
}

d_internal_fit <- make_dmatrix(
  x_internal_fit,
  label =
    final_xgb_internal_split$
      fit$
      outcome
)

d_internal_validation <- make_dmatrix(
  x_internal_validation,
  label =
    final_xgb_internal_split$
      early_stop$
      outcome
)

set.seed(
  20261122L
)

final_xgb_early_stop_model <- xgboost::xgb.train(
  params =
    xgb_params,
  data =
    d_internal_fit,
  nrounds =
    1200L,
  evals =
    list(
      internal_validation =
        d_internal_validation
    ),
  early_stopping_rounds =
    60L,
  maximize =
    FALSE,
  verbose =
    0
)

final_xgb_round_info <- get_best_round_from_model(
  final_xgb_early_stop_model
)

final_xgb_rounds <- final_xgb_round_info$
  best_round[[1]]

cat(
  "Final XGBoost selected rounds: ",
  final_xgb_rounds,
  "\n",
  sep = ""
)

# ---- 10. Final XGBoost refit on ALL development -----------------------------

x_development <- make_sparse_matrix(
  model_development
)

x_final_test <- make_sparse_matrix(
  model_final_test
)

if (!identical(
  colnames(
    x_development
  ),
  colnames(
    x_final_test
  )
)) {
  stop(
    "Final development and final-test XGBoost matrices do not match."
  )
}

d_development <- make_dmatrix(
  x_development,
  label =
    model_development$
      outcome
)

d_final_test <- make_dmatrix(
  x_final_test
)

cat(
  "Refitting XGBoost on ALL development shots...\n"
)

set.seed(
  20261122L
)

final_xgboost_model <- xgboost::xgb.train(
  params =
    xgb_params,
  data =
    d_development,
  nrounds =
    final_xgb_rounds,
  verbose =
    0
)

final_test_xgboost_probability <- predict(
  final_xgboost_model,
  d_final_test
)

# ---- 11. Constant benchmark prediction --------------------------------------

development_prevalence <- mean(
  model_development$
    outcome
)

final_test_constant_probability <- rep(
  development_prevalence,
  nrow(
    model_final_test
  )
)

# ---- 12. BLINDED final-test prediction artifact -----------------------------

final_test_predictions_blinded <- model_final_test %>%
  select(
    shot_id,
    game_id,
    game_date,
    hoopr_season,
    nba_season
  ) %>%
  mutate(
    constant_probability =
      final_test_constant_probability,

    logistic_probability =
      as.numeric(
        final_test_logistic_probability
      ),

    gam_probability =
      as.numeric(
        final_test_gam_probability
      ),

    xgboost_probability =
      as.numeric(
        final_test_xgboost_probability
      ),

    selected_model_probability =
      case_when(
        selected_model ==
          "constant_prevalence" ~
          constant_probability,

        selected_model ==
          "logistic_baseline" ~
          logistic_probability,

        selected_model ==
          "gam_spatial" ~
          gam_probability,

        selected_model ==
          "xgboost_restrained" ~
          xgboost_probability,

        TRUE ~
          NA_real_
      ),

    selected_model =
      selected_model
  )

if (anyNA(
  final_test_predictions_blinded$
    selected_model_probability
)) {
  stop(
    "Selected-model final-test probabilities contain missing values."
  )
}

# Summary contains predictions only—never outcome.
blinded_prediction_summary <- tibble(
  model = c(
    "constant_prevalence",
    "logistic_baseline",
    "gam_spatial",
    "xgboost_restrained"
  ),

  mean_probability = c(
    mean(
      final_test_predictions_blinded$
        constant_probability
    ),
    mean(
      final_test_predictions_blinded$
        logistic_probability
    ),
    mean(
      final_test_predictions_blinded$
        gam_probability
    ),
    mean(
      final_test_predictions_blinded$
        xgboost_probability
    )
  ),

  sd_probability = c(
    stats::sd(
      final_test_predictions_blinded$
        constant_probability
    ),
    stats::sd(
      final_test_predictions_blinded$
        logistic_probability
    ),
    stats::sd(
      final_test_predictions_blinded$
        gam_probability
    ),
    stats::sd(
      final_test_predictions_blinded$
        xgboost_probability
    )
  ),

  min_probability = c(
    min(
      final_test_predictions_blinded$
        constant_probability
    ),
    min(
      final_test_predictions_blinded$
        logistic_probability
    ),
    min(
      final_test_predictions_blinded$
        gam_probability
    ),
    min(
      final_test_predictions_blinded$
        xgboost_probability
    )
  ),

  max_probability = c(
    max(
      final_test_predictions_blinded$
        constant_probability
    ),
    max(
      final_test_predictions_blinded$
        logistic_probability
    ),
    max(
      final_test_predictions_blinded$
        gam_probability
    ),
    max(
      final_test_predictions_blinded$
        xgboost_probability
    )
  )
)

cat(
  "\n--- BLINDED 2025-26 PREDICTION SUMMARY ---\n"
)

print(
  blinded_prediction_summary,
  n = Inf
)

cat(
  "\nNo 2025-26 outcomes were loaded or evaluated in Stage 4D.\n"
)

# ---- 13. Final model metadata ------------------------------------------------

final_xgb_refit_summary <- tibble(
  development_rows =
    nrow(
      model_development
    ),

  internal_fit_rows =
    nrow(
      final_xgb_internal_split$
        fit
    ),

  internal_early_stop_rows =
    nrow(
      final_xgb_internal_split$
        early_stop
    ),

  internal_cutoff_date =
    final_xgb_internal_split$
      cutoff_date,

  selected_rounds =
    final_xgb_rounds,

  best_internal_logloss =
    final_xgb_round_info$
      best_internal_logloss[[1]],

  round_selection_source =
    final_xgb_round_info$
      selection_source[[1]]
)

readr::write_csv(
  final_xgb_refit_summary,
  "docs/validation/stage4d_final_xgboost_refit_summary.csv"
)

cat(
  "\n--- FINAL XGBOOST REFIT SUMMARY ---\n"
)

print(
  final_xgb_refit_summary,
  n = Inf
)

# ---- 14. Save final models / prediction artifacts ---------------------------

saveRDS(
  final_logistic_model,
  "models/final_logistic_model.rds",
  compress =
    "xz"
)

saveRDS(
  final_gam_model,
  "models/final_gam_model.rds",
  compress =
    "xz"
)

saveRDS(
  final_xgboost_model,
  "models/final_xgboost_model.rds",
  compress =
    "xz"
)

saveRDS(
  list(
    selected_model =
      selected_model,

    selection_summary =
      selection_summary,

    aggregate_comparison =
      aggregate_comparison,

    logistic_formula =
      logistic_formula,

    gam_formula =
      gam_formula,

    xgb_feature_formula =
      xgb_feature_formula,

    xgb_params =
      xgb_params,

    xgb_feature_names =
      colnames(
        x_development
      ),

    xgb_selected_rounds =
      final_xgb_rounds,

    development_prevalence =
      development_prevalence
  ),
  "models/final_model_selection_metadata.rds",
  compress =
    "xz"
)

saveRDS(
  final_test_predictions_blinded,
  "data/processed/stage4d_final_test_predictions_blinded.rds",
  compress =
    "xz"
)

readr::write_csv(
  blinded_prediction_summary,
  "docs/validation/stage4d_blinded_prediction_summary.csv"
)

# ---- 15. Verification --------------------------------------------------------

all_expected_models_present <- all(
  c(
    "constant_prevalence",
    "logistic_baseline",
    "gam_spatial",
    "xgboost_restrained"
  ) %in%
    aggregate_comparison$
      model
)

selected_model_is_best_logloss <-
  selection_summary$
    selected_weighted_log_loss[[1]] ==
  min(
    aggregate_comparison$
      weighted_log_loss
  )

selected_best_both_folds <-
  selection_summary$
    selected_best_fold1[[1]] &&
  selection_summary$
    selected_best_fold2[[1]]

final_test_still_blinded <-
  !(
    "outcome" %in%
      names(
        model_final_test
      )
  ) &&
  !(
    "outcome" %in%
      names(
        final_test_predictions_blinded
      )
  )

probability_columns <- c(
  "constant_probability",
  "logistic_probability",
  "gam_probability",
  "xgboost_probability",
  "selected_model_probability"
)

all_final_probabilities_valid <- all(
  map_lgl(
    probability_columns,
    function(column_name) {
      values <-
        final_test_predictions_blinded[[
          column_name
        ]]

      all(
        is.finite(
          values
        )
      ) &&
        all(
          values >
            0 &
          values <
            1
        )
    }
  )
)

prediction_rows_match <-
  nrow(
    final_test_predictions_blinded
  ) ==
  nrow(
    model_final_test
  )

xgb_rounds_valid <-
  is.finite(
    final_xgb_rounds
  ) &&
  final_xgb_rounds >=
    1L &&
  final_xgb_rounds <=
    1200L

model_files_exist <- all(
  file.exists(
    c(
      "models/final_logistic_model.rds",
      "models/final_gam_model.rds",
      "models/final_xgboost_model.rds",
      "models/final_model_selection_metadata.rds"
    )
  )
)

prediction_file_exists <- file.exists(
  "data/processed/stage4d_final_test_predictions_blinded.rds"
)

stage4d_checks <- tibble(
  check = c(
    "All four Stage 4 benchmark models are present in model comparison",
    "Selected model has the lowest weighted chronological validation log-loss",
    "Selected model is best on log-loss in both chronological folds",
    "2025-26 final-test data remain blinded with no outcome",
    "All final-test model probabilities are finite and strictly between 0 and 1",
    "Blinded prediction rows match frozen final-test population",
    "Final XGBoost selected round count is valid",
    "Final logistic/GAM/XGBoost model files exist",
    "Final model-selection metadata exists",
    "Blinded 2025-26 prediction RDS exists"
  ),

  passed = c(
    all_expected_models_present,
    selected_model_is_best_logloss,
    selected_best_both_folds,
    final_test_still_blinded,
    all_final_probabilities_valid,
    prediction_rows_match,
    xgb_rounds_valid,
    model_files_exist,
    file.exists(
      "models/final_model_selection_metadata.rds"
    ),
    prediction_file_exists
  )
)

cat(
  "\n--- STAGE 4D MODEL-SELECTION / FINAL-REFIT VERIFICATION ---\n"
)

print(
  stage4d_checks,
  n = Inf
)

if (!all(
  stage4d_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 4D checks failed. ",
      "Do not reveal final-test outcomes until the failure is resolved."
    )
  )
}

if (requireNamespace(
  "renv",
  quietly =
    TRUE
)) {

  renv::snapshot(
    prompt =
      FALSE
  )

  cat(
    "\nUpdated renv.lock after Stage 4D final refits.\n"
  )
}

cat(
  "\nSTAGE 4D COMPLETE.\n",
  "The model family is now selected and all finalist models are frozen using ",
  "development data only. The 2025-26 prediction file is still blinded. ",
  "Stage 5 can now perform the one-time final holdout reveal, calibration ",
  "analysis, and model interpretation.\n",
  sep = ""
)
