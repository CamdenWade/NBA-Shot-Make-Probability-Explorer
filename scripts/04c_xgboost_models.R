# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 4C — Restrained XGBoost
# File: scripts/04c_xgboost_models.R
#
# Purpose:
#   Evaluate a deliberately restrained gradient-boosted tree model against the
#   locked constant, logistic, and GAM benchmarks.
#
# Validation strategy:
#   - Same chronological validation folds as Stages 4A/4B.
#   - Within each TRAIN fold only, reserve the latest ~10% of unique game dates
#     as an internal early-stopping set.
#   - Use early stopping to select boosting rounds WITHOUT touching the locked
#     validation fold.
#   - Refit on the full fold training data using the selected number of rounds.
#   - Evaluate once on the locked validation fold.
#
# This preserves the chronological validation design while allowing XGBoost to
# select a sensible tree count.
#
# Primary metric:
#   Log-loss
#
# Secondary:
#   Brier score
#   Calibration intercept
#   Calibration slope
#
# IMPORTANT:
#   - 2025-26 final-test outcome is never loaded or accessed.
#   - No player/team identity enters the model.
#   - No raw type_id/type_text enters the model.
#   - No score_value/scoring_play leakage enters the model.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "Matrix",
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
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(xgboost)
})

# ---- 2. Input checks ---------------------------------------------------------

model_development_path <-
  "data/processed/model_development.rds"

baseline_metrics_path <-
  "docs/validation/stage4a_baseline_fold_metrics.csv"

gam_metrics_path <-
  "docs/validation/stage4b_gam_fold_metrics.csv"

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

if (!file.exists(gam_metrics_path)) {
  stop(
    "Missing Stage 4B GAM metrics. ",
    "Run scripts/04b_gam_models.R first."
  )
}

model_development <- readRDS(
  model_development_path
)

baseline_metrics <- readr::read_csv(
  baseline_metrics_path,
  show_col_types = FALSE
)

gam_metrics <- readr::read_csv(
  gam_metrics_path,
  show_col_types = FALSE
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

# ---- 3. Restrained XGBoost feature set --------------------------------------
#
# This is intentionally not "everything available".
# We keep the core continuous geometry, grouped shot family, selected action
# tags, and game context, while excluding redundant zone/distance-band fields
# and all identity variables.

xgb_feature_formula <- stats::as.formula(
  paste(
    "~",
    "court_x_folded +",
    "court_y_folded +",
    "distance_to_rim_check_ft +",
    "absolute_angle_deg +",
    "is_three_geometry +",
    "is_corner_three_geometry +",
    "is_heave +",
    "shot_type_group +",
    "is_bank_shot +",
    "is_driving +",
    "is_running +",
    "is_cutting +",
    "is_alley_oop +",
    "is_putback +",
    "is_turnaround +",
    "is_fadeaway +",
    "period_number +",
    "clock_seconds_remaining +",
    "is_overtime +",
    "is_last_2_minutes_period +",
    "is_last_24_seconds_period +",
    "is_last_5_seconds_period +",
    "is_home_shooter",
    "- 1"
  )
)

xgb_source_features <- c(
  "court_x_folded",
  "court_y_folded",
  "distance_to_rim_check_ft",
  "absolute_angle_deg",
  "is_three_geometry",
  "is_corner_three_geometry",
  "is_heave",
  "shot_type_group",
  "is_bank_shot",
  "is_driving",
  "is_running",
  "is_cutting",
  "is_alley_oop",
  "is_putback",
  "is_turnaround",
  "is_fadeaway",
  "period_number",
  "clock_seconds_remaining",
  "is_overtime",
  "is_last_2_minutes_period",
  "is_last_24_seconds_period",
  "is_last_5_seconds_period",
  "is_home_shooter"
)

missing_features <- setdiff(
  xgb_source_features,
  names(
    model_development
  )
)

if (length(missing_features) > 0) {
  stop(
    paste0(
      "XGBoost source feature(s) missing: ",
      paste(
        missing_features,
        collapse = ", "
      )
    )
  )
}

# ---- 4. Restrained fixed hyperparameters ------------------------------------
#
# No broad tuning search is performed here. The goal is a stable benchmark that
# can model nonlinearities/interactions without aggressively chasing validation
# noise.

detected_cores <- parallel::detectCores(
  logical = TRUE
)

xgb_threads <- if (
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

xgb_params <- list(
  objective =
    "binary:logistic",
  eval_metric =
    "logloss",
  tree_method =
    "hist",
  learning_rate =
    0.03,
  max_depth =
    4L,
  min_child_weight =
    20,
  subsample =
    0.80,
  colsample_bytree =
    0.80,
  reg_lambda =
    5,
  reg_alpha =
    0.25,
  max_bin =
    256L,
  nthread =
    xgb_threads,
  seed =
    20261122L
)

max_boosting_rounds <- 1200L
early_stopping_rounds <- 60L

cat(
  "\nXGBoost threads: ",
  xgb_threads,
  "\n",
  sep = ""
)

# ---- 5. Helpers --------------------------------------------------------------

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

make_sparse_matrix <- function(df) {

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
      xgb_threads
  )
}

# Internal chronological early-stopping split:
# last ~10% of unique game dates in the training fold.
split_internal_early_stopping <- function(
    train_data) {

  unique_dates <- sort(
    unique(
      train_data$
        game_date
    )
  )

  if (length(unique_dates) < 20L) {
    stop(
      "Training fold has too few unique game dates for internal early stopping."
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

  last_fit_date <- unique_dates[
    cutoff_index
  ]

  fit_data <- train_data %>%
    filter(
      game_date <=
        last_fit_date
    )

  early_stop_data <- train_data %>%
    filter(
      game_date >
        last_fit_date
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
      "Internal chronological early-stopping split produced an empty partition."
    )
  }

  list(
    fit =
      fit_data,
    early_stop =
      early_stop_data,
    cutoff_date =
      last_fit_date
  )
}

get_best_round_from_model <- function(model) {

  # ---------------------------------------------------------------------------
  # Preferred path for current XGBoost R versions:
  #
  # xgb.cb.early.stop() stores:
  #   - C-level booster attribute "best_iteration" using 0-based indexing
  #   - C-level booster attribute "best_score"
  #
  # Convert best_iteration to R's 1-based boosting-round count.
  # ---------------------------------------------------------------------------

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

    best_round <- suppressWarnings(
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

  # ---------------------------------------------------------------------------
  # Compatibility path:
  # Newer callback implementations can keep an R attribute named "early_stop".
  # Its best_iteration follows base-1 indexing.
  # ---------------------------------------------------------------------------

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

    best_round <- as.integer(
      early_stop_attr$
        best_iteration
    )

    best_score <- if (
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
    }

    return(
      tibble(
        best_round =
          best_round,
        best_internal_logloss =
          best_score,
        selection_source =
          "R early_stop attribute"
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Evaluation-log fallback:
  # Current xgb.Booster objects may store evaluation_log as an R attribute
  # rather than a normal list element. Older versions may expose it with `$`.
  # ---------------------------------------------------------------------------

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
      paste0(
        "Unable to retrieve XGBoost early-stopping information from ",
        "best_iteration, early_stop, or evaluation_log. ",
        "Run packageVersion('xgboost') and attributes(early_stop_model) ",
        "for diagnostics."
      )
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
      paste0(
        "Could not uniquely identify internal validation logloss column. ",
        "Evaluation log columns: ",
        paste(
          names(
            evaluation_log
          ),
          collapse = ", "
        )
      )
    )
  }

  metric_column <- metric_candidates[[1]]

  best_row <- which.min(
    evaluation_log[[
      metric_column
    ]]
  )

  best_round <- as.integer(
    best_row
  )

  tibble(
    best_round =
      best_round,
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

# ---- 6. Locked validation folds ---------------------------------------------

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

# ---- 7. Fit/evaluate one fold ------------------------------------------------

run_xgb_fold <- function(
    train_data,
    validation_data,
    fold_name) {

  cat(
    "\nPreparing ",
    fold_name,
    " internal early-stopping split...\n",
    sep = ""
  )

  internal_split <- split_internal_early_stopping(
    train_data
  )

  x_internal_fit <- make_sparse_matrix(
    internal_split$
      fit
  )

  x_internal_validation <- make_sparse_matrix(
    internal_split$
      early_stop
  )

  # Column identity should be guaranteed by frozen factor levels, but verify.
  if (!identical(
    colnames(
      x_internal_fit
    ),
    colnames(
      x_internal_validation
    )
  )) {
    stop(
      "Internal XGBoost design matrices do not have identical columns."
    )
  }

  d_internal_fit <- make_dmatrix(
    x_internal_fit,
    label =
      internal_split$
        fit$
        outcome
  )

  d_internal_validation <- make_dmatrix(
    x_internal_validation,
    label =
      internal_split$
        early_stop$
        outcome
  )

  cat(
    "Selecting boosting rounds for ",
    fold_name,
    " using training data only...\n",
    sep = ""
  )

  set.seed(
    20261122L
  )

  early_stop_model <- xgboost::xgb.train(
    params =
      xgb_params,
    data =
      d_internal_fit,
    nrounds =
      max_boosting_rounds,
    evals =
      list(
        internal_validation =
          d_internal_validation
      ),
    early_stopping_rounds =
      early_stopping_rounds,
    maximize =
      FALSE,
    verbose =
      0
  )

  best_round_info <- get_best_round_from_model(
    early_stop_model
  )

  selected_rounds <- best_round_info$
    best_round[[1]]

  cat(
    fold_name,
    " selected boosting rounds: ",
    selected_rounds,
    "\n",
    sep = ""
  )

  # Rebuild full fold training + locked validation matrices.
  x_train <- make_sparse_matrix(
    train_data
  )

  x_validation <- make_sparse_matrix(
    validation_data
  )

  if (!identical(
    colnames(
      x_train
    ),
    colnames(
      x_validation
    )
  )) {
    stop(
      "Full-training and locked-validation XGBoost matrices do not match."
    )
  }

  d_train <- make_dmatrix(
    x_train,
    label =
      train_data$
        outcome
  )

  d_validation <- make_dmatrix(
    x_validation,
    label =
      validation_data$
        outcome
  )

  set.seed(
    20261122L
  )

  final_fold_model <- xgboost::xgb.train(
    params =
      xgb_params,
    data =
      d_train,
    nrounds =
      selected_rounds,
    verbose =
      0
  )

  probability <- predict(
    final_fold_model,
    d_validation
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
        "xgboost_restrained",
      train_rows =
        nrow(
          train_data
        ),
      validation_rows =
        nrow(
          validation_data
        ),
      selected_rounds =
        selected_rounds,
      internal_cutoff_date =
        internal_split$
          cutoff_date,
      internal_fit_rows =
        nrow(
          internal_split$
            fit
        ),
      internal_early_stop_rows =
        nrow(
          internal_split$
            early_stop
        ),
      best_internal_logloss =
        best_round_info$
          best_internal_logloss[[1]],
      round_selection_source =
        best_round_info$
          selection_source[[1]],
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
        "xgboost_restrained",
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
    xgboost_probability =
      probability
  )

  importance <- xgboost::xgb.importance(
    model =
      final_fold_model
  ) %>%
    as_tibble() %>%
    mutate(
      fold =
        fold_name,
      .before =
        1
    )

  internal_summary <- tibble(
    fold =
      fold_name,
    train_rows =
      nrow(
        train_data
      ),
    internal_fit_rows =
      nrow(
        internal_split$
          fit
      ),
    internal_early_stop_rows =
      nrow(
        internal_split$
          early_stop
      ),
    internal_cutoff_date =
      internal_split$
        cutoff_date,
    selected_rounds =
      selected_rounds,
    best_internal_logloss =
      best_round_info$
        best_internal_logloss[[1]],
    round_selection_source =
      best_round_info$
        selection_source[[1]]
  )

  list(
    model =
      final_fold_model,
    metrics =
      metrics,
    calibration =
      calibration,
    predictions =
      predictions,
    importance =
      importance,
    internal_summary =
      internal_summary,
    feature_names =
      colnames(
        x_train
      )
  )
}

# ---- 8. Run both folds -------------------------------------------------------

fold1_xgb <- run_xgb_fold(
  train_data =
    fold1_train,
  validation_data =
    fold1_validation,
  fold_name =
    "fold1"
)

fold2_xgb <- run_xgb_fold(
  train_data =
    fold2_train,
  validation_data =
    fold2_validation,
  fold_name =
    "fold2"
)

xgb_metrics <- bind_rows(
  fold1_xgb$
    metrics,
  fold2_xgb$
    metrics
)

xgb_calibration <- bind_rows(
  fold1_xgb$
    calibration,
  fold2_xgb$
    calibration
)

xgb_predictions <- bind_rows(
  fold1_xgb$
    predictions,
  fold2_xgb$
    predictions
)

xgb_importance <- bind_rows(
  fold1_xgb$
    importance,
  fold2_xgb$
    importance
)

xgb_internal_summary <- bind_rows(
  fold1_xgb$
    internal_summary,
  fold2_xgb$
    internal_summary
)

# ---- 9. Aggregate XGBoost metrics -------------------------------------------

xgb_aggregate <- xgb_metrics %>%
  summarise(
    model =
      "xgboost_restrained",
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

# ---- 10. Compare all Stage 4 models -----------------------------------------

comparison_rows <- bind_rows(
  baseline_metrics %>%
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
    ),

  xgb_metrics %>%
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

aggregate_model_comparison <- comparison_rows %>%
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
  ) %>%
  arrange(
    weighted_log_loss
  )

xgb_vs_gam <- comparison_rows %>%
  filter(
    model %in%
      c(
        "gam_spatial",
        "xgboost_restrained"
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
    log_loss_gain_vs_gam =
      log_loss_gam_spatial -
      log_loss_xgboost_restrained,
    brier_gain_vs_gam =
      brier_gam_spatial -
      brier_xgboost_restrained
  )

# ---- 11. Save outputs --------------------------------------------------------

readr::write_csv(
  xgb_metrics,
  "docs/validation/stage4c_xgboost_fold_metrics.csv"
)

readr::write_csv(
  xgb_aggregate,
  "docs/validation/stage4c_xgboost_aggregate_metrics.csv"
)

readr::write_csv(
  xgb_calibration,
  "docs/validation/stage4c_xgboost_calibration.csv"
)

readr::write_csv(
  xgb_internal_summary,
  "docs/validation/stage4c_xgboost_internal_early_stopping.csv"
)

readr::write_csv(
  xgb_importance,
  "docs/validation/stage4c_xgboost_feature_importance.csv"
)

readr::write_csv(
  comparison_rows,
  "docs/validation/stage4c_all_model_fold_comparison.csv"
)

readr::write_csv(
  aggregate_model_comparison,
  "docs/validation/stage4c_all_model_aggregate_comparison.csv"
)

readr::write_csv(
  xgb_vs_gam,
  "docs/validation/stage4c_xgboost_vs_gam.csv"
)

saveRDS(
  xgb_predictions,
  "data/processed/stage4c_xgboost_validation_predictions.rds",
  compress =
    "xz"
)

saveRDS(
  list(
    params =
      xgb_params,
    formula =
      xgb_feature_formula,
    fold1_selected_rounds =
      fold1_xgb$
        internal_summary$
        selected_rounds[[1]],
    fold2_selected_rounds =
      fold2_xgb$
        internal_summary$
        selected_rounds[[1]],
    fold1_feature_names =
      fold1_xgb$
        feature_names,
    fold2_feature_names =
      fold2_xgb$
        feature_names,
    fold1_model =
      fold1_xgb$
        model,
    fold2_model =
      fold2_xgb$
        model
  ),
  "models/stage4c_xgboost_models.rds",
  compress =
    "xz"
)

# ---- 12. Console output ------------------------------------------------------

cat(
  "\n--- STAGE 4C INTERNAL EARLY-STOPPING SUMMARY ---\n"
)

print(
  xgb_internal_summary,
  n = Inf
)

cat(
  "\n--- STAGE 4C XGBOOST FOLD METRICS ---\n"
)

print(
  xgb_metrics,
  n = Inf
)

cat(
  "\n--- XGBOOST VS GAM BY FOLD ---\n"
)

print(
  xgb_vs_gam,
  n = Inf
)

cat(
  "\n--- STAGE 4C AGGREGATE MODEL COMPARISON ---\n"
)

print(
  aggregate_model_comparison,
  n = Inf
)

cat(
  "\n--- XGBOOST FEATURE IMPORTANCE (TOP 30 PER FOLD) ---\n"
)

xgb_importance %>%
  group_by(
    fold
  ) %>%
  slice_max(
    order_by =
      Gain,
    n =
      30,
    with_ties =
      FALSE
  ) %>%
  ungroup() %>%
  print(
    n = Inf
  )

# ---- 13. Verification --------------------------------------------------------

both_folds_present <- setequal(
  unique(
    xgb_metrics$
      fold
  ),
  c(
    "fold1",
    "fold2"
  )
)

selected_rounds_valid <- all(
  xgb_internal_summary$
    selected_rounds >=
    1L
) &&
  all(
    xgb_internal_summary$
      selected_rounds <=
      max_boosting_rounds
  )

internal_split_chronological <- all(
  map_lgl(
    list(
      list(
        full =
          fold1_train,
        cutoff =
          fold1_xgb$
            internal_summary$
            internal_cutoff_date[[1]]
      ),
      list(
        full =
          fold2_train,
        cutoff =
          fold2_xgb$
            internal_summary$
            internal_cutoff_date[[1]]
      )
    ),
    function(x) {
      any(
        x$
          full$
          game_date <=
          x$
          cutoff
      ) &&
      any(
        x$
          full$
          game_date >
          x$
          cutoff
      )
    }
  )
)

all_probabilities_valid <- all(
  xgb_predictions$
    xgboost_probability >
    0 &
  xgb_predictions$
    xgboost_probability <
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
    xgb_predictions
  ) ==
  expected_validation_rows

metrics_finite <- all(
  is.finite(
    xgb_metrics$
      log_loss
  )
) &&
  all(
    is.finite(
      xgb_metrics$
        brier
    )
  )

feature_names_match_across_folds <- identical(
  fold1_xgb$
    feature_names,
  fold2_xgb$
    feature_names
)

all_models_present <- all(
  c(
    "constant_prevalence",
    "logistic_baseline",
    "gam_spatial",
    "xgboost_restrained"
  ) %in%
    aggregate_model_comparison$
      model
)

stage4c_checks <- tibble(
  check = c(
    "Both locked chronological validation folds were evaluated",
    "Internal early-stopping partitions exist chronologically within train folds",
    "Selected boosting rounds are valid",
    "XGBoost feature matrices use matching columns across folds",
    "All XGBoost validation probabilities are strictly between 0 and 1",
    "XGBoost validation prediction row count matches both folds",
    "XGBoost fold metrics are finite",
    "Feature-importance audit was created",
    "All four Stage 4 benchmark models are present in aggregate comparison",
    "XGBoost validation predictions RDS exists",
    "XGBoost model RDS exists"
  ),
  passed = c(
    both_folds_present,
    internal_split_chronological,
    selected_rounds_valid,
    feature_names_match_across_folds,
    all_probabilities_valid,
    prediction_rows_correct,
    metrics_finite,
    nrow(
      xgb_importance
    ) >
      0,
    all_models_present,
    file.exists(
      "data/processed/stage4c_xgboost_validation_predictions.rds"
    ),
    file.exists(
      "models/stage4c_xgboost_models.rds"
    )
  )
)

cat(
  "\n--- STAGE 4C XGBOOST VERIFICATION ---\n"
)

print(
  stage4c_checks,
  n = Inf
)

if (!all(
  stage4c_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 4C checks failed. ",
      "Review the XGBoost diagnostics before proceeding."
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
    "\nUpdated renv.lock after Stage 4C XGBoost modeling.\n"
  )
}

cat(
  "\nSTAGE 4C COMPLETE.\n",
  "Next: review whether restrained XGBoost materially improves on the GAM. ",
  "Then Stage 4D will lock the model-selection rule and prepare final ",
  "development refits without touching 2025-26 outcomes.\n",
  sep = ""
)
