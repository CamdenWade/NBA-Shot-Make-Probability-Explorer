# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 5A — One-Time Final Holdout Reveal & Evaluation
# File: scripts/05a_final_holdout_evaluation.R
#
# PURPOSE:
#   Reveal 2025-26 outcomes exactly once AFTER model selection/final refits.
#
#   This script:
#     1. Verifies that Stage 4D selected/froze the model before test reveal.
#     2. Reconstructs 2025-26 make/miss outcome from the original raw hoopR
#        shot release using score_value ONLY for evaluation.
#     3. Joins truth to the frozen 220,085-shot final-test population by shot_id.
#     4. Evaluates constant, logistic, GAM, and restrained XGBoost predictions.
#     5. Produces calibration tables and validation-vs-test comparisons.
#     6. Saves a scored final-test artifact for Stage 5 calibration/diagnostics.
#
# CRITICAL METHODOLOGY RULE:
#   Once this script is run, 2025-26 is NO LONGER an untouched holdout.
#   Do not use its results to retune features, hyperparameters, or model choice.
#
# PRIMARY METRIC:
#   Log-loss
#
# SECONDARY:
#   Brier score
#   Calibration intercept
#   Calibration slope
#   Calibration deciles
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

# ---- 2. Required files -------------------------------------------------------

required_files <- c(
  raw_2026 =
    "data/raw/nba_shots_2026.rds",

  model_final_test =
    "data/processed/model_final_test.rds",

  blinded_predictions =
    "data/processed/stage4d_final_test_predictions_blinded.rds",

  model_selection_metadata =
    "models/final_model_selection_metadata.rds",

  stage4d_selection_summary =
    "docs/validation/stage4d_model_selection_summary.csv",

  stage4d_aggregate_comparison =
    "docs/validation/stage4d_aggregate_model_comparison.csv"
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required Stage 5A input(s): ",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

raw_2026 <- readRDS(
  required_files[[
    "raw_2026"
  ]]
)

model_final_test <- readRDS(
  required_files[[
    "model_final_test"
  ]]
)

blinded_predictions <- readRDS(
  required_files[[
    "blinded_predictions"
  ]]
)

selection_metadata <- readRDS(
  required_files[[
    "model_selection_metadata"
  ]]
)

selection_summary <- readr::read_csv(
  required_files[[
    "stage4d_selection_summary"
  ]],
  show_col_types = FALSE
)

stage4d_aggregate_comparison <- readr::read_csv(
  required_files[[
    "stage4d_aggregate_comparison"
  ]],
  show_col_types = FALSE
)

if (!dir.exists("docs/validation")) {
  dir.create(
    "docs/validation",
    recursive = TRUE
  )
}

# ---- 3. Pre-reveal integrity guards -----------------------------------------

if ("outcome" %in% names(
  model_final_test
)) {
  stop(
    paste0(
      "model_final_test.rds already contains outcome. ",
      "Expected the blinded Stage 3C artifact."
    )
  )
}

if ("outcome" %in% names(
  blinded_predictions
)) {
  stop(
    paste0(
      "Stage 4D prediction artifact already contains outcome. ",
      "Expected a blinded prediction file."
    )
  )
}

if (
  nrow(
    selection_summary
  ) != 1L
) {
  stop(
    "Stage 4D model-selection summary should contain exactly one row."
  )
}

selected_model <- selection_summary$
  selected_model[[1]]

if (
  is.null(
    selection_metadata$
      selected_model
  ) ||
  selection_metadata$
    selected_model !=
    selected_model
) {
  stop(
    "Selected-model metadata do not agree across Stage 4D artifacts."
  )
}

if (
  !("test_outcome_used_for_selection" %in%
      names(
        selection_summary
      )) ||
  !identical(
    as.logical(
      selection_summary$
        test_outcome_used_for_selection[[1]]
    ),
    FALSE
  )
) {
  stop(
    paste0(
      "Stage 4D does not explicitly confirm that test outcome was excluded ",
      "from model selection."
    )
  )
}

if (
  selected_model !=
  "xgboost_restrained"
) {
  warning(
    paste0(
      "Stage 4D selected ",
      selected_model,
      ", not xgboost_restrained. ",
      "Stage 5A will honor the frozen Stage 4D selection."
    )
  )
}

cat(
  "\n--- PRE-REVEAL MODEL LOCK ---\n"
)

print(
  selection_summary,
  n = Inf
)

cat(
  "\nWARNING: running the remainder of this script reveals the 2025-26 ",
  "holdout outcome for final evaluation.\n",
  sep = ""
)

# ---- 4. Reconstruct raw 2025-26 outcome lookup -------------------------------
#
# Raw ESPN/hoopR score_value behavior was established during development:
#   score_value = 0  -> missed field goal
#   score_value = 2  -> made 2PT field goal
#   score_value = 3  -> made 3PT field goal
#
# The frozen final-test population already excludes free throws and ambiguous
# default/no-shot records. score_value is used HERE ONLY as the final truth
# label; it is never a predictive feature.

if (!("score_value" %in% names(
  raw_2026
))) {
  stop(
    "Raw 2026 hoopR shot release does not contain score_value."
  )
}

raw_truth_lookup <- tibble(
  source_row =
    seq_len(
      nrow(
        raw_2026
      )
    ),

  shot_id =
    paste0(
      "2026_",
      source_row
    ),

  raw_score_value =
    raw_2026$
      score_value
) %>%
  mutate(
    outcome =
      case_when(
        raw_score_value ==
          0 ~
          0L,

        raw_score_value %in%
          c(
            2,
            3
          ) ~
          1L,

        TRUE ~
          NA_integer_
      )
  )

raw_score_value_audit <- raw_truth_lookup %>%
  count(
    raw_score_value,
    outcome,
    name =
      "raw_rows",
    sort =
      TRUE
  )

cat(
  "\n--- RAW 2025-26 SCORE-VALUE AUDIT ---\n"
)

print(
  raw_score_value_audit,
  n = Inf
)

readr::write_csv(
  raw_score_value_audit,
  "docs/validation/stage5a_raw_score_value_audit.csv"
)

# ---- 5. Join truth ONLY to the frozen final-test population ------------------

truth_for_frozen_test <- model_final_test %>%
  select(
    shot_id
  ) %>%
  left_join(
    raw_truth_lookup %>%
      select(
        shot_id,
        raw_score_value,
        outcome
      ),
    by =
      "shot_id"
  )

truth_join_audit <- tibble(
  frozen_test_rows =
    nrow(
      model_final_test
    ),

  truth_rows =
    nrow(
      truth_for_frozen_test
    ),

  unique_truth_shot_ids =
    n_distinct(
      truth_for_frozen_test$
        shot_id
    ),

  missing_truth =
    sum(
      is.na(
        truth_for_frozen_test$
          outcome
      )
    ),

  invalid_raw_score_values =
    sum(
      !is.na(
        truth_for_frozen_test$
          raw_score_value
      ) &
      !(
        truth_for_frozen_test$
          raw_score_value %in%
          c(
            0,
            2,
            3
          )
      )
    )
)

cat(
  "\n--- FINAL-TEST TRUTH JOIN AUDIT ---\n"
)

print(
  truth_join_audit,
  n = Inf
)

readr::write_csv(
  truth_join_audit,
  "docs/validation/stage5a_truth_join_audit.csv"
)

if (
  truth_join_audit$
    truth_rows[[1]] !=
    truth_join_audit$
      frozen_test_rows[[1]] ||
  truth_join_audit$
    unique_truth_shot_ids[[1]] !=
    truth_join_audit$
      frozen_test_rows[[1]] ||
  truth_join_audit$
    missing_truth[[1]] !=
    0L ||
  truth_join_audit$
    invalid_raw_score_values[[1]] !=
    0L
) {
  stop(
    paste0(
      "Final-test truth join failed integrity checks. ",
      "Do not continue with holdout evaluation."
    )
  )
}

# ---- 6. Join truth to frozen blinded predictions -----------------------------

scored_final_test <- blinded_predictions %>%
  left_join(
    truth_for_frozen_test %>%
      select(
        shot_id,
        outcome
      ),
    by =
      "shot_id"
  )

if (
  nrow(
    scored_final_test
  ) !=
    nrow(
      model_final_test
    )
) {
  stop(
    "Scored final-test row count does not match frozen test population."
  )
}

if (anyNA(
  scored_final_test$
    outcome
)) {
  stop(
    "Scored final-test data contain missing outcomes."
  )
}

if (!setequal(
  unique(
    scored_final_test$
      outcome
  ),
  c(
    0L,
    1L
  )
)) {
  stop(
    "Final-test outcome is not binary 0/1."
  )
}

# ---- 7. Metric helpers -------------------------------------------------------

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
    probability,
    model_name) {

  calibration <- calibration_metrics(
    truth,
    probability
  )

  tibble(
    model =
      model_name,

    test_rows =
      length(
        truth
      ),

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
    model_name,
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

      min_predicted_probability =
        min(
          probability
        ),

      max_predicted_probability =
        max(
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

      calibration_gap =
        observed_make_rate -
        mean_predicted_probability,

      absolute_gap =
        abs(
          calibration_gap
        ),

      .groups =
        "drop"
    ) %>%
    mutate(
      model =
        model_name,
      .before =
        1
    )
}

# ---- 8. Final holdout evaluation --------------------------------------------

model_probability_columns <- c(
  constant_prevalence =
    "constant_probability",

  logistic_baseline =
    "logistic_probability",

  gam_spatial =
    "gam_probability",

  xgboost_restrained =
    "xgboost_probability"
)

final_test_metrics <- imap_dfr(
  model_probability_columns,
  function(
      probability_column,
      model_name) {

    evaluate_probabilities(
      truth =
        scored_final_test$
          outcome,

      probability =
        scored_final_test[[
          probability_column
        ]],

      model_name =
        model_name
    )
  }
) %>%
  arrange(
    log_loss,
    brier
  ) %>%
  mutate(
    test_rank =
      row_number(),

    log_loss_gap_to_best =
      log_loss -
      min(
        log_loss
      )
  )

cat(
  "\n--- FINAL 2025-26 HOLDOUT METRICS ---\n"
)

print(
  final_test_metrics,
  n = Inf
)

readr::write_csv(
  final_test_metrics,
  "docs/validation/stage5a_final_holdout_metrics.csv"
)

# ---- 9. Test calibration deciles --------------------------------------------

final_test_calibration <- imap_dfr(
  model_probability_columns,
  function(
      probability_column,
      model_name) {

    make_calibration_table(
      truth =
        scored_final_test$
          outcome,

      probability =
        scored_final_test[[
          probability_column
        ]],

      model_name =
        model_name,
      bins =
        10L
    )
  }
)

readr::write_csv(
  final_test_calibration,
  "docs/validation/stage5a_final_holdout_calibration_deciles.csv"
)

# ---- 10. Validation-to-test generalization comparison ------------------------

validation_vs_test <- stage4d_aggregate_comparison %>%
  select(
    model,
    validation_weighted_log_loss =
      weighted_log_loss,

    validation_weighted_brier =
      weighted_brier,

    validation_weighted_mean_predicted_probability =
      weighted_mean_predicted_probability,

    validation_weighted_observed_make_rate =
      weighted_observed_make_rate
  ) %>%
  left_join(
    final_test_metrics %>%
      select(
        model,
        test_log_loss =
          log_loss,

        test_brier =
          brier,

        test_mean_predicted_probability =
          mean_predicted_probability,

        test_observed_make_rate =
          observed_make_rate,

        test_calibration_intercept =
          calibration_intercept,

        test_calibration_slope =
          calibration_slope,

        test_rank
      ),
    by =
      "model"
  ) %>%
  mutate(
    log_loss_change_validation_to_test =
      test_log_loss -
      validation_weighted_log_loss,

    brier_change_validation_to_test =
      test_brier -
      validation_weighted_brier
  ) %>%
  arrange(
    test_log_loss
  )

cat(
  "\n--- VALIDATION VS FINAL-TEST GENERALIZATION ---\n"
)

print(
  validation_vs_test,
  n = Inf
)

readr::write_csv(
  validation_vs_test,
  "docs/validation/stage5a_validation_vs_final_test.csv"
)

# ---- 11. Frozen-selected-model evaluation -----------------------------------

selected_model_test_result <- final_test_metrics %>%
  filter(
    model ==
      selected_model
  )

if (
  nrow(
    selected_model_test_result
  ) !=
    1L
) {
  stop(
    "Could not uniquely locate the frozen selected model in final metrics."
  )
}

selected_model_evaluation <- selection_summary %>%
  transmute(
    selected_model,

    validation_weighted_log_loss =
      selected_weighted_log_loss,

    validation_weighted_brier =
      selected_weighted_brier,

    test_log_loss =
      selected_model_test_result$
        log_loss[[1]],

    test_brier =
      selected_model_test_result$
        brier[[1]],

    test_observed_make_rate =
      selected_model_test_result$
        observed_make_rate[[1]],

    test_mean_predicted_probability =
      selected_model_test_result$
        mean_predicted_probability[[1]],

    test_calibration_intercept =
      selected_model_test_result$
        calibration_intercept[[1]],

    test_calibration_slope =
      selected_model_test_result$
        calibration_slope[[1]],

    test_rank =
      selected_model_test_result$
        test_rank[[1]],

    selected_before_test_reveal =
      TRUE,

    holdout_now_revealed =
      TRUE
  )

cat(
  "\n--- FROZEN SELECTED-MODEL FINAL EVALUATION ---\n"
)

print(
  selected_model_evaluation,
  n = Inf
)

readr::write_csv(
  selected_model_evaluation,
  "docs/validation/stage5a_selected_model_final_evaluation.csv"
)

# ---- 12. Overall holdout outcome audit --------------------------------------

holdout_outcome_summary <- scored_final_test %>%
  summarise(
    shots =
      n(),

    makes =
      sum(
        outcome ==
          1L
      ),

    misses =
      sum(
        outcome ==
          0L
      ),

    make_rate =
      mean(
        outcome
      )
  )

cat(
  "\n--- 2025-26 HOLDOUT OUTCOME SUMMARY ---\n"
)

print(
  holdout_outcome_summary,
  n = Inf
)

readr::write_csv(
  holdout_outcome_summary,
  "docs/validation/stage5a_holdout_outcome_summary.csv"
)

# ---- 13. Save revealed/scored artifact --------------------------------------

saveRDS(
  scored_final_test,
  "data/processed/stage5a_final_test_scored.rds",
  compress =
    "xz"
)

saveRDS(
  list(
    reveal_date =
      Sys.Date(),

    selected_model =
      selected_model,

    selected_before_reveal =
      TRUE,

    stage4_selection_summary =
      selection_summary,

    final_test_metrics =
      final_test_metrics,

    selected_model_evaluation =
      selected_model_evaluation
  ),
  "data/processed/stage5a_holdout_reveal_metadata.rds",
  compress =
    "xz"
)

# ---- 14. Verification --------------------------------------------------------

truth_join_exact <-
  truth_join_audit$
    truth_rows[[1]] ==
  truth_join_audit$
    frozen_test_rows[[1]] &&
  truth_join_audit$
    unique_truth_shot_ids[[1]] ==
  truth_join_audit$
    frozen_test_rows[[1]] &&
  truth_join_audit$
    missing_truth[[1]] ==
  0L &&
  truth_join_audit$
    invalid_raw_score_values[[1]] ==
  0L

all_four_models_evaluated <- setequal(
  final_test_metrics$
    model,
  c(
    "constant_prevalence",
    "logistic_baseline",
    "gam_spatial",
    "xgboost_restrained"
  )
)

all_metrics_finite <- all(
  is.finite(
    final_test_metrics$
      log_loss
  )
) &&
  all(
    is.finite(
      final_test_metrics$
        brier
    )
  ) &&
  all(
    is.finite(
      final_test_metrics$
        mean_predicted_probability
    )
  ) &&
  all(
    is.finite(
      final_test_metrics$
        observed_make_rate
    )
  )

selected_model_preserved <-
  selected_model ==
  selection_metadata$
    selected_model

selection_preceded_reveal <-
  identical(
    as.logical(
      selection_summary$
        test_outcome_used_for_selection[[1]]
    ),
    FALSE
  )

scored_rows_correct <-
  nrow(
    scored_final_test
  ) ==
  nrow(
    model_final_test
  )

stage5a_checks <- tibble(
  check = c(
    "Stage 4D selected model is preserved unchanged before reveal",
    "Stage 4D confirms test outcome was not used for model selection",
    "Final-test truth joins exactly to the frozen test population",
    "Final-test outcome is binary 0/1 with no missing values",
    "All four Stage 4 models were evaluated on the final holdout",
    "All final holdout metrics are finite",
    "Selected-model final evaluation exists exactly once",
    "Scored final-test rows match frozen final-test population",
    "Final holdout metric CSV exists",
    "Calibration-decile CSV exists",
    "Validation-vs-test comparison CSV exists",
    "Scored final-test RDS exists",
    "Holdout reveal metadata RDS exists"
  ),

  passed = c(
    selected_model_preserved,
    selection_preceded_reveal,
    truth_join_exact,
    setequal(
      unique(
        scored_final_test$
          outcome
      ),
      c(
        0L,
        1L
      )
    ) &&
      !anyNA(
        scored_final_test$
          outcome
      ),
    all_four_models_evaluated,
    all_metrics_finite,
    nrow(
      selected_model_evaluation
    ) ==
      1L,
    scored_rows_correct,
    file.exists(
      "docs/validation/stage5a_final_holdout_metrics.csv"
    ),
    file.exists(
      "docs/validation/stage5a_final_holdout_calibration_deciles.csv"
    ),
    file.exists(
      "docs/validation/stage5a_validation_vs_final_test.csv"
    ),
    file.exists(
      "data/processed/stage5a_final_test_scored.rds"
    ),
    file.exists(
      "data/processed/stage5a_holdout_reveal_metadata.rds"
    )
  )
)

cat(
  "\n--- STAGE 5A FINAL-HOLDOUT VERIFICATION ---\n"
)

print(
  stage5a_checks,
  n = Inf
)

if (!all(
  stage5a_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 5A holdout checks failed. ",
      "Do not perform any further model changes based on test results."
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
    "\nUpdated renv.lock after Stage 5A final holdout evaluation.\n"
  )
}

cat(
  "\nSTAGE 5A COMPLETE.\n",
  "The 2025-26 holdout has now been revealed exactly once. ",
  "From this point forward it is evaluation-only and must not be used to ",
  "retune the model. Next: Stage 5B calibration diagnostics and model ",
  "interpretation using the frozen selected model.\n",
  sep = ""
)
