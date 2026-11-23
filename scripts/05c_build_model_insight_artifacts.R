# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 5C — App-Ready Model Insight Artifacts
# File: scripts/05c_build_model_insight_artifacts.R
#
# PURPOSE:
#   Convert the frozen Stage 4 / Stage 5 evaluation outputs into compact,
#   app-ready artifacts for the future Shiny "Model Comparison",
#   "Calibration & Validation", and "Model Insights" tabs.
#
# IMPORTANT:
#   - No model is fit, tuned, recalibrated, or changed here.
#   - 2025-26 remains evaluation-only.
#   - This script only packages already-completed validation/test diagnostics.
#
# OUTPUTS:
#   data/app/model_performance.rds
#   data/app/model_calibration.rds
#   data/app/model_segment_diagnostics.rds
#   data/app/model_feature_importance.rds
#   data/app/model_insight_cards.rds
#   data/app/model_insights_bundle.rds
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
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
  library(readr)
  library(tibble)
  library(tidyr)
})

# ---- 2. Required inputs ------------------------------------------------------

required_files <- c(
  stage4d_aggregate =
    "docs/validation/stage4d_aggregate_model_comparison.csv",

  stage5a_final_metrics =
    "docs/validation/stage5a_final_holdout_metrics.csv",

  stage5a_selected_eval =
    "docs/validation/stage5a_selected_model_final_evaluation.csv",

  stage5a_outcome_summary =
    "docs/validation/stage5a_holdout_outcome_summary.csv",

  stage5b_calibration_summary =
    "docs/interpretation/stage5b_selected_model_calibration_summary.csv",

  stage5b_calibration_20 =
    "docs/interpretation/stage5b_selected_model_calibration_20bin.csv",

  stage5b_segments =
    "docs/interpretation/stage5b_segment_diagnostics.csv",

  stage5b_shap =
    "docs/interpretation/stage5b_tree_shap_by_source_feature.csv",

  stage5b_gain =
    "docs/interpretation/stage5b_xgboost_gain_importance_by_source.csv",

  stage5b_methodology =
    "docs/interpretation/stage5b_methodology_audit.csv",

  model_selection_metadata =
    "models/final_model_selection_metadata.rds"
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required Stage 5C input(s): ",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

if (!dir.exists("data/app")) {
  dir.create(
    "data/app",
    recursive = TRUE
  )
}

if (!dir.exists("docs/interpretation")) {
  dir.create(
    "docs/interpretation",
    recursive = TRUE
  )
}

# ---- 3. Read frozen evaluation artifacts ------------------------------------

stage4d_aggregate <- readr::read_csv(
  required_files[[
    "stage4d_aggregate"
  ]],
  show_col_types = FALSE
)

stage5a_final_metrics <- readr::read_csv(
  required_files[[
    "stage5a_final_metrics"
  ]],
  show_col_types = FALSE
)

stage5a_selected_eval <- readr::read_csv(
  required_files[[
    "stage5a_selected_eval"
  ]],
  show_col_types = FALSE
)

stage5a_outcome_summary <- readr::read_csv(
  required_files[[
    "stage5a_outcome_summary"
  ]],
  show_col_types = FALSE
)

stage5b_calibration_summary <- readr::read_csv(
  required_files[[
    "stage5b_calibration_summary"
  ]],
  show_col_types = FALSE
)

stage5b_calibration_20 <- readr::read_csv(
  required_files[[
    "stage5b_calibration_20"
  ]],
  show_col_types = FALSE
)

stage5b_segments <- readr::read_csv(
  required_files[[
    "stage5b_segments"
  ]],
  show_col_types = FALSE
)

stage5b_shap <- readr::read_csv(
  required_files[[
    "stage5b_shap"
  ]],
  show_col_types = FALSE
)

stage5b_gain <- readr::read_csv(
  required_files[[
    "stage5b_gain"
  ]],
  show_col_types = FALSE
)

stage5b_methodology <- readr::read_csv(
  required_files[[
    "stage5b_methodology"
  ]],
  show_col_types = FALSE
)

model_selection_metadata <- readRDS(
  required_files[[
    "model_selection_metadata"
  ]]
)

selected_model <- model_selection_metadata$
  selected_model

if (
  is.null(
    selected_model
  ) ||
  selected_model !=
    "xgboost_restrained"
) {
  stop(
    paste0(
      "Expected frozen selected model xgboost_restrained; found: ",
      selected_model
    )
  )
}

# ---- 4. Human-readable labels ------------------------------------------------

model_labels <- c(
  constant_prevalence =
    "Constant Benchmark",

  logistic_baseline =
    "Logistic Regression",

  gam_spatial =
    "Spatial GAM",

  xgboost_restrained =
    "Restrained XGBoost"
)

feature_labels <- c(
  distance_to_rim_check_ft =
    "Distance to Rim",

  shot_type_group =
    "Shot Type",

  court_x_folded =
    "Court X Position",

  court_y_folded =
    "Court Y Position",

  absolute_angle_deg =
    "Shot Angle",

  is_three_geometry =
    "2PT / 3PT Geometry",

  is_corner_three_geometry =
    "Corner-Three Geometry",

  is_cutting =
    "Cutting Action",

  is_running =
    "Running Action",

  is_driving =
    "Driving Action",

  is_bank_shot =
    "Bank Shot",

  clock_seconds_remaining =
    "Game Clock",

  period_number =
    "Period",

  is_alley_oop =
    "Alley-Oop",

  is_home_shooter =
    "Home / Away",

  is_turnaround =
    "Turnaround",

  is_last_5_seconds_period =
    "Final 5 Seconds",

  is_last_24_seconds_period =
    "Final 24 Seconds",

  is_last_2_minutes_period =
    "Final 2 Minutes",

  is_putback =
    "Putback",

  is_fadeaway =
    "Fadeaway",

  is_overtime =
    "Overtime",

  is_heave =
    "Geometry Heave"
)

lookup_label <- function(
    values,
    lookup) {

  labels <- unname(
    lookup[
      values
    ]
  )

  fallback <- gsub(
    "_",
    " ",
    values
  )

  ifelse(
    is.na(
      labels
    ),
    tools::toTitleCase(
      fallback
    ),
    labels
  )
}

# ---- 5. App-ready model performance -----------------------------------------

model_performance <- stage4d_aggregate %>%
  select(
    model,
    validation_log_loss =
      weighted_log_loss,
    validation_brier =
      weighted_brier,
    validation_mean_prediction =
      weighted_mean_predicted_probability,
    validation_observed_rate =
      weighted_observed_make_rate,
    validation_rank =
      rank
  ) %>%
  left_join(
    stage5a_final_metrics %>%
      select(
        model,
        test_log_loss =
          log_loss,
        test_brier =
          brier,
        test_mean_prediction =
          mean_predicted_probability,
        test_observed_rate =
          observed_make_rate,
        calibration_intercept,
        calibration_slope,
        test_rank
      ),
    by =
      "model"
  ) %>%
  mutate(
    model_label =
      lookup_label(
        model,
        model_labels
      ),

    selected_model =
      model ==
      selected_model,

    log_loss_change_to_test =
      test_log_loss -
      validation_log_loss,

    brier_change_to_test =
      test_brier -
      validation_brier
  ) %>%
  select(
    model,
    model_label,
    selected_model,
    validation_rank,
    test_rank,
    validation_log_loss,
    test_log_loss,
    log_loss_change_to_test,
    validation_brier,
    test_brier,
    brier_change_to_test,
    validation_mean_prediction,
    validation_observed_rate,
    test_mean_prediction,
    test_observed_rate,
    calibration_intercept,
    calibration_slope
  ) %>%
  arrange(
    test_rank
  )

# Long format is convenient for plotting in Shiny.
model_performance_long <- bind_rows(
  model_performance %>%
    transmute(
      model,
      model_label,
      selected_model,
      evaluation_set =
        "Chronological Validation",
      log_loss =
        validation_log_loss,
      brier =
        validation_brier,
      mean_prediction =
        validation_mean_prediction,
      observed_rate =
        validation_observed_rate
    ),

  model_performance %>%
    transmute(
      model,
      model_label,
      selected_model,
      evaluation_set =
        "2025-26 Final Holdout",
      log_loss =
        test_log_loss,
      brier =
        test_brier,
      mean_prediction =
        test_mean_prediction,
      observed_rate =
        test_observed_rate
    )
)

# ---- 6. App-ready calibration ------------------------------------------------

model_calibration <- list(
  summary =
    stage5b_calibration_summary,

  reliability_20bin =
    stage5b_calibration_20
)

# ---- 7. App-ready segment diagnostics ---------------------------------------

segment_diagnostics_app <- stage5b_segments %>%
  mutate(
    direction =
      case_when(
        calibration_gap >
          0 ~
          "Underprediction",

        calibration_gap <
          0 ~
          "Overprediction",

        TRUE ~
          "Exact"
      ),

    calibration_gap_pp =
      100 *
      calibration_gap,

    absolute_calibration_gap_pp =
      100 *
      absolute_calibration_gap
  ) %>%
  arrange(
    segment,
    desc(
      shots
    )
  )

# Largest meaningful calibration misses, excluding very small samples.
largest_segment_gaps <- segment_diagnostics_app %>%
  filter(
    shots >=
      1000L
  ) %>%
  arrange(
    desc(
      absolute_calibration_gap
    )
  ) %>%
  slice_head(
    n =
      10L
  )

# ---- 8. App-ready feature importance ----------------------------------------

feature_importance <- full_join(
  stage5b_shap %>%
    select(
      source_feature,
      mean_abs_shap,
      mean_shap,
      shap_share
    ),

  stage5b_gain %>%
    select(
      source_feature,
      gain,
      gain_share
    ),

  by =
    "source_feature"
) %>%
  mutate(
    mean_abs_shap =
      tidyr::replace_na(
        mean_abs_shap,
        0
      ),

    mean_shap =
      tidyr::replace_na(
        mean_shap,
        0
      ),

    shap_share =
      tidyr::replace_na(
        shap_share,
        0
      ),

    gain =
      tidyr::replace_na(
        gain,
        0
      ),

    gain_share =
      tidyr::replace_na(
        gain_share,
        0
      ),

    feature_label =
      lookup_label(
        source_feature,
        feature_labels
      ),

    shap_rank =
      rank(
        -shap_share,
        ties.method =
          "min"
      ),

    gain_rank =
      rank(
        -gain_share,
        ties.method =
          "min"
      )
  ) %>%
  arrange(
    shap_rank,
    gain_rank
  ) %>%
  select(
    source_feature,
    feature_label,
    shap_rank,
    shap_share,
    mean_abs_shap,
    mean_shap,
    gain_rank,
    gain_share,
    gain
  )

top_feature_importance <- feature_importance %>%
  slice_head(
    n =
      15L
  )

# ---- 9. Build concise model-insight cards -----------------------------------

selected_test <- model_performance %>%
  filter(
    selected_model
  )

if (
  nrow(
    selected_test
  ) !=
    1L
) {
  stop(
    "Could not uniquely identify selected model in app performance table."
  )
}

top_feature <- feature_importance %>%
  slice(
    1
  )

second_feature <- feature_importance %>%
  slice(
    2
  )

largest_gap <- largest_segment_gaps %>%
  slice(
    1
  )

holdout_make_rate <- stage5a_outcome_summary$
  make_rate[[1]]

model_insight_cards <- tibble(
  card_id = c(
    "selected_model",
    "final_log_loss",
    "final_brier",
    "holdout_generalization",
    "overall_calibration",
    "top_feature",
    "second_feature",
    "largest_segment_gap"
  ),

  title = c(
    "Selected Model",
    "2025-26 Log-Loss",
    "2025-26 Brier Score",
    "Validation → Holdout",
    "Overall Calibration",
    "Top Model Driver",
    "Second Model Driver",
    "Largest Major Segment Gap"
  ),

  value = c(
    selected_test$
      model_label[[1]],

    sprintf(
      "%.3f",
      selected_test$
        test_log_loss[[1]]
    ),

    sprintf(
      "%.3f",
      selected_test$
        test_brier[[1]]
    ),

    sprintf(
      "%.3f → %.3f",
      selected_test$
        validation_log_loss[[1]],
      selected_test$
        test_log_loss[[1]]
    ),

    sprintf(
      "%.1f%% predicted vs %.1f%% observed",
      100 *
        selected_test$
          test_mean_prediction[[1]],
      100 *
        holdout_make_rate
    ),

    sprintf(
      "%s (%.1f%% SHAP share)",
      top_feature$
        feature_label[[1]],
      100 *
        top_feature$
          shap_share[[1]]
    ),

    sprintf(
      "%s (%.1f%% SHAP share)",
      second_feature$
        feature_label[[1]],
      100 *
        second_feature$
          shap_share[[1]]
    ),

    sprintf(
      "%s — %s: %.1f pp",
      largest_gap$
        segment[[1]],
      largest_gap$
        segment_value[[1]],
      largest_gap$
        absolute_calibration_gap_pp[[1]]
    )
  ),

  detail = c(
    "Chosen before the 2025-26 holdout reveal using weighted chronological validation log-loss.",

    "Primary probability metric on the untouched 2025-26 evaluation season.",

    "Secondary proper scoring rule on the untouched 2025-26 evaluation season.",

    "Lower is better. The selected model generalized at least as well out of season as it did in validation.",

    "Mean predicted make probability compared with the actual 2025-26 make rate.",

    "TreeSHAP global importance; importance describes model reliance, not causality.",

    "TreeSHAP global importance; importance describes model reliance, not causality.",

    "Largest absolute calibration gap among diagnostic segments with at least 1,000 shots. No retuning was performed."
  )
)

# ---- 10. Key findings / limitations table -----------------------------------

key_findings <- tibble(
  finding_type = c(
    "Performance",
    "Generalization",
    "Calibration",
    "Interpretation",
    "Interpretation",
    "Limitation"
  ),

  headline = c(
    "Restrained XGBoost is the frozen champion",
    "Holdout performance remained stable",
    "Overall probability calibration is strong",
    "Distance is the dominant model driver",
    "Shot type adds substantial information",
    "Late-clock attempts remain the clearest calibration weakness"
  ),

  detail = c(
    sprintf(
      "Final holdout log-loss %.3f and Brier %.3f.",
      selected_test$
        test_log_loss[[1]],
      selected_test$
        test_brier[[1]]
    ),

    sprintf(
      "Validation log-loss %.3f versus final-holdout log-loss %.3f.",
      selected_test$
        validation_log_loss[[1]],
      selected_test$
        test_log_loss[[1]]
    ),

    sprintf(
      "Mean predicted probability %.1f%% versus observed make rate %.1f%%.",
      100 *
        selected_test$
          test_mean_prediction[[1]],
      100 *
        holdout_make_rate
    ),

    sprintf(
      "%s accounts for %.1f%% of global TreeSHAP importance.",
      top_feature$
        feature_label[[1]],
      100 *
        top_feature$
          shap_share[[1]]
    ),

    sprintf(
      "%s accounts for %.1f%% of global TreeSHAP importance.",
      second_feature$
        feature_label[[1]],
      100 *
        second_feature$
          shap_share[[1]]
    ),

    sprintf(
      "%s / %s has the largest major-segment calibration gap at %.1f percentage points among segments with at least 1,000 shots.",
      largest_gap$
        segment[[1]],
      largest_gap$
        segment_value[[1]],
      largest_gap$
        absolute_calibration_gap_pp[[1]]
    )
  )
)

# ---- 11. Methodology metadata for Shiny -------------------------------------

methodology_metadata <- list(
  selected_model =
    selected_model,

  selection_metric =
    "Weighted chronological validation log-loss",

  validation_design =
    list(
      fold1 =
        "Train 2022-23; validate 2023-24",

      fold2 =
        "Train 2022-23 + 2023-24; validate 2024-25",

      final_holdout =
        "2025-26 revealed once after model selection"
    ),

  final_holdout_rows =
    stage5a_outcome_summary$
      shots[[1]],

  final_holdout_make_rate =
    stage5a_outcome_summary$
      make_rate[[1]],

  holdout_evaluation_only =
    TRUE,

  model_retrained_after_holdout =
    FALSE,

  calibration_retrained_after_holdout =
    FALSE,

  stage5b_methodology =
    stage5b_methodology
)

# ---- 12. Save individual app artifacts --------------------------------------

saveRDS(
  model_performance,
  "data/app/model_performance.rds",
  compress =
    "xz"
)

saveRDS(
  model_performance_long,
  "data/app/model_performance_long.rds",
  compress =
    "xz"
)

saveRDS(
  model_calibration,
  "data/app/model_calibration.rds",
  compress =
    "xz"
)

saveRDS(
  segment_diagnostics_app,
  "data/app/model_segment_diagnostics.rds",
  compress =
    "xz"
)

saveRDS(
  feature_importance,
  "data/app/model_feature_importance.rds",
  compress =
    "xz"
)

saveRDS(
  model_insight_cards,
  "data/app/model_insight_cards.rds",
  compress =
    "xz"
)

# ---- 13. Save one combined app bundle ---------------------------------------

model_insights_bundle <- list(
  performance =
    model_performance,

  performance_long =
    model_performance_long,

  calibration =
    model_calibration,

  segment_diagnostics =
    segment_diagnostics_app,

  largest_segment_gaps =
    largest_segment_gaps,

  feature_importance =
    feature_importance,

  top_feature_importance =
    top_feature_importance,

  insight_cards =
    model_insight_cards,

  key_findings =
    key_findings,

  methodology =
    methodology_metadata
)

saveRDS(
  model_insights_bundle,
  "data/app/model_insights_bundle.rds",
  compress =
    "xz"
)

# ---- 14. Save reviewable CSV summaries --------------------------------------

readr::write_csv(
  model_performance,
  "docs/interpretation/stage5c_model_performance_app_table.csv"
)

readr::write_csv(
  largest_segment_gaps,
  "docs/interpretation/stage5c_largest_segment_calibration_gaps.csv"
)

readr::write_csv(
  feature_importance,
  "docs/interpretation/stage5c_feature_importance_app_table.csv"
)

readr::write_csv(
  model_insight_cards,
  "docs/interpretation/stage5c_model_insight_cards.csv"
)

readr::write_csv(
  key_findings,
  "docs/interpretation/stage5c_key_findings.csv"
)

# ---- 15. Console review ------------------------------------------------------

cat(
  "\n--- STAGE 5C APP-READY MODEL PERFORMANCE ---\n"
)

print(
  model_performance,
  n = Inf
)

cat(
  "\n--- STAGE 5C TOP FEATURE IMPORTANCE ---\n"
)

print(
  top_feature_importance,
  n = Inf
)

cat(
  "\n--- STAGE 5C LARGEST SEGMENT CALIBRATION GAPS ---\n"
)

print(
  largest_segment_gaps,
  n = Inf
)

cat(
  "\n--- STAGE 5C MODEL INSIGHT CARDS ---\n"
)

print(
  model_insight_cards,
  n = Inf
)

cat(
  "\n--- STAGE 5C KEY FINDINGS ---\n"
)

print(
  key_findings,
  n = Inf
)

# ---- 16. Verification --------------------------------------------------------

expected_app_files <- c(
  "data/app/model_performance.rds",
  "data/app/model_performance_long.rds",
  "data/app/model_calibration.rds",
  "data/app/model_segment_diagnostics.rds",
  "data/app/model_feature_importance.rds",
  "data/app/model_insight_cards.rds",
  "data/app/model_insights_bundle.rds"
)

selected_model_matches <-
  model_performance %>%
  filter(
    selected_model
  ) %>%
  pull(
    model
  ) %>%
  identical(
    selected_model
  )

selected_metrics_match_stage5a <-
  abs(
    selected_test$
      test_log_loss[[1]] -
    stage5a_selected_eval$
      test_log_loss[[1]]
  ) <
  1e-12 &&
  abs(
    selected_test$
      test_brier[[1]] -
    stage5a_selected_eval$
      test_brier[[1]]
  ) <
  1e-12

shap_shares_sum_to_one <-
  abs(
    sum(
      feature_importance$
        shap_share
    ) -
    1
  ) <
  1e-8

gain_shares_sum_to_one <-
  abs(
    sum(
      feature_importance$
        gain_share
    ) -
    1
  ) <
  1e-8

no_intercept_importance <- !any(
  feature_importance$
    source_feature %in%
    c(
      "BIAS",
      "(Intercept)"
    )
)

bundle_sections_complete <- setequal(
  names(
    model_insights_bundle
  ),
  c(
    "performance",
    "performance_long",
    "calibration",
    "segment_diagnostics",
    "largest_segment_gaps",
    "feature_importance",
    "top_feature_importance",
    "insight_cards",
    "key_findings",
    "methodology"
  )
)

stage5c_checks <- tibble(
  check = c(
    "Frozen selected model is preserved in app performance artifact",
    "Selected-model test metrics exactly match Stage 5A",
    "All four model families are present",
    "TreeSHAP shares sum to 1 after intercept removal",
    "Gain-importance shares sum to 1",
    "No SHAP intercept/bias row appears in app feature importance",
    "20-bin calibration table is preserved",
    "Segment diagnostics are non-empty",
    "Largest segment-gap table uses minimum 1,000-shot filter",
    "Insight cards were created",
    "Combined model-insights bundle contains all required sections",
    "All expected app RDS files exist"
  ),

  passed = c(
    selected_model_matches,
    selected_metrics_match_stage5a,
    setequal(
      model_performance$
        model,
      c(
        "constant_prevalence",
        "logistic_baseline",
        "gam_spatial",
        "xgboost_restrained"
      )
    ),
    shap_shares_sum_to_one,
    gain_shares_sum_to_one,
    no_intercept_importance,
    nrow(
      model_calibration$
        reliability_20bin
    ) ==
      20L,
    nrow(
      segment_diagnostics_app
    ) >
      0L,
    all(
      largest_segment_gaps$
        shots >=
        1000L
    ),
    nrow(
      model_insight_cards
    ) >
      0L,
    bundle_sections_complete,
    all(
      file.exists(
        expected_app_files
      )
    )
  )
)

cat(
  "\n--- STAGE 5C APP-ARTIFACT VERIFICATION ---\n"
)

print(
  stage5c_checks,
  n = Inf
)

if (!all(
  stage5c_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 5C checks failed. ",
      "Review the packaging artifacts before beginning the Shiny build."
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
    "\nUpdated renv.lock after Stage 5C artifact packaging.\n"
  )
}

cat(
  "\nSTAGE 5C COMPLETE.\n",
  "All frozen model-performance, calibration, segment-diagnostic, and ",
  "feature-importance outputs are now packaged under data/app for the Shiny ",
  "application. Stage 5 is complete. Next: Stage 6 — Shiny Application.\n",
  sep = ""
)
