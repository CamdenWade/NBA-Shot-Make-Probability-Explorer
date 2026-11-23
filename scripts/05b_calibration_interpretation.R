# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 5B — Calibration Diagnostics & Frozen-Model Interpretation
# File: scripts/05b_calibration_interpretation.R
#
# PURPOSE:
#   Diagnose and interpret the already-frozen Stage 4D XGBoost model after the
#   one-time Stage 5A 2025-26 holdout reveal.
#
# IMPORTANT METHODOLOGY RULE:
#   This script DOES NOT retune, recalibrate, or alter the selected model.
#   2025-26 is evaluation-only.
#
# Outputs:
#   - Selected-model calibration summary
#   - 20-bin reliability table
#   - Segment-level performance/calibration
#   - Final XGBoost gain importance
#   - TreeSHAP global importance on a deterministic 25k final-test sample
#   - Portfolio-quality calibration / interpretation PNGs
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "ggplot2",
  "Matrix",
  "purrr",
  "readr",
  "scales",
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
  library(ggplot2)
  library(Matrix)
  library(purrr)
  library(readr)
  library(scales)
  library(tibble)
  library(tidyr)
  library(xgboost)
})

# ---- 2. Required inputs ------------------------------------------------------

required_files <- c(
  scored_final_test =
    "data/processed/stage5a_final_test_scored.rds",

  model_final_test =
    "data/processed/model_final_test.rds",

  final_xgboost_model =
    "models/final_xgboost_model.rds",

  final_model_metadata =
    "models/final_model_selection_metadata.rds",

  stage5a_final_metrics =
    "docs/validation/stage5a_final_holdout_metrics.csv",

  stage5a_selected_eval =
    "docs/validation/stage5a_selected_model_final_evaluation.csv"
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required Stage 5B input(s): ",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

scored_final_test <- readRDS(
  required_files[[
    "scored_final_test"
  ]]
)

model_final_test <- readRDS(
  required_files[[
    "model_final_test"
  ]]
)

final_xgboost_model <- readRDS(
  required_files[[
    "final_xgboost_model"
  ]]
)

final_model_metadata <- readRDS(
  required_files[[
    "final_model_metadata"
  ]]
)

stage5a_final_metrics <- readr::read_csv(
  required_files[[
    "stage5a_final_metrics"
  ]],
  show_col_types =
    FALSE
)

stage5a_selected_eval <- readr::read_csv(
  required_files[[
    "stage5a_selected_eval"
  ]],
  show_col_types =
    FALSE
)

if (!dir.exists("docs/interpretation")) {
  dir.create(
    "docs/interpretation",
    recursive =
      TRUE
  )
}

# ---- 3. Integrity checks -----------------------------------------------------

if (!("outcome" %in% names(
  scored_final_test
))) {
  stop(
    "Stage 5A scored final-test artifact does not contain outcome."
  )
}

if ("outcome" %in% names(
  model_final_test
)) {
  stop(
    paste0(
      "model_final_test.rds should remain the blinded feature artifact. ",
      "Use stage5a_final_test_scored.rds for revealed truth."
    )
  )
}

selected_model <- final_model_metadata$
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

if (
  nrow(
    scored_final_test
  ) !=
    nrow(
      model_final_test
    )
) {
  stop(
    "Scored test and frozen feature test row counts do not match."
  )
}

if (
  !identical(
    scored_final_test$
      shot_id,
    model_final_test$
      shot_id
  )
) {
  stop(
    paste0(
      "Scored test and frozen feature test are not in identical shot_id order. ",
      "Do not continue until alignment is restored."
    )
  )
}

if (!("selected_model_probability" %in% names(
  scored_final_test
))) {
  stop(
    "Scored final-test artifact lacks selected_model_probability."
  )
}

# ---- 4. Plot theme -----------------------------------------------------------

theme_nba_explorer <- function(
    base_size = 12) {

  ggplot2::theme_minimal(
    base_size =
      base_size
  ) +
    ggplot2::theme(
      plot.title.position =
        "plot",

      plot.title =
        element_text(
          face =
            "bold",
          size =
            base_size +
            4
        ),

      plot.subtitle =
        element_text(
          size =
            base_size
        ),

      plot.caption =
        element_text(
          size =
            base_size -
            2,
          hjust =
            0
        ),

      panel.grid.minor =
        element_blank(),

      legend.position =
        "bottom",

      axis.title =
        element_text(
          face =
            "bold"
        )
    )
}

# ---- 5. Metric helpers -------------------------------------------------------

clip_probability <- function(
    p,
    eps = 1e-15) {

  pmin(
    pmax(
      p,
      eps
    ),
    1 -
      eps
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

  intercept_fit <- suppressWarnings(
    stats::glm(
      truth ~ 1,
      offset =
        lp,
      family =
        stats::binomial()
    )
  )

  slope_fit <- suppressWarnings(
    stats::glm(
      truth ~ lp,
      family =
        stats::binomial()
    )
  )

  tibble(
    calibration_intercept =
      unname(
        stats::coef(
          intercept_fit
        )[[1]]
      ),

    calibration_slope =
      unname(
        stats::coef(
          slope_fit
        )[[2]]
      )
  )
}

# ---- 6. Selected-model 20-bin reliability table -----------------------------

selected_truth <- scored_final_test$
  outcome

selected_probability <- scored_final_test$
  selected_model_probability

selected_calibration_20 <- tibble(
  outcome =
    selected_truth,

  probability =
    selected_probability
) %>%
  mutate(
    calibration_bin =
      dplyr::ntile(
        probability,
        20L
      )
  ) %>%
  group_by(
    calibration_bin
  ) %>%
  summarise(
    shots =
      n(),

    min_probability =
      min(
        probability
      ),

    max_probability =
      max(
        probability
      ),

    mean_predicted_probability =
      mean(
        probability
      ),

    observed_make_rate =
      mean(
        outcome
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
  )

selected_calibration_metrics <- calibration_metrics(
  selected_truth,
  selected_probability
)

selected_calibration_summary <- tibble(
  model =
    selected_model,

  shots =
    length(
      selected_truth
    ),

  log_loss =
    log_loss(
      selected_truth,
      selected_probability
    ),

  brier =
    brier_score(
      selected_truth,
      selected_probability
    ),

  mean_predicted_probability =
    mean(
      selected_probability
    ),

  observed_make_rate =
    mean(
      selected_truth
    ),

  calibration_intercept =
    selected_calibration_metrics$
      calibration_intercept,

  calibration_slope =
    selected_calibration_metrics$
      calibration_slope,

  expected_calibration_error_20bin =
    weighted.mean(
      selected_calibration_20$
        absolute_gap,
      w =
        selected_calibration_20$
          shots
    ),

  maximum_calibration_error_20bin =
    max(
      selected_calibration_20$
        absolute_gap
    ),

  mean_signed_calibration_gap_20bin =
    weighted.mean(
      selected_calibration_20$
        calibration_gap,
      w =
        selected_calibration_20$
          shots
    )
)

readr::write_csv(
  selected_calibration_summary,
  "docs/interpretation/stage5b_selected_model_calibration_summary.csv"
)

readr::write_csv(
  selected_calibration_20,
  "docs/interpretation/stage5b_selected_model_calibration_20bin.csv"
)

cat(
  "\n--- STAGE 5B SELECTED-MODEL CALIBRATION SUMMARY ---\n"
)

print(
  selected_calibration_summary,
  n = Inf
)

cat(
  "\n--- STAGE 5B SELECTED-MODEL 20-BIN RELIABILITY TABLE ---\n"
)

print(
  selected_calibration_20,
  n = Inf
)

# ---- 7. Reliability plot -----------------------------------------------------

p_calibration <- ggplot(
  selected_calibration_20,
  aes(
    x =
      mean_predicted_probability,
    y =
      observed_make_rate,
    size =
      shots
  )
) +
  geom_abline(
    intercept =
      0,
    slope =
      1,
    linetype =
      "dashed",
    linewidth =
      0.7
  ) +
  geom_line(
    linewidth =
      0.8
  ) +
  geom_point(
    alpha =
      0.85
  ) +
  scale_x_continuous(
    labels =
      scales::percent_format(
        accuracy =
          1
      ),
    limits =
      c(
        0,
        1
      )
  ) +
  scale_y_continuous(
    labels =
      scales::percent_format(
        accuracy =
          1
      ),
    limits =
      c(
        0,
        1
      )
  ) +
  labs(
    title =
      "Frozen XGBoost Calibration on the 2025–26 Holdout",

    subtitle =
      "Twenty equal-frequency probability bins; dashed line represents perfect calibration",

    x =
      "Mean predicted make probability",

    y =
      "Observed make rate",

    size =
      "Shots",

    caption =
      "The holdout is evaluation-only; no calibration parameters are fit on 2025–26."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/interpretation/stage5b_calibration_reliability.png",
  p_calibration,
  width =
    8,
  height =
    7,
  dpi =
    200
)

# ---- 8. Prediction distribution by outcome ----------------------------------

prediction_distribution <- scored_final_test %>%
  transmute(
    outcome =
      factor(
        outcome,
        levels =
          c(
            0,
            1
          ),
        labels =
          c(
            "Miss",
            "Make"
          )
      ),

    selected_model_probability
  )

p_probability_distribution <- ggplot(
  prediction_distribution,
  aes(
    x =
      selected_model_probability,
    fill =
      outcome
  )
) +
  geom_density(
    alpha =
      0.35,
    linewidth =
      0.7
  ) +
  scale_x_continuous(
    labels =
      scales::percent_format(
        accuracy =
          1
      ),
    limits =
      c(
        0,
        1
      )
  ) +
  labs(
    title =
      "Predicted Make Probability Separates Makes and Misses",

    subtitle =
      "Frozen XGBoost predictions on the 2025–26 holdout",

    x =
      "Predicted make probability",

    y =
      "Density",

    fill =
      "Observed result",

    caption =
      "Probability distributions overlap substantially, as expected for a noisy shot-making process."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/interpretation/stage5b_probability_distribution_by_outcome.png",
  p_probability_distribution,
  width =
    9,
  height =
    6,
  dpi =
    200
)

# ---- 9. Join frozen features for segment diagnostics -------------------------

segment_features <- model_final_test %>%
  select(
    shot_id,
    shot_zone_geometry,
    shot_type_group,
    distance_band,
    period_group,
    is_home_shooter,
    is_three_geometry,
    is_heave,
    is_last_5_seconds_period,
    distance_to_rim_check_ft
  )

scored_with_features <- scored_final_test %>%
  left_join(
    segment_features,
    by =
      "shot_id"
  )

if (
  nrow(
    scored_with_features
  ) !=
    nrow(
      scored_final_test
    )
) {
  stop(
    "Segment-feature join changed final-test row count."
  )
}

# ---- 10. Generic segment diagnostic helper ----------------------------------

summarise_segment <- function(
    data,
    segment_column,
    segment_label) {

  data %>%
    mutate(
      segment_value =
        as.character(
          .data[[
            segment_column
          ]]
        )
    ) %>%
    group_by(
      segment_value
    ) %>%
    summarise(
      shots =
        n(),

      observed_make_rate =
        mean(
          outcome
        ),

      mean_predicted_probability =
        mean(
          selected_model_probability
        ),

      calibration_gap =
        observed_make_rate -
        mean_predicted_probability,

      absolute_calibration_gap =
        abs(
          calibration_gap
        ),

      log_loss =
        log_loss(
          outcome,
          selected_model_probability
        ),

      brier =
        brier_score(
          outcome,
          selected_model_probability
        ),

      .groups =
        "drop"
    ) %>%
    mutate(
      segment =
        segment_label,
      .before =
        1
    )
}

segment_diagnostics <- bind_rows(
  summarise_segment(
    scored_with_features,
    "shot_zone_geometry",
    "Shot Zone"
  ),

  summarise_segment(
    scored_with_features,
    "shot_type_group",
    "Shot Type Group"
  ),

  summarise_segment(
    scored_with_features,
    "distance_band",
    "Distance Band"
  ),

  summarise_segment(
    scored_with_features,
    "period_group",
    "Period"
  ),

  summarise_segment(
    scored_with_features,
    "is_home_shooter",
    "Home / Away"
  ),

  summarise_segment(
    scored_with_features,
    "is_three_geometry",
    "Two / Three"
  ),

  summarise_segment(
    scored_with_features,
    "is_heave",
    "Geometry Heave"
  ),

  summarise_segment(
    scored_with_features,
    "is_last_5_seconds_period",
    "Last 5 Seconds"
  )
)

readr::write_csv(
  segment_diagnostics,
  "docs/interpretation/stage5b_segment_diagnostics.csv"
)

cat(
  "\n--- STAGE 5B SEGMENT DIAGNOSTICS ---\n"
)

print(
  segment_diagnostics,
  n = Inf
)

# ---- 11. Zone calibration plot ----------------------------------------------

zone_diagnostics <- segment_diagnostics %>%
  filter(
    segment ==
      "Shot Zone"
  ) %>%
  arrange(
    mean_predicted_probability
  )

p_zone_calibration <- ggplot(
  zone_diagnostics,
  aes(
    x =
      mean_predicted_probability,
    y =
      observed_make_rate,
    label =
      segment_value,
    size =
      shots
  )
) +
  geom_abline(
    intercept =
      0,
    slope =
      1,
    linetype =
      "dashed",
    linewidth =
      0.7
  ) +
  geom_point(
    alpha =
      0.85
  ) +
  geom_text(
    nudge_y =
      0.025,
    check_overlap =
      TRUE,
    size =
      3.5
  ) +
  scale_x_continuous(
    labels =
      scales::percent_format(
        accuracy =
          1
      ),
    limits =
      c(
        0,
        1
      )
  ) +
  scale_y_continuous(
    labels =
      scales::percent_format(
        accuracy =
          1
      ),
    limits =
      c(
        0,
        1
      )
  ) +
  labs(
    title =
      "Frozen XGBoost Calibration by Shot Zone",

    subtitle =
      "2025–26 holdout; point size represents attempt volume",

    x =
      "Mean predicted make probability",

    y =
      "Observed make rate",

    size =
      "Shots",

    caption =
      "Zone-level differences are diagnostics only; the final model is not retuned from holdout results."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/interpretation/stage5b_calibration_by_shot_zone.png",
  p_zone_calibration,
  width =
    9,
  height =
    7,
  dpi =
    200
)

# ---- 12. Final XGBoost gain importance --------------------------------------

final_gain_importance <- xgboost::xgb.importance(
  model =
    final_xgboost_model
) %>%
  as_tibble()

if (
  nrow(
    final_gain_importance
  ) ==
    0L
) {
  stop(
    "Final XGBoost gain importance is empty."
  )
}

# Aggregate one-hot / logical columns to their original source feature.
map_xgb_source_feature <- function(
    feature_name) {

  dplyr::case_when(
    grepl(
      "^shot_type_group",
      feature_name
    ) ~
      "shot_type_group",

    grepl(
      "^is_bank_shot",
      feature_name
    ) ~
      "is_bank_shot",

    grepl(
      "^is_driving",
      feature_name
    ) ~
      "is_driving",

    grepl(
      "^is_running",
      feature_name
    ) ~
      "is_running",

    grepl(
      "^is_cutting",
      feature_name
    ) ~
      "is_cutting",

    grepl(
      "^is_alley_oop",
      feature_name
    ) ~
      "is_alley_oop",

    grepl(
      "^is_putback",
      feature_name
    ) ~
      "is_putback",

    grepl(
      "^is_turnaround",
      feature_name
    ) ~
      "is_turnaround",

    grepl(
      "^is_fadeaway",
      feature_name
    ) ~
      "is_fadeaway",

    grepl(
      "^is_three_geometry",
      feature_name
    ) ~
      "is_three_geometry",

    grepl(
      "^is_corner_three_geometry",
      feature_name
    ) ~
      "is_corner_three_geometry",

    grepl(
      "^is_heave",
      feature_name
    ) ~
      "is_heave",

    grepl(
      "^is_overtime",
      feature_name
    ) ~
      "is_overtime",

    grepl(
      "^is_last_2_minutes_period",
      feature_name
    ) ~
      "is_last_2_minutes_period",

    grepl(
      "^is_last_24_seconds_period",
      feature_name
    ) ~
      "is_last_24_seconds_period",

    grepl(
      "^is_last_5_seconds_period",
      feature_name
    ) ~
      "is_last_5_seconds_period",

    grepl(
      "^is_home_shooter",
      feature_name
    ) ~
      "is_home_shooter",

    TRUE ~
      feature_name
  )
}

final_gain_importance <- final_gain_importance %>%
  mutate(
    source_feature =
      map_xgb_source_feature(
        Feature
      )
  )

gain_importance_by_source <- final_gain_importance %>%
  group_by(
    source_feature
  ) %>%
  summarise(
    gain =
      sum(
        Gain
      ),

    cover =
      sum(
        Cover
      ),

    frequency =
      sum(
        Frequency
      ),

    .groups =
      "drop"
  ) %>%
  arrange(
    desc(
      gain
    )
  ) %>%
  mutate(
    gain_share =
      gain /
      sum(
        gain
      )
  )

readr::write_csv(
  final_gain_importance,
  "docs/interpretation/stage5b_xgboost_gain_importance_raw.csv"
)

readr::write_csv(
  gain_importance_by_source,
  "docs/interpretation/stage5b_xgboost_gain_importance_by_source.csv"
)

cat(
  "\n--- FINAL XGBOOST GAIN IMPORTANCE BY SOURCE FEATURE ---\n"
)

print(
  gain_importance_by_source,
  n = Inf
)

# ---- 13. TreeSHAP global importance -----------------------------------------
#
# Use a deterministic evenly-spaced 25,000-row sample from the final-test
# feature population. Outcome is not used to calculate SHAP values.
#
# XGBoost predcontrib returns per-feature contributions on the margin/log-odds
# scale plus one final baseline/BIAS column.

xgb_feature_formula <- final_model_metadata$
  xgb_feature_formula

expected_xgb_feature_names <- final_model_metadata$
  xgb_feature_names

if (
  is.null(
    xgb_feature_formula
  ) ||
  is.null(
    expected_xgb_feature_names
  )
) {
  stop(
    "Final XGBoost feature metadata are unavailable."
  )
}

shap_sample_n <- min(
  25000L,
  nrow(
    model_final_test
  )
)

shap_sample_index <- unique(
  round(
    seq(
      from =
        1,
      to =
        nrow(
          model_final_test
        ),
      length.out =
        shap_sample_n
    )
  )
)

shap_sample <- model_final_test[
  shap_sample_index,
  ,
  drop =
    FALSE
]

x_shap <- Matrix::sparse.model.matrix(
  object =
    xgb_feature_formula,
  data =
    shap_sample
)

if (!identical(
  colnames(
    x_shap
  ),
  expected_xgb_feature_names
)) {
  stop(
    paste0(
      "TreeSHAP model matrix columns do not match the frozen final-model ",
      "feature names."
    )
  )
}

d_shap <- xgboost::xgb.DMatrix(
  data =
    x_shap
)

shap_matrix <- predict(
  final_xgboost_model,
  d_shap,
  predcontrib =
    TRUE
)

if (!is.matrix(
  shap_matrix
)) {
  shap_matrix <- as.matrix(
    shap_matrix
  )
}

expected_shap_columns <-
  ncol(
    x_shap
  ) +
  1L

if (
  ncol(
    shap_matrix
  ) !=
    expected_shap_columns
) {
  stop(
    paste0(
      "Unexpected TreeSHAP output width. Expected ",
      expected_shap_columns,
      " columns; received ",
      ncol(
        shap_matrix
      ),
      "."
    )
  )
}

shap_feature_names <- colnames(
  shap_matrix
)

if (
  is.null(
    shap_feature_names
  ) ||
  length(
    shap_feature_names
  ) !=
    ncol(
      shap_matrix
    )
) {
  shap_feature_names <- c(
    colnames(
      x_shap
    ),
    "BIAS"
  )
}

shap_summary_raw <- tibble(
  Feature =
    shap_feature_names,

  mean_abs_shap =
    colMeans(
      abs(
        shap_matrix
      )
    ),

  mean_shap =
    colMeans(
      shap_matrix
    )
) %>%
  # XGBoost versions can label the baseline SHAP contribution either
  # "BIAS" or "(Intercept)". It is the expected-value offset, not a
  # predictive basketball feature, so exclude it from global importance.
  filter(
    !Feature %in%
      c(
        "BIAS",
        "(Intercept)"
      )
  ) %>%
  mutate(
    source_feature =
      map_xgb_source_feature(
        Feature
      )
  ) %>%
  arrange(
    desc(
      mean_abs_shap
    )
  )

shap_importance_by_source <- shap_summary_raw %>%
  group_by(
    source_feature
  ) %>%
  summarise(
    mean_abs_shap =
      sum(
        mean_abs_shap
      ),

    mean_shap =
      sum(
        mean_shap
      ),

    .groups =
      "drop"
  ) %>%
  arrange(
    desc(
      mean_abs_shap
    )
  ) %>%
  mutate(
    shap_share =
      mean_abs_shap /
      sum(
        mean_abs_shap
      )
  )

readr::write_csv(
  shap_summary_raw,
  "docs/interpretation/stage5b_tree_shap_raw_features.csv"
)

readr::write_csv(
  shap_importance_by_source,
  "docs/interpretation/stage5b_tree_shap_by_source_feature.csv"
)

cat(
  "\n--- TREE SHAP GLOBAL IMPORTANCE BY SOURCE FEATURE ---\n"
)

print(
  shap_importance_by_source,
  n = Inf
)

# ---- 14. Feature-importance comparison --------------------------------------

importance_comparison <- full_join(
  gain_importance_by_source %>%
    select(
      source_feature,
      gain_share
    ),

  shap_importance_by_source %>%
    select(
      source_feature,
      shap_share,
      mean_abs_shap
    ),

  by =
    "source_feature"
) %>%
  mutate(
    gain_share =
      tidyr::replace_na(
        gain_share,
        0
      ),

    shap_share =
      tidyr::replace_na(
        shap_share,
        0
      ),

    mean_abs_shap =
      tidyr::replace_na(
        mean_abs_shap,
        0
      )
  ) %>%
  arrange(
    desc(
      shap_share
    )
  )

readr::write_csv(
  importance_comparison,
  "docs/interpretation/stage5b_feature_importance_comparison.csv"
)

# ---- 15. SHAP importance plot -----------------------------------------------

top_shap <- shap_importance_by_source %>%
  slice_max(
    order_by =
      mean_abs_shap,
    n =
      15L,
    with_ties =
      FALSE
  ) %>%
  mutate(
    source_feature =
      reorder(
        source_feature,
        mean_abs_shap
      )
  )

p_shap <- ggplot(
  top_shap,
  aes(
    x =
      source_feature,
    y =
      mean_abs_shap
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title =
      "Global XGBoost Importance from TreeSHAP",

    subtitle =
      "Mean absolute contribution on the log-odds scale; deterministic 25k holdout sample",

    x =
      NULL,

    y =
      "Mean absolute SHAP contribution",

    caption =
      "SHAP explains the frozen model's predictions; it does not imply causal effects."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/interpretation/stage5b_tree_shap_global_importance.png",
  p_shap,
  width =
    9,
  height =
    7,
  dpi =
    200
)

# ---- 16. Gain importance plot ------------------------------------------------

top_gain <- gain_importance_by_source %>%
  slice_max(
    order_by =
      gain,
    n =
      15L,
    with_ties =
      FALSE
  ) %>%
  mutate(
    source_feature =
      reorder(
        source_feature,
        gain
      )
  )

p_gain <- ggplot(
  top_gain,
  aes(
    x =
      source_feature,
    y =
      gain
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title =
      "Final XGBoost Split-Gain Importance",

    subtitle =
      "Aggregated from encoded model-matrix columns to source features",

    x =
      NULL,

    y =
      "Gain importance",

    caption =
      "Gain reflects training split improvement; TreeSHAP provides a complementary prediction-level view."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/interpretation/stage5b_xgboost_gain_importance.png",
  p_gain,
  width =
    9,
  height =
    7,
  dpi =
    200
)

# ---- 17. Interpretation audit ------------------------------------------------
#
# We explicitly document that Stage 5B is diagnostic only.

interpretation_methodology <- tibble(
  item = c(
    "Selected model",
    "Holdout status",
    "Calibration treatment",
    "TreeSHAP sample",
    "TreeSHAP scale",
    "Model retuned in Stage 5B"
  ),

  value = c(
    selected_model,
    "2025-26 revealed once in Stage 5A; evaluation-only",
    "Diagnostics only; no calibrator fit on holdout",
    paste0(
      length(
        shap_sample_index
      ),
      " deterministic evenly spaced final-test rows"
    ),
    "Log-odds contribution",
    "FALSE"
  )
)

readr::write_csv(
  interpretation_methodology,
  "docs/interpretation/stage5b_methodology_audit.csv"
)

# ---- 18. Output inventory ----------------------------------------------------

expected_csvs <- c(
  "docs/interpretation/stage5b_selected_model_calibration_summary.csv",
  "docs/interpretation/stage5b_selected_model_calibration_20bin.csv",
  "docs/interpretation/stage5b_segment_diagnostics.csv",
  "docs/interpretation/stage5b_xgboost_gain_importance_raw.csv",
  "docs/interpretation/stage5b_xgboost_gain_importance_by_source.csv",
  "docs/interpretation/stage5b_tree_shap_raw_features.csv",
  "docs/interpretation/stage5b_tree_shap_by_source_feature.csv",
  "docs/interpretation/stage5b_feature_importance_comparison.csv",
  "docs/interpretation/stage5b_methodology_audit.csv"
)

expected_pngs <- c(
  "docs/interpretation/stage5b_calibration_reliability.png",
  "docs/interpretation/stage5b_probability_distribution_by_outcome.png",
  "docs/interpretation/stage5b_calibration_by_shot_zone.png",
  "docs/interpretation/stage5b_tree_shap_global_importance.png",
  "docs/interpretation/stage5b_xgboost_gain_importance.png"
)

# ---- 19. Verification --------------------------------------------------------

calibration_summary_matches_stage5a <-
  abs(
    selected_calibration_summary$
      log_loss[[1]] -
    stage5a_selected_eval$
      test_log_loss[[1]]
  ) <
  1e-12 &&
  abs(
    selected_calibration_summary$
      brier[[1]] -
    stage5a_selected_eval$
      test_brier[[1]]
  ) <
  1e-12

calibration_intercept_reasonable <- is.finite(
  selected_calibration_summary$
    calibration_intercept[[1]]
)

calibration_slope_reasonable <- is.finite(
  selected_calibration_summary$
    calibration_slope[[1]]
)

shap_nonempty <-
  nrow(
    shap_importance_by_source
  ) >
  0L

gain_nonempty <-
  nrow(
    gain_importance_by_source
  ) >
  0L

segment_nonempty <-
  nrow(
    segment_diagnostics
  ) >
  0L

all_csvs_exist <- all(
  file.exists(
    expected_csvs
  )
)

all_pngs_exist <- all(
  file.exists(
    expected_pngs
  )
)

no_model_changes <- identical(
  interpretation_methodology$
    value[
      interpretation_methodology$
        item ==
        "Model retuned in Stage 5B"
    ],
  "FALSE"
)

stage5b_checks <- tibble(
  check = c(
    "Stage 5B selected-model log-loss/Brier exactly match Stage 5A",
    "Calibration intercept is finite",
    "Calibration slope is finite",
    "20-bin reliability table was created",
    "Segment diagnostics were created",
    "Final-model gain importance was created",
    "TreeSHAP global importance was created",
    "TreeSHAP baseline/intercept contribution is excluded from feature importance",
    "TreeSHAP used frozen final-model feature columns exactly",
    "Stage 5B methodology confirms no model retuning",
    "All expected interpretation CSV files exist",
    "All expected interpretation PNG files exist"
  ),

  passed = c(
    calibration_summary_matches_stage5a,
    calibration_intercept_reasonable,
    calibration_slope_reasonable,
    nrow(
      selected_calibration_20
    ) ==
      20L,
    segment_nonempty,
    gain_nonempty,
    shap_nonempty,
    !any(
      shap_importance_by_source$
        source_feature %in%
        c(
          "BIAS",
          "(Intercept)"
        )
    ),
    identical(
      colnames(
        x_shap
      ),
      expected_xgb_feature_names
    ),
    no_model_changes,
    all_csvs_exist,
    all_pngs_exist
  )
)

cat(
  "\n--- STAGE 5B CALIBRATION / INTERPRETATION VERIFICATION ---\n"
)

print(
  stage5b_checks,
  n = Inf
)

if (!all(
  stage5b_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 5B checks failed. ",
      "Review calibration/interpretation outputs without altering the frozen model."
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
    "\nUpdated renv.lock after Stage 5B diagnostics.\n"
  )
}

cat(
  "\nSTAGE 5B COMPLETE.\n",
  "The frozen XGBoost model has now been evaluated for calibration, subgroup ",
  "generalization, gain importance, and TreeSHAP importance without any ",
  "retuning. Next: review these diagnostics and then prepare Stage 5C's ",
  "final model-insight artifacts for the Shiny application.\n",
  sep = ""
)
