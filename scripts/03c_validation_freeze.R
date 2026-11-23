# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 3C — Validation & Feature Freeze
# File: scripts/03c_validation_freeze.R
#
# Purpose:
#   1. Freeze the modeling population after Stage 3A/3B review.
#   2. Exclude ambiguous "No Shot (Default Shot)" records from modeling.
#   3. Freeze approved predictor roles and categorical levels.
#   4. Create chronological development folds.
#   5. Save model-ready development/final-test datasets.
#
# Locked validation design:
#   Fold 1:
#     Train      = 2022-23 (hoopR 2023)
#     Validation = 2023-24 (hoopR 2024)
#
#   Fold 2:
#     Train      = 2022-23 + 2023-24 (hoopR 2023-2024)
#     Validation = 2024-25 (hoopR 2025)
#
#   Final refit:
#     Train      = all development seasons (2022-23 through 2024-25)
#     Test       = untouched 2025-26 feature dataset
#
# IMPORTANT:
#   - No 2025-26 outcome is accessed or reconstructed.
#   - Raw type_id/type_text are retained only in audit/source data, not as core
#     predictors, because 2025-26 introduced a new "Heave Jump Shot" label.
#   - Geometry-derived is_heave is used instead of trusting source taxonomy.
#   - Player/team identities remain exploration/filter variables, not core
#     predictive features.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "purrr",
  "readr",
  "tibble"
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
})

# ---- 2. Input checks ---------------------------------------------------------

development_path <-
  "data/processed/shots_development_features.rds"

final_test_path <-
  "data/processed/shots_final_test_features.rds"

if (!file.exists(development_path)) {
  stop(
    "Missing shots_development_features.rds. ",
    "Run scripts/03a_feature_engineering.R first."
  )
}

if (!file.exists(final_test_path)) {
  stop(
    "Missing shots_final_test_features.rds. ",
    "Run scripts/03a_feature_engineering.R first."
  )
}

development <- readRDS(
  development_path
)

final_test <- readRDS(
  final_test_path
)

if (!("outcome" %in% names(development))) {
  stop(
    "Development feature data do not contain outcome."
  )
}

if ("outcome" %in% names(final_test)) {
  stop(
    "Final-test feature data unexpectedly contain outcome."
  )
}

if (!dir.exists("docs/validation")) {
  dir.create(
    "docs/validation",
    recursive = TRUE
  )
}

# ---- 3. Freeze modeling population ------------------------------------------

is_default_no_shot <- function(type_id, type_text) {
  type_id == 91L |
    type_text ==
      "No Shot (Default Shot)"
}

population_exclusion_audit <- bind_rows(
  development %>%
    filter(
      is_default_no_shot(
        type_id,
        type_text
      )
    ) %>%
    transmute(
      dataset =
        "development",
      shot_id,
      hoopr_season,
      nba_season,
      game_id,
      game_date,
      type_id,
      type_text,
      outcome
    ),

  final_test %>%
    filter(
      is_default_no_shot(
        type_id,
        type_text
      )
    ) %>%
    transmute(
      dataset =
        "final_test",
      shot_id,
      hoopr_season,
      nba_season,
      game_id,
      game_date,
      type_id,
      type_text,
      outcome =
        NA_integer_
    )
)

readr::write_csv(
  population_exclusion_audit,
  "docs/validation/stage3c_population_exclusions.csv"
)

development_model_population <- development %>%
  filter(
    !is_default_no_shot(
      type_id,
      type_text
    )
  )

final_test_model_population <- final_test %>%
  filter(
    !is_default_no_shot(
      type_id,
      type_text
    )
  )

population_summary <- tibble(
  dataset = c(
    "development_before_freeze",
    "development_after_freeze",
    "final_test_before_freeze",
    "final_test_after_freeze"
  ),
  rows = c(
    nrow(
      development
    ),
    nrow(
      development_model_population
    ),
    nrow(
      final_test
    ),
    nrow(
      final_test_model_population
    )
  )
)

readr::write_csv(
  population_summary,
  "docs/validation/stage3c_population_summary.csv"
)

cat(
  "\n--- MODELING POPULATION FREEZE ---\n"
)

print(
  population_summary,
  n = Inf
)

cat(
  "\n--- EXCLUDED DEFAULT / NO-SHOT ROWS ---\n"
)

print(
  population_exclusion_audit,
  n = Inf
)

# ---- 4. Explicit feature-role freeze ----------------------------------------

feature_manifest <- tibble(
  feature = c(
    "outcome",

    "shot_id",
    "game_id",
    "game_date",
    "hoopr_season",
    "nba_season",

    "court_x_folded",
    "court_y_folded",
    "distance_to_rim_check_ft",
    "shot_angle_rad",
    "absolute_angle_deg",

    "is_three_geometry",
    "is_corner_three_geometry",
    "three_region",
    "is_heave",
    "in_restricted_area_geometry",
    "in_paint_geometry",
    "shot_zone_geometry",
    "distance_band",

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
    "period_group",
    "game_phase",
    "is_last_2_minutes_period",
    "is_last_24_seconds_period",
    "is_last_5_seconds_period",
    "is_home_shooter",

    "team_id",
    "team_abbrev",
    "team_abbrev_canonical",
    "shooter_id",
    "shooter_name",
    "venue_side",

    "type_id",
    "type_text",

    "coordinate_x",
    "coordinate_y",
    "coordinate_x_raw",
    "coordinate_y_raw",
    "distance_to_rim_ft",

    "score_value",
    "source_score_value",
    "scoring_play",
    "source_scoring_play"
  ),

  role = c(
    "target_only",

    "identifier_only",
    "identifier_only",
    "identifier_only",
    "split_only",
    "split_only",

    rep(
      "candidate_predictive",
      5
    ),

    rep(
      "candidate_predictive",
      8
    ),

    rep(
      "candidate_predictive",
      9
    ),

    rep(
      "candidate_predictive",
      9
    ),

    rep(
      "exploration_only",
      6
    ),

    rep(
      "source_label_only",
      2
    ),

    rep(
      "provenance_only",
      5
    ),

    rep(
      "prohibited_leakage",
      4
    )
  ),

  rationale = c(
    "Binary make/miss response; never a predictor.",

    "Stable row identifier.",
    "Game identifier for audit/grouping.",
    "Game date for chronology and audit.",
    "Locked chronological split variable.",
    "Human-readable chronological split variable.",

    "Folded half-court x coordinate for spatial models.",
    "Folded half-court y coordinate for spatial models.",
    "Primary continuous shot-distance feature.",
    "Signed shot angle for spatial/nonlinear models.",
    "Absolute shot angle for interpretable models.",

    "2PT/3PT attempt value derived from court geometry.",
    "Corner-three geometry indicator.",
    "Interpretable two/corner-three/above-break grouping.",
    "Geometry-based long-heave indicator; preferred over source label.",
    "Restricted-area geometry indicator.",
    "Paint geometry indicator.",
    "Interpretable spatial zone.",
    "Interpretable distance grouping.",

    "Stable grouped pre-shot action family.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",
    "Pre-shot action descriptor.",

    "Game period known before shot.",
    "Game clock known before shot.",
    "Overtime context.",
    "Interpretable quarter/overtime grouping.",
    "First-half/second-half/overtime grouping.",
    "End-period context.",
    "End-period context.",
    "End-period context.",
    "Home/away context.",

    "High-cardinality identity; retain for exploration only.",
    "Source team label; retain for app filtering.",
    "Canonical team label; retain for app filtering.",
    "High-cardinality identity; not in core model.",
    "Display/filter field only.",
    "Display-friendly home/away label; binary predictor uses is_home_shooter.",

    "Raw ESPN taxonomy ID can change across seasons.",
    "Raw ESPN taxonomy changed in 2025-26; grouped feature is preferred.",

    "Original full-court coordinate retained for traceability.",
    "Original full-court coordinate retained for traceability.",
    "Raw provider coordinate retained for traceability.",
    "Raw provider coordinate retained for traceability.",
    "Stage 2 distance retained only as reconstruction reference.",

    "Outcome leakage in ESPN shot release.",
    "Outcome leakage in ESPN shot release.",
    "Outcome indicator; prohibited as predictor.",
    "Outcome indicator; prohibited as predictor."
  )
)

readr::write_csv(
  feature_manifest,
  "docs/validation/stage3c_feature_manifest.csv"
)

cat(
  "\n--- FROZEN FEATURE MANIFEST ---\n"
)

print(
  feature_manifest,
  n = Inf
)

approved_predictors <- feature_manifest %>%
  filter(
    role ==
      "candidate_predictive"
  ) %>%
  pull(
    feature
  )

# ---- 5. Freeze categorical levels -------------------------------------------

categorical_predictors <- c(
  "three_region",
  "shot_zone_geometry",
  "distance_band",
  "shot_type_group",
  "period_group",
  "game_phase"
)

missing_categorical <- setdiff(
  categorical_predictors,
  names(
    development_model_population
  )
)

if (length(missing_categorical) > 0) {
  stop(
    paste0(
      "Missing categorical feature(s): ",
      paste(
        missing_categorical,
        collapse = ", "
      )
    )
  )
}

categorical_level_manifest <- map_dfr(
  categorical_predictors,
  function(feature_name) {

    levels_dev <- unique(
      as.character(
        development_model_population[[
          feature_name
        ]]
      )
    )

    levels_dev <- levels_dev[
      !is.na(
        levels_dev
      )
    ]

    tibble(
      feature =
        feature_name,
      level =
        levels_dev
    )
  }
)

readr::write_csv(
  categorical_level_manifest,
  "docs/validation/stage3c_categorical_levels.csv"
)

unseen_final_levels <- map_dfr(
  categorical_predictors,
  function(feature_name) {

    dev_levels <- categorical_level_manifest %>%
      filter(
        feature ==
          feature_name
      ) %>%
      pull(
        level
      )

    test_values <- unique(
      as.character(
        final_test_model_population[[
          feature_name
        ]]
      )
    )

    unseen <- setdiff(
      test_values[
        !is.na(
          test_values
        )
      ],
      dev_levels
    )

    tibble(
      feature =
        feature_name,
      unseen_level =
        unseen
    )
  }
)

readr::write_csv(
  unseen_final_levels,
  "docs/validation/stage3c_unseen_final_levels.csv"
)

cat(
  "\n--- UNSEEN FINAL-TEST CATEGORICAL LEVELS ---\n"
)

print(
  unseen_final_levels,
  n = Inf
)

apply_frozen_levels <- function(df) {

  out <- df

  for (
    feature_name in
    categorical_predictors
  ) {

    feature_levels <-
      categorical_level_manifest %>%
      filter(
        feature ==
          feature_name
      ) %>%
      pull(
        level
      )

    out[[
      feature_name
    ]] <- factor(
      as.character(
        out[[
          feature_name
        ]]
      ),
      levels =
        feature_levels
    )
  }

  out
}

development_frozen <- apply_frozen_levels(
  development_model_population
)

final_test_frozen <- apply_frozen_levels(
  final_test_model_population
)

# ---- 6. Create model-ready datasets -----------------------------------------

development_model_columns <- c(
  "shot_id",
  "game_id",
  "game_date",
  "hoopr_season",
  "nba_season",
  approved_predictors,
  "outcome"
)

final_test_model_columns <- c(
  "shot_id",
  "game_id",
  "game_date",
  "hoopr_season",
  "nba_season",
  approved_predictors
)

missing_dev_model_columns <- setdiff(
  development_model_columns,
  names(
    development_frozen
  )
)

missing_test_model_columns <- setdiff(
  final_test_model_columns,
  names(
    final_test_frozen
  )
)

if (length(missing_dev_model_columns) > 0) {
  stop(
    paste0(
      "Development model-ready columns are missing: ",
      paste(
        missing_dev_model_columns,
        collapse = ", "
      )
    )
  )
}

if (length(missing_test_model_columns) > 0) {
  stop(
    paste0(
      "Final-test model-ready columns are missing: ",
      paste(
        missing_test_model_columns,
        collapse = ", "
      )
    )
  )
}

model_development <- development_frozen %>%
  select(
    all_of(
      development_model_columns
    )
  )

model_final_test <- final_test_frozen %>%
  select(
    all_of(
      final_test_model_columns
    )
  )

# ---- 7. Freeze chronological validation assignments -------------------------

split_assignments <- model_development %>%
  transmute(
    shot_id,
    hoopr_season,
    nba_season,

    fold1_role =
      case_when(
        hoopr_season ==
          2023L ~
          "train",
        hoopr_season ==
          2024L ~
          "validation",
        hoopr_season ==
          2025L ~
          "not_used",
        TRUE ~
          NA_character_
      ),

    fold2_role =
      case_when(
        hoopr_season %in%
          c(
            2023L,
            2024L
          ) ~
          "train",
        hoopr_season ==
          2025L ~
          "validation",
        TRUE ~
          NA_character_
      ),

    final_refit_role =
      "train"
  )

split_summary <- bind_rows(
  split_assignments %>%
    count(
      fold1_role,
      name = "rows"
    ) %>%
    transmute(
      split =
        "fold1",
      role =
        fold1_role,
      rows
    ),

  split_assignments %>%
    count(
      fold2_role,
      name = "rows"
    ) %>%
    transmute(
      split =
        "fold2",
      role =
        fold2_role,
      rows
    ),

  tibble(
    split =
      "final_refit",
    role =
      "train",
    rows =
      nrow(
        model_development
      )
  ),

  tibble(
    split =
      "final_test",
    role =
      "test",
    rows =
      nrow(
        model_final_test
      )
  )
)

readr::write_csv(
  split_summary,
  "docs/validation/stage3c_split_summary.csv"
)

cat(
  "\n--- CHRONOLOGICAL VALIDATION SPLIT SUMMARY ---\n"
)

print(
  split_summary,
  n = Inf
)

season_split_manifest <- tibble(
  hoopr_season = c(
    2023L,
    2024L,
    2025L,
    2026L
  ),
  nba_season = c(
    "2022-23",
    "2023-24",
    "2024-25",
    "2025-26"
  ),
  fold1 = c(
    "train",
    "validation",
    "not_used",
    "untouched"
  ),
  fold2 = c(
    "train",
    "train",
    "validation",
    "untouched"
  ),
  final = c(
    "train",
    "train",
    "train",
    "test"
  )
)

readr::write_csv(
  season_split_manifest,
  "docs/validation/stage3c_season_split_manifest.csv"
)

# ---- 8. Chronology audits ----------------------------------------------------

fold1_train_dates <- development_frozen %>%
  filter(
    hoopr_season ==
      2023L
  ) %>%
  summarise(
    min_date =
      min(
        game_date
      ),
    max_date =
      max(
        game_date
      )
  )

fold1_validation_dates <- development_frozen %>%
  filter(
    hoopr_season ==
      2024L
  ) %>%
  summarise(
    min_date =
      min(
        game_date
      ),
    max_date =
      max(
        game_date
      )
  )

fold2_train_dates <- development_frozen %>%
  filter(
    hoopr_season %in%
      c(
        2023L,
        2024L
      )
  ) %>%
  summarise(
    min_date =
      min(
        game_date
      ),
    max_date =
      max(
        game_date
      )
  )

fold2_validation_dates <- development_frozen %>%
  filter(
    hoopr_season ==
      2025L
  ) %>%
  summarise(
    min_date =
      min(
        game_date
      ),
    max_date =
      max(
        game_date
      )
  )

chronology_audit <- tibble(
  fold = c(
    "fold1",
    "fold2"
  ),
  train_max_date = c(
    fold1_train_dates$
      max_date,
    fold2_train_dates$
      max_date
  ),
  validation_min_date = c(
    fold1_validation_dates$
      min_date,
    fold2_validation_dates$
      min_date
  )
) %>%
  mutate(
    chronological =
      train_max_date <
      validation_min_date
  )

readr::write_csv(
  chronology_audit,
  "docs/validation/stage3c_chronology_audit.csv"
)

cat(
  "\n--- CHRONOLOGY AUDIT ---\n"
)

print(
  chronology_audit,
  n = Inf
)

# ---- 9. Outcome freeze -------------------------------------------------------

development_outcome_freeze <- model_development %>%
  group_by(
    hoopr_season,
    nba_season
  ) %>%
  summarise(
    shots =
      n(),
    makes =
      sum(
        outcome ==
          1L
      ),
    make_rate =
      mean(
        outcome
      ),
    .groups =
      "drop"
  )

readr::write_csv(
  development_outcome_freeze,
  "docs/validation/stage3c_development_outcome_freeze.csv"
)

cat(
  "\n--- FROZEN DEVELOPMENT OUTCOME SUMMARY ---\n"
)

print(
  development_outcome_freeze,
  n = Inf
)

cat(
  "\n2025-26 outcome remains unavailable and unreported.\n"
)

# ---- 10. Save frozen modeling artifacts -------------------------------------

saveRDS(
  model_development,
  "data/processed/model_development.rds",
  compress =
    "xz"
)

saveRDS(
  model_final_test,
  "data/processed/model_final_test.rds",
  compress =
    "xz"
)

saveRDS(
  split_assignments,
  "data/processed/model_split_assignments.rds",
  compress =
    "xz"
)

saveRDS(
  list(
    approved_predictors =
      approved_predictors,
    categorical_predictors =
      categorical_predictors,
    categorical_levels =
      categorical_level_manifest,
    feature_manifest =
      feature_manifest,
    season_split_manifest =
      season_split_manifest
  ),
  "data/processed/modeling_freeze_metadata.rds",
  compress =
    "xz"
)

# ---- 11. Final Stage 3C verification ----------------------------------------

prohibited_columns <- feature_manifest %>%
  filter(
    role ==
      "prohibited_leakage"
  ) %>%
  pull(
    feature
  )

identity_columns <- c(
  "team_id",
  "team_abbrev",
  "team_abbrev_canonical",
  "shooter_id",
  "shooter_name",
  "type_id",
  "type_text"
)

dev_default_removed <- !any(
  development_frozen$type_id ==
    91L,
  na.rm =
    TRUE
)

test_default_removed <- !any(
  final_test_frozen$type_id ==
    91L,
  na.rm =
    TRUE
)

factor_levels_match <- all(
  map_lgl(
    categorical_predictors,
    function(feature_name) {
      identical(
        levels(
          model_development[[
            feature_name
          ]]
        ),
        levels(
          model_final_test[[
            feature_name
          ]]
        )
      )
    }
  )
)

no_unseen_final_levels <-
  nrow(
    unseen_final_levels
  ) ==
  0L

no_leakage_dev <- !any(
  prohibited_columns %in%
    names(
      model_development
    )
)

no_leakage_test <- !any(
  prohibited_columns %in%
    names(
      model_final_test
    )
)

no_identity_dev <- !any(
  identity_columns %in%
    names(
      model_development
    )
)

no_identity_test <- !any(
  identity_columns %in%
    names(
      model_final_test
    )
)

test_outcome_absent <- !(
  "outcome" %in%
    names(
      model_final_test
    )
)

dev_outcome_binary <- setequal(
  unique(
    model_development$
      outcome
  ),
  c(
    0L,
    1L
  )
)

splits_complete <- all(
  !is.na(
    split_assignments$
      fold1_role
  )
) &&
  all(
    !is.na(
      split_assignments$
        fold2_role
    )
  ) &&
  all(
    !is.na(
      split_assignments$
        final_refit_role
    )
  )

chronology_valid <- all(
  chronology_audit$
    chronological
)

stage3c_checks <- tibble(
  check = c(
    "Exactly 14 development default/no-shot rows were excluded",
    "No default/no-shot rows remain in frozen development population",
    "No default/no-shot rows remain in frozen final-test population",
    "Approved predictor list is non-empty",
    "No prohibited leakage columns exist in model development data",
    "No prohibited leakage columns exist in model final-test data",
    "Player/team/raw-taxonomy identity fields are absent from model-ready data",
    "Final-test outcome remains absent",
    "Development outcome remains binary 0/1",
    "No unseen final-test categorical levels remain",
    "Frozen categorical factor levels match development and final test",
    "Chronological fold assignments are complete",
    "Both validation folds are strictly chronological",
    "Model development RDS exists",
    "Model final-test RDS exists",
    "Split assignments RDS exists",
    "Modeling freeze metadata RDS exists"
  ),
  passed = c(
    sum(
      population_exclusion_audit$
        dataset ==
        "development"
    ) ==
      14L,
    dev_default_removed,
    test_default_removed,
    length(
      approved_predictors
    ) >
      0,
    no_leakage_dev,
    no_leakage_test,
    no_identity_dev &&
      no_identity_test,
    test_outcome_absent,
    dev_outcome_binary,
    no_unseen_final_levels,
    factor_levels_match,
    splits_complete,
    chronology_valid,
    file.exists(
      "data/processed/model_development.rds"
    ),
    file.exists(
      "data/processed/model_final_test.rds"
    ),
    file.exists(
      "data/processed/model_split_assignments.rds"
    ),
    file.exists(
      "data/processed/modeling_freeze_metadata.rds"
    )
  )
)

cat(
  "\n--- STAGE 3C VALIDATION / FEATURE-FREEZE VERIFICATION ---\n"
)

print(
  stage3c_checks,
  n = Inf
)

if (!all(
  stage3c_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 3C freeze checks failed. ",
      "Review the audits before beginning Stage 4 modeling."
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
    "\nUpdated renv.lock after Stage 3C freeze.\n"
  )
}

cat(
  "\nSTAGE 3C COMPLETE.\n",
  "Stage 3 is now frozen. Do not change the modeling population, feature ",
  "roles, categorical levels, or validation folds during Stage 4 unless a ",
  "documented modeling defect requires reopening Stage 3.\n",
  sep = ""
)
