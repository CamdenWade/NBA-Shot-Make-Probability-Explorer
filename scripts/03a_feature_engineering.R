# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 3A — Feature Engineering
# File: scripts/03a_feature_engineering.R
#
# Purpose:
#   1. Inventory the COMPLETE public ESPN shot-type taxonomy in the three
#      development seasons before grouping categories.
#   2. Create basketball-specific, leakage-safe spatial, shot-type, time, and
#      contextual features.
#   3. Apply the exact same feature logic to the untouched 2025-26 test set.
#   4. Audit geometry, shot-type coverage, missingness, and train/test schema.
#
# IMPORTANT MODELING RULES:
#   - 2025-26 outcome remains unavailable and is never reconstructed.
#   - score_value/scoring_play are NOT used as predictors.
#   - score_value from RAW DEVELOPMENT DATA is used only in one validation
#     audit on MADE shots to check geometry-based 2PT/3PT classification.
#   - Player/team identifiers remain available for exploration, but are NOT
#     automatically approved as predictive features.
#   - No model is trained in this script.
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

# ---- 2. Input / project checks ----------------------------------------------

development_path <- "data/processed/shots_development.rds"
final_test_path <- "data/processed/shots_final_test.rds"

if (!file.exists(development_path)) {
  stop(
    "Missing data/processed/shots_development.rds. ",
    "Complete Stage 2 successfully before Stage 3A."
  )
}

if (!file.exists(final_test_path)) {
  stop(
    "Missing data/processed/shots_final_test.rds. ",
    "Complete Stage 2 successfully before Stage 3A."
  )
}

if (!dir.exists("docs/validation")) {
  dir.create(
    "docs/validation",
    recursive = TRUE
  )
}

development <- readRDS(
  development_path
)

final_test <- readRDS(
  final_test_path
)

required_common_columns <- c(
  "shot_id",
  "source_row",
  "hoopr_season",
  "nba_season",
  "data_role",
  "game_id",
  "game_date",
  "period_number",
  "clock_display_value",
  "clock_seconds_remaining",
  "team_id",
  "team_abbrev",
  "is_home_shooter",
  "home_team_id",
  "home_team_abbrev",
  "away_team_id",
  "away_team_abbrev",
  "shooter_id",
  "shooter_name",
  "type_id",
  "type_text",
  "coordinate_x",
  "coordinate_y",
  "coordinate_x_raw",
  "coordinate_y_raw",
  "court_x_folded",
  "court_y_folded",
  "distance_to_rim_ft",
  "absolute_angle_rad"
)

missing_dev_columns <- setdiff(
  c(
    required_common_columns,
    "outcome"
  ),
  names(development)
)

missing_test_columns <- setdiff(
  required_common_columns,
  names(final_test)
)

if (length(missing_dev_columns) > 0) {
  stop(
    paste0(
      "Development data are missing required columns: ",
      paste(
        missing_dev_columns,
        collapse = ", "
      )
    )
  )
}

if (length(missing_test_columns) > 0) {
  stop(
    paste0(
      "Final-test data are missing required columns: ",
      paste(
        missing_test_columns,
        collapse = ", "
      )
    )
  )
}

if ("outcome" %in% names(final_test)) {
  stop(
    "Final-test data unexpectedly contain outcome. ",
    "Stop and review Stage 2 before continuing."
  )
}

prohibited_input_columns <- c(
  "score_value",
  "source_score_value",
  "scoring_play",
  "source_scoring_play"
)

if (any(
  prohibited_input_columns %in%
    names(development)
)) {
  stop(
    paste0(
      "Development processed data contain prohibited leakage fields: ",
      paste(
        intersect(
          prohibited_input_columns,
          names(development)
        ),
        collapse = ", "
      )
    )
  )
}

if (any(
  prohibited_input_columns %in%
    names(final_test)
)) {
  stop(
    paste0(
      "Final-test processed data contain prohibited leakage fields: ",
      paste(
        intersect(
          prohibited_input_columns,
          names(final_test)
        ),
        collapse = ", "
      )
    )
  )
}

# ---- 3. COMPLETE development shot-type inventory -----------------------------
#
# Per project design, inspect ALL distinct type_id/type_text combinations BEFORE
# defining grouped categories.

development_type_by_season <- development %>%
  count(
    hoopr_season,
    nba_season,
    type_id,
    type_text,
    name = "shots",
    sort = TRUE
  )

development_type_inventory <- development %>%
  count(
    type_id,
    type_text,
    name = "development_shots",
    sort = TRUE
  ) %>%
  left_join(
    development_type_by_season %>%
      select(
        hoopr_season,
        type_id,
        type_text,
        shots
      ) %>%
      tidyr::pivot_wider(
        names_from = hoopr_season,
        values_from = shots,
        names_prefix = "shots_",
        values_fill = 0
      ),
    by = c(
      "type_id",
      "type_text"
    )
  ) %>%
  arrange(
    desc(development_shots),
    type_id,
    type_text
  )

readr::write_csv(
  development_type_inventory,
  "docs/validation/stage3a_complete_development_shot_types.csv"
)

cat(
  "\n--- COMPLETE DEVELOPMENT SHOT-TYPE INVENTORY ---\n"
)

print(
  development_type_inventory,
  n = Inf
)

cat(
  "\nDistinct development type_id/type_text combinations: ",
  nrow(development_type_inventory),
  "\n",
  sep = ""
)

# ---- 4. Final-test structural shot-type comparison ---------------------------
#
# This is a schema/coverage check only. No 2025-26 outcome information exists.

final_test_type_inventory <- final_test %>%
  count(
    type_id,
    type_text,
    name = "final_test_shots",
    sort = TRUE
  )

unseen_final_test_types <- final_test_type_inventory %>%
  anti_join(
    development_type_inventory %>%
      select(
        type_id,
        type_text
      ),
    by = c(
      "type_id",
      "type_text"
    )
  )

readr::write_csv(
  final_test_type_inventory,
  "docs/validation/stage3a_final_test_shot_types.csv"
)

readr::write_csv(
  unseen_final_test_types,
  "docs/validation/stage3a_unseen_final_test_shot_types.csv"
)

cat(
  "\n--- FINAL-TEST SHOT TYPES NOT SEEN IN DEVELOPMENT ---\n"
)

print(
  unseen_final_test_types,
  n = Inf
)

# ---- 5. Basketball / court constants ----------------------------------------
#
# Public NBA rules:
#   - Court: 94 x 50 feet.
#   - Basket center on this hoopR frame: x = -41.75, y = 0 after folding.
#   - Three-point arc radius: 23 ft 9 in = 23.75 ft.
#   - Straight corner-three lines: 3 ft from sidelines => |y| = 22 ft.
#   - Lane width: 16 ft => |y| <= 8 ft.
#   - Free-throw line: 19 ft from baseline.
#   - Restricted-area arc: 4 ft from basket center.
#
# hoopR ESPN coordinates use feet from center court.

court_x_min <- -47
court_x_max <- 47
court_y_min <- -25
court_y_max <- 25

left_rim_x <- -41.75
left_rim_y <- 0

three_arc_radius_ft <- 23.75
corner_three_y_ft <- 22
corner_three_transition_dx_ft <- sqrt(
  three_arc_radius_ft^2 -
    corner_three_y_ft^2
)

restricted_area_radius_ft <- 4
paint_half_width_ft <- 8
free_throw_line_x_left <- -28

# Analytical definition, not an official NBA zone:
heave_distance_ft <- 40

# ---- 6. Canonical team abbreviation helper ----------------------------------
#
# ESPN differs from common NBA abbreviations for six franchises. Canonical
# abbreviations are useful for app filters, labels, and future joins.
# Team identity remains exploration-only unless explicitly approved later.

canonicalize_team_abbrev <- function(x) {

  x <- toupper(
    as.character(
      x
    )
  )

  dplyr::recode(
    x,
    "GS" = "GSW",
    "NO" = "NOP",
    "NY" = "NYK",
    "SA" = "SAS",
    "UTAH" = "UTA",
    "WSH" = "WAS",
    .default = x
  )
}

# ---- 7. Shot-type grouping helper -------------------------------------------
#
# Mutually exclusive primary group.
#
# Precedence matters:
#   Putbacks/tips first so "Layup Shot Putback" is not simply "Layup".
#   Hooks before turnaround.
#   Floaters / step-backs / pull-ups / fadeaways before generic jump shots.
#
# The source `type_text` is retained for transparency and Shiny filtering.

group_shot_type <- function(type_text) {

  x <- tolower(
    trimws(
      as.character(
        type_text
      )
    )
  )

  dplyr::case_when(
    grepl(
      "putback|tip shot|tip-in|tip in",
      x
    ) ~
      "Putback / Tip",

    grepl(
      "dunk",
      x
    ) ~
      "Dunk",

    grepl(
      "layup|finger roll",
      x
    ) ~
      "Layup",

    grepl(
      "hook",
      x
    ) ~
      "Hook",

    grepl(
      "float|floater",
      x
    ) ~
      "Floater",

    grepl(
      "step back|step-back",
      x
    ) ~
      "Step-Back",

    grepl(
      "pullup|pull-up|pull up",
      x
    ) ~
      "Pull-Up",

    grepl(
      "fade away|fadeaway|fade-away",
      x
    ) ~
      "Fadeaway",

    grepl(
      "turnaround",
      x
    ) ~
      "Turnaround",

    grepl(
      "jump shot|jumper|bank shot",
      x
    ) ~
      "Jump Shot",

    TRUE ~
      "Other"
  )
}

# ---- 8. Reusable feature-engineering function -------------------------------

add_shot_features <- function(df) {

  out <- df %>%
    mutate(
      # -----------------------------------------------------------------------
      # Team/display fields
      # -----------------------------------------------------------------------
      team_abbrev_canonical =
        canonicalize_team_abbrev(
          team_abbrev
        ),

      home_team_abbrev_canonical =
        canonicalize_team_abbrev(
          home_team_abbrev
        ),

      away_team_abbrev_canonical =
        canonicalize_team_abbrev(
          away_team_abbrev
        ),

      venue_side =
        if_else(
          is_home_shooter,
          "Home",
          "Away"
        ),

      # -----------------------------------------------------------------------
      # Geometry rebuilt from the folded hoopR coordinates
      # -----------------------------------------------------------------------
      rim_dx_ft =
        court_x_folded -
        left_rim_x,

      rim_dy_ft =
        court_y_folded -
        left_rim_y,

      distance_to_rim_check_ft =
        sqrt(
          rim_dx_ft^2 +
          rim_dy_ft^2
        ),

      shot_angle_rad =
        atan2(
          rim_dy_ft,
          rim_dx_ft
        ),

      absolute_angle_deg =
        abs(
          shot_angle_rad
        ) *
        180 /
        pi,

      # -----------------------------------------------------------------------
      # Geometry-based 3PT classification
      #
      # ESPN's public shot release records score_value = 0 on misses, so shot
      # value cannot be used as a predictor. We classify 3PT status strictly
      # from official court geometry.
      # -----------------------------------------------------------------------
      is_corner_three_geometry =
        abs(
          court_y_folded
        ) >=
          corner_three_y_ft &
        rim_dx_ft <=
          corner_three_transition_dx_ft,

      is_three_geometry =
        is_corner_three_geometry |
        distance_to_rim_check_ft >=
          three_arc_radius_ft,

      three_region =
        case_when(
          is_corner_three_geometry ~
            "Corner 3",
          is_three_geometry ~
            "Above-the-Break 3",
          TRUE ~
            "Two-Point"
        ),

      # -----------------------------------------------------------------------
      # Spatial zones
      #
      # "Heave / Backcourt" is an analytical category based on distance >= 40
      # feet; it is not an official NBA scoring zone.
      # -----------------------------------------------------------------------
      is_heave =
        distance_to_rim_check_ft >=
          heave_distance_ft,

      in_restricted_area_geometry =
        distance_to_rim_check_ft <=
          restricted_area_radius_ft,

      in_paint_geometry =
        abs(
          court_y_folded
        ) <=
          paint_half_width_ft &
        court_x_folded <=
          free_throw_line_x_left,

      shot_zone_geometry =
        case_when(
          is_heave ~
            "Heave / Backcourt",

          is_corner_three_geometry ~
            "Corner 3",

          is_three_geometry ~
            "Above-the-Break 3",

          in_restricted_area_geometry ~
            "Restricted Area",

          in_paint_geometry ~
            "Paint (Non-RA)",

          TRUE ~
            "Mid-Range"
        ),

      distance_band =
        cut(
          distance_to_rim_check_ft,
          breaks = c(
            -Inf,
            4,
            8,
            16,
            22,
            24,
            30,
            40,
            Inf
          ),
          labels = c(
            "0-4 ft",
            "4-8 ft",
            "8-16 ft",
            "16-22 ft",
            "22-24 ft",
            "24-30 ft",
            "30-40 ft",
            "40+ ft"
          ),
          right = FALSE,
          ordered_result = TRUE
        ),

      # -----------------------------------------------------------------------
      # Primary shot-type group + descriptive action tags
      # -----------------------------------------------------------------------
      shot_type_group =
        group_shot_type(
          type_text
        ),

      is_bank_shot =
        grepl(
          "bank",
          type_text,
          ignore.case = TRUE
        ),

      is_driving =
        grepl(
          "driving",
          type_text,
          ignore.case = TRUE
        ),

      is_running =
        grepl(
          "running",
          type_text,
          ignore.case = TRUE
        ),

      is_cutting =
        grepl(
          "cutting",
          type_text,
          ignore.case = TRUE
        ),

      is_alley_oop =
        grepl(
          "alley oop|alley-oop",
          type_text,
          ignore.case = TRUE
        ),

      is_putback =
        grepl(
          "putback|tip shot|tip-in|tip in",
          type_text,
          ignore.case = TRUE
        ),

      is_turnaround =
        grepl(
          "turnaround",
          type_text,
          ignore.case = TRUE
        ),

      is_fadeaway =
        grepl(
          "fade away|fadeaway|fade-away",
          type_text,
          ignore.case = TRUE
        ),

      # -----------------------------------------------------------------------
      # Time / game-context features
      #
      # NBA regulation quarters = 12 minutes.
      # NBA overtime periods = 5 minutes.
      # These are GAME-clock features, not shot-clock features.
      # -----------------------------------------------------------------------
      is_overtime =
        period_number >=
          5L,

      period_group =
        case_when(
          period_number == 1L ~
            "Q1",
          period_number == 2L ~
            "Q2",
          period_number == 3L ~
            "Q3",
          period_number == 4L ~
            "Q4",
          period_number >= 5L ~
            "OT",
          TRUE ~
            NA_character_
        ),

      game_phase =
        case_when(
          period_number %in%
            c(
              1L,
              2L
            ) ~
            "First Half",

          period_number %in%
            c(
              3L,
              4L
            ) ~
            "Second Half",

          period_number >=
            5L ~
            "Overtime",

          TRUE ~
            NA_character_
        ),

      period_length_seconds =
        if_else(
          period_number <=
            4L,
          12 * 60,
          5 * 60
        ),

      seconds_elapsed_period =
        period_length_seconds -
        clock_seconds_remaining,

      elapsed_game_seconds =
        case_when(
          period_number <=
            4L ~
            (
              period_number -
                1L
            ) *
              12 *
              60 +
              seconds_elapsed_period,

          period_number >=
            5L ~
            4 *
              12 *
              60 +
              (
                period_number -
                  5L
              ) *
                5 *
                60 +
              seconds_elapsed_period,

          TRUE ~
            NA_real_
        ),

      is_last_2_minutes_period =
        clock_seconds_remaining <=
          120,

      is_last_24_seconds_period =
        clock_seconds_remaining <=
          24,

      is_last_5_seconds_period =
        clock_seconds_remaining <=
          5
    )

  out
}

# ---- 9. Apply identical feature engineering ---------------------------------

development_features <- add_shot_features(
  development
)

final_test_features <- add_shot_features(
  final_test
)

# ---- 10. Distance reconstruction audit --------------------------------------

distance_reconstruction_audit <- bind_rows(
  development_features %>%
    summarise(
      dataset = "development",
      rows = n(),
      max_abs_difference_ft =
        max(
          abs(
            distance_to_rim_ft -
              distance_to_rim_check_ft
          ),
          na.rm = TRUE
        ),
      mean_abs_difference_ft =
        mean(
          abs(
            distance_to_rim_ft -
              distance_to_rim_check_ft
          ),
          na.rm = TRUE
        )
    ),

  final_test_features %>%
    summarise(
      dataset = "final_test",
      rows = n(),
      max_abs_difference_ft =
        max(
          abs(
            distance_to_rim_ft -
              distance_to_rim_check_ft
          ),
          na.rm = TRUE
        ),
      mean_abs_difference_ft =
        mean(
          abs(
            distance_to_rim_ft -
              distance_to_rim_check_ft
          ),
          na.rm = TRUE
        )
    )
)

readr::write_csv(
  distance_reconstruction_audit,
  "docs/validation/stage3a_distance_reconstruction_audit.csv"
)

cat(
  "\n--- DISTANCE RECONSTRUCTION AUDIT ---\n"
)

print(
  distance_reconstruction_audit,
  n = Inf
)

# ---- 11. Official-geometry 3PT validation using DEVELOPMENT makes only -------
#
# IMPORTANT:
# Raw score_value is used ONLY here, as a validation target on MADE shots.
# It is never joined into final_test_features and is dropped immediately after
# this audit.
#
# For made field goals, raw score_value identifies whether the made shot was
# worth 2 or 3 points. This lets us test the coordinate geometry.

raw_development_score_lookup <- map_dfr(
  c(
    2023L,
    2024L,
    2025L
  ),
  function(season_value) {

    raw_path <- file.path(
      "data",
      "raw",
      paste0(
        "nba_shots_",
        season_value,
        ".rds"
      )
    )

    if (!file.exists(raw_path)) {
      stop(
        "Missing raw shot file needed for geometry audit: ",
        raw_path
      )
    }

    raw <- readRDS(
      raw_path
    )

    tibble(
      hoopr_season =
        season_value,
      source_row =
        seq_len(
          nrow(raw)
        ),
      source_score_value =
        raw$score_value
    )
  }
)

three_point_geometry_validation <- development_features %>%
  select(
    shot_id,
    hoopr_season,
    source_row,
    outcome,
    distance_to_rim_check_ft,
    court_x_folded,
    court_y_folded,
    is_corner_three_geometry,
    is_three_geometry
  ) %>%
  inner_join(
    raw_development_score_lookup,
    by = c(
      "hoopr_season",
      "source_row"
    )
  ) %>%
  filter(
    outcome == 1L,
    source_score_value %in%
      c(
        2L,
        3L
      )
  ) %>%
  mutate(
    source_is_three =
      source_score_value ==
        3L,

    geometry_matches_source =
      is_three_geometry ==
        source_is_three
  )

three_point_geometry_summary <- three_point_geometry_validation %>%
  group_by(
    hoopr_season
  ) %>%
  summarise(
    made_field_goals_checked =
      n(),
    geometry_matches =
      sum(
        geometry_matches_source
      ),
    geometry_mismatches =
      sum(
        !geometry_matches_source
      ),
    mismatch_rate =
      mean(
        !geometry_matches_source
      ),
    false_three =
      sum(
        is_three_geometry &
          !source_is_three
      ),
    false_two =
      sum(
        !is_three_geometry &
          source_is_three
      ),
    .groups =
      "drop"
  ) %>%
  bind_rows(
    three_point_geometry_validation %>%
      summarise(
        hoopr_season =
          NA_integer_,
        made_field_goals_checked =
          n(),
        geometry_matches =
          sum(
            geometry_matches_source
          ),
        geometry_mismatches =
          sum(
            !geometry_matches_source
          ),
        mismatch_rate =
          mean(
            !geometry_matches_source
          ),
        false_three =
          sum(
            is_three_geometry &
              !source_is_three
          ),
        false_two =
          sum(
            !is_three_geometry &
              source_is_three
          )
      )
  )

readr::write_csv(
  three_point_geometry_summary,
  "docs/validation/stage3a_three_point_geometry_validation.csv"
)

readr::write_csv(
  three_point_geometry_validation %>%
    filter(
      !geometry_matches_source
    ) %>%
    select(
      -source_score_value
    ),
  "docs/validation/stage3a_three_point_geometry_mismatches.csv"
)

cat(
  "\n--- THREE-POINT GEOMETRY VALIDATION (MADE DEVELOPMENT SHOTS) ---\n"
)

print(
  three_point_geometry_summary,
  n = Inf
)

# Remove the raw leakage-source objects from the workspace as soon as the
# validation audit is complete.
rm(
  raw_development_score_lookup,
  three_point_geometry_validation
)

# ---- 12. Shot-type grouping audit -------------------------------------------

shot_type_group_audit <- development_features %>%
  count(
    shot_type_group,
    type_id,
    type_text,
    name = "development_shots",
    sort = TRUE
  ) %>%
  arrange(
    shot_type_group,
    desc(
      development_shots
    ),
    type_id
  )

shot_type_group_summary <- development_features %>%
  count(
    shot_type_group,
    name = "development_shots",
    sort = TRUE
  ) %>%
  mutate(
    development_pct =
      development_shots /
      sum(
        development_shots
      )
  )

other_type_audit <- shot_type_group_audit %>%
  filter(
    shot_type_group ==
      "Other"
  )

readr::write_csv(
  shot_type_group_audit,
  "docs/validation/stage3a_shot_type_group_mapping.csv"
)

readr::write_csv(
  shot_type_group_summary,
  "docs/validation/stage3a_shot_type_group_summary.csv"
)

readr::write_csv(
  other_type_audit,
  "docs/validation/stage3a_other_shot_types.csv"
)

cat(
  "\n--- SHOT-TYPE GROUP SUMMARY ---\n"
)

print(
  shot_type_group_summary,
  n = Inf
)

cat(
  "\n--- TYPE LABELS CURRENTLY MAPPED TO 'OTHER' ---\n"
)

print(
  other_type_audit,
  n = Inf
)

# ---- 13. Group stability by season ------------------------------------------

shot_type_group_by_season <- development_features %>%
  count(
    hoopr_season,
    nba_season,
    shot_type_group,
    name = "shots"
  ) %>%
  group_by(
    hoopr_season
  ) %>%
  mutate(
    season_pct =
      shots /
      sum(
        shots
      )
  ) %>%
  ungroup() %>%
  arrange(
    hoopr_season,
    desc(
      shots
    )
  )

readr::write_csv(
  shot_type_group_by_season,
  "docs/validation/stage3a_shot_type_group_by_season.csv"
)

cat(
  "\n--- SHOT-TYPE GROUP STABILITY BY SEASON ---\n"
)

print(
  shot_type_group_by_season,
  n = Inf
)

# ---- 14. Spatial feature audit ----------------------------------------------

spatial_feature_audit <- bind_rows(
  development_features %>%
    count(
      hoopr_season,
      shot_zone_geometry,
      name = "shots"
    ) %>%
    mutate(
      dataset =
        "development"
    ),

  final_test_features %>%
    count(
      hoopr_season,
      shot_zone_geometry,
      name = "shots"
    ) %>%
    mutate(
      dataset =
        "final_test"
    )
) %>%
  group_by(
    dataset,
    hoopr_season
  ) %>%
  mutate(
    season_pct =
      shots /
      sum(
        shots
      )
  ) %>%
  ungroup() %>%
  arrange(
    hoopr_season,
    desc(
      shots
    )
  )

readr::write_csv(
  spatial_feature_audit,
  "docs/validation/stage3a_spatial_zone_audit.csv"
)

cat(
  "\n--- SPATIAL ZONE AUDIT ---\n"
)

print(
  spatial_feature_audit,
  n = Inf
)

# ---- 15. Time-feature audit --------------------------------------------------

time_feature_audit <- bind_rows(
  development_features %>%
    transmute(
      dataset =
        "development",
      hoopr_season,
      period_number,
      period_length_seconds,
      clock_seconds_remaining,
      seconds_elapsed_period,
      is_overtime
    ),

  final_test_features %>%
    transmute(
      dataset =
        "final_test",
      hoopr_season,
      period_number,
      period_length_seconds,
      clock_seconds_remaining,
      seconds_elapsed_period,
      is_overtime
    )
) %>%
  group_by(
    dataset,
    hoopr_season
  ) %>%
  summarise(
    rows =
      n(),
    overtime_shots =
      sum(
        is_overtime
      ),
    min_clock =
      min(
        clock_seconds_remaining,
        na.rm = TRUE
      ),
    max_regulation_clock =
      max(
        clock_seconds_remaining[
          period_number <=
            4L
        ],
        na.rm = TRUE
      ),
    max_overtime_clock =
      if (
        any(
          period_number >=
            5L
        )
      ) {
        max(
          clock_seconds_remaining[
            period_number >=
              5L
          ],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
    invalid_by_period_length =
      sum(
        clock_seconds_remaining <
          0 |
        clock_seconds_remaining >
          period_length_seconds,
        na.rm = TRUE
      ),
    .groups =
      "drop"
  )

readr::write_csv(
  time_feature_audit,
  "docs/validation/stage3a_time_feature_audit.csv"
)

cat(
  "\n--- TIME-FEATURE AUDIT ---\n"
)

print(
  time_feature_audit,
  n = Inf
)

# ---- 16. Development-only descriptive feature summary -----------------------
#
# Outcome summaries are permitted ONLY for development seasons.

development_feature_summary <- development_features %>%
  group_by(
    hoopr_season,
    nba_season
  ) %>%
  summarise(
    shots =
      n(),
    make_rate =
      mean(
        outcome
      ),
    mean_distance_ft =
      mean(
        distance_to_rim_check_ft
      ),
    three_geometry_pct =
      mean(
        is_three_geometry
      ),
    corner_three_pct =
      mean(
        is_corner_three_geometry
      ),
    heave_pct =
      mean(
        is_heave
      ),
    home_shot_pct =
      mean(
        is_home_shooter
      ),
    overtime_pct =
      mean(
        is_overtime
      ),
    .groups =
      "drop"
  )

readr::write_csv(
  development_feature_summary,
  "docs/validation/stage3a_development_feature_summary.csv"
)

cat(
  "\n--- DEVELOPMENT FEATURE SUMMARY ---\n"
)

print(
  development_feature_summary,
  n = Inf
)

cat(
  "\n2025-26 outcome remains intentionally unavailable and unreported.\n"
)

# ---- 17. Missingness audit ---------------------------------------------------

new_feature_columns <- c(
  "team_abbrev_canonical",
  "home_team_abbrev_canonical",
  "away_team_abbrev_canonical",
  "venue_side",
  "rim_dx_ft",
  "rim_dy_ft",
  "distance_to_rim_check_ft",
  "shot_angle_rad",
  "absolute_angle_deg",
  "is_corner_three_geometry",
  "is_three_geometry",
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
  "is_overtime",
  "period_group",
  "game_phase",
  "period_length_seconds",
  "seconds_elapsed_period",
  "elapsed_game_seconds",
  "is_last_2_minutes_period",
  "is_last_24_seconds_period",
  "is_last_5_seconds_period"
)

feature_missingness <- map_dfr(
  new_feature_columns,
  function(feature_name) {

    tibble(
      feature =
        feature_name,

      development_missing_n =
        sum(
          is.na(
            development_features[[
              feature_name
            ]]
          )
        ),

      development_missing_pct =
        mean(
          is.na(
            development_features[[
              feature_name
            ]]
          )
        ),

      final_test_missing_n =
        sum(
          is.na(
            final_test_features[[
              feature_name
            ]]
          )
        ),

      final_test_missing_pct =
        mean(
          is.na(
            final_test_features[[
              feature_name
            ]]
          )
        )
    )
  }
)

readr::write_csv(
  feature_missingness,
  "docs/validation/stage3a_feature_missingness.csv"
)

cat(
  "\n--- NEW FEATURE MISSINGNESS ---\n"
)

print(
  feature_missingness,
  n = Inf
)

# ---- 18. Feature-role draft --------------------------------------------------
#
# This is a DRAFT for Stage 3C. It is intentionally conservative.
# Nothing here automatically enters a model.

feature_role_draft <- tibble(
  feature = c(
    "court_x_folded",
    "court_y_folded",
    "distance_to_rim_check_ft",
    "absolute_angle_deg",
    "is_three_geometry",
    "is_corner_three_geometry",
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
    "team_abbrev_canonical",
    "shooter_id",
    "shooter_name",
    "nba_season",
    "outcome",
    "score_value",
    "scoring_play"
  ),
  draft_role = c(
    rep(
      "candidate_predictive",
      26
    ),
    "exploration_only",
    "exploration_only",
    "exploration_only",
    "exploration_only",
    "validation_split_only",
    "target_only",
    "prohibited_leakage",
    "prohibited_leakage"
  ),
  rationale = c(
    "Spatial shot location",
    "Spatial shot location",
    "Core shot-difficulty geometry",
    "Court geometry",
    "Attempt value inferred from official court geometry",
    "Corner-three geometry",
    "Interpretable spatial zone",
    "Interpretable distance grouping",
    "Pre-shot action/type descriptor",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Pre-shot action tag",
    "Game period known before shot",
    "Game clock known before shot",
    "Game context",
    "Interpretable period group",
    "Interpretable game phase",
    "End-period context",
    "End-period context",
    "End-period context",
    "Home/away context",
    "Keep for filtering; high-cardinality identity not approved yet",
    "Keep for filtering; team identity not approved yet",
    "Keep for player filters; player identity not approved yet",
    "Display label only",
    "Used for chronological validation, not final core predictor",
    "Prediction target; never a predictor",
    "Outcome leakage in ESPN shot release",
    "Outcome indicator; prohibited as predictor"
  )
)

readr::write_csv(
  feature_role_draft,
  "docs/validation/stage3a_feature_role_draft.csv"
)

# ---- 19. Save feature-engineered local datasets -----------------------------

saveRDS(
  development_features,
  "data/processed/shots_development_features.rds",
  compress = "xz"
)

saveRDS(
  final_test_features,
  "data/processed/shots_final_test_features.rds",
  compress = "xz"
)

# ---- 20. Final Stage 3A verification ----------------------------------------

common_feature_names_dev <- setdiff(
  names(
    development_features
  ),
  "outcome"
)

common_feature_names_test <- names(
  final_test_features
)

schema_matches_except_outcome <- identical(
  common_feature_names_dev,
  common_feature_names_test
)

no_prohibited_dev <- !any(
  prohibited_input_columns %in%
    names(
      development_features
    )
)

no_prohibited_test <- !any(
  prohibited_input_columns %in%
    names(
      final_test_features
    )
)

all_new_features_present_dev <- all(
  new_feature_columns %in%
    names(
      development_features
    )
)

all_new_features_present_test <- all(
  new_feature_columns %in%
    names(
      final_test_features
    )
)

new_features_complete <- all(
  feature_missingness$
    development_missing_n == 0
) &&
  all(
    feature_missingness$
      final_test_missing_n == 0
  )

distance_rebuild_matches <- all(
  distance_reconstruction_audit$
    max_abs_difference_ft <
    1e-10
)

clock_valid_by_period <- all(
  time_feature_audit$
    invalid_by_period_length == 0
)

type_group_complete <- !anyNA(
  development_features$
    shot_type_group
) &&
  !anyNA(
    final_test_features$
      shot_type_group
  )

zone_complete <- !anyNA(
  development_features$
    shot_zone_geometry
) &&
  !anyNA(
    final_test_features$
      shot_zone_geometry
  )

final_test_outcome_absent <-
  !(
    "outcome" %in%
      names(
        final_test_features
      )
  )

stage3a_checks <- tibble(
  check = c(
    "Development feature dataset is non-empty",
    "Final-test feature dataset is non-empty",
    "Complete development shot-type inventory was created",
    "All engineered features exist in development data",
    "All engineered features exist in final-test data",
    "Development/test schemas match except for outcome",
    "No prohibited leakage fields exist in development features",
    "No prohibited leakage fields exist in final-test features",
    "Final-test outcome remains absent",
    "Recomputed shot distance matches Stage 2 distance",
    "Game clock is valid for regulation and overtime period lengths",
    "Shot-type grouping contains no missing values",
    "Spatial-zone grouping contains no missing values",
    "Engineered feature missingness is zero",
    "Feature-engineered development RDS exists",
    "Feature-engineered final-test RDS exists"
  ),
  passed = c(
    nrow(
      development_features
    ) > 0,
    nrow(
      final_test_features
    ) > 0,
    nrow(
      development_type_inventory
    ) > 0,
    all_new_features_present_dev,
    all_new_features_present_test,
    schema_matches_except_outcome,
    no_prohibited_dev,
    no_prohibited_test,
    final_test_outcome_absent,
    distance_rebuild_matches,
    clock_valid_by_period,
    type_group_complete,
    zone_complete,
    new_features_complete,
    file.exists(
      "data/processed/shots_development_features.rds"
    ),
    file.exists(
      "data/processed/shots_final_test_features.rds"
    )
  )
)

cat(
  "\n--- STAGE 3A FEATURE-ENGINEERING VERIFICATION ---\n"
)

print(
  stage3a_checks,
  n = Inf
)

if (!all(
  stage3a_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 3A feature-engineering checks failed. ",
      "Review the audit outputs before continuing."
    )
  )
}

# Geometry validation should be reviewed rather than hard-failed because
# ESPN coordinates can be rounded/coarsened near the three-point line.
overall_geometry_row <- three_point_geometry_summary %>%
  filter(
    is.na(
      hoopr_season
    )
  )

if (
  nrow(
    overall_geometry_row
  ) == 1 &&
  overall_geometry_row$
    mismatch_rate >
    0.02
) {
  warning(
    paste0(
      "Geometry-based 2PT/3PT mismatch rate exceeds 2% on made ",
      "development shots. Review stage3a_three_point_geometry_validation.csv ",
      "before using is_three_geometry in modeling."
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
    "\nUpdated renv.lock after Stage 3A feature engineering.\n"
  )
}

cat(
  "\nSTAGE 3A COMPLETE.\n",
  "Next: review the complete shot taxonomy, 'Other' mapping, geometry ",
  "validation, spatial zones, and Stage 3A verification before beginning ",
  "03b_exploratory_analysis.R.\n",
  sep = ""
)
