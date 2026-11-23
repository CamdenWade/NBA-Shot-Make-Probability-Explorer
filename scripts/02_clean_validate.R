# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 2 — Clean & Validate Public Shot Data
# File: scripts/02_clean_validate.R
#
# CORRECTED VERSION (v6)
#
# Fixes based on Stage 2 audit and v3 verification:
#   1. Uses ESPN NBA abbreviations rather than NBA.com-style abbreviations.
#      ESPN's six differing codes are:
#        GS, NO, NY, SA, UTAH, WSH
#      rather than:
#        GSW, NOP, NYK, SAS, UTA, WAS
#
#   2. Removes NBA Cup Championship games from the countable regular-season
#      population when ESPN tags them as season_type == 2.
#
#   3. Treats score_value as outcome leakage. It is audited from source data
#      but is NOT retained in either processed modeling dataset.
#
#   4. Keeps 2025-26 outcome/scoring_play hidden from the processed test set.
#
#   5. Applies the 1,230-game validation to played games represented in the
#      shot release, while auditing schedule-only entries separately.
#
# IMPORTANT:
#   - Raw files are never modified.
#   - Stage 3 feature engineering should not begin until this corrected Stage 2
#     audit is reviewed.
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

# ---- 2. Locked season plan ---------------------------------------------------

season_plan <- tibble(
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
  role = c(
    "development_train",
    "development_validation",
    "development_validation",
    "final_untouched_test"
  )
)


# ---- 2A. Explicit non-standings NBA Cup Championship reference --------------
#
# ESPN's historical schedule notes are not consistently populated, so note
# text alone is not reliable for identifying the non-standings Cup final.
#
# These three championship games are documented NBA Cup finals and do NOT
# count toward the 82-game regular-season standings. We identify them using
# hoopR season + exact date + unordered matchup.

known_nonstandings_cup_finals <- tibble(
  hoopr_season = c(
    2024L,
    2025L,
    2026L
  ),
  cup_date = as.Date(
    c(
      "2023-12-09",
      "2024-12-17",
      "2025-12-16"
    )
  ),
  team_a = c(
    "IND",
    "MIL",
    "NY"
  ),
  team_b = c(
    "LAL",
    "OKC",
    "SA"
  )
)

shot_paths <- file.path(
  "data",
  "raw",
  paste0(
    "nba_shots_",
    season_plan$hoopr_season,
    ".rds"
  )
)

schedule_paths <- file.path(
  "data",
  "raw",
  paste0(
    "nba_schedule_",
    season_plan$hoopr_season,
    ".rds"
  )
)

if (!all(file.exists(shot_paths))) {
  stop(
    "One or more raw shot files are missing. Run scripts/01_acquire_data.R."
  )
}

if (!all(file.exists(schedule_paths))) {
  stop(
    paste0(
      "One or more raw schedule files are missing. ",
      "Run scripts/01b_acquire_schedule_metadata.R."
    )
  )
}

if (!dir.exists("data/processed")) {
  dir.create(
    "data/processed",
    recursive = TRUE
  )
}

if (!dir.exists("docs/validation")) {
  dir.create(
    "docs/validation",
    recursive = TRUE
  )
}

# Keep processed modeling data out of Git.
if (file.exists(".gitignore")) {

  gitignore_lines <- readLines(
    ".gitignore",
    warn = FALSE
  )

  if (!any(
    trimws(gitignore_lines) ==
      "data/processed/"
  )) {

    write(
      c(
        "",
        "# Large processed modeling datasets",
        "data/processed/"
      ),
      file = ".gitignore",
      append = TRUE
    )
  }
}

# ---- 3. ESPN NBA franchise abbreviations ------------------------------------
#
# IMPORTANT:
# ESPN abbreviations differ from common NBA.com abbreviations for six teams:
#
# ESPN  Common
# GS    GSW
# NO    NOP
# NY    NYK
# SA    SAS
# UTAH  UTA
# WSH   WAS
#
# We use ESPN codes because BOTH the hoopR shot and schedule releases here are
# ESPN-derived.

espn_nba_team_abbrevs <- c(
  "ATL", "BOS", "BKN", "CHA", "CHI",
  "CLE", "DAL", "DEN", "DET", "GS",
  "HOU", "IND", "LAC", "LAL", "MEM",
  "MIA", "MIL", "MIN", "NO", "NY",
  "OKC", "ORL", "PHI", "PHX", "POR",
  "SAC", "SA", "TOR", "UTAH", "WSH"
)

stopifnot(
  length(espn_nba_team_abbrevs) == 30L,
  anyDuplicated(espn_nba_team_abbrevs) == 0
)

# ---- 4. Court constants ------------------------------------------------------

court_x_min <- -47
court_x_max <- 47
court_y_min <- -25
court_y_max <- 25

left_rim_x <- -41.75
left_rim_y <- 0

# ---- 5. Helper functions -----------------------------------------------------

parse_clock_seconds <- function(x) {

  x <- as.character(x)

  vapply(
    strsplit(
      x,
      ":",
      fixed = TRUE
    ),
    function(parts) {

      if (
        length(parts) != 2 ||
        anyNA(parts)
      ) {
        return(NA_real_)
      }

      minutes <- suppressWarnings(
        as.numeric(parts[[1]])
      )

      seconds <- suppressWarnings(
        as.numeric(parts[[2]])
      )

      if (
        !is.finite(minutes) ||
        !is.finite(seconds)
      ) {
        return(NA_real_)
      }

      minutes * 60 + seconds
    },
    numeric(1)
  )
}

first_existing_col <- function(
    df,
    candidates,
    required = TRUE) {

  found <- candidates[
    candidates %in%
      names(df)
  ]

  if (length(found) == 0) {

    if (required) {
      return(NA_character_)
    }

    return(NULL)
  }

  found[[1]]
}

normalize_schedule_columns <- function(schedule) {

  game_id_col <- first_existing_col(
    schedule,
    c("game_id", "id")
  )

  season_type_col <- first_existing_col(
    schedule,
    c("season_type")
  )

  game_date_col <- first_existing_col(
    schedule,
    c("game_date", "date")
  )

  home_id_col <- first_existing_col(
    schedule,
    c(
      "home_id",
      "home_team_id"
    )
  )

  home_abbrev_col <- first_existing_col(
    schedule,
    c(
      "home_abbreviation",
      "home_team_abbrev",
      "home_team_abb",
      "home_abbrev"
    )
  )

  away_id_col <- first_existing_col(
    schedule,
    c(
      "away_id",
      "away_team_id"
    )
  )

  away_abbrev_col <- first_existing_col(
    schedule,
    c(
      "away_abbreviation",
      "away_team_abbrev",
      "away_team_abb",
      "away_abbrev"
    )
  )

  required_mapping <- c(
    game_id =
      game_id_col,
    season_type =
      season_type_col,
    game_date =
      game_date_col,
    home_team_id =
      home_id_col,
    home_team_abbrev =
      home_abbrev_col,
    away_team_id =
      away_id_col,
    away_team_abbrev =
      away_abbrev_col
  )

  missing_mapping <- names(
    required_mapping
  )[
    is.na(
      required_mapping
    )
  ]

  if (length(missing_mapping) > 0) {

    cat(
      "\nAvailable schedule columns:\n",
      paste(
        names(schedule),
        collapse = ", "
      ),
      "\n\n",
      sep = ""
    )

    stop(
      paste0(
        "Could not map required schedule fields: ",
        paste(
          missing_mapping,
          collapse = ", "
        )
      )
    )
  }

  notes_headline_col <- first_existing_col(
    schedule,
    c(
      "notes_headline",
      "note_headline"
    ),
    required = FALSE
  )

  notes_type_col <- first_existing_col(
    schedule,
    c(
      "notes_type",
      "note_type"
    ),
    required = FALSE
  )

  out <- tibble(
    game_id =
      schedule[[game_id_col]],
    season_type =
      schedule[[season_type_col]],
    game_date =
      schedule[[game_date_col]],
    home_team_id =
      schedule[[home_id_col]],
    home_team_abbrev =
      schedule[[home_abbrev_col]],
    away_team_id =
      schedule[[away_id_col]],
    away_team_abbrev =
      schedule[[away_abbrev_col]]
  )

  out$notes_headline <- if (
    is.null(
      notes_headline_col
    )
  ) {
    NA_character_
  } else {
    as.character(
      schedule[[
        notes_headline_col
      ]]
    )
  }

  out$notes_type <- if (
    is.null(
      notes_type_col
    )
  ) {
    NA_character_
  } else {
    as.character(
      schedule[[
        notes_type_col
      ]]
    )
  }

  out
}

# ---- 6. Clean one season -----------------------------------------------------

clean_one_season <- function(
    hoopr_season,
    nba_season,
    role) {

  shots <- readRDS(
    file.path(
      "data",
      "raw",
      paste0(
        "nba_shots_",
        hoopr_season,
        ".rds"
      )
    )
  )

  schedule_raw <- readRDS(
    file.path(
      "data",
      "raw",
      paste0(
        "nba_schedule_",
        hoopr_season,
        ".rds"
      )
    )
  )

  # ---------------------------------------------------------------------------
  # 6A. Required shot fields
  # ---------------------------------------------------------------------------

  required_shot_columns <- c(
    "game_id",
    "season",
    "period_number",
    "clock_display_value",
    "team_id",
    "athlete_id_1",
    "type_id",
    "type_text",
    "scoring_play",
    "score_value",
    "coordinate_x",
    "coordinate_y",
    "coordinate_x_raw",
    "coordinate_y_raw",
    "athlete_name_1",
    "team_name",
    "team_mascot",
    "team_abbrev"
  )

  missing_shot_columns <- setdiff(
    required_shot_columns,
    names(shots)
  )

  if (length(missing_shot_columns) > 0) {
    stop(
      paste0(
        "Season ",
        hoopr_season,
        " is missing required shot columns: ",
        paste(
          missing_shot_columns,
          collapse = ", "
        )
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 6B. Normalize schedule schema
  # ---------------------------------------------------------------------------

  schedule <- normalize_schedule_columns(
    schedule_raw
  ) %>%
    mutate(
      season_type =
        as.integer(
          season_type
        ),
      home_team_abbrev =
        toupper(
          as.character(
            home_team_abbrev
          )
        ),
      away_team_abbrev =
        toupper(
          as.character(
            away_team_abbrev
          )
        ),
      notes_headline =
        as.character(
          notes_headline
        )
    )

  # ---------------------------------------------------------------------------
  # 6C. Detect NBA Cup Championship rows
  #
  # ESPN can classify the Cup Championship inside season_type == 2 even though
  # the championship does not count toward regular-season standings.
  #
  # We use:
  #   1. schedule-note detection when available; and
  #   2. an explicit season/date/matchup reference for the three Cup finals
  #      in this project's study window.
  # ---------------------------------------------------------------------------

  cup_reference <- known_nonstandings_cup_finals %>%
    filter(
      .data$hoopr_season ==
        .env$hoopr_season
    )

  schedule <- schedule %>%
    mutate(
      game_date =
        as.Date(
          game_date
        ),
      cup_note_match =
        !is.na(
          notes_headline
        ) &
        grepl(
          "Cup Championship|NBA Cup.*Championship|In-Season Tournament.*Championship",
          notes_headline,
          ignore.case = TRUE
        ),
      cup_reference_match =
        if (
          nrow(
            cup_reference
          ) == 0
        ) {
          FALSE
        } else {
          (
            game_date ==
              cup_reference$cup_date[[1]]
          ) &
          (
            (
              home_team_abbrev ==
                cup_reference$team_a[[1]] &
              away_team_abbrev ==
                cup_reference$team_b[[1]]
            ) |
            (
              home_team_abbrev ==
                cup_reference$team_b[[1]] &
              away_team_abbrev ==
                cup_reference$team_a[[1]]
            )
          )
        },
      is_nba_cup_championship =
        cup_note_match |
        cup_reference_match,
      cup_detection_method =
        case_when(
          cup_note_match &
            cup_reference_match ~
            "note_and_reference",
          cup_reference_match ~
            "documented_date_matchup",
          cup_note_match ~
            "schedule_note",
          TRUE ~
            NA_character_
        )
    )

  cup_final_audit <- schedule %>%
    filter(
      is_nba_cup_championship
    ) %>%
    select(
      game_id,
      game_date,
      home_team_abbrev,
      away_team_abbrev,
      notes_headline,
      cup_detection_method
    )

  # ---------------------------------------------------------------------------
  # 6D. Countable NBA regular-season schedule
  # ---------------------------------------------------------------------------

  nba_regular_schedule <- schedule %>%
    filter(
      season_type == 2L,
      home_team_abbrev %in%
        espn_nba_team_abbrevs,
      away_team_abbrev %in%
        espn_nba_team_abbrevs,
      !is_nba_cup_championship
    ) %>%
    distinct(
      game_id,
      .keep_all = TRUE
    )

  if (nrow(nba_regular_schedule) == 0) {
    stop(
      "No NBA regular-season games identified for season ",
      hoopr_season,
      "."
    )
  }

  # ---------------------------------------------------------------------------
  # 6E. Prepare raw shot flags
  # ---------------------------------------------------------------------------

  shots_flagged <- shots %>%
    mutate(
      source_row =
        row_number(),
      team_abbrev =
        toupper(
          as.character(
            team_abbrev
          )
        ),
      type_text =
        as.character(
          type_text
        ),
      is_free_throw_text =
        !is.na(
          type_text
        ) &
        grepl(
          "Free Throw",
          type_text,
          ignore.case = TRUE
        ),
      clock_seconds_remaining =
        parse_clock_seconds(
          clock_display_value
        ),
      coordinate_finite =
        is.finite(
          coordinate_x
        ) &
        is.finite(
          coordinate_y
        ),
      coordinate_in_court =
        coordinate_finite &
        coordinate_x >=
          court_x_min &
        coordinate_x <=
          court_x_max &
        coordinate_y >=
          court_y_min &
        coordinate_y <=
          court_y_max,
      court_x_folded =
        -abs(
          coordinate_x
        ),
      court_y_folded =
        coordinate_y,
      distance_to_rim_ft =
        sqrt(
          (
            court_x_folded -
              left_rim_x
          )^2 +
          (
            court_y_folded -
              left_rim_y
          )^2
        ),
      absolute_angle_rad =
        abs(
          atan2(
            court_y_folded -
              left_rim_y,
            court_x_folded -
              left_rim_x
          )
        )
    )

  # ---------------------------------------------------------------------------
  # 6F. Join schedule metadata
  # ---------------------------------------------------------------------------

  schedule_join <- nba_regular_schedule %>%
    select(
      game_id,
      game_date,
      season_type,
      home_team_id,
      home_team_abbrev,
      away_team_id,
      away_team_abbrev
    )

  joined <- shots_flagged %>%
    inner_join(
      schedule_join,
      by = "game_id"
    )

  # ---------------------------------------------------------------------------
  # 6G. Structural quality flags
  # ---------------------------------------------------------------------------

  joined <- joined %>%
    mutate(
      team_matches_game =
        (
          team_id ==
            home_team_id
        ) |
        (
          team_id ==
            away_team_id
        ),
      team_abbrev_matches_game =
        (
          team_abbrev ==
            home_team_abbrev
        ) |
        (
          team_abbrev ==
            away_team_abbrev
        ),
      valid_clock =
        is.finite(
          clock_seconds_remaining
        ) &
        clock_seconds_remaining >= 0 &
        clock_seconds_remaining <=
          12 * 60,
      valid_period =
        !is.na(
          period_number
        ) &
        period_number >= 1L,
      valid_shot_type =
        !is.na(
          type_text
        ) &
        nzchar(
          trimws(
            type_text
          )
        ),
      valid_team =
        !is.na(
          team_id
        ) &
        team_abbrev %in%
          espn_nba_team_abbrevs,
      valid_game_team =
        team_matches_game &
        team_abbrev_matches_game
    )

  # ---------------------------------------------------------------------------
  # 6H. Duplicate audit
  # ---------------------------------------------------------------------------

  exact_duplicate_rows <- duplicated(
    joined
  )

  natural_key <- joined %>%
    transmute(
      game_id,
      period_number,
      clock_display_value,
      team_id,
      athlete_id_1,
      type_id,
      coordinate_x,
      coordinate_y
    )

  natural_duplicate_rows <- duplicated(
    natural_key
  )

  # ---------------------------------------------------------------------------
  # 6I. Filter to structurally valid FGA
  # ---------------------------------------------------------------------------

  fga <- joined %>%
    filter(
      !is_free_throw_text,
      coordinate_in_court,
      valid_clock,
      valid_period,
      valid_shot_type,
      valid_team,
      valid_game_team
    ) %>%
    mutate(
      hoopr_season =
        hoopr_season,
      nba_season =
        nba_season,
      data_role =
        role,
      shot_id =
        paste0(
          hoopr_season,
          "_",
          source_row
        ),
      is_home_shooter =
        team_id ==
          home_team_id
    )

  # ---------------------------------------------------------------------------
  # 6J. Leakage audit source
  #
  # score_value is intentionally retained ONLY here for auditing.
  # It is NOT written into the processed modeling dataset.
  # ---------------------------------------------------------------------------

  leakage_source <- fga %>%
    transmute(
      shot_id,
      hoopr_season,
      nba_season,
      type_text,
      source_score_value =
        score_value,
      source_scoring_play =
        scoring_play
    )

  # ---------------------------------------------------------------------------
  # 6K. Processed modeling dataset
  # ---------------------------------------------------------------------------

  processed <- fga %>%
    transmute(
      shot_id,
      source_row,
      hoopr_season,
      nba_season,
      data_role,
      game_id,
      game_date,
      period_number,
      clock_display_value,
      clock_seconds_remaining,
      team_id,
      team_abbrev,
      is_home_shooter,
      home_team_id,
      home_team_abbrev,
      away_team_id,
      away_team_abbrev,
      shooter_id =
        athlete_id_1,
      shooter_name =
        athlete_name_1,
      type_id,
      type_text,
      coordinate_x,
      coordinate_y,
      coordinate_x_raw,
      coordinate_y_raw,
      court_x_folded,
      court_y_folded,
      distance_to_rim_ft,
      absolute_angle_rad,
      outcome =
        if (
          role !=
            "final_untouched_test"
        ) {
          as.integer(
            scoring_play
          )
        } else {
          NA_integer_
        }
    )

  if (
    role ==
      "final_untouched_test"
  ) {
    processed <- processed %>%
      select(
        -outcome
      )
  }

  # ---------------------------------------------------------------------------
  # 6L. Season audit
  # ---------------------------------------------------------------------------

  audit <- tibble(
    hoopr_season =
      hoopr_season,
    nba_season =
      nba_season,
    raw_rows =
      nrow(shots),
    raw_games =
      n_distinct(
        shots$game_id
      ),
    season_type_2_games =
      sum(
        schedule$season_type == 2L,
        na.rm = TRUE
      ),
    nba_cup_championship_games_excluded =
      sum(
        schedule$is_nba_cup_championship,
        na.rm = TRUE
      ),
    nba_regular_games =
      n_distinct(
        nba_regular_schedule$game_id
      ),
    rows_in_nba_regular_games =
      nrow(joined),
    free_throw_rows_in_regular_games =
      sum(
        joined$is_free_throw_text,
        na.rm = TRUE
      ),
    non_free_throw_rows =
      sum(
        !joined$is_free_throw_text,
        na.rm = TRUE
      ),
    invalid_coordinate_rows =
      sum(
        !joined$coordinate_in_court,
        na.rm = TRUE
      ),
    invalid_clock_rows =
      sum(
        !joined$valid_clock,
        na.rm = TRUE
      ),
    invalid_period_rows =
      sum(
        !joined$valid_period,
        na.rm = TRUE
      ),
    missing_or_blank_type_rows =
      sum(
        !joined$valid_shot_type,
        na.rm = TRUE
      ),
    invalid_team_rows =
      sum(
        !joined$valid_team,
        na.rm = TRUE
      ),
    game_team_mismatch_rows =
      sum(
        !joined$valid_game_team,
        na.rm = TRUE
      ),
    exact_duplicate_rows =
      sum(
        exact_duplicate_rows
      ),
    natural_key_duplicate_rows =
      sum(
        natural_duplicate_rows
      ),
    processed_fga_rows =
      nrow(processed),
    processed_games =
      n_distinct(
        processed$game_id
      ),
    processed_players =
      n_distinct(
        processed$shooter_id,
        na.rm = TRUE
      )
  )

  list(
    processed =
      processed,
    leakage_source =
      leakage_source,
    joined =
      joined,
    audit =
      audit,
    nba_regular_schedule =
      nba_regular_schedule,
    cup_final_audit =
      cup_final_audit
  )
}

# ---- 7. Run all four seasons -------------------------------------------------

clean_results <- pmap(
  season_plan,
  clean_one_season
)

names(clean_results) <- as.character(
  season_plan$hoopr_season
)

# ---- 8. Cleaning audit -------------------------------------------------------

cleaning_audit <- bind_rows(
  map(
    clean_results,
    "audit"
  )
)

readr::write_csv(
  cleaning_audit,
  "docs/validation/stage2_cleaning_audit.csv"
)

cat(
  "\n--- STAGE 2 CLEANING AUDIT ---\n"
)

print(
  cleaning_audit,
  n = Inf
)

# ---- 9. NBA Cup Championship audit ------------------------------------------

cup_final_audit <- imap_dfr(
  clean_results,
  function(result, season_key) {

    result$cup_final_audit %>%
      mutate(
        hoopr_season =
          as.integer(
            season_key
          ),
        .before = 1
      )
  }
)

readr::write_csv(
  cup_final_audit,
  "docs/validation/stage2_cup_final_audit.csv"
)

cat(
  "\n--- NBA CUP CHAMPIONSHIP EXCLUSION AUDIT ---\n"
)

print(
  cup_final_audit,
  n = Inf
)

# ---- 10. Regular-season game audit ------------------------------------------
#
# IMPORTANT:
#   The ESPN schedule release can include schedule rows that never appear in
#   the shot release (for example, cancelled/replaced schedule entries). Those
#   rows are useful metadata, but they are not part of the shot-modeling
#   population. Therefore we audit BOTH:
#
#   1. schedule_candidate_games = eligible schedule rows; and
#   2. games_with_shots        = eligible games actually represented by shots.
#
# The 1,230-game NBA regular-season check is applied to games_with_shots.

regular_game_audit <- imap_dfr(
  clean_results,
  function(result, season_key) {

    schedule_candidates <- result$nba_regular_schedule

    played_game_ids <- unique(
      result$processed$game_id
    )

    played_schedule <- schedule_candidates %>%
      filter(
        game_id %in% played_game_ids
      )

    tibble(
      hoopr_season =
        as.integer(
          season_key
        ),
      schedule_candidate_games =
        n_distinct(
          schedule_candidates$game_id
        ),
      games_with_shots =
        n_distinct(
          played_schedule$game_id
        ),
      schedule_only_games =
        n_distinct(
          setdiff(
            schedule_candidates$game_id,
            played_game_ids
          )
        ),
      unique_home_teams =
        n_distinct(
          played_schedule$home_team_abbrev
        ),
      unique_away_teams =
        n_distinct(
          played_schedule$away_team_abbrev
        ),
      unique_teams =
        n_distinct(
          c(
            played_schedule$home_team_abbrev,
            played_schedule$away_team_abbrev
          )
        ),
      first_game_date =
        min(
          played_schedule$game_date,
          na.rm = TRUE
        ),
      last_game_date =
        max(
          played_schedule$game_date,
          na.rm = TRUE
        )
    )
  }
)

schedule_only_game_audit <- imap_dfr(
  clean_results,
  function(result, season_key) {

    played_game_ids <- unique(
      result$processed$game_id
    )

    result$nba_regular_schedule %>%
      filter(
        !game_id %in% played_game_ids
      ) %>%
      transmute(
        hoopr_season =
          as.integer(
            season_key
          ),
        game_id,
        game_date,
        home_team_abbrev,
        away_team_abbrev,
        notes_headline,
        notes_type
      )
  }
)

readr::write_csv(
  regular_game_audit,
  "docs/validation/stage2_regular_game_audit.csv"
)

readr::write_csv(
  schedule_only_game_audit,
  "docs/validation/stage2_schedule_only_game_audit.csv"
)

cat(
  "\n--- NBA REGULAR-SEASON GAME AUDIT ---\n"
)

print(
  regular_game_audit,
  n = Inf
)

cat(
  "\n--- SCHEDULE-ONLY REGULAR-SEASON ENTRIES ---\n"
)

print(
  schedule_only_game_audit,
  n = Inf
)

# ---- 11. Coordinate / distance audit ----------------------------------------

coordinate_audit <- imap_dfr(
  clean_results,
  function(result, season_key) {

    df <- result$processed

    tibble(
      hoopr_season =
        as.integer(
          season_key
        ),
      n =
        nrow(df),
      x_min =
        min(
          df$coordinate_x,
          na.rm = TRUE
        ),
      x_max =
        max(
          df$coordinate_x,
          na.rm = TRUE
        ),
      y_min =
        min(
          df$coordinate_y,
          na.rm = TRUE
        ),
      y_max =
        max(
          df$coordinate_y,
          na.rm = TRUE
        ),
      folded_x_min =
        min(
          df$court_x_folded,
          na.rm = TRUE
        ),
      folded_x_max =
        max(
          df$court_x_folded,
          na.rm = TRUE
        ),
      distance_min =
        min(
          df$distance_to_rim_ft,
          na.rm = TRUE
        ),
      distance_p50 =
        median(
          df$distance_to_rim_ft,
          na.rm = TRUE
        ),
      distance_p95 =
        quantile(
          df$distance_to_rim_ft,
          0.95,
          na.rm = TRUE,
          names = FALSE
        ),
      distance_p99 =
        quantile(
          df$distance_to_rim_ft,
          0.99,
          na.rm = TRUE,
          names = FALSE
        ),
      distance_max =
        max(
          df$distance_to_rim_ft,
          na.rm = TRUE
        )
    )
  }
)

readr::write_csv(
  coordinate_audit,
  "docs/validation/stage2_coordinate_audit.csv"
)

cat(
  "\n--- COORDINATE / DISTANCE AUDIT ---\n"
)

print(
  coordinate_audit,
  n = Inf
)

# ---- 12. Shot-type audit -----------------------------------------------------

shot_type_audit <- imap_dfr(
  clean_results,
  function(result, season_key) {

    result$processed %>%
      count(
        type_id,
        type_text,
        name = "shots",
        sort = TRUE
      ) %>%
      mutate(
        hoopr_season =
          as.integer(
            season_key
          ),
        .before = 1
      )
  }
)

readr::write_csv(
  shot_type_audit,
  "docs/validation/stage2_field_goal_shot_types.csv"
)

cat(
  "\n--- TOP FIELD-GOAL SHOT TYPES BY SEASON ---\n"
)

shot_type_audit %>%
  group_by(
    hoopr_season
  ) %>%
  slice_max(
    order_by = shots,
    n = 20,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  print(
    n = Inf
  )

shot_type_presence <- shot_type_audit %>%
  distinct(
    hoopr_season,
    type_id,
    type_text
  ) %>%
  mutate(
    present = TRUE
  ) %>%
  tidyr::pivot_wider(
    names_from =
      hoopr_season,
    values_from =
      present,
    names_prefix =
      "season_",
    values_fill =
      FALSE
  )

readr::write_csv(
  shot_type_presence,
  "docs/validation/stage2_shot_type_presence.csv"
)

# ---- 13. type_text leakage audit --------------------------------------------

development_type_source <- bind_rows(
  clean_results[["2023"]]$leakage_source,
  clean_results[["2024"]]$leakage_source,
  clean_results[["2025"]]$leakage_source
)

type_text_leakage_audit <- development_type_source %>%
  mutate(
    contains_outcome_language =
      !is.na(
        type_text
      ) &
      grepl(
        "\\b(made|make|miss|missed)\\b",
        type_text,
        ignore.case = TRUE
      )
  ) %>%
  count(
    type_text,
    contains_outcome_language,
    name = "rows",
    sort = TRUE
  )

readr::write_csv(
  type_text_leakage_audit,
  "docs/validation/stage2_type_text_leakage_audit.csv"
)

cat(
  "\n--- TYPE_TEXT LEAKAGE AUDIT ---\n"
)

type_text_leakage_audit %>%
  filter(
    contains_outcome_language
  ) %>%
  print(
    n = Inf
  )

type_text_outcome_language_rows <- sum(
  type_text_leakage_audit$rows[
    type_text_leakage_audit$
      contains_outcome_language
  ]
)

cat(
  "Development non-FT rows whose type_text contains outcome language: ",
  type_text_outcome_language_rows,
  "\n",
  sep = ""
)

# ---- 14. score_value leakage audit ------------------------------------------

score_value_outcome_audit <- development_type_source %>%
  transmute(
    outcome =
      as.integer(
        source_scoring_play
      ),
    source_score_value
  ) %>%
  count(
    outcome,
    source_score_value,
    name = "shots",
    sort = TRUE
  ) %>%
  group_by(
    outcome
  ) %>%
  mutate(
    pct_within_outcome =
      shots /
      sum(
        shots
      )
  ) %>%
  ungroup()

readr::write_csv(
  score_value_outcome_audit,
  "docs/validation/stage2_score_value_outcome_audit.csv"
)

cat(
  "\n--- SCORE_VALUE / OUTCOME AUDIT ---\n"
)

print(
  score_value_outcome_audit,
  n = Inf
)

cat(
  "\nIMPORTANT: score_value is intentionally EXCLUDED from processed data.\n"
)

# ---- 15. Development dataset -------------------------------------------------

development_processed <- bind_rows(
  clean_results[["2023"]]$processed,
  clean_results[["2024"]]$processed,
  clean_results[["2025"]]$processed
)

development_outcome_audit <- development_processed %>%
  group_by(
    hoopr_season,
    nba_season
  ) %>%
  summarise(
    shots =
      n(),
    made =
      sum(
        outcome == 1L
      ),
    missed =
      sum(
        outcome == 0L
      ),
    make_rate =
      mean(
        outcome
      ),
    .groups =
      "drop"
  )

readr::write_csv(
  development_outcome_audit,
  "docs/validation/stage2_clean_development_outcomes.csv"
)

cat(
  "\n--- CLEAN DEVELOPMENT OUTCOME AUDIT ---\n"
)

print(
  development_outcome_audit,
  n = Inf
)

cat(
  "\n2025-26 outcome remains intentionally unreported.\n"
)

# ---- 16. Final-test dataset --------------------------------------------------

final_test_processed <-
  clean_results[["2026"]]$processed

# ---- 17. Save corrected processed datasets ----------------------------------

saveRDS(
  development_processed,
  "data/processed/shots_development.rds",
  compress = "xz"
)

saveRDS(
  final_test_processed,
  "data/processed/shots_final_test.rds",
  compress = "xz"
)

processed_manifest <- tibble(
  dataset = c(
    "shots_development.rds",
    "shots_final_test.rds"
  ),
  seasons = c(
    "2023,2024,2025",
    "2026"
  ),
  rows = c(
    nrow(
      development_processed
    ),
    nrow(
      final_test_processed
    )
  ),
  columns = c(
    ncol(
      development_processed
    ),
    ncol(
      final_test_processed
    )
  ),
  outcome_included = c(
    TRUE,
    FALSE
  )
)

readr::write_csv(
  processed_manifest,
  "docs/validation/stage2_processed_manifest.csv"
)

cat(
  "\n--- PROCESSED DATA MANIFEST ---\n"
)

print(
  processed_manifest,
  n = Inf
)

# ---- 18. Final verification --------------------------------------------------

dev_outcome_binary <- setequal(
  unique(
    development_processed$outcome
  ),
  c(
    0L,
    1L
  )
)

dev_unique_ids <- anyDuplicated(
  development_processed$shot_id
) == 0

test_unique_ids <- anyDuplicated(
  final_test_processed$shot_id
) == 0

dev_coordinates_valid <- all(
  development_processed$coordinate_x >=
    court_x_min &
  development_processed$coordinate_x <=
    court_x_max &
  development_processed$coordinate_y >=
    court_y_min &
  development_processed$coordinate_y <=
    court_y_max
)

test_coordinates_valid <- all(
  final_test_processed$coordinate_x >=
    court_x_min &
  final_test_processed$coordinate_x <=
    court_x_max &
  final_test_processed$coordinate_y >=
    court_y_min &
  final_test_processed$coordinate_y <=
    court_y_max
)

test_outcome_hidden <-
  !(
    "outcome" %in%
      names(
        final_test_processed
      )
  ) &&
  !(
    "scoring_play" %in%
      names(
        final_test_processed
      )
  )

score_value_removed <-
  !(
    "source_score_value" %in%
      names(
        development_processed
      )
  ) &&
  !(
    "source_score_value" %in%
      names(
        final_test_processed
      )
  ) &&
  !(
    "score_value" %in%
      names(
        development_processed
      )
  ) &&
  !(
    "score_value" %in%
      names(
        final_test_processed
      )
  )

dev_no_free_throw_text <- !any(
  grepl(
    "Free Throw",
    development_processed$type_text,
    ignore.case = TRUE
  ),
  na.rm = TRUE
)

test_no_free_throw_text <- !any(
  grepl(
    "Free Throw",
    final_test_processed$type_text,
    ignore.case = TRUE
  ),
  na.rm = TRUE
)

all_30_teams <- all(
  regular_game_audit$unique_teams ==
    30L
)

expected_cup_finals_detected <- all(
  known_nonstandings_cup_finals$hoopr_season %in%
    cup_final_audit$hoopr_season
)

# Source-coverage validation.
#
# The hoopR/SportsDataverse shot release contains 1,230 played regular-season
# games with shots for 2022-23 through 2024-25, but 1,229 for 2025-26.
#
# Diagnostic work identified the single missing played game as:
#   ESPN game_id 401810469
#   2026-01-20
#   LAC at CHI
#
# The schedule entry is present, but the season-level hoopR shot release has no
# shot rows for that game. A direct hoopR::espn_nba_pbp() request returned NULL
# during manual verification, so this project documents the source gap rather
# than mixing a different provider into the modeling dataset.

expected_played_games_with_shots <- tibble(
  hoopr_season = c(2023L, 2024L, 2025L, 2026L),
  expected_games_with_shots = c(1230L, 1230L, 1230L, 1229L)
)

coverage_validation <- regular_game_audit %>%
  select(
    hoopr_season,
    games_with_shots
  ) %>%
  left_join(
    expected_played_games_with_shots,
    by = "hoopr_season"
  ) %>%
  mutate(
    coverage_matches_expected =
      games_with_shots ==
      expected_games_with_shots
  )

known_2026_gap <- schedule_only_game_audit %>%
  filter(
    hoopr_season == 2026L,
    game_id == 401810469L,
    game_date == as.Date("2026-01-20"),
    home_team_abbrev == "CHI",
    away_team_abbrev == "LAC"
  )

known_2026_gap_documented <-
  nrow(known_2026_gap) == 1L

all_expected_game_coverage <- all(
  coverage_validation$coverage_matches_expected
)

source_coverage_gap_audit <- tibble(
  hoopr_season = 2026L,
  nba_season = "2025-26",
  espn_game_id = 401810469L,
  game_date = as.Date("2026-01-20"),
  away_team = "LAC",
  home_team = "CHI",
  expected_regular_season_games = 1230L,
  games_available_in_hoopr_shot_release = 1229L,
  missing_games = 1L,
  treatment = paste0(
    "Document source gap; do not impute or mix providers. ",
    "Game is present in schedule metadata but absent from hoopR shot release."
  )
)

readr::write_csv(
  source_coverage_gap_audit,
  "docs/validation/stage2_source_coverage_gap.csv"
)

cat(
  "\n--- SOURCE COVERAGE VALIDATION ---\n"
)

print(
  coverage_validation,
  n = Inf
)

cat(
  "\n--- DOCUMENTED 2025-26 SOURCE GAP ---\n"
)

print(
  source_coverage_gap_audit,
  n = Inf
)

stage2_clean_checks <- tibble(
  check = c(
    "Development dataset is non-empty",
    "Final-test dataset is non-empty",
    "Development outcome is binary 0/1",
    "Development shot_id is unique",
    "Final-test shot_id is unique",
    "Development coordinates are inside NBA court bounds",
    "Final-test coordinates are inside NBA court bounds",
    "Development dataset contains no free throws",
    "Final-test dataset contains no free throws",
    "All 30 NBA teams are represented every season",
    "All three non-standings NBA Cup finals are detected",
    "Played-game coverage matches documented hoopR release by season",
    "Known 2025-26 one-game source gap is documented exactly once",
    "score_value leakage is absent from processed datasets",
    "Final-test processed data contains no outcome/scoring_play",
    "Processed datasets are gitignored"
  ),
  passed = c(
    nrow(
      development_processed
    ) > 0,
    nrow(
      final_test_processed
    ) > 0,
    dev_outcome_binary,
    dev_unique_ids,
    test_unique_ids,
    dev_coordinates_valid,
    test_coordinates_valid,
    dev_no_free_throw_text,
    test_no_free_throw_text,
    all_30_teams,
    expected_cup_finals_detected,
    all_expected_game_coverage,
    known_2026_gap_documented,
    score_value_removed,
    test_outcome_hidden,
    file.exists(
      ".gitignore"
    ) &&
      any(
        trimws(
          readLines(
            ".gitignore",
            warn = FALSE
          )
        ) ==
          "data/processed/"
      )
  )
)

cat(
  "\n--- STAGE 2 CLEANING VERIFICATION ---\n"
)

print(
  stage2_clean_checks,
  n = Inf
)

if (!all(stage2_clean_checks$passed)) {
  stop(
    paste0(
      "One or more corrected Stage 2 cleaning checks failed. ",
      "Review the audit outputs before continuing."
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
    "\nUpdated renv.lock after corrected Stage 2 cleaning.\n"
  )
}

cat(
  "\nSTAGE 2 CLEANING COMPLETE.\n",
  "The modeling population now uses all 30 NBA franchises, excludes the ",
  "NBA Cup Championship, removes score_value leakage, and explicitly ",
  "documents the one-game 2025-26 hoopR shot-release coverage gap.\n",
  "Do not begin Stage 3 until the corrected audit outputs are reviewed.\n",
  sep = ""
)
