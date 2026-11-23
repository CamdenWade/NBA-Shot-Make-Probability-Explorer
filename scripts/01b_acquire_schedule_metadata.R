# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 2 Addendum — Schedule Metadata Acquisition
# File: scripts/01b_acquire_schedule_metadata.R
#
# Why this addendum exists:
#   The public hoopR shot release contains 20 shot-level columns but does NOT
#   include season_type or game_date. The Stage 2 audit also showed roughly
#   1,320 games per season, which is more than an NBA regular season alone.
#
#   To preserve the project's locked scope of REGULAR-SEASON field-goal
#   attempts, we acquire the corresponding hoopR schedule releases and use
#   game_id to identify which shot rows belong to season_type == 2.
#
# Important:
#   - This script does NOT filter or transform the shot data yet.
#   - Raw schedule files are saved under data/raw/ and remain gitignored.
#   - The 2025-26 season is still not used for outcome/model-performance review.
# =============================================================================

required_packages <- c(
  "hoopR",
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

if (!exists(
  "load_nba_schedule",
  envir = asNamespace("hoopR"),
  mode = "function"
)) {
  stop(
    "The installed hoopR version does not expose load_nba_schedule(). ",
    "Update hoopR and rerun this script."
  )
}

# -----------------------------------------------------------------------------
# 1. Locked seasons
# -----------------------------------------------------------------------------

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
  )
)

# -----------------------------------------------------------------------------
# 2. Confirm Stage 2 shot files exist
# -----------------------------------------------------------------------------

shot_paths <- file.path(
  "data",
  "raw",
  paste0(
    "nba_shots_",
    season_plan$hoopr_season,
    ".rds"
  )
)

if (!all(file.exists(shot_paths))) {
  stop(
    paste0(
      "One or more Stage 2 raw shot files are missing. ",
      "Run scripts/01_acquire_data.R before this addendum."
    )
  )
}

# -----------------------------------------------------------------------------
# 3. Download schedule metadata one season at a time
# -----------------------------------------------------------------------------

schedule_results <- vector(
  "list",
  nrow(season_plan)
)

names(schedule_results) <- as.character(
  season_plan$hoopr_season
)

for (i in seq_len(nrow(season_plan))) {

  hoopr_season <- season_plan$hoopr_season[[i]]
  nba_season <- season_plan$nba_season[[i]]

  cat(
    "\n============================================================\n",
    "Downloading schedule metadata for hoopR season ",
    hoopr_season,
    " (NBA ",
    nba_season,
    ")\n",
    "============================================================\n",
    sep = ""
  )

  schedule <- hoopR::load_nba_schedule(
    seasons = hoopr_season
  )

  if (!inherits(schedule, "data.frame")) {
    stop(
      "load_nba_schedule() did not return a data frame for season ",
      hoopr_season,
      "."
    )
  }

  if (nrow(schedule) == 0) {
    stop(
      "load_nba_schedule() returned zero rows for season ",
      hoopr_season,
      "."
    )
  }

  required_schedule_columns <- c(
    "game_id",
    "season_type"
  )

  missing_required <- setdiff(
    required_schedule_columns,
    names(schedule)
  )

  if (length(missing_required) > 0) {
    stop(
      paste0(
        "Schedule release ",
        hoopr_season,
        " is missing required columns: ",
        paste(
          missing_required,
          collapse = ", "
        )
      )
    )
  }

  raw_path <- file.path(
    "data",
    "raw",
    paste0(
      "nba_schedule_",
      hoopr_season,
      ".rds"
    )
  )

  saveRDS(
    schedule,
    raw_path,
    compress = "xz"
  )

  schedule_check <- readRDS(
    raw_path
  )

  stopifnot(
    nrow(schedule_check) ==
      nrow(schedule),
    identical(
      names(schedule_check),
      names(schedule)
    )
  )

  schedule_results[[i]] <- schedule

  cat(
    "Rows:    ",
    format(
      nrow(schedule),
      big.mark = ","
    ),
    "\nColumns: ",
    ncol(schedule),
    "\nSaved:   ",
    raw_path,
    "\n",
    sep = ""
  )
}

# -----------------------------------------------------------------------------
# 4. Schedule season-type audit
# -----------------------------------------------------------------------------

schedule_type_audit <- imap_dfr(
  schedule_results,
  function(schedule, season_key) {

    schedule %>%
      mutate(
        season_type =
          as.integer(
            season_type
          )
      ) %>%
      count(
        season_type,
        name = "games",
        sort = TRUE
      ) %>%
      mutate(
        hoopr_season =
          as.integer(
            season_key
          ),
        season_type_label =
          case_when(
            season_type == 1L ~
              "preseason",
            season_type == 2L ~
              "regular_season",
            season_type == 3L ~
              "postseason",
            season_type == 4L ~
              "offseason",
            TRUE ~
              "other_or_unknown"
          ),
        .before = 1
      )
  }
)

readr::write_csv(
  schedule_type_audit,
  "docs/validation/stage2_schedule_season_type_audit.csv"
)

cat(
  "\n--- SCHEDULE SEASON TYPE AUDIT ---\n"
)

print(
  schedule_type_audit,
  n = Inf
)

# -----------------------------------------------------------------------------
# 5. Validate game_id uniqueness in schedule releases
# -----------------------------------------------------------------------------

schedule_game_id_audit <- imap_dfr(
  schedule_results,
  function(schedule, season_key) {

    tibble(
      hoopr_season =
        as.integer(
          season_key
        ),
      schedule_rows =
        nrow(schedule),
      unique_game_ids =
        dplyr::n_distinct(
          schedule$game_id
        ),
      duplicated_game_id_rows =
        sum(
          duplicated(
            schedule$game_id
          )
        ),
      missing_game_id_rows =
        sum(
          is.na(
            schedule$game_id
          )
        )
    )
  }
)

readr::write_csv(
  schedule_game_id_audit,
  "docs/validation/stage2_schedule_game_id_audit.csv"
)

cat(
  "\n--- SCHEDULE GAME-ID AUDIT ---\n"
)

print(
  schedule_game_id_audit,
  n = Inf
)

# -----------------------------------------------------------------------------
# 6. Shot-to-schedule game coverage audit
# -----------------------------------------------------------------------------

shot_schedule_coverage <- map2_dfr(
  season_plan$hoopr_season,
  seq_len(nrow(season_plan)),
  function(hoopr_season, i) {

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

    schedule <- schedule_results[[i]]

    shot_game_ids <- unique(
      shots$game_id
    )

    schedule_game_ids <- unique(
      schedule$game_id
    )

    unmatched_shot_game_ids <- setdiff(
      shot_game_ids,
      schedule_game_ids
    )

    regular_game_ids <- schedule %>%
      filter(
        as.integer(
          season_type
        ) == 2L
      ) %>%
      pull(
        game_id
      ) %>%
      unique()

    tibble(
      hoopr_season =
        hoopr_season,
      shot_rows =
        nrow(shots),
      shot_games =
        length(
          shot_game_ids
        ),
      schedule_games =
        length(
          schedule_game_ids
        ),
      unmatched_shot_games =
        length(
          unmatched_shot_game_ids
        ),
      matched_shot_games =
        length(
          intersect(
            shot_game_ids,
            schedule_game_ids
          )
        ),
      regular_season_games_with_shots =
        length(
          intersect(
            shot_game_ids,
            regular_game_ids
          )
        ),
      raw_shot_rows_in_regular_season_games =
        sum(
          shots$game_id %in%
            regular_game_ids
        )
    )
  }
)

readr::write_csv(
  shot_schedule_coverage,
  "docs/validation/stage2_shot_schedule_coverage.csv"
)

cat(
  "\n--- SHOT / SCHEDULE COVERAGE AUDIT ---\n"
)

print(
  shot_schedule_coverage,
  n = Inf
)

# -----------------------------------------------------------------------------
# 7. Optional game-date availability audit
# -----------------------------------------------------------------------------

schedule_date_audit <- imap_dfr(
  schedule_results,
  function(schedule, season_key) {

    tibble(
      hoopr_season =
        as.integer(
          season_key
        ),
      game_date_present =
        "game_date" %in%
          names(schedule),
      game_date_missing =
        if (
          "game_date" %in%
            names(schedule)
        ) {
          sum(
            is.na(
              schedule$game_date
            )
          )
        } else {
          NA_integer_
        },
      first_game_date =
        if (
          "game_date" %in%
            names(schedule) &&
          !all(
            is.na(
              schedule$game_date
            )
          )
        ) {
          as.character(
            min(
              schedule$game_date,
              na.rm = TRUE
            )
          )
        } else {
          NA_character_
        },
      last_game_date =
        if (
          "game_date" %in%
            names(schedule) &&
          !all(
            is.na(
              schedule$game_date
            )
          )
        ) {
          as.character(
            max(
              schedule$game_date,
              na.rm = TRUE
            )
          )
        } else {
          NA_character_
        }
    )
  }
)

cat(
  "\n--- SCHEDULE DATE AUDIT ---\n"
)

print(
  schedule_date_audit,
  n = Inf
)

readr::write_csv(
  schedule_date_audit,
  "docs/validation/stage2_schedule_date_audit.csv"
)

# -----------------------------------------------------------------------------
# 8. Stage 2 addendum verification
# -----------------------------------------------------------------------------

schedule_paths <- file.path(
  "data",
  "raw",
  paste0(
    "nba_schedule_",
    season_plan$hoopr_season,
    ".rds"
  )
)

all_schedule_files_exist <- all(
  file.exists(
    schedule_paths
  )
)

all_schedule_game_ids_unique <- all(
  schedule_game_id_audit$
    duplicated_game_id_rows == 0
)

all_shot_games_match_schedule <- all(
  shot_schedule_coverage$
    unmatched_shot_games == 0
)

regular_season_identified <- all(
  schedule_type_audit %>%
    filter(
      season_type == 2L
    ) %>%
    count(
      hoopr_season
    ) %>%
    pull(
      n
    ) >= 1
) &&
  dplyr::n_distinct(
    schedule_type_audit$
      hoopr_season[
        schedule_type_audit$
          season_type == 2L
      ]
  ) ==
  4L

stage2_schedule_checks <- tibble(
  check = c(
    "All four raw schedule RDS files exist",
    "Schedule game_id is unique within each season",
    "Every shot game_id matches schedule metadata",
    "Regular-season games identified for all four seasons",
    "2025-26 still used only for structural metadata"
  ),
  passed = c(
    all_schedule_files_exist,
    all_schedule_game_ids_unique,
    all_shot_games_match_schedule,
    regular_season_identified,
    TRUE
  )
)

cat(
  "\n--- STAGE 2 SCHEDULE-METADATA VERIFICATION ---\n"
)

print(
  stage2_schedule_checks,
  n = Inf
)

if (!all(stage2_schedule_checks$passed)) {
  stop(
    paste0(
      "One or more schedule-metadata checks failed. ",
      "Do not begin cleaning until the issue is reviewed."
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
    "\nUpdated renv.lock after schedule metadata acquisition.\n"
  )
}

cat(
  "\nSTAGE 2 METADATA ADDENDUM COMPLETE.\n",
  "We can now identify regular-season games by joining schedule metadata ",
  "to shot rows on game_id.\n",
  "Next step after reviewing these audits: scripts/02_clean_validate.R.\n",
  sep = ""
)
