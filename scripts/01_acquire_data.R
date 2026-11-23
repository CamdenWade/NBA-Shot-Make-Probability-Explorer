# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 2 — Data Acquisition
# File: scripts/01_acquire_data.R
#
# Purpose:
#   Download the four locked NBA shot seasons from SportsDataverse/hoopR,
#   preserve each raw season locally as an RDS file, record provenance, and
#   audit the returned schema BEFORE making any modeling/filtering decisions.
#
# Locked design:
#   hoopR season 2023 = NBA 2022-23
#   hoopR season 2024 = NBA 2023-24
#   hoopR season 2025 = NBA 2024-25
#   hoopR season 2026 = NBA 2025-26 FINAL UNTOUCHED TEST
#
# Important:
#   - Raw releases are saved unchanged in data/raw/.
#   - Do NOT filter free throws, postseason, coordinates, or outcomes here.
#   - Do NOT calculate final-test (2025-26) shooting performance here.
#   - data/raw/ should already be excluded by .gitignore.
# =============================================================================

required_packages <- c("hoopR", "dplyr", "purrr", "readr", "tibble")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

if (!exists("load_nba_shots", envir = asNamespace("hoopR"), mode = "function")) {
  stop(
    "The installed hoopR version does not expose load_nba_shots(). ",
    "Update hoopR and rerun this script."
  )
}

required_dirs <- c("scripts", "data/raw", "docs/validation")

missing_dirs <- required_dirs[
  !vapply(required_dirs, dir.exists, logical(1))
]

if (length(missing_dirs) > 0) {
  stop(
    paste0(
      "Run scripts/00_project_setup.R first. Missing directories: ",
      paste(missing_dirs, collapse = ", ")
    )
  )
}

if (!file.exists(".gitignore")) {
  stop(".gitignore is missing. Run Stage 1 setup before acquiring data.")
}

gitignore_lines <- readLines(".gitignore", warn = FALSE)

if (!any(grepl("^data/raw/?$", trimws(gitignore_lines)))) {
  stop(
    "data/raw/ is not excluded by .gitignore. ",
    "Fix this before downloading raw NBA data."
  )
}

season_plan <- tibble(
  hoopr_season = c(2023L, 2024L, 2025L, 2026L),
  nba_season = c("2022-23", "2023-24", "2024-25", "2025-26"),
  role = c(
    "development_train",
    "development_validation",
    "development_validation",
    "final_untouched_test"
  ),
  inspect_outcomes = c(TRUE, TRUE, TRUE, FALSE)
)

cat("\n--- LOCKED SEASON PLAN ---\n")
print(season_plan, n = Inf)

source_name <- "SportsDataverse / hoopR NBA shots"
source_loader <- "hoopR::load_nba_shots()"
source_family <- "ESPN NBA play-by-play shot releases"
source_license <- "CC BY 4.0 (hoopR-nba-data repository)"
acquired_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
hoopr_version <- as.character(utils::packageVersion("hoopR"))
r_version <- paste(R.version$major, R.version$minor, sep = ".")

cat("\n--- ACQUISITION ENVIRONMENT ---\n")
cat(
  "hoopR version: ", hoopr_version, "\n",
  "R version:     ", r_version, "\n",
  "Acquired UTC:  ", acquired_at_utc, "\n",
  sep = ""
)

safe_n_unique <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE)
}

missing_rate_if_exists <- function(df, column) {
  if (!(column %in% names(df))) return(NA_real_)
  mean(is.na(df[[column]]))
}

range_text_if_exists <- function(df, column) {
  if (!(column %in% names(df))) return(NA_character_)
  x <- df[[column]]
  if (all(is.na(x))) return(NA_character_)
  paste(range(x, na.rm = TRUE), collapse = " to ")
}

season_results <- vector("list", nrow(season_plan))
names(season_results) <- as.character(season_plan$hoopr_season)

for (i in seq_len(nrow(season_plan))) {
  hoopr_season <- season_plan$hoopr_season[[i]]
  nba_season <- season_plan$nba_season[[i]]
  season_role <- season_plan$role[[i]]

  cat(
    "\n============================================================\n",
    "Downloading hoopR season ", hoopr_season,
    " (NBA ", nba_season, ")\n",
    "Role: ", season_role, "\n",
    "============================================================\n",
    sep = ""
  )

  shots <- hoopR::load_nba_shots(seasons = hoopr_season)

  if (!inherits(shots, "data.frame")) {
    stop(
      "load_nba_shots() did not return a data frame for season ",
      hoopr_season,
      "."
    )
  }

  if (nrow(shots) == 0) {
    stop(
      "load_nba_shots() returned zero rows for season ",
      hoopr_season,
      ". Check release availability before continuing."
    )
  }

  raw_path <- file.path(
    "data",
    "raw",
    paste0("nba_shots_", hoopr_season, ".rds")
  )

  saveRDS(shots, file = raw_path, compress = "xz")

  raw_check <- readRDS(raw_path)

  stopifnot(
    nrow(raw_check) == nrow(shots),
    identical(names(raw_check), names(shots))
  )

  season_results[[i]] <- shots

  cat(
    "Rows:    ", format(nrow(shots), big.mark = ","), "\n",
    "Columns: ", ncol(shots), "\n",
    "Saved:   ", raw_path, "\n",
    sep = ""
  )
}

manifest <- map2_dfr(
  season_results,
  seq_len(nrow(season_plan)),
  function(shots, i) {
    hoopr_season <- season_plan$hoopr_season[[i]]

    raw_path <- file.path(
      "data",
      "raw",
      paste0("nba_shots_", hoopr_season, ".rds")
    )

    tibble(
      hoopr_season = hoopr_season,
      nba_season = season_plan$nba_season[[i]],
      role = season_plan$role[[i]],
      source_name = source_name,
      source_loader = source_loader,
      source_family = source_family,
      source_license = source_license,
      hoopR_version = hoopr_version,
      R_version = r_version,
      acquired_at_utc = acquired_at_utc,
      rows = nrow(shots),
      columns = ncol(shots),
      file_path = raw_path,
      file_size_mb = as.numeric(file.info(raw_path)$size) / 1024^2
    )
  }
)

readr::write_csv(
  manifest,
  "docs/validation/stage2_acquisition_manifest.csv"
)

cat("\n--- ACQUISITION MANIFEST ---\n")
print(
  manifest %>%
    select(
      hoopr_season,
      nba_season,
      role,
      rows,
      columns,
      file_size_mb
    ),
  n = Inf
)

schema_by_season <- imap_dfr(
  season_results,
  function(shots, season_key) {
    tibble(
      hoopr_season = as.integer(season_key),
      column = names(shots),
      r_class = vapply(
        shots,
        function(x) paste(class(x), collapse = "/"),
        character(1)
      )
    )
  }
)

readr::write_csv(
  schema_by_season,
  "docs/validation/stage2_schema_by_season.csv"
)

all_columns <- sort(unique(schema_by_season$column))

column_presence <- map_dfr(
  all_columns,
  function(column_name) {
    tibble(
      column = column_name,
      season_2023 = column_name %in% names(season_results[["2023"]]),
      season_2024 = column_name %in% names(season_results[["2024"]]),
      season_2025 = column_name %in% names(season_results[["2025"]]),
      season_2026 = column_name %in% names(season_results[["2026"]])
    )
  }
)

readr::write_csv(
  column_presence,
  "docs/validation/stage2_column_presence.csv"
)

cat("\n--- COLUMN PRESENCE ACROSS SEASONS ---\n")
print(column_presence, n = Inf)

key_columns <- c(
  "game_id",
  "season",
  "season_type",
  "period_number",
  "clock_display_value",
  "team_id",
  "team_abbrev",
  "athlete_id_1",
  "athlete_name_1",
  "type_text",
  "type_abbreviation",
  "scoring_play",
  "score_value",
  "shooting_play",
  "coordinate_x",
  "coordinate_y",
  "game_date"
)

key_field_audit <- imap_dfr(
  season_results,
  function(shots, season_key) {
    map_dfr(
      key_columns,
      function(column_name) {
        tibble(
          hoopr_season = as.integer(season_key),
          column = column_name,
          present = column_name %in% names(shots),
          missing_n = if (column_name %in% names(shots)) {
            sum(is.na(shots[[column_name]]))
          } else {
            NA_integer_
          },
          missing_pct = if (column_name %in% names(shots)) {
            mean(is.na(shots[[column_name]]))
          } else {
            NA_real_
          },
          unique_nonmissing = if (column_name %in% names(shots)) {
            safe_n_unique(shots[[column_name]])
          } else {
            NA_integer_
          }
        )
      }
    )
  }
)

readr::write_csv(
  key_field_audit,
  "docs/validation/stage2_key_field_audit.csv"
)

cat("\n--- KEY-FIELD AUDIT ---\n")
print(key_field_audit, n = Inf)

season_structure <- imap_dfr(
  season_results,
  function(shots, season_key) {
    tibble(
      hoopr_season = as.integer(season_key),
      rows = nrow(shots),
      games = if ("game_id" %in% names(shots)) {
        safe_n_unique(shots$game_id)
      } else {
        NA_integer_
      },
      players = if ("athlete_id_1" %in% names(shots)) {
        safe_n_unique(shots$athlete_id_1)
      } else {
        NA_integer_
      },
      teams = if ("team_id" %in% names(shots)) {
        safe_n_unique(shots$team_id)
      } else {
        NA_integer_
      },
      coordinate_x_missing_pct =
        missing_rate_if_exists(shots, "coordinate_x"),
      coordinate_y_missing_pct =
        missing_rate_if_exists(shots, "coordinate_y"),
      game_date_range =
        range_text_if_exists(shots, "game_date")
    )
  }
) %>%
  left_join(season_plan, by = "hoopr_season") %>%
  select(
    hoopr_season,
    nba_season,
    role,
    everything(),
    -inspect_outcomes
  )

readr::write_csv(
  season_structure,
  "docs/validation/stage2_season_structure.csv"
)

cat("\n--- SEASON STRUCTURE ---\n")
print(season_structure, n = Inf)

season_type_audit <- imap_dfr(
  season_results,
  function(shots, season_key) {
    if (!("season_type" %in% names(shots))) {
      return(
        tibble(
          hoopr_season = as.integer(season_key),
          season_type = NA_character_,
          n = NA_integer_
        )
      )
    }

    shots %>%
      mutate(season_type = as.character(season_type)) %>%
      count(season_type, name = "n", sort = TRUE) %>%
      mutate(
        hoopr_season = as.integer(season_key),
        .before = 1
      )
  }
)

readr::write_csv(
  season_type_audit,
  "docs/validation/stage2_season_type_audit.csv"
)

cat("\n--- SEASON TYPE AUDIT ---\n")
print(season_type_audit, n = Inf)

shot_type_structure <- imap_dfr(
  season_results,
  function(shots, season_key) {
    if (!("type_text" %in% names(shots))) {
      return(
        tibble(
          hoopr_season = as.integer(season_key),
          rows = nrow(shots),
          type_text_present = FALSE,
          free_throw_rows = NA_integer_,
          free_throw_pct = NA_real_,
          unique_type_text = NA_integer_
        )
      )
    }

    type_text_character <- as.character(shots$type_text)

    is_free_throw <- grepl(
      "Free Throw",
      type_text_character,
      ignore.case = TRUE
    )

    tibble(
      hoopr_season = as.integer(season_key),
      rows = nrow(shots),
      type_text_present = TRUE,
      free_throw_rows = sum(is_free_throw, na.rm = TRUE),
      free_throw_pct = mean(is_free_throw, na.rm = TRUE),
      unique_type_text = safe_n_unique(type_text_character)
    )
  }
)

readr::write_csv(
  shot_type_structure,
  "docs/validation/stage2_shot_type_structure.csv"
)

cat("\n--- SHOT TYPE / FREE-THROW STRUCTURE ---\n")
print(shot_type_structure, n = Inf)

development_outcome_audit <- map2_dfr(
  season_results,
  seq_len(nrow(season_plan)),
  function(shots, i) {
    if (!isTRUE(season_plan$inspect_outcomes[[i]])) {
      return(NULL)
    }

    if (!("scoring_play" %in% names(shots))) {
      return(
        tibble(
          hoopr_season = season_plan$hoopr_season[[i]],
          nba_season = season_plan$nba_season[[i]],
          rows = nrow(shots),
          scoring_play_missing = NA_integer_,
          raw_scoring_play_rate = NA_real_
        )
      )
    }

    tibble(
      hoopr_season = season_plan$hoopr_season[[i]],
      nba_season = season_plan$nba_season[[i]],
      rows = nrow(shots),
      scoring_play_missing = sum(is.na(shots$scoring_play)),
      raw_scoring_play_rate = mean(
        shots$scoring_play,
        na.rm = TRUE
      )
    )
  }
)

readr::write_csv(
  development_outcome_audit,
  "docs/validation/stage2_development_outcome_audit.csv"
)

cat("\n--- DEVELOPMENT-SEASON OUTCOME AUDIT ---\n")
print(development_outcome_audit, n = Inf)

cat(
  "\nNOTE: 2025-26 outcome performance was intentionally not summarized.\n"
)

schema_signature <- imap_dfr(
  season_results,
  function(shots, season_key) {
    tibble(
      hoopr_season = as.integer(season_key),
      columns_n = ncol(shots),
      column_signature = paste(names(shots), collapse = "|")
    )
  }
)

all_schema_identical <- length(
  unique(schema_signature$column_signature)
) == 1

cat("\n--- CROSS-SEASON SCHEMA CHECK ---\n")
cat(
  "All four seasons have identical column names/order: ",
  all_schema_identical,
  "\n",
  sep = ""
)

expected_seasons_downloaded <- identical(
  sort(as.integer(names(season_results))),
  sort(season_plan$hoopr_season)
)

all_nonempty <- all(
  vapply(season_results, nrow, integer(1)) > 0
)

raw_files_exist <- all(
  file.exists(
    file.path(
      "data",
      "raw",
      paste0(
        "nba_shots_",
        season_plan$hoopr_season,
        ".rds"
      )
    )
  )
)

manifest_exists <- file.exists(
  "docs/validation/stage2_acquisition_manifest.csv"
)

stage2_checks <- tibble(
  check = c(
    "All four locked seasons downloaded",
    "Every season contains at least one row",
    "All four raw RDS files exist",
    "Acquisition manifest exists",
    "2025-26 excluded from outcome summary",
    "Raw data directory remains gitignored"
  ),
  passed = c(
    expected_seasons_downloaded,
    all_nonempty,
    raw_files_exist,
    manifest_exists,
    !2026L %in% development_outcome_audit$hoopr_season,
    any(
      grepl(
        "^data/raw/?$",
        trimws(gitignore_lines)
      )
    )
  )
)

cat("\n--- STAGE 2 ACQUISITION VERIFICATION ---\n")
print(stage2_checks, n = Inf)

if (!all(stage2_checks$passed)) {
  stop(
    "One or more Stage 2 acquisition checks failed. ",
    "Review the verification table before continuing."
  )
}

if (requireNamespace("renv", quietly = TRUE)) {
  renv::snapshot(prompt = FALSE)
  cat("\nUpdated renv.lock after Stage 2 acquisition.\n")
}

cat(
  "\nSTAGE 2 ACQUISITION COMPLETE.\n",
  "Raw releases are preserved unchanged in data/raw/.\n",
  "Next: inspect the printed schema/audit results before writing ",
  "scripts/02_clean_validate.R.\n",
  sep = ""
)
