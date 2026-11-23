# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 7A — Pre-Deployment QA & Reproducibility Audit
# File: scripts/07a_predeployment_qa.R
#
# PURPOSE:
#   Validate the completed Stage 6 Shiny app as a deployable, reproducible
#   artifact before GitHub publishing / shinyapps.io deployment.
#
# This stage DOES NOT:
#   - refit models
#   - retune models
#   - alter final probabilities
#   - modify the 2025-26 holdout evaluation
#
# It DOES:
#   1. Verify all runtime files exist.
#   2. Parse all runtime R files.
#   3. Instantiate app.R without launching a browser.
#   4. Confirm the frozen final model and app artifacts remain internally valid.
#   5. Confirm app.R contains no model-training or raw-data acquisition calls.
#   6. Confirm the blinded final-test feature artifact still has no outcome.
#   7. Audit runtime file sizes for deployment.
#   8. Audit package versions used by the app.
#   9. Confirm responsive CSS breakpoints exist.
#  10. Write deploy-readiness manifests/checklists under docs/validation.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "shiny",
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
  library(readr)
  library(shiny)
  library(tibble)
})

dir.create(
  "docs/validation",
  recursive = TRUE,
  showWarnings = FALSE
)

# ---- 2. Runtime file manifest ------------------------------------------------
#
# Only files needed to RUN the deployed app are listed here.
# Raw acquisition / model-training data are intentionally excluded.

runtime_files <- c(
  "app.R",
  "R/app_helpers.R",
  "R/court_helpers.R",
  "www/custom.css",
  "data/app/model_insights_bundle.rds",
  "data/app/shot_chart_data.rds",
  "data/processed/model_final_test.rds",
  "models/final_xgboost_model.rds",
  "models/final_model_selection_metadata.rds"
)

runtime_manifest <- tibble(
  file =
    runtime_files,

  exists =
    file.exists(
      runtime_files
    )
) %>%
  mutate(
    size_bytes =
      ifelse(
        exists,
        file.info(
          file
        )$
          size,
        NA_real_
      ),

    size_mib =
      size_bytes /
      1024^2
  )

cat(
  "\n--- STAGE 7A RUNTIME FILE MANIFEST ---\n"
)

print(
  runtime_manifest,
  n = Inf
)

readr::write_csv(
  runtime_manifest,
  "docs/validation/stage7a_runtime_file_manifest.csv"
)

if (!all(
  runtime_manifest$
    exists
)) {
  stop(
    "One or more required runtime files are missing."
  )
}

runtime_total_mib <- sum(
  runtime_manifest$
    size_mib,
  na.rm = TRUE
)

# ---- 3. Parse runtime R files ------------------------------------------------

runtime_r_files <- c(
  "app.R",
  "R/app_helpers.R",
  "R/court_helpers.R"
)

parse_results <- lapply(
  runtime_r_files,
  function(path) {
    tryCatch(
      {
        parse(
          file =
            path
        )

        tibble(
          file =
            path,
          parses =
            TRUE,
          error =
            NA_character_
        )
      },
      error = function(e) {
        tibble(
          file =
            path,
          parses =
            FALSE,
          error =
            conditionMessage(
              e
            )
        )
      }
    )
  }
) %>%
  bind_rows()

cat(
  "\n--- RUNTIME R-FILE PARSE AUDIT ---\n"
)

print(
  parse_results,
  n = Inf
)

readr::write_csv(
  parse_results,
  "docs/validation/stage7a_parse_audit.csv"
)

# ---- 4. Load frozen runtime artifacts ---------------------------------------

model_final_test <- readRDS(
  "data/processed/model_final_test.rds"
)

shot_chart_data <- readRDS(
  "data/app/shot_chart_data.rds"
)

insights_bundle <- readRDS(
  "data/app/model_insights_bundle.rds"
)

final_xgboost_model <- readRDS(
  "models/final_xgboost_model.rds"
)

final_model_metadata <- readRDS(
  "models/final_model_selection_metadata.rds"
)

# ---- 5. Frozen-artifact integrity audit -------------------------------------

artifact_audit <- tibble(
  check = c(
    "Final-test feature artifact remains blinded (no outcome)",
    "Frozen selected model remains restrained XGBoost",
    "Shot-chart rows match frozen final-test rows",
    "Shot-chart outcome is complete",
    "Shot-chart probability is complete",
    "Shot-chart contains all 30 teams",
    "Shot-chart contains at least 500 shooters",
    "Model-insight bundle contains performance section",
    "Model-insight bundle contains calibration section",
    "Model-insight bundle contains feature importance section"
  ),

  passed = c(
    !(
      "outcome" %in%
        names(
          model_final_test
        )
    ),

    identical(
      final_model_metadata$
        selected_model,
      "xgboost_restrained"
    ),

    nrow(
      shot_chart_data
    ) ==
      nrow(
        model_final_test
      ),

    "outcome" %in%
      names(
        shot_chart_data
      ) &&
      !anyNA(
        shot_chart_data$
          outcome
      ),

    "selected_model_probability" %in%
      names(
        shot_chart_data
      ) &&
      !anyNA(
        shot_chart_data$
          selected_model_probability
      ),

    "team_abbrev_canonical" %in%
      names(
        shot_chart_data
      ) &&
      dplyr::n_distinct(
        shot_chart_data$
          team_abbrev_canonical,
        na.rm = TRUE
      ) >=
        30L,

    "shooter_name" %in%
      names(
        shot_chart_data
      ) &&
      dplyr::n_distinct(
        shot_chart_data$
          shooter_name,
        na.rm = TRUE
      ) >=
        500L,

    "performance" %in%
      names(
        insights_bundle
      ),

    "calibration" %in%
      names(
        insights_bundle
      ),

    "feature_importance" %in%
      names(
        insights_bundle
      )
  )
)

cat(
  "\n--- FROZEN ARTIFACT INTEGRITY AUDIT ---\n"
)

print(
  artifact_audit,
  n = Inf
)

readr::write_csv(
  artifact_audit,
  "docs/validation/stage7a_artifact_integrity.csv"
)

# ---- 6. app.R safety audit ---------------------------------------------------
#
# The deployed app should score the frozen model and visualize precomputed
# artifacts. It should not train models or reacquire raw NBA data.

app_text <- paste(
  readLines(
    "app.R",
    warn = FALSE
  ),
  collapse = "\n"
)

forbidden_runtime_patterns <- tibble(
  pattern = c(
    "xgb.train(",
    "mgcv::bam(",
    "mgcv::gam(",
    "glm(",
    "load_nba_shots(",
    "espn_nba_pbp(",
    "hoopR::",
    "install.packages("
  ),

  rationale = c(
    "No XGBoost training should occur inside the deployed app",
    "No GAM training should occur inside the deployed app",
    "No GAM training should occur inside the deployed app",
    "No regression fitting should occur inside the deployed app",
    "No raw shot acquisition should occur inside the deployed app",
    "No live ESPN PBP acquisition should occur inside the deployed app",
    "No hoopR data calls should occur inside the deployed app",
    "The deployed app should not install packages at runtime"
  )
) %>%
  rowwise() %>%
  mutate(
    found =
      grepl(
        pattern,
        app_text,
        fixed = TRUE
      ),

    passed =
      !found
  ) %>%
  ungroup()

cat(
  "\n--- APP RUNTIME SAFETY AUDIT ---\n"
)

print(
  forbidden_runtime_patterns,
  n = Inf
)

readr::write_csv(
  forbidden_runtime_patterns,
  "docs/validation/stage7a_runtime_safety_audit.csv"
)

# ---- 7. Instantiate the Shiny app without launching --------------------------

app_environment <- new.env(
  parent =
    globalenv()
)

app_source_result <- tryCatch(
  source(
    "app.R",
    local =
      app_environment,
    echo =
      FALSE,
    print.eval =
      FALSE
  ),
  error =
    function(e) e
)

app_instantiates <- !inherits(
  app_source_result,
  "error"
)

app_is_shiny_object <- FALSE
app_instantiation_error <- NA_character_

if (app_instantiates) {

  app_value <- app_source_result$
    value

  app_is_shiny_object <- inherits(
    app_value,
    "shiny.appobj"
  )

} else {

  app_instantiation_error <- conditionMessage(
    app_source_result
  )
}

app_instantiation_audit <- tibble(
  app_instantiates =
    app_instantiates,

  returns_shiny_app_object =
    app_is_shiny_object,

  error =
    app_instantiation_error
)

cat(
  "\n--- SHINY APP INSTANTIATION AUDIT ---\n"
)

print(
  app_instantiation_audit,
  n = Inf
)

readr::write_csv(
  app_instantiation_audit,
  "docs/validation/stage7a_app_instantiation.csv"
)

# ---- 8. Package-version audit ------------------------------------------------

app_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "Matrix",
  "scales",
  "tibble",
  "xgboost"
)

package_versions <- tibble(
  package =
    app_packages,

  installed =
    vapply(
      app_packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
) %>%
  rowwise() %>%
  mutate(
    version =
      if (
        installed
      ) {
        as.character(
          utils::packageVersion(
            package
          )
        )
      } else {
        NA_character_
      }
  ) %>%
  ungroup()

cat(
  "\n--- APP PACKAGE VERSION AUDIT ---\n"
)

print(
  package_versions,
  n = Inf
)

readr::write_csv(
  package_versions,
  "docs/validation/stage7a_package_versions.csv"
)

# ---- 9. Responsive CSS audit -------------------------------------------------

css_text <- paste(
  readLines(
    "www/custom.css",
    warn = FALSE
  ),
  collapse = "\n"
)

responsive_css_audit <- tibble(
  check = c(
    "CSS contains a <=900px responsive breakpoint",
    "CSS contains a <=560px mobile breakpoint",
    "KPI grid has mobile-responsive rules",
    "Shot-detail grid has mobile-responsive rules"
  ),

  passed = c(
    grepl(
      "@media (max-width: 900px)",
      css_text,
      fixed = TRUE
    ),

    grepl(
      "@media (max-width: 560px)",
      css_text,
      fixed = TRUE
    ),

    grepl(
      ".kpi-grid",
      css_text,
      fixed = TRUE
    ),

    grepl(
      ".shot-detail-grid",
      css_text,
      fixed = TRUE
    )
  )
)

cat(
  "\n--- RESPONSIVE CSS AUDIT ---\n"
)

print(
  responsive_css_audit,
  n = Inf
)

readr::write_csv(
  responsive_css_audit,
  "docs/validation/stage7a_responsive_css_audit.csv"
)

# ---- 10. Create manual browser QA checklist ---------------------------------

manual_qa_checklist <- tibble(
  area = c(
    "Navigation",
    "Probability Explorer",
    "Probability Explorer",
    "Probability Explorer",
    "Interactive Shot Chart",
    "Interactive Shot Chart",
    "Interactive Shot Chart",
    "Model Comparison",
    "Calibration & Validation",
    "Model Insights",
    "Responsive Layout",
    "Responsive Layout",
    "General"
  ),

  manual_check = c(
    "All five tabs open without errors",
    "Clicking multiple court locations updates probability and shot details",
    "Changing shot type / period / clock / actions updates probability",
    "Reset Example restores the default scenario",
    "Team filter updates available player choices",
    "Zone / shot type / team / player / result filters all work",
    "Clicking a plotted shot shows result, probability, player, team, and date",
    "Charts and table show three-decimal model metrics",
    "Reliability and segment-calibration plots render cleanly",
    "TreeSHAP / Gain toggle works",
    "At approximately tablet width, cards stack without horizontal overflow",
    "At approximately phone width, KPI/detail cards stack into one column",
    "Browser console / R console shows no recurring errors or warnings during use"
  ),

  status =
    "REVIEW MANUALLY"
)

readr::write_csv(
  manual_qa_checklist,
  "docs/validation/stage7a_manual_browser_qa_checklist.csv"
)

# ---- 11. Runtime-size summary ------------------------------------------------

runtime_size_summary <- tibble(
  runtime_files =
    nrow(
      runtime_manifest
    ),

  runtime_total_mib =
    runtime_total_mib,

  largest_runtime_file =
    runtime_manifest$
      file[
        which.max(
          runtime_manifest$
            size_mib
        )
      ],

  largest_runtime_file_mib =
    max(
      runtime_manifest$
        size_mib,
      na.rm = TRUE
    )
)

cat(
  "\n--- STAGE 7A RUNTIME SIZE SUMMARY ---\n"
)

print(
  runtime_size_summary,
  n = Inf
)

readr::write_csv(
  runtime_size_summary,
  "docs/validation/stage7a_runtime_size_summary.csv"
)

# ---- 12. Final automated QA checks ------------------------------------------

stage7a_checks <- tibble(
  check = c(
    "All required runtime files exist",
    "All runtime R files parse successfully",
    "Frozen final-test feature artifact remains blinded",
    "Frozen selected model remains restrained XGBoost",
    "Shot-chart rows match frozen final-test rows",
    "Shot-chart contains complete outcomes and probabilities",
    "Shot-chart retains all 30 teams",
    "Shot-chart retains at least 500 shooters",
    "App runtime contains no forbidden model-training/data-acquisition calls",
    "app.R instantiates without launching",
    "app.R returns a Shiny app object",
    "All required app packages are installed",
    "Responsive <=900px breakpoint exists",
    "Responsive <=560px breakpoint exists",
    "Runtime deployment manifest was written",
    "Manual browser QA checklist was written"
  ),

  passed = c(
    all(
      runtime_manifest$
        exists
    ),

    all(
      parse_results$
        parses
    ),

    artifact_audit$
      passed[
        artifact_audit$
          check ==
          "Final-test feature artifact remains blinded (no outcome)"
      ],

    artifact_audit$
      passed[
        artifact_audit$
          check ==
          "Frozen selected model remains restrained XGBoost"
      ],

    artifact_audit$
      passed[
        artifact_audit$
          check ==
          "Shot-chart rows match frozen final-test rows"
      ],

    all(
      artifact_audit$
        passed[
          artifact_audit$
            check %in%
            c(
              "Shot-chart outcome is complete",
              "Shot-chart probability is complete"
            )
        ]
    ),

    artifact_audit$
      passed[
        artifact_audit$
          check ==
          "Shot-chart contains all 30 teams"
      ],

    artifact_audit$
      passed[
        artifact_audit$
          check ==
          "Shot-chart contains at least 500 shooters"
      ],

    all(
      forbidden_runtime_patterns$
        passed
    ),

    app_instantiates,

    app_is_shiny_object,

    all(
      package_versions$
        installed
    ),

    responsive_css_audit$
      passed[
        responsive_css_audit$
          check ==
          "CSS contains a <=900px responsive breakpoint"
      ],

    responsive_css_audit$
      passed[
        responsive_css_audit$
          check ==
          "CSS contains a <=560px mobile breakpoint"
      ],

    file.exists(
      "docs/validation/stage7a_runtime_file_manifest.csv"
    ),

    file.exists(
      "docs/validation/stage7a_manual_browser_qa_checklist.csv"
    )
  )
)

cat(
  "\n--- STAGE 7A PRE-DEPLOYMENT QA VERIFICATION ---\n"
)

print(
  stage7a_checks,
  n = Inf
)

if (!all(
  stage7a_checks$
    passed
)) {
  stop(
    paste0(
      "One or more Stage 7A automated QA checks failed. ",
      "Resolve these before deployment."
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
    "\nUpdated renv.lock after Stage 7A QA.\n"
  )
}

cat(
  "\nSTAGE 7A COMPLETE.\n",
  "Automated pre-deployment QA passed. Complete the manual browser checklist ",
  "next, especially tablet/phone responsiveness and all interactive controls. ",
  "Then proceed to Stage 7B deployment-bundle and publishing readiness.\n",
  sep = ""
)
