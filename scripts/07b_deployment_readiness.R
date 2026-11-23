# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 7B — Deployment Bundle & Publishing Readiness
# File: scripts/07b_deployment_readiness.R
# =============================================================================

required_packages <- c("dplyr", "jsonlite", "readr", "rsconnect", "tibble")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(readr)
  library(rsconnect)
  library(tibble)
})

dir.create("docs/validation", recursive = TRUE, showWarnings = FALSE)
dir.create("scripts", recursive = TRUE, showWarnings = FALSE)

# ---- 1. Confirm Stage 7A component outputs -----------------------------------

stage7a_files <- c(
  "docs/validation/stage7a_runtime_file_manifest.csv",
  "docs/validation/stage7a_parse_audit.csv",
  "docs/validation/stage7a_artifact_integrity.csv",
  "docs/validation/stage7a_runtime_safety_audit.csv",
  "docs/validation/stage7a_app_instantiation.csv",
  "docs/validation/stage7a_package_versions.csv",
  "docs/validation/stage7a_responsive_css_audit.csv",
  "docs/validation/stage7a_manual_browser_qa_checklist.csv"
)

if (!all(file.exists(stage7a_files))) {
  stop("Stage 7A outputs are incomplete. Run scripts/07a_predeployment_qa.R first.")
}

stage7a_component_pass <- tibble(
  component = c(
    "Runtime files",
    "R parsing",
    "Artifact integrity",
    "Runtime safety",
    "App instantiation",
    "Required packages",
    "Responsive CSS"
  ),
  passed = c(
    all(read_csv(stage7a_files[[1]], show_col_types = FALSE)$exists),
    all(read_csv(stage7a_files[[2]], show_col_types = FALSE)$parses),
    all(read_csv(stage7a_files[[3]], show_col_types = FALSE)$passed),
    all(read_csv(stage7a_files[[4]], show_col_types = FALSE)$passed),
    {
      x <- read_csv(stage7a_files[[5]], show_col_types = FALSE)
      isTRUE(x$app_instantiates[[1]]) &&
        isTRUE(x$returns_shiny_app_object[[1]])
    },
    all(read_csv(stage7a_files[[6]], show_col_types = FALSE)$installed),
    all(read_csv(stage7a_files[[7]], show_col_types = FALSE)$passed)
  )
)

if (!all(stage7a_component_pass$passed)) {
  print(stage7a_component_pass, n = Inf)
  stop("At least one Stage 7A component audit is not passing.")
}

# ---- 2. Freeze explicit deployment allowlist ---------------------------------

deployment_files <- c(
  "app.R",
  "R/app_helpers.R",
  "R/court_helpers.R",
  "www/custom.css",
  "data/app/model_insights_bundle.rds",
  "data/app/shot_chart_data.rds",
  "data/processed/model_final_test.rds",
  "models/final_xgboost_model.rds",
  "models/final_model_selection_metadata.rds",
  "renv.lock"
)

deployment_file_audit <- tibble(
  file = deployment_files,
  exists = file.exists(deployment_files)
) %>%
  mutate(
    size_bytes = ifelse(exists, file.info(file)$size, NA_real_),
    size_mib = size_bytes / 1024^2
  )

cat("\n--- STAGE 7B EXPLICIT DEPLOYMENT FILES ---\n")
print(deployment_file_audit, n = Inf)

if (!all(deployment_file_audit$exists)) {
  stop("At least one explicit deployment file is missing.")
}

write_lines(deployment_files, "docs/validation/stage7b_deployment_files.txt")
write_csv(
  deployment_file_audit,
  "docs/validation/stage7b_deployment_file_audit.csv"
)

# ---- 3. Enumerate bundle exactly as rsconnect sees it ------------------------

listed_deployment_files <- sort(
  rsconnect::listDeploymentFiles(
    appDir = getwd(),
    appFiles = deployment_files
  )
)

expected_deployment_files <- sort(deployment_files)

deployment_list_exact <- identical(
  listed_deployment_files,
  expected_deployment_files
)

deployment_list_audit <- tibble(
  listed_file = listed_deployment_files,
  expected = listed_deployment_files %in% expected_deployment_files
)

cat("\n--- RSCONNECT DEPLOYMENT FILE ENUMERATION ---\n")
print(deployment_list_audit, n = Inf)

write_csv(
  deployment_list_audit,
  "docs/validation/stage7b_rsconnect_file_enumeration.csv"
)

# ---- 4. Strict dependency audit ---------------------------------------------

dependencies <- rsconnect::appDependencies(
  appDir = getwd(),
  appFiles = deployment_files,
  appMode = "shiny",
  dependencyResolution = "strict"
)

required_direct_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "Matrix",
  "scales",
  "tibble",
  "xgboost"
)

dependency_audit <- tibble(
  package = required_direct_packages,
  found = required_direct_packages %in% dependencies$Package
)

cat("\n--- STAGE 7B REQUIRED DEPENDENCY AUDIT ---\n")
print(dependency_audit, n = Inf)

write_csv(
  dependencies,
  "docs/validation/stage7b_app_dependencies.csv"
)

write_csv(
  dependency_audit,
  "docs/validation/stage7b_required_dependency_audit.csv"
)

# ---- 5. Write reproducible manifest.json ------------------------------------

rsconnect::writeManifest(
  appDir = getwd(),
  appFiles = deployment_files,
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  dependencyResolution = "strict",
  quiet = TRUE
)

if (!file.exists("manifest.json")) {
  stop("rsconnect::writeManifest() did not create manifest.json.")
}

manifest <- jsonlite::read_json(
  "manifest.json",
  simplifyVector = TRUE
)

manifest_files <- names(manifest$files)

if (is.null(manifest_files)) {
  stop("manifest.json has no auditable files section.")
}

manifest_files_sorted <- sort(manifest_files)

manifest_expected_exact <- identical(
  manifest_files_sorted,
  expected_deployment_files
)

manifest_file_audit <- tibble(
  file = manifest_files_sorted,
  explicitly_allowed = manifest_files_sorted %in% expected_deployment_files
)

cat("\n--- STAGE 7B MANIFEST FILE AUDIT ---\n")
print(manifest_file_audit, n = Inf)

write_csv(
  manifest_file_audit,
  "docs/validation/stage7b_manifest_file_audit.csv"
)

# ---- 6. Ensure no raw/intermediate/secret content is bundled ----------------

forbidden_manifest_patterns <- c(
  "data/raw/",
  "scripts/",
  "docs/",
  "data/processed/shots_development",
  "data/processed/model_development",
  "models/stage4",
  ".Renviron",
  ".env"
)

forbidden_content_audit <- tibble(
  forbidden_pattern = forbidden_manifest_patterns
) %>%
  rowwise() %>%
  mutate(
    found_in_manifest =
      any(startsWith(manifest_files, forbidden_pattern)) ||
      forbidden_pattern %in% manifest_files,
    passed = !found_in_manifest
  ) %>%
  ungroup()

cat("\n--- STAGE 7B FORBIDDEN BUNDLE-CONTENT AUDIT ---\n")
print(forbidden_content_audit, n = Inf)

write_csv(
  forbidden_content_audit,
  "docs/validation/stage7b_forbidden_bundle_content.csv"
)

# ---- 7. Public repository safety --------------------------------------------

if (!file.exists(".gitignore")) {
  file.create(".gitignore")
}

gitignore_lines <- readLines(".gitignore", warn = FALSE)

required_ignore_entries <- c(
  ".Renviron",
  ".env",
  ".Rproj.user/",
  "renv/library/",
  "renv/staging/",
  "data/raw/"
)

missing_ignore_entries <- setdiff(
  required_ignore_entries,
  gitignore_lines
)

if (length(missing_ignore_entries) > 0) {
  write(
    c(
      "",
      "# Public-repository safety / local runtime",
      missing_ignore_entries
    ),
    file = ".gitignore",
    append = TRUE
  )
}

gitignore_lines_after <- readLines(".gitignore", warn = FALSE)

gitignore_audit <- tibble(
  required_entry = required_ignore_entries,
  present = required_ignore_entries %in% gitignore_lines_after
)

cat("\n--- STAGE 7B .GITIGNORE SAFETY AUDIT ---\n")
print(gitignore_audit, n = Inf)

write_csv(
  gitignore_audit,
  "docs/validation/stage7b_gitignore_safety_audit.csv"
)

# ---- 8. Optional Git tracked-file audit --------------------------------------

git_available <- nzchar(Sys.which("git"))
git_repo <- FALSE
tracked_files <- character(0)

if (git_available) {
  repo_probe <- suppressWarnings(
    system2(
      "git",
      c("rev-parse", "--is-inside-work-tree"),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  git_repo <- length(repo_probe) > 0 &&
    identical(trimws(repo_probe[[1]]), "true")

  if (git_repo) {
    tracked_files <- suppressWarnings(
      system2(
        "git",
        "ls-files",
        stdout = TRUE,
        stderr = FALSE
      )
    )
  }
}

tracked_forbidden_patterns <- c(
  ".Renviron",
  ".env",
  "data/raw/"
)

tracked_forbidden_audit <- tibble(
  pattern = tracked_forbidden_patterns
) %>%
  rowwise() %>%
  mutate(
    tracked =
      if (!git_repo) {
        FALSE
      } else {
        any(tracked_files == pattern) ||
          any(startsWith(tracked_files, pattern))
      },
    passed = !tracked
  ) %>%
  ungroup()

git_repository_summary <- tibble(
  git_available = git_available,
  git_repository_detected = git_repo,
  tracked_files = length(tracked_files),
  forbidden_tracked_files_detected =
    any(tracked_forbidden_audit$tracked)
)

cat("\n--- STAGE 7B GIT REPOSITORY SUMMARY ---\n")
print(git_repository_summary, n = Inf)

if (git_repo) {
  cat("\n--- TRACKED-FILE SAFETY AUDIT ---\n")
  print(tracked_forbidden_audit, n = Inf)
}

write_csv(
  git_repository_summary,
  "docs/validation/stage7b_git_repository_summary.csv"
)

write_csv(
  tracked_forbidden_audit,
  "docs/validation/stage7b_tracked_file_safety.csv"
)

# ---- 9. Create credential-free shinyapps.io deployment helper ---------------

deploy_helper <- r'---(
# =============================================================================
# NBA Shot-Make Probability Explorer
# shinyapps.io Deployment Helper
#
# No token or secret is stored in this file.
# Run scripts/07b_deployment_readiness.R before using this helper.
# =============================================================================

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect before deploying.")
}

if (!file.exists("manifest.json")) {
  stop(
    "manifest.json is missing. Run scripts/07b_deployment_readiness.R first."
  )
}

registered_accounts <- rsconnect::accounts(
  server = "shinyapps.io"
)

if (nrow(registered_accounts) == 0L) {
  stop(
    paste0(
      "No shinyapps.io account is registered in rsconnect. ",
      "Register an account using the token instructions from shinyapps.io."
    )
  )
}

account_name <- Sys.getenv(
  "SHINYAPPS_ACCOUNT",
  unset = ""
)

if (!nzchar(account_name)) {
  if (nrow(registered_accounts) == 1L) {
    account_name <- registered_accounts$name[[1]]
  } else {
    stop(
      paste0(
        "Multiple shinyapps.io accounts are registered. ",
        "Choose one for this R session with: ",
        'Sys.setenv(SHINYAPPS_ACCOUNT = "your-account-name")'
      )
    )
  }
}

if (!(account_name %in% registered_accounts$name)) {
  stop(
    paste0(
      "SHINYAPPS_ACCOUNT='",
      account_name,
      "' is not a registered shinyapps.io account."
    )
  )
}

cat(
  "Deploying with shinyapps.io account: ",
  account_name,
  "\n",
  sep = ""
)

rsconnect::deployApp(
  appDir = getwd(),
  manifestPath = "manifest.json",
  appName = "nba-shot-make-probability-explorer",
  appTitle = "NBA Shot-Make Probability Explorer",
  account = account_name,
  server = "shinyapps.io",
  appVisibility = "public",
  launch.browser = TRUE,
  logLevel = "normal"
)
)---'

writeLines(
  deploy_helper,
  "scripts/deploy_shinyapps.R"
)

deploy_helper_text <- paste(
  readLines(
    "scripts/deploy_shinyapps.R",
    warn = FALSE
  ),
  collapse = "\n"
)

credential_risk_patterns <- c(
  "setAccountInfo(",
  "token =",
  "secret =",
  "apiKey =",
  "api_key ="
)

deployment_helper_security <- tibble(
  pattern = credential_risk_patterns
) %>%
  rowwise() %>%
  mutate(
    found = grepl(
      pattern,
      deploy_helper_text,
      fixed = TRUE
    ),
    passed = !found
  ) %>%
  ungroup()

cat("\n--- DEPLOYMENT HELPER CREDENTIAL AUDIT ---\n")
print(deployment_helper_security, n = Inf)

write_csv(
  deployment_helper_security,
  "docs/validation/stage7b_deploy_helper_security.csv"
)

# ---- 10. Deployment readiness summary ---------------------------------------

deployment_total_mib <- sum(
  deployment_file_audit$size_mib,
  na.rm = TRUE
)

deployment_summary <- tibble(
  deployment_files = length(deployment_files),
  deployment_total_mib = deployment_total_mib,
  manifest_files = length(manifest_files),
  direct_dependencies_found = sum(dependency_audit$found),
  direct_dependencies_required = nrow(dependency_audit),
  git_repository_detected = git_repo
)

cat("\n--- STAGE 7B DEPLOYMENT READINESS SUMMARY ---\n")
print(deployment_summary, n = Inf)

write_csv(
  deployment_summary,
  "docs/validation/stage7b_deployment_readiness_summary.csv"
)

# ---- 11. Final verification --------------------------------------------------

stage7b_checks <- tibble(
  check = c(
    "All explicit deployment files exist",
    "rsconnect deployment-file enumeration exactly matches allowlist",
    "All required direct app dependencies were detected",
    "manifest.json was created",
    "manifest.json file list exactly matches deployment allowlist",
    "No raw/intermediate/secret content appears in manifest",
    ".gitignore contains required public-repository safety entries",
    "No forbidden files are tracked when a Git repository is present",
    "Deployment helper exists",
    "Deployment helper contains no embedded credential-setting call",
    "Deployment bundle is under 25 MiB",
    "Stage 7A manual browser QA checklist still exists"
  ),
  passed = c(
    all(deployment_file_audit$exists),
    deployment_list_exact,
    all(dependency_audit$found),
    file.exists("manifest.json"),
    manifest_expected_exact,
    all(forbidden_content_audit$passed),
    all(gitignore_audit$present),
    if (git_repo) {
      all(tracked_forbidden_audit$passed)
    } else {
      TRUE
    },
    file.exists("scripts/deploy_shinyapps.R"),
    all(deployment_helper_security$passed),
    deployment_total_mib < 25,
    file.exists(
      "docs/validation/stage7a_manual_browser_qa_checklist.csv"
    )
  )
)

cat(
  "\n--- STAGE 7B DEPLOYMENT / PUBLISHING READINESS VERIFICATION ---\n"
)

print(
  stage7b_checks,
  n = Inf
)

if (!all(stage7b_checks$passed)) {
  stop(
    paste0(
      "One or more Stage 7B checks failed. ",
      "Resolve these before publishing."
    )
  )
}

cat(
  "\nSTAGE 7B COMPLETE.\n",
  "The exact deployment bundle and manifest are frozen. ",
  "Complete the manual Stage 7A browser checklist before running ",
  "scripts/deploy_shinyapps.R. Then proceed to Stage 8 for GitHub and ",
  "shinyapps.io publication.\n",
  sep = ""
)
