
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

