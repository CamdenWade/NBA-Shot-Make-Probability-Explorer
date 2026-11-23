# =============================================================================
# NBA Shot-Make Probability Explorer
# Targeted Shot Chart Fix
# File: scripts/07c_fix_shot_chart_validate.R
#
# Fixes:
#   Error: is.character(txt) is not TRUE
#
# Cause:
#   jsonlite::validate() can mask shiny::validate() in the R search path.
#   The Interactive Shot Chart was using unqualified validate(need(...)).
#
# This patch changes:
#   validate(need(...))
#
# to:
#   shiny::validate(shiny::need(...))
#
# No data, model, probabilities, filters, or visualizations are changed.
# =============================================================================

app_path <- "app.R"

if (!file.exists(app_path)) {
  stop("app.R was not found in the current project directory.")
}

app_text <- paste(
  readLines(
    app_path,
    warn = FALSE
  ),
  collapse = "\n"
)

old_block <- r'---(      validate(
        need(
          nrow(
            data
          ) >
            0L,
          "No shots match the current filters."
        )
      ))---'

new_block <- r'---(      shiny::validate(
        shiny::need(
          nrow(
            data
          ) >
            0L,
          "No shots match the current filters."
        )
      ))---'

positions <- gregexpr(
  old_block,
  app_text,
  fixed = TRUE
)[[1]]

match_count <- if (
  length(positions) == 1L &&
  positions[[1]] == -1L
) {
  0L
} else {
  length(positions)
}

if (match_count == 0L) {

  # Allow safe re-running if the patch has already been applied.
  if (
    grepl(
      "shiny::validate(",
      app_text,
      fixed = TRUE
    ) &&
    grepl(
      "shiny::need(",
      app_text,
      fixed = TRUE
    )
  ) {
    message("Shot Chart validate/need call is already namespaced. No change needed.")
  } else {
    stop(
      paste0(
        "Could not find the expected Shot Chart validate(need(...)) block in app.R. ",
        "No changes were made."
      )
    )
  }

} else if (match_count == 1L) {

  app_text <- sub(
    old_block,
    new_block,
    app_text,
    fixed = TRUE
  )

  writeLines(
    strsplit(
      app_text,
      "\n",
      fixed = TRUE
    )[[1]],
    app_path
  )

  message("Patched app.R successfully.")

} else {

  stop(
    paste0(
      "Found ",
      match_count,
      " matching validate blocks. No automatic changes were made."
    )
  )
}

# Parse verification.
parse(
  file = app_path
)

updated_text <- paste(
  readLines(
    app_path,
    warn = FALSE
  ),
  collapse = "\n"
)

checks <- tibble::tibble(
  check = c(
    "app.R parses successfully",
    "Shot Chart uses shiny::validate",
    "Shot Chart uses shiny::need",
    "Unqualified no-shots validate block is absent"
  ),
  passed = c(
    TRUE,
    grepl(
      "shiny::validate(",
      updated_text,
      fixed = TRUE
    ),
    grepl(
      "shiny::need(",
      updated_text,
      fixed = TRUE
    ),
    !grepl(
      old_block,
      updated_text,
      fixed = TRUE
    )
  )
)

cat(
  "\n--- SHOT CHART VALIDATE FIX VERIFICATION ---\n"
)

print(
  checks,
  n = Inf
)

if (!all(checks$passed)) {
  stop("One or more Shot Chart fix checks failed.")
}

cat(
  "\nFIX COMPLETE.\n",
  "Restart the Shiny app with shiny::runApp() and re-open the ",
  "Interactive Shot Chart tab.\n",
  sep = ""
)
