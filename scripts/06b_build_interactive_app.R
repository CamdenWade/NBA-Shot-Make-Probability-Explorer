# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 6B — Interactive Probability Explorer + Shot Chart
# File: scripts/06b_build_interactive_app.R
#
# PURPOSE:
#   Extend the verified Stage 6A Shiny foundation with:
#     1. Interactive Shot Probability Explorer
#     2. Filterable Interactive Shot Chart
#     3. Stage 6A visual-polish refinements
#
# METHODOLOGY:
#   - Uses the already-frozen final restrained XGBoost model.
#   - No model fitting or tuning occurs.
#   - 2025-26 remains evaluation-only.
#   - Probability Explorer scores user-created pre-shot scenarios only.
#   - Shot Chart uses precomputed 2025-26 probabilities.
#
# INPUTS CREATED IN EARLIER STAGES:
#   models/final_xgboost_model.rds
#   models/final_model_selection_metadata.rds
#   data/processed/model_final_test.rds
#   data/processed/stage5a_final_test_scored.rds
#   data/app/model_insights_bundle.rds
#
# OUTPUTS:
#   app.R
#   R/app_helpers.R
#   R/court_helpers.R
#   www/custom.css
#   data/app/shot_chart_data.rds
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "ggplot2",
  "Matrix",
  "readr",
  "scales",
  "shiny",
  "tibble",
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
  library(readr)
  library(scales)
  library(shiny)
  library(tibble)
  library(xgboost)
})

# ---- 2. Required frozen inputs ----------------------------------------------

required_files <- c(
  insights_bundle =
    "data/app/model_insights_bundle.rds",

  final_xgboost_model =
    "models/final_xgboost_model.rds",

  final_model_metadata =
    "models/final_model_selection_metadata.rds",

  model_final_test =
    "data/processed/model_final_test.rds",

  scored_final_test =
    "data/processed/stage5a_final_test_scored.rds"
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing Stage 6B input(s): ",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

insights <- readRDS(
  required_files[[
    "insights_bundle"
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

model_final_test <- readRDS(
  required_files[[
    "model_final_test"
  ]]
)

scored_final_test <- readRDS(
  required_files[[
    "scored_final_test"
  ]]
)

if (
  is.null(
    final_model_metadata$
      selected_model
  ) ||
  final_model_metadata$
    selected_model !=
    "xgboost_restrained"
) {
  stop(
    "Stage 6B requires the frozen restrained XGBoost selected model."
  )
}

if (
  nrow(
    model_final_test
  ) !=
    nrow(
      scored_final_test
    )
) {
  stop(
    "Frozen final-test features and scored holdout rows do not match."
  )
}

if (!identical(
  model_final_test$
    shot_id,
  scored_final_test$
    shot_id
)) {
  stop(
    "Frozen final-test features and scored holdout are not in identical shot order."
  )
}

# ---- 3. Directories ----------------------------------------------------------

dir.create(
  "www",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "R",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/app",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "docs/validation",
  recursive = TRUE,
  showWarnings = FALSE
)

# ---- 4. Build compact shot-chart artifact -----------------------------------

shot_chart_data <- model_final_test %>%
  select(
    shot_id,
    game_id,
    game_date,
    nba_season,
    court_x_folded,
    court_y_folded,
    distance_to_rim_check_ft,
    shot_zone_geometry,
    distance_band,
    shot_type_group,
    period_group,
    is_home_shooter
  ) %>%
  mutate(
    plot_x =
      court_y_folded,

    plot_y =
      court_x_folded +
      47
  ) %>%
  left_join(
    scored_final_test %>%
      select(
        shot_id,
        outcome,
        selected_model_probability
      ),
    by =
      "shot_id"
  )

if (
  nrow(
    shot_chart_data
  ) !=
    nrow(
      model_final_test
    ) ||
  anyNA(
    shot_chart_data$
      selected_model_probability
  ) ||
  anyNA(
    shot_chart_data$
      outcome
  )
) {
  stop(
    "Shot-chart artifact failed row-count or missing-value checks."
  )
}

saveRDS(
  shot_chart_data,
  "data/app/shot_chart_data.rds",
  compress = "xz"
)

# ---- 5. Write R/court_helpers.R ---------------------------------------------

court_helpers <- r"---(
# =============================================================================
# NBA Shot-Make Probability Explorer
# Court geometry + frozen-model inference helpers
# =============================================================================

NBA_BASELINE_X <- -47
NBA_RIM_X <- -41.75
NBA_RIM_Y <- 0
NBA_HALFCOURT_X <- 0
NBA_SIDELINE_Y <- 25
NBA_THREE_RADIUS <- 23.75
NBA_CORNER_THREE_Y <- 22

court_to_plot <- function(
    court_x,
    court_y) {

  tibble::tibble(
    plot_x =
      court_y,
    plot_y =
      court_x +
      47
  )
}

plot_to_court <- function(
    plot_x,
    plot_y) {

  tibble::tibble(
    court_x =
      plot_y -
      47,
    court_y =
      plot_x
  )
}

calculate_shot_geometry <- function(
    court_x,
    court_y) {

  distance <- sqrt(
    (
      court_x -
      NBA_RIM_X
    )^2 +
      (
        court_y -
          NBA_RIM_Y
      )^2
  )

  signed_angle_rad <- atan2(
    court_y -
      NBA_RIM_Y,
    court_x -
      NBA_RIM_X
  )

  absolute_angle_deg <- abs(
    signed_angle_rad *
      180 /
      pi
  )

  corner_transition_x <-
    NBA_RIM_X +
    sqrt(
      NBA_THREE_RADIUS^2 -
        NBA_CORNER_THREE_Y^2
    )

  is_corner_three <-
    abs(
      court_y
    ) >=
      NBA_CORNER_THREE_Y &&
    court_x <=
      corner_transition_x

  is_three <-
    is_corner_three ||
    distance >=
      NBA_THREE_RADIUS

  three_region <- dplyr::case_when(
    !is_three ~
      "2PT",

    is_corner_three ~
      "Corner 3",

    TRUE ~
      "Above-the-Break 3"
  )

  tibble::tibble(
    distance_to_rim_check_ft =
      distance,

    absolute_angle_deg =
      absolute_angle_deg,

    is_three_geometry =
      is_three,

    is_corner_three_geometry =
      is_corner_three,

    is_heave =
      distance >=
      40,

    three_region =
      three_region
  )
}

build_half_court_layers <- function() {

  three_y <- seq(
    from =
      -NBA_CORNER_THREE_Y,
    to =
      NBA_CORNER_THREE_Y,
    length.out =
      250
  )

  three_x <-
    NBA_RIM_X +
    sqrt(
      pmax(
        0,
        NBA_THREE_RADIUS^2 -
          three_y^2
      )
    )

  three_arc <- tibble::tibble(
    plot_x =
      three_y,
    plot_y =
      three_x +
      47
  )

  restricted_theta <- seq(
    from =
      -pi / 2,
    to =
      pi / 2,
    length.out =
      150
  )

  restricted_x <-
    NBA_RIM_X +
    4 *
    cos(
      restricted_theta
    )

  restricted_y <-
    4 *
    sin(
      restricted_theta
    )

  restricted_arc <- tibble::tibble(
    plot_x =
      restricted_y,
    plot_y =
      restricted_x +
      47
  )

  list(
    three_arc =
      three_arc,

    restricted_arc =
      restricted_arc,

    corner_transition_plot_y =
      (
        NBA_RIM_X +
        sqrt(
          NBA_THREE_RADIUS^2 -
            NBA_CORNER_THREE_Y^2
        )
      ) +
      47,

    rim_plot_x =
      0,

    rim_plot_y =
      NBA_RIM_X +
      47,

    free_throw_plot_y =
      -28 +
      47
  )
}

add_half_court <- function(
    plot_object,
    line_width = 0.65) {

  court <- build_half_court_layers()

  plot_object +
    ggplot2::annotate(
      "segment",
      x =
        -25,
      xend =
        25,
      y =
        0,
      yend =
        0,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "segment",
      x =
        -25,
      xend =
        -25,
      y =
        0,
      yend =
        47,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "segment",
      x =
        25,
      xend =
        25,
      y =
        0,
      yend =
        47,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "segment",
      x =
        -25,
      xend =
        25,
      y =
        47,
      yend =
        47,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "rect",
      xmin =
        -8,
      xmax =
        8,
      ymin =
        0,
      ymax =
        court$
          free_throw_plot_y,
      fill =
        NA,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "segment",
      x =
        -3,
      xend =
        3,
      y =
        4,
      yend =
        4,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "point",
      x =
        court$
          rim_plot_x,
      y =
        court$
          rim_plot_y,
      shape =
        1,
      size =
        3,
      stroke =
        0.8
    ) +
    ggplot2::geom_path(
      data =
        court$
          restricted_arc,
      ggplot2::aes(
        x =
          plot_x,
        y =
          plot_y
      ),
      inherit.aes =
        FALSE,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "segment",
      x =
        -NBA_CORNER_THREE_Y,
      xend =
        -NBA_CORNER_THREE_Y,
      y =
        0,
      yend =
        court$
          corner_transition_plot_y,
      linewidth =
        line_width
    ) +
    ggplot2::annotate(
      "segment",
      x =
        NBA_CORNER_THREE_Y,
      xend =
        NBA_CORNER_THREE_Y,
      y =
        0,
      yend =
        court$
          corner_transition_plot_y,
      linewidth =
        line_width
    ) +
    ggplot2::geom_path(
      data =
        court$
          three_arc,
      ggplot2::aes(
        x =
          plot_x,
        y =
          plot_y
      ),
      inherit.aes =
        FALSE,
      linewidth =
        line_width
    ) +
    ggplot2::coord_fixed(
      xlim =
        c(
          -25,
          25
        ),
      ylim =
        c(
          0,
          47
        ),
      expand =
        FALSE,
      clip =
        "on"
    )
}

build_explorer_row <- function(
    court_x,
    court_y,
    shot_type_group,
    period_choice,
    clock_seconds_remaining,
    is_home_shooter,
    actions,
    shot_type_levels) {

  geometry <- calculate_shot_geometry(
    court_x =
      court_x,
    court_y =
      court_y
  )

  is_overtime <-
    identical(
      period_choice,
      "OT"
    )

  period_number <- if (
    is_overtime
  ) {
    5L
  } else {
    as.integer(
      sub(
        "Q",
        "",
        period_choice
      )
    )
  }

  tibble::tibble(
    court_x_folded =
      court_x,

    court_y_folded =
      court_y,

    distance_to_rim_check_ft =
      geometry$
        distance_to_rim_check_ft,

    absolute_angle_deg =
      geometry$
        absolute_angle_deg,

    is_three_geometry =
      geometry$
        is_three_geometry,

    is_corner_three_geometry =
      geometry$
        is_corner_three_geometry,

    is_heave =
      geometry$
        is_heave,

    shot_type_group =
      factor(
        shot_type_group,
        levels =
          shot_type_levels
      ),

    is_bank_shot =
      "Bank Shot" %in%
      actions,

    is_driving =
      "Driving" %in%
      actions,

    is_running =
      "Running" %in%
      actions,

    is_cutting =
      "Cutting" %in%
      actions,

    is_alley_oop =
      "Alley-Oop" %in%
      actions,

    is_putback =
      "Putback" %in%
      actions,

    is_turnaround =
      "Turnaround" %in%
      actions,

    is_fadeaway =
      "Fadeaway" %in%
      actions,

    period_number =
      period_number,

    clock_seconds_remaining =
      clock_seconds_remaining,

    is_overtime =
      is_overtime,

    is_last_2_minutes_period =
      clock_seconds_remaining <=
      120,

    is_last_24_seconds_period =
      clock_seconds_remaining <=
      24,

    is_last_5_seconds_period =
      clock_seconds_remaining <=
      5,

    is_home_shooter =
      is_home_shooter
  )
}

score_explorer_row <- function(
    scenario_row,
    model,
    xgb_feature_formula,
    expected_feature_names) {

  x <- Matrix::sparse.model.matrix(
    object =
      xgb_feature_formula,
    data =
      scenario_row
  )

  if (!identical(
    colnames(
      x
    ),
    expected_feature_names
  )) {
    stop(
      "Explorer model matrix does not match frozen XGBoost feature columns."
    )
  }

  d <- xgboost::xgb.DMatrix(
    data =
      x
  )

  as.numeric(
    predict(
      model,
      d
    )
  )[[1]]
}
)---"

writeLines(
  court_helpers,
  "R/court_helpers.R"
)

# ---- 6. Rewrite R/app_helpers.R ---------------------------------------------

app_helpers <- r"---(
# =============================================================================
# NBA Shot-Make Probability Explorer
# Shared Shiny helpers
# =============================================================================

format_pct <- function(
    x,
    accuracy = 0.1) {

  scales::percent(
    x,
    accuracy =
      accuracy
  )
}

format_metric <- function(
    x,
    digits = 3) {

  formatC(
    x,
    format =
      "f",
    digits =
      digits
  )
}

metric_card <- function(
    title,
    value,
    detail = NULL,
    accent_class = "") {

  shiny::div(
    class =
      paste(
        "metric-card",
        accent_class
      ),

    shiny::div(
      class =
        "metric-card-title",
      title
    ),

    shiny::div(
      class =
        "metric-card-value",
      value
    ),

    if (!is.null(
      detail
    )) {
      shiny::div(
        class =
          "metric-card-detail",
        detail
      )
    }
  )
}

app_plot_theme <- function(
    base_size = 12) {

  ggplot2::theme_minimal(
    base_size =
      base_size
  ) +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          face =
            "bold",
          size =
            base_size +
            3
        ),

      plot.subtitle =
        ggplot2::element_text(
          size =
            base_size
        ),

      plot.caption =
        ggplot2::element_text(
          size =
            base_size -
            2,
          hjust =
            0
        ),

      panel.grid.minor =
        ggplot2::element_blank(),

      legend.position =
        "bottom",

      axis.title =
        ggplot2::element_text(
          face =
            "bold"
        )
    )
}

court_plot_theme <- function(
    base_size = 12) {

  ggplot2::theme_void(
    base_size =
      base_size
  ) +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          face =
            "bold"
        ),

      plot.subtitle =
        ggplot2::element_text(),

      plot.caption =
        ggplot2::element_text(
          hjust =
            0
        ),

      legend.position =
        "bottom"
    )
}
)---"

writeLines(
  app_helpers,
  "R/app_helpers.R"
)

# ---- 7. Rewrite www/custom.css ----------------------------------------------

custom_css <- r"---(
:root {
  --app-bg: #f4f7fb;
  --surface: #ffffff;
  --surface-dark: #111827;
  --text: #152033;
  --muted: #65758b;
  --line: #dfe7f1;
  --accent: #2563eb;
  --accent-soft: #e8f0ff;
  --success: #0f8a5f;
  --warning: #b66a00;
  --shadow: 0 8px 28px rgba(17, 24, 39, 0.08);
}

body {
  background: var(--app-bg);
  color: var(--text);
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
}

.navbar-default {
  background: var(--surface-dark);
  border: none;
  border-radius: 0;
  box-shadow: 0 3px 18px rgba(17, 24, 39, 0.18);
}

.navbar-default .navbar-brand,
.navbar-default .navbar-nav > li > a {
  color: #f8fafc !important;
}

.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
  background: rgba(255, 255, 255, 0.12) !important;
  color: #ffffff !important;
}

.app-hero {
  background: linear-gradient(135deg, #111827 0%, #1f3b71 100%);
  color: white;
  padding: 28px 30px;
  border-radius: 16px;
  margin: 20px 0 18px 0;
  box-shadow: var(--shadow);
}

.app-hero h1 {
  margin: 0 0 8px 0;
  font-size: 30px;
  font-weight: 750;
}

.app-hero p {
  margin: 0;
  max-width: 950px;
  color: #dbe7ff;
  font-size: 15px;
  line-height: 1.6;
}

.kpi-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(0, 1fr));
  gap: 14px;
  margin-bottom: 20px;
}

.metric-card {
  background: var(--surface);
  border: 1px solid var(--line);
  border-top: 4px solid var(--accent);
  border-radius: 13px;
  padding: 16px 17px 15px 17px;
  box-shadow: var(--shadow);
  min-height: 126px;
}

.metric-card.success {
  border-top-color: var(--success);
}

.metric-card.warning {
  border-top-color: var(--warning);
}

.metric-card-title {
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  font-size: 11px;
  font-weight: 750;
}

.metric-card-value {
  color: var(--text);
  font-size: 25px;
  line-height: 1.15;
  font-weight: 780;
  margin-top: 8px;
}

.metric-card-detail {
  color: var(--muted);
  font-size: 12px;
  line-height: 1.45;
  margin-top: 7px;
}

.content-card {
  background: var(--surface);
  border: 1px solid var(--line);
  border-radius: 14px;
  padding: 19px;
  box-shadow: var(--shadow);
  margin-bottom: 18px;
}

.content-card h3 {
  margin-top: 0;
  font-size: 18px;
  font-weight: 740;
}

.section-intro {
  color: var(--muted);
  line-height: 1.6;
  max-width: 950px;
  margin: 0 0 18px 0;
}

.method-note {
  background: var(--accent-soft);
  border-left: 4px solid var(--accent);
  border-radius: 8px;
  padding: 13px 15px;
  margin: 12px 0 18px 0;
  color: #274266;
  line-height: 1.55;
}

.control-card {
  background: var(--surface);
  border: 1px solid var(--line);
  border-radius: 14px;
  padding: 19px;
  box-shadow: var(--shadow);
  margin-bottom: 18px;
}

.control-card h3 {
  margin-top: 0;
  margin-bottom: 15px;
  font-size: 18px;
  font-weight: 740;
}

.control-section {
  border-top: 1px solid #edf1f6;
  margin-top: 14px;
  padding-top: 14px;
}

.control-section:first-of-type {
  border-top: 0;
  margin-top: 0;
  padding-top: 0;
}

.control-section-title {
  color: var(--muted);
  text-transform: uppercase;
  font-size: 11px;
  font-weight: 800;
  letter-spacing: 0.06em;
  margin-bottom: 9px;
}

.probability-callout {
  background: linear-gradient(135deg, #111827 0%, #1f3b71 100%);
  border-radius: 14px;
  padding: 19px;
  color: white;
  margin-bottom: 18px;
}

.probability-callout-label {
  color: #cbd8ef;
  font-size: 11px;
  font-weight: 800;
  letter-spacing: 0.08em;
  text-transform: uppercase;
}

.probability-callout-value {
  font-size: 44px;
  font-weight: 800;
  margin-top: 4px;
  line-height: 1;
}

.probability-callout-detail {
  color: #dbe7ff;
  margin-top: 9px;
  font-size: 13px;
  line-height: 1.5;
}

.shot-detail-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 10px;
}

.shot-detail {
  background: #f8fafc;
  border: 1px solid var(--line);
  border-radius: 9px;
  padding: 10px 11px;
}

.shot-detail-label {
  color: var(--muted);
  text-transform: uppercase;
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 0.05em;
}

.shot-detail-value {
  margin-top: 3px;
  font-weight: 750;
  color: var(--text);
}

.table {
  background: white;
}

.table > thead > tr > th {
  border-bottom: 2px solid var(--line);
  color: var(--muted);
  font-size: 12px;
}

.table > tbody > tr > td {
  border-top: 1px solid #edf1f6;
}

.shiny-input-container {
  margin-bottom: 14px;
  width: 100%;
}

.btn-default {
  border-radius: 8px;
  border-color: #cdd8e6;
}

@media (max-width: 900px) {
  .kpi-grid {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }

  .app-hero {
    padding: 23px 22px;
  }
}

@media (max-width: 560px) {
  .kpi-grid,
  .shot-detail-grid {
    grid-template-columns: 1fr;
  }

  .app-hero h1 {
    font-size: 25px;
  }

  .probability-callout-value {
    font-size: 36px;
  }
}
)---"

writeLines(
  custom_css,
  "www/custom.css"
)

# ---- 8. Rewrite app.R --------------------------------------------------------

app_code <- r"---(
# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 6B Interactive Shiny App
# =============================================================================

required_packages <- c(
  "dplyr",
  "ggplot2",
  "Matrix",
  "scales",
  "shiny",
  "tibble",
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
  stop(
    paste0(
      "Missing required Shiny package(s): ",
      paste(
        missing_packages,
        collapse = ", "
      ),
      ". Restore the project renv environment before launching."
    )
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(Matrix)
  library(scales)
  library(shiny)
  library(tibble)
  library(xgboost)
})

source(
  "R/app_helpers.R"
)

source(
  "R/court_helpers.R"
)

required_app_files <- c(
  "data/app/model_insights_bundle.rds",
  "data/app/shot_chart_data.rds",
  "data/processed/model_final_test.rds",
  "models/final_xgboost_model.rds",
  "models/final_model_selection_metadata.rds"
)

missing_app_files <- required_app_files[
  !file.exists(
    required_app_files
  )
]

if (length(
  missing_app_files
) > 0) {
  stop(
    paste0(
      "Missing app artifact(s): ",
      paste(
        missing_app_files,
        collapse = ", "
      )
    )
  )
}

insights <- readRDS(
  "data/app/model_insights_bundle.rds"
)

shot_chart_data <- readRDS(
  "data/app/shot_chart_data.rds"
)

model_final_test <- readRDS(
  "data/processed/model_final_test.rds"
)

final_xgboost_model <- readRDS(
  "models/final_xgboost_model.rds"
)

final_model_metadata <- readRDS(
  "models/final_model_selection_metadata.rds"
)

performance <-
  insights$
  performance

performance_long <-
  insights$
  performance_long

calibration_summary <-
  insights$
  calibration$
  summary

calibration_bins <-
  insights$
  calibration$
  reliability_20bin

segment_diagnostics <-
  insights$
  segment_diagnostics

feature_importance <-
  insights$
  feature_importance

key_findings <-
  insights$
  key_findings

selected_perf <- performance %>%
  filter(
    selected_model
  )

top_feature <- feature_importance %>%
  arrange(
    shap_rank
  ) %>%
  slice(
    1
  )

segment_choices <- sort(
  unique(
    segment_diagnostics$
      segment
  )
)

shot_type_levels <- levels(
  model_final_test$
    shot_type_group
)

if (is.null(
  shot_type_levels
)) {
  shot_type_levels <- unique(
    as.character(
      model_final_test$
        shot_type_group
    )
  )
}

shot_zone_choices <- sort(
  unique(
    as.character(
      shot_chart_data$
        shot_zone_geometry
    )
  )
)

chart_shot_type_choices <- sort(
  unique(
    as.character(
      shot_chart_data$
        shot_type_group
    )
  )
)

xgb_feature_formula <-
  final_model_metadata$
  xgb_feature_formula

expected_xgb_feature_names <-
  final_model_metadata$
  xgb_feature_names

initial_location <- list(
  court_x =
    -26.75,
  court_y =
    0
)

ui <- navbarPage(
  title =
    "NBA Shot-Make Probability Explorer",

  id =
    "main_nav",

  header =
    tagList(
      tags$head(
        tags$link(
          rel =
            "stylesheet",
          type =
            "text/css",
          href =
            "custom.css"
        )
      )
    ),

  tabPanel(
    "Shot Probability Explorer",

    div(
      class =
        "container-fluid",

      div(
        class =
          "app-hero",

        h1(
          "Shot Probability Explorer"
        ),

        p(
          "Create a pre-shot NBA scenario, click a location on the court, and ",
          "score it with the frozen restrained XGBoost model. The model uses ",
          "only information available before the shot result is known."
        )
      ),

      fluidRow(
        column(
          width =
            4,

          div(
            class =
              "control-card",

            h3(
              "Shot Context"
            ),

            div(
              class =
                "control-section",

              div(
                class =
                  "control-section-title",
                "Shot Type"
              ),

              selectInput(
                "explorer_shot_type",
                label =
                  NULL,
                choices =
                  shot_type_levels,
                selected =
                  if (
                    "Jump Shot" %in%
                    shot_type_levels
                  ) {
                    "Jump Shot"
                  } else {
                    shot_type_levels[[1]]
                  }
              )
            ),

            div(
              class =
                "control-section",

              div(
                class =
                  "control-section-title",
                "Game Situation"
              ),

              selectInput(
                "explorer_period",
                "Period",
                choices =
                  c(
                    "Q1",
                    "Q2",
                    "Q3",
                    "Q4",
                    "OT"
                  ),
                selected =
                  "Q2"
              ),

              sliderInput(
                "explorer_clock",
                "Seconds Remaining in Period",
                min =
                  0,
                max =
                  720,
                value =
                  360,
                step =
                  1
              ),

              checkboxInput(
                "explorer_home",
                "Home shooter",
                value =
                  TRUE
              )
            ),

            div(
              class =
                "control-section",

              div(
                class =
                  "control-section-title",
                "Optional Pre-Shot Actions"
              ),

              checkboxGroupInput(
                "explorer_actions",
                label =
                  NULL,
                choices =
                  c(
                    "Bank Shot",
                    "Driving",
                    "Running",
                    "Cutting",
                    "Alley-Oop",
                    "Putback",
                    "Turnaround",
                    "Fadeaway"
                  ),
                selected =
                  character(
                    0
                  )
              )
            ),

            actionButton(
              "reset_explorer",
              "Reset Example"
            )
          )
        ),

        column(
          width =
            5,

          div(
            class =
              "content-card",

            h3(
              "Click a Shot Location"
            ),

            p(
              class =
                "section-intro",
              "The basket is at the bottom center of the half court."
            ),

            plotOutput(
              "explorer_court",
              height =
                "650px",
              click =
                "explorer_court_click"
            )
          )
        ),

        column(
          width =
            3,

          div(
            class =
              "probability-callout",

            div(
              class =
                "probability-callout-label",
              "Predicted Make Probability"
            ),

            div(
              class =
                "probability-callout-value",
              textOutput(
                "explorer_probability",
                inline =
                  TRUE
              )
            ),

            div(
              class =
                "probability-callout-detail",
              "Frozen restrained XGBoost; this scenario does not alter the model."
            )
          ),

          div(
            class =
              "content-card",

            h3(
              "Selected Shot"
            ),

            uiOutput(
              "explorer_shot_details"
            )
          ),

          div(
            class =
              "method-note",

            strong(
              "Interpret carefully: "
            ),

            "This is a population-level pre-shot probability model. It does ",
            "not include defender distance, player identity, tracking data, ",
            "fatigue, or other unavailable context."
          )
        )
      )
    )
  ),

  tabPanel(
    "Interactive Shot Chart",

    div(
      class =
        "container-fluid",

      div(
        class =
          "app-hero",

        h1(
          "Interactive Shot Chart"
        ),

        p(
          "Filter the frozen 2025–26 evaluation season and explore where ",
          "shots were taken, how the model scored them, and whether they were made."
        )
      ),

      fluidRow(
        column(
          width =
            3,

          div(
            class =
              "control-card",

            h3(
              "Chart Filters"
            ),

            selectInput(
              "chart_zone",
              "Shot Zone",
              choices =
                c(
                  "All",
                  shot_zone_choices
                ),
              selected =
                "All"
            ),

            selectInput(
              "chart_shot_type",
              "Shot Type",
              choices =
                c(
                  "All",
                  chart_shot_type_choices
                ),
              selected =
                "All"
            ),

            selectInput(
              "chart_outcome",
              "Observed Result",
              choices =
                c(
                  "All",
                  "Make",
                  "Miss"
                ),
              selected =
                "All"
            ),

            radioButtons(
              "chart_color",
              "Point Encoding",
              choices =
                c(
                  "Predicted Probability" =
                    "probability",
                  "Observed Result" =
                    "outcome"
                ),
              selected =
                "probability"
            ),

            div(
              class =
                "method-note",

              "For responsive plotting, a deterministic sample of up to ",
              "20,000 matching shots is displayed. Summary KPIs use the ",
              "entire filtered population."
            )
          )
        ),

        column(
          width =
            6,

          div(
            class =
              "content-card",

            h3(
              "2025–26 Shot Map"
            ),

            plotOutput(
              "shot_chart",
              height =
                "720px",
              click =
                "shot_chart_click"
            )
          )
        ),

        column(
          width =
            3,

          div(
            class =
              "kpi-grid",
            style =
              "grid-template-columns: 1fr;",

            metric_card(
              "Filtered Shots",
              textOutput(
                "chart_shot_count",
                inline =
                  TRUE
              ),
              "Full filtered population."
            ),

            metric_card(
              "Observed Make Rate",
              textOutput(
                "chart_make_rate",
                inline =
                  TRUE
              ),
              "2025–26 result."
            ),

            metric_card(
              "Mean Predicted Probability",
              textOutput(
                "chart_mean_probability",
                inline =
                  TRUE
              ),
              "Frozen XGBoost."
            )
          ),

          div(
            class =
              "content-card",

            h3(
              "Clicked Shot"
            ),

            uiOutput(
              "clicked_shot_details"
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Model Comparison",

    div(
      class =
        "container-fluid",

      div(
        class =
          "app-hero",

        h1(
          "Model Comparison"
        ),

        p(
          "Compare the constant benchmark, logistic regression, spatial GAM, ",
          "and restrained XGBoost across chronological validation and the ",
          "one-time 2025–26 holdout."
        )
      ),

      div(
        class =
          "method-note",

        strong(
          "Selection rule: "
        ),

        "The champion was chosen using weighted chronological-validation ",
        "log-loss before the 2025–26 holdout was revealed."
      ),

      fluidRow(
        column(
          width =
            6,

          div(
            class =
              "content-card",

            h3(
              "Log-Loss"
            ),

            plotOutput(
              "model_logloss_plot",
              height =
                "390px"
            )
          )
        ),

        column(
          width =
            6,

          div(
            class =
              "content-card",

            h3(
              "Brier Score"
            ),

            plotOutput(
              "model_brier_plot",
              height =
                "390px"
            )
          )
        )
      ),

      div(
        class =
          "content-card",

        h3(
          "Performance Table"
        ),

        tableOutput(
          "model_performance_table"
        )
      )
    )
  ),

  tabPanel(
    "Calibration & Validation",

    div(
      class =
        "container-fluid",

      div(
        class =
          "app-hero",

        h1(
          "Calibration & Validation"
        ),

        p(
          "Inspect whether predicted shot probabilities align with observed ",
          "make rates on the frozen 2025–26 evaluation season."
        )
      ),

      div(
        class =
          "kpi-grid",

        metric_card(
          "Calibration Intercept",
          format_metric(
            calibration_summary$
              calibration_intercept[[1]]
          ),
          "Ideal value: 0."
        ),

        metric_card(
          "Calibration Slope",
          format_metric(
            calibration_summary$
              calibration_slope[[1]]
          ),
          "Ideal value: 1."
        ),

        metric_card(
          "Mean Prediction",
          format_pct(
            calibration_summary$
              mean_predicted_probability[[1]]
          ),
          "Frozen XGBoost."
        ),

        metric_card(
          "Observed Make Rate",
          format_pct(
            calibration_summary$
              observed_make_rate[[1]]
          ),
          "2025–26 holdout."
        )
      ),

      fluidRow(
        column(
          width =
            7,

          div(
            class =
              "content-card",

            h3(
              "20-Bin Reliability Curve"
            ),

            plotOutput(
              "calibration_plot",
              height =
                "470px"
            )
          )
        ),

        column(
          width =
            5,

          div(
            class =
              "content-card",

            h3(
              "Segment Calibration"
            ),

            selectInput(
              "segment_group",
              "Diagnostic Segment",
              choices =
                segment_choices,
              selected =
                "Shot Zone"
            ),

            plotOutput(
              "segment_gap_plot",
              height =
                "410px"
            )
          )
        )
      ),

      div(
        class =
          "content-card",

        h3(
          "Largest Major Calibration Gaps"
        ),

        p(
          class =
            "section-intro",
          "Only segments with at least 1,000 shots are included in this ranking."
        ),

        tableOutput(
          "largest_gap_table"
        )
      )
    )
  ),

  tabPanel(
    "Model Insights",

    div(
      class =
        "container-fluid",

      div(
        class =
          "app-hero",

        h1(
          "Model Insights"
        ),

        p(
          "Understand which pre-shot characteristics the frozen XGBoost model ",
          "relies on most. Importance describes model behavior, not causality."
        )
      ),

      fluidRow(
        column(
          width =
            8,

          div(
            class =
              "content-card",

            h3(
              "Global Feature Importance"
            ),

            radioButtons(
              "importance_method",
              "Importance Method",
              choices =
                c(
                  "TreeSHAP" =
                    "shap",
                  "XGBoost Gain" =
                    "gain"
                ),
              selected =
                "shap",
              inline =
                TRUE
            ),

            plotOutput(
              "feature_importance_plot",
              height =
                "500px"
            )
          )
        ),

        column(
          width =
            4,

          div(
            class =
              "content-card",

            h3(
              "Key Findings"
            ),

            tableOutput(
              "key_findings_table"
            )
          )
        )
      ),

      div(
        class =
          "content-card",

        h3(
          "Top Importance Rankings"
        ),

        p(
          class =
            "section-intro",
          paste0(
            "Distance and shot type dominate the frozen model. ",
            "Late-clock attempts remain the clearest calibration weakness, ",
            "but the holdout was never used for retuning."
          )
        ),

        tableOutput(
          "importance_table"
        )
      )
    )
  )
)

server <- function(
    input,
    output,
    session) {

  # ---------------------------------------------------------------------------
  # Probability Explorer
  # ---------------------------------------------------------------------------

  selected_location <- reactiveVal(
    initial_location
  )

  observeEvent(
    input$
      explorer_court_click,
    {

      click <- input$
        explorer_court_click

      req(
        click$
          x,
        click$
          y
      )

      bounded_x <- max(
        -25,
        min(
          25,
          click$
            x
        )
      )

      bounded_y <- max(
        0,
        min(
          47,
          click$
            y
        )
      )

      converted <- plot_to_court(
        plot_x =
          bounded_x,
        plot_y =
          bounded_y
      )

      selected_location(
        list(
          court_x =
            converted$
              court_x[[1]],
          court_y =
            converted$
              court_y[[1]]
        )
      )
    }
  )

  observeEvent(
    input$
      reset_explorer,
    {

      selected_location(
        initial_location
      )

      updateSelectInput(
        session,
        "explorer_shot_type",
        selected =
          if (
            "Jump Shot" %in%
            shot_type_levels
          ) {
            "Jump Shot"
          } else {
            shot_type_levels[[1]]
          }
      )

      updateSelectInput(
        session,
        "explorer_period",
        selected =
          "Q2"
      )

      updateSliderInput(
        session,
        "explorer_clock",
        min =
          0,
        max =
          720,
        value =
          360,
        step =
          1
      )

      updateCheckboxInput(
        session,
        "explorer_home",
        value =
          TRUE
      )

      updateCheckboxGroupInput(
        session,
        "explorer_actions",
        selected =
          character(
            0
          )
      )
    }
  )

  observeEvent(
    input$
      explorer_period,
    {

      req(
        input$
          explorer_period
      )

      if (
        identical(
          input$
            explorer_period,
          "OT"
        )
      ) {
        updateSliderInput(
          session,
          "explorer_clock",
          min =
            0,
          max =
            300,
          value =
            min(
              300,
              input$
                explorer_clock
            ),
          step =
            1
        )
      } else {
        updateSliderInput(
          session,
          "explorer_clock",
          min =
            0,
          max =
            720,
          value =
            input$
              explorer_clock,
          step =
            1
        )
      }
    },
    ignoreInit =
      TRUE
  )

  explorer_scenario <- reactive({

    location <- selected_location()

    req(
      input$
        explorer_shot_type,
      input$
        explorer_period,
      input$
        explorer_clock
    )

    build_explorer_row(
      court_x =
        location$
          court_x,

      court_y =
        location$
          court_y,

      shot_type_group =
        input$
          explorer_shot_type,

      period_choice =
        input$
          explorer_period,

      clock_seconds_remaining =
        input$
          explorer_clock,

      is_home_shooter =
        isTRUE(
          input$
            explorer_home
        ),

      actions =
        input$
          explorer_actions,

      shot_type_levels =
        shot_type_levels
    )
  })

  explorer_geometry <- reactive({

    location <- selected_location()

    calculate_shot_geometry(
      court_x =
        location$
          court_x,
      court_y =
        location$
          court_y
    )
  })

  explorer_probability_value <- reactive({

    score_explorer_row(
      scenario_row =
        explorer_scenario(),

      model =
        final_xgboost_model,

      xgb_feature_formula =
        xgb_feature_formula,

      expected_feature_names =
        expected_xgb_feature_names
    )
  })

  output$
    explorer_probability <- renderText({

      scales::percent(
        explorer_probability_value(),
        accuracy =
          0.1
      )
    })

  output$
    explorer_court <- renderPlot({

      location <- selected_location()

      plot_location <- court_to_plot(
        court_x =
          location$
            court_x,
        court_y =
          location$
            court_y
      )

      base <- ggplot() +
        geom_point(
          data =
            plot_location,
          aes(
            x =
              plot_x,
            y =
              plot_y
          ),
          size =
            5
        ) +
        labs(
          caption =
            "Click anywhere inside the half court to move the shot location."
        ) +
        court_plot_theme()

      add_half_court(
        base
      )
    })

  output$
    explorer_shot_details <- renderUI({

      location <- selected_location()

      geometry <- explorer_geometry()

      shot_value <- if (
        isTRUE(
          geometry$
            is_three_geometry[[1]]
        )
      ) {
        "3PT"
      } else {
        "2PT"
      }

      div(
        class =
          "shot-detail-grid",

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Distance"
          ),
          div(
            class =
              "shot-detail-value",
            paste0(
              round(
                geometry$
                  distance_to_rim_check_ft[[1]],
                1
              ),
              " ft"
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Shot Value"
          ),
          div(
            class =
              "shot-detail-value",
            shot_value
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "3PT Region"
          ),
          div(
            class =
              "shot-detail-value",
            geometry$
              three_region[[1]]
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Location"
          ),
          div(
            class =
              "shot-detail-value",
            paste0(
              "x ",
              round(
                location$
                  court_x,
                1
              ),
              ", y ",
              round(
                location$
                  court_y,
                1
              )
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Shot Type"
          ),
          div(
            class =
              "shot-detail-value",
            input$
              explorer_shot_type
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Clock"
          ),
          div(
            class =
              "shot-detail-value",
            paste0(
              input$
                explorer_period,
              " · ",
              input$
                explorer_clock,
              " sec"
            )
          )
        )
      )
    })

  # ---------------------------------------------------------------------------
  # Interactive Shot Chart
  # ---------------------------------------------------------------------------

  filtered_shots <- reactive({

    data <- shot_chart_data

    if (
      !is.null(
        input$
          chart_zone
      ) &&
      input$
        chart_zone !=
        "All"
    ) {
      data <- data %>%
        filter(
          as.character(
            shot_zone_geometry
          ) ==
            input$
              chart_zone
        )
    }

    if (
      !is.null(
        input$
          chart_shot_type
      ) &&
      input$
        chart_shot_type !=
        "All"
    ) {
      data <- data %>%
        filter(
          as.character(
            shot_type_group
          ) ==
            input$
              chart_shot_type
        )
    }

    if (
      !is.null(
        input$
          chart_outcome
      ) &&
      input$
        chart_outcome ==
        "Make"
    ) {
      data <- data %>%
        filter(
          outcome ==
            1L
        )
    }

    if (
      !is.null(
        input$
          chart_outcome
      ) &&
      input$
        chart_outcome ==
        "Miss"
    ) {
      data <- data %>%
        filter(
          outcome ==
            0L
        )
    }

    data
  })

  chart_plot_data <- reactive({

    data <- filtered_shots()

    if (
      nrow(
        data
      ) <=
        20000L
    ) {
      return(
        data
      )
    }

    index <- unique(
      round(
        seq(
          from =
            1,
          to =
            nrow(
              data
            ),
          length.out =
            20000L
        )
      )
    )

    data[
      index,
      ,
      drop =
        FALSE
    ]
  })

  output$
    chart_shot_count <- renderText({

      scales::comma(
        nrow(
          filtered_shots()
        )
      )
    })

  output$
    chart_make_rate <- renderText({

      data <- filtered_shots()

      if (
        nrow(
          data
        ) ==
          0L
      ) {
        return(
          "—"
        )
      }

      scales::percent(
        mean(
          data$
            outcome
        ),
        accuracy =
          0.1
      )
    })

  output$
    chart_mean_probability <- renderText({

      data <- filtered_shots()

      if (
        nrow(
          data
        ) ==
          0L
      ) {
        return(
          "—"
        )
      }

      scales::percent(
        mean(
          data$
            selected_model_probability
        ),
        accuracy =
          0.1
      )
    })

  output$
    shot_chart <- renderPlot({

      data <- chart_plot_data()

      validate(
        need(
          nrow(
            data
          ) >
            0L,
          "No shots match the current filters."
        )
      )

      if (
        identical(
          input$
            chart_color,
          "outcome"
        )
      ) {

        plot_data <- data %>%
          mutate(
            result =
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
              )
          )

        base <- ggplot(
          plot_data,
          aes(
            x =
              plot_x,
            y =
              plot_y,
            shape =
              result
          )
        ) +
          geom_point(
            alpha =
              0.36,
            size =
              1.15
          ) +
          labs(
            shape =
              "Observed Result",
            caption =
              "Up to 20,000 matching shots shown; click a point for details."
          ) +
          court_plot_theme()

      } else {

        base <- ggplot(
          data,
          aes(
            x =
              plot_x,
            y =
              plot_y,
            alpha =
              selected_model_probability
          )
        ) +
          geom_point(
            size =
              1.15
          ) +
          scale_alpha_continuous(
            range =
              c(
                0.12,
                0.80
              ),
            labels =
              scales::percent_format(
                accuracy =
                  1
              )
          ) +
          labs(
            alpha =
              "Predicted Make Probability",
            caption =
              "Darker points indicate higher frozen-model probability; click a point for details."
          ) +
          court_plot_theme()
      }

      add_half_court(
        base
      )
    })

  clicked_shot <- reactive({

    click <- input$
      shot_chart_click

    data <- chart_plot_data()

    if (
      is.null(
        click
      ) ||
      nrow(
        data
      ) ==
        0L
    ) {
      return(
        NULL
      )
    }

    clicked <- shiny::nearPoints(
      data,
      coordinfo =
        click,
      xvar =
        "plot_x",
      yvar =
        "plot_y",
      threshold =
        12,
      maxpoints =
        1,
      addDist =
        TRUE
    )

    if (
      nrow(
        clicked
      ) ==
        0L
    ) {
      return(
        NULL
      )
    }

    clicked
  })

  output$
    clicked_shot_details <- renderUI({

      shot <- clicked_shot()

      if (is.null(
        shot
      )) {
        return(
          p(
            class =
              "section-intro",
            "Click a plotted shot to inspect its frozen probability and result."
          )
        )
      }

      result <- if (
        shot$
          outcome[[1]] ==
          1L
      ) {
        "Make"
      } else {
        "Miss"
      }

      div(
        class =
          "shot-detail-grid",

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Result"
          ),
          div(
            class =
              "shot-detail-value",
            result
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Probability"
          ),
          div(
            class =
              "shot-detail-value",
            scales::percent(
              shot$
                selected_model_probability[[1]],
              accuracy =
                0.1
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Distance"
          ),
          div(
            class =
              "shot-detail-value",
            paste0(
              round(
                shot$
                  distance_to_rim_check_ft[[1]],
                1
              ),
              " ft"
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Zone"
          ),
          div(
            class =
              "shot-detail-value",
            as.character(
              shot$
                shot_zone_geometry[[1]]
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Shot Type"
          ),
          div(
            class =
              "shot-detail-value",
            as.character(
              shot$
                shot_type_group[[1]]
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Date"
          ),
          div(
            class =
              "shot-detail-value",
            as.character(
              shot$
                game_date[[1]]
            )
          )
        )
      )
    })

  # ---------------------------------------------------------------------------
  # Model Comparison
  # ---------------------------------------------------------------------------

  output$
    model_logloss_plot <- renderPlot({

      plot_data <- performance_long %>%
        mutate(
          evaluation_set =
            factor(
              evaluation_set,
              levels =
                c(
                  "Chronological Validation",
                  "2025-26 Final Holdout"
                )
            )
        )

      ggplot(
        plot_data,
        aes(
          x =
            reorder(
              model_label,
              log_loss
            ),
          y =
            log_loss,
          fill =
            evaluation_set
        )
      ) +
        geom_col(
          position =
            position_dodge(
              width =
                0.75
            ),
          width =
            0.68
        ) +
        geom_text(
          aes(
            label =
              sprintf(
                "%.3f",
                log_loss
              ),
            group =
              evaluation_set
          ),
          position =
            position_dodge(
              width =
                0.75
            ),
          hjust =
            -0.12,
          size =
            3.2
        ) +
        coord_flip(
          clip =
            "off"
        ) +
        expand_limits(
          y =
            max(
              plot_data$
                log_loss
            ) *
            1.10
        ) +
        labs(
          x =
            NULL,
          y =
            "Log-loss",
          fill =
            NULL,
          subtitle =
            "Lower is better"
        ) +
        app_plot_theme()
    })

  output$
    model_brier_plot <- renderPlot({

      plot_data <- performance_long %>%
        mutate(
          evaluation_set =
            factor(
              evaluation_set,
              levels =
                c(
                  "Chronological Validation",
                  "2025-26 Final Holdout"
                )
            )
        )

      ggplot(
        plot_data,
        aes(
          x =
            reorder(
              model_label,
              brier
            ),
          y =
            brier,
          fill =
            evaluation_set
        )
      ) +
        geom_col(
          position =
            position_dodge(
              width =
                0.75
            ),
          width =
            0.68
        ) +
        geom_text(
          aes(
            label =
              sprintf(
                "%.3f",
                brier
              ),
            group =
              evaluation_set
          ),
          position =
            position_dodge(
              width =
                0.75
            ),
          hjust =
            -0.12,
          size =
            3.2
        ) +
        coord_flip(
          clip =
            "off"
        ) +
        expand_limits(
          y =
            max(
              plot_data$
                brier
            ) *
            1.10
        ) +
        labs(
          x =
            NULL,
          y =
            "Brier score",
          fill =
            NULL,
          subtitle =
            "Lower is better"
        ) +
        app_plot_theme()
    })

  output$
    model_performance_table <- renderTable({

      performance %>%
        transmute(
          Model =
            model_label,

          Selected =
            ifelse(
              selected_model,
              "Yes",
              ""
            ),

          `Validation Log-Loss` =
            sprintf(
              "%.3f",
              validation_log_loss
            ),

          `Holdout Log-Loss` =
            sprintf(
              "%.3f",
              test_log_loss
            ),

          `Validation Brier` =
            sprintf(
              "%.3f",
              validation_brier
            ),

          `Holdout Brier` =
            sprintf(
              "%.3f",
              test_brier
            )
        )
    },
    striped =
      TRUE,
    bordered =
      FALSE,
    spacing =
      "s"
    )

  # ---------------------------------------------------------------------------
  # Calibration & Validation
  # ---------------------------------------------------------------------------

  output$
    calibration_plot <- renderPlot({

      ggplot(
        calibration_bins,
        aes(
          x =
            mean_predicted_probability,
          y =
            observed_make_rate
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
          size =
            2.8
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
          x =
            "Mean predicted make probability",
          y =
            "Observed make rate",
          caption =
            "Each point represents an approximately equal-frequency probability bin; dashed line = perfect calibration."
        ) +
        app_plot_theme()
    })

  output$
    segment_gap_plot <- renderPlot({

      req(
        input$
          segment_group
      )

      plot_data <- segment_diagnostics %>%
        filter(
          segment ==
            input$
              segment_group
        ) %>%
        mutate(
          calibration_gap_pp =
            100 *
            calibration_gap
        ) %>%
        arrange(
          calibration_gap_pp
        )

      ggplot(
        plot_data,
        aes(
          x =
            reorder(
              segment_value,
              calibration_gap_pp
            ),
          y =
            calibration_gap_pp
        )
      ) +
        geom_col() +
        geom_hline(
          yintercept =
            0,
          linetype =
            "dashed",
          linewidth =
            0.6
        ) +
        coord_flip() +
        labs(
          x =
            NULL,
          y =
            "Observed − predicted (percentage points)",
          caption =
            "Positive = underprediction; negative = overprediction."
        ) +
        app_plot_theme()
    })

  output$
    largest_gap_table <- renderTable({

      insights$
        largest_segment_gaps %>%
        transmute(
          Segment =
            segment,

          Group =
            segment_value,

          Shots =
            scales::comma(
              shots
            ),

          Observed =
            scales::percent(
              observed_make_rate,
              accuracy =
                0.1
            ),

          Predicted =
            scales::percent(
              mean_predicted_probability,
              accuracy =
                0.1
            ),

          `Gap (pp)` =
            round(
              100 *
              calibration_gap,
              1
            )
        )
    },
    striped =
      TRUE,
    bordered =
      FALSE,
    spacing =
      "s"
    )

  # ---------------------------------------------------------------------------
  # Model Insights
  # ---------------------------------------------------------------------------

  output$
    feature_importance_plot <- renderPlot({

      if (
        identical(
          input$
            importance_method,
          "gain"
        )
      ) {

        plot_data <- feature_importance %>%
          arrange(
            desc(
              gain_share
            )
          ) %>%
          slice_head(
            n =
              15
          )

        ggplot(
          plot_data,
          aes(
            x =
              reorder(
                feature_label,
                gain_share
              ),
            y =
              gain_share
          )
        ) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            labels =
              scales::percent_format(
                accuracy =
                  1
              )
          ) +
          labs(
            x =
              NULL,
            y =
              "Gain importance share",
            caption =
              "Training split-gain importance."
          ) +
          app_plot_theme()

      } else {

        plot_data <- feature_importance %>%
          arrange(
            shap_rank
          ) %>%
          slice_head(
            n =
              15
          )

        ggplot(
          plot_data,
          aes(
            x =
              reorder(
                feature_label,
                shap_share
              ),
            y =
              shap_share
          )
        ) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            labels =
              scales::percent_format(
                accuracy =
                  1
              )
          ) +
          labs(
            x =
              NULL,
            y =
              "Global SHAP importance share",
            caption =
              "Importance describes model reliance, not causal effect."
          ) +
          app_plot_theme()
      }
    })

  output$
    key_findings_table <- renderTable({

      key_findings %>%
        transmute(
          Finding =
            headline,
          Detail =
            detail
        )
    },
    striped =
      TRUE,
    bordered =
      FALSE,
    spacing =
      "s"
    )

  output$
    importance_table <- renderTable({

      feature_importance %>%
        arrange(
          shap_rank
        ) %>%
        slice_head(
          n =
            12
        ) %>%
        transmute(
          Feature =
            feature_label,

          `SHAP Rank` =
            shap_rank,

          `SHAP Share` =
            scales::percent(
              shap_share,
              accuracy =
                0.1
            ),

          `Gain Rank` =
            gain_rank,

          `Gain Share` =
            scales::percent(
              gain_share,
              accuracy =
                0.1
            )
        )
    },
    striped =
      TRUE,
    bordered =
      FALSE,
    spacing =
      "s"
    )
}

shinyApp(
  ui =
    ui,
  server =
    server
)
)---"

writeLines(
  app_code,
  "app.R"
)

# ---- 9. Parse generated R files ---------------------------------------------

parse(
  file =
    "app.R"
)

parse(
  file =
    "R/app_helpers.R"
)

parse(
  file =
    "R/court_helpers.R"
)

# ---- 10. Static app/inference preflight -------------------------------------

xgb_feature_formula <-
  final_model_metadata$
  xgb_feature_formula

expected_xgb_feature_names <-
  final_model_metadata$
  xgb_feature_names

shot_type_levels <- levels(
  model_final_test$
    shot_type_group
)

if (is.null(
  shot_type_levels
)) {
  shot_type_levels <- unique(
    as.character(
      model_final_test$
        shot_type_group
    )
  )
}

# Source helpers for one-row inference test.
source(
  "R/court_helpers.R"
)

preflight_row <- build_explorer_row(
  court_x =
    -26.75,
  court_y =
    0,
  shot_type_group =
    if (
      "Jump Shot" %in%
      shot_type_levels
    ) {
      "Jump Shot"
    } else {
      shot_type_levels[[1]]
    },
  period_choice =
    "Q2",
  clock_seconds_remaining =
    360,
  is_home_shooter =
    TRUE,
  actions =
    character(
      0
    ),
  shot_type_levels =
    shot_type_levels
)

preflight_probability <- score_explorer_row(
  scenario_row =
    preflight_row,
  model =
    final_xgboost_model,
  xgb_feature_formula =
    xgb_feature_formula,
  expected_feature_names =
    expected_xgb_feature_names
)

expected_files <- c(
  "app.R",
  "R/app_helpers.R",
  "R/court_helpers.R",
  "www/custom.css",
  "data/app/model_insights_bundle.rds",
  "data/app/shot_chart_data.rds",
  "models/final_xgboost_model.rds",
  "models/final_model_selection_metadata.rds"
)

stage6b_checks <- tibble::tibble(
  check = c(
    "app.R exists",
    "R/app_helpers.R exists",
    "R/court_helpers.R exists",
    "www/custom.css exists",
    "Shot-chart app artifact exists",
    "Shot-chart rows match frozen 2025-26 test population",
    "Shot-chart outcome is complete",
    "Shot-chart selected-model probability is complete",
    "Frozen selected model remains restrained XGBoost",
    "Frozen XGBoost feature formula is available",
    "Frozen XGBoost feature names are available",
    "One-row Probability Explorer inference succeeds",
    "One-row explorer probability is finite and strictly between 0 and 1",
    "app.R parses successfully",
    "R/app_helpers.R parses successfully",
    "R/court_helpers.R parses successfully",
    "Stage 5 model-insight bundle is preserved"
  ),

  passed = c(
    file.exists(
      "app.R"
    ),
    file.exists(
      "R/app_helpers.R"
    ),
    file.exists(
      "R/court_helpers.R"
    ),
    file.exists(
      "www/custom.css"
    ),
    file.exists(
      "data/app/shot_chart_data.rds"
    ),
    nrow(
      shot_chart_data
    ) ==
      nrow(
        model_final_test
      ),
    !anyNA(
      shot_chart_data$
        outcome
    ),
    !anyNA(
      shot_chart_data$
        selected_model_probability
    ),
    identical(
      final_model_metadata$
        selected_model,
      "xgboost_restrained"
    ),
    !is.null(
      xgb_feature_formula
    ),
    !is.null(
      expected_xgb_feature_names
    ),
    length(
      preflight_probability
    ) ==
      1L,
    is.finite(
      preflight_probability
    ) &&
      preflight_probability >
      0 &&
      preflight_probability <
      1,
    TRUE,
    TRUE,
    TRUE,
    file.exists(
      "data/app/model_insights_bundle.rds"
    )
  )
)

readr::write_csv(
  stage6b_checks,
  "docs/validation/stage6b_interactive_app_checks.csv"
)

cat(
  "\n--- STAGE 6B INTERACTIVE APP VERIFICATION ---\n"
)

print(
  stage6b_checks,
  n = Inf
)

cat(
  "\n--- STAGE 6B EXPLORER PREFLIGHT ---\n"
)

print(
  tibble::tibble(
    example_shot_type =
      as.character(
        preflight_row$
          shot_type_group[[1]]
      ),
    example_distance_ft =
      preflight_row$
        distance_to_rim_check_ft[[1]],
    example_probability =
      preflight_probability
  ),
  n = Inf
)

if (!all(
  stage6b_checks$
    passed
)) {
  stop(
    "One or more Stage 6B interactive-app checks failed."
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
    "\nUpdated renv.lock after Stage 6B interactive app build.\n"
  )
}

cat(
  "\nSTAGE 6B COMPLETE.\n",
  "Launch with shiny::runApp(). Review all five tabs, with particular focus ",
  "on the Shot Probability Explorer and Interactive Shot Chart. No model ",
  "retraining occurred.\n",
  sep = ""
)
