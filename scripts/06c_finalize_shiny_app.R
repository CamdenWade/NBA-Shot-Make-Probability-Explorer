
# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 6C — Final App Polish + Exploration Filters
# File: scripts/06c_finalize_shiny_app.R
#
# PURPOSE:
#   Finalize Stage 6 application functionality before Stage 7 QA.
#
# Changes:
#   1. Complete NBA half-court markings
#   2. Enrich 2025-26 shot chart with team/player exploration fields
#   3. Add team and player filters (identity is NOT used by the model)
#   4. Replace alpha-only probability encoding with clearer color encoding
#   5. Improve Probability Explorer controls/labels
#
# No model fitting, tuning, or recalibration occurs.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

# ---- 1. Required files -------------------------------------------------------

required_files <- c(
  "app.R",
  "R/app_helpers.R",
  "R/court_helpers.R",
  "www/custom.css",
  "data/processed/model_final_test.rds",
  "data/processed/shots_final_test_features.rds",
  "data/processed/stage5a_final_test_scored.rds",
  "models/final_xgboost_model.rds",
  "models/final_model_selection_metadata.rds",
  "data/app/model_insights_bundle.rds"
)

missing_files <- required_files[
  !file.exists(
    required_files
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing Stage 6C input(s): ",
      paste(
        missing_files,
        collapse = ", "
      )
    )
  )
}

model_final_test <- readRDS(
  "data/processed/model_final_test.rds"
)

full_final_features <- readRDS(
  "data/processed/shots_final_test_features.rds"
)

scored_final_test <- readRDS(
  "data/processed/stage5a_final_test_scored.rds"
)

final_xgboost_model <- readRDS(
  "models/final_xgboost_model.rds"
)

final_model_metadata <- readRDS(
  "models/final_model_selection_metadata.rds"
)

if (
  final_model_metadata$
    selected_model !=
    "xgboost_restrained"
) {
  stop(
    "Frozen selected model is no longer restrained XGBoost."
  )
}

# ---- 2. Rebuild enriched shot-chart artifact --------------------------------

identity_columns <- c(
  "shot_id",
  "team_abbrev_canonical",
  "shooter_name"
)

missing_identity_columns <- setdiff(
  identity_columns,
  names(
    full_final_features
  )
)

if (length(missing_identity_columns) > 0) {
  stop(
    paste0(
      "Missing exploration identity column(s): ",
      paste(
        missing_identity_columns,
        collapse = ", "
      )
    )
  )
}

identity_lookup <- full_final_features %>%
  select(
    all_of(
      identity_columns
    )
  ) %>%
  distinct(
    shot_id,
    .keep_all =
      TRUE
  )

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
  left_join(
    identity_lookup,
    by =
      "shot_id"
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
    )
) {
  stop(
    "Enriched shot-chart artifact changed the frozen final-test row count."
  )
}

if (
  anyNA(
    shot_chart_data$
      outcome
  ) ||
  anyNA(
    shot_chart_data$
      selected_model_probability
  )
) {
  stop(
    "Enriched shot-chart artifact contains missing outcome/probability."
  )
}

saveRDS(
  shot_chart_data,
  "data/app/shot_chart_data.rds",
  compress =
    "xz"
)

# ---- 3. Rewrite court helper with complete court markings --------------------

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
    -NBA_CORNER_THREE_Y,
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
    -pi / 2,
    pi / 2,
    length.out =
      150
  )

  restricted_arc <- tibble::tibble(
    plot_x =
      4 *
      sin(
        restricted_theta
      ),
    plot_y =
      (
        NBA_RIM_X +
        4 *
        cos(
          restricted_theta
        )
      ) +
      47
  )

  circle_theta <- seq(
    0,
    2 *
      pi,
    length.out =
      240
  )

  free_throw_circle <- tibble::tibble(
    plot_x =
      6 *
      cos(
        circle_theta
      ),
    plot_y =
      19 +
      6 *
      sin(
        circle_theta
      )
  )

  rim_circle <- tibble::tibble(
    plot_x =
      0.75 *
      cos(
        circle_theta
      ),
    plot_y =
      5.25 +
      0.75 *
      sin(
        circle_theta
      )
  )

  corner_transition_plot_y <-
    (
      NBA_RIM_X +
      sqrt(
        NBA_THREE_RADIUS^2 -
          NBA_CORNER_THREE_Y^2
      )
    ) +
    47

  list(
    three_arc =
      three_arc,
    restricted_arc =
      restricted_arc,
    free_throw_circle =
      free_throw_circle,
    rim_circle =
      rim_circle,
    corner_transition_plot_y =
      corner_transition_plot_y
  )
}

add_half_court <- function(
    plot_object,
    line_width = 0.65) {

  court <- build_half_court_layers()

  plot_object +

    # Court boundary.
    ggplot2::annotate(
      "segment",
      x = -25,
      xend = 25,
      y = 0,
      yend = 0,
      linewidth = line_width
    ) +
    ggplot2::annotate(
      "segment",
      x = -25,
      xend = -25,
      y = 0,
      yend = 47,
      linewidth = line_width
    ) +
    ggplot2::annotate(
      "segment",
      x = 25,
      xend = 25,
      y = 0,
      yend = 47,
      linewidth = line_width
    ) +
    ggplot2::annotate(
      "segment",
      x = -25,
      xend = 25,
      y = 47,
      yend = 47,
      linewidth = line_width
    ) +

    # Paint / key: 16 feet wide; free-throw line 19 feet from baseline.
    ggplot2::annotate(
      "segment",
      x = -8,
      xend = -8,
      y = 0,
      yend = 19,
      linewidth = line_width
    ) +
    ggplot2::annotate(
      "segment",
      x = 8,
      xend = 8,
      y = 0,
      yend = 19,
      linewidth = line_width
    ) +
    ggplot2::annotate(
      "segment",
      x = -8,
      xend = 8,
      y = 19,
      yend = 19,
      linewidth = line_width
    ) +

    # Backboard.
    ggplot2::annotate(
      "segment",
      x = -3,
      xend = 3,
      y = 4,
      yend = 4,
      linewidth = line_width
    ) +

    # Rim.
    ggplot2::geom_path(
      data =
        court$
          rim_circle,
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

    # Restricted area.
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

    # Free-throw circle.
    ggplot2::geom_path(
      data =
        court$
          free_throw_circle,
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

    # Corner-three lines.
    ggplot2::annotate(
      "segment",
      x = -NBA_CORNER_THREE_Y,
      xend = -NBA_CORNER_THREE_Y,
      y = 0,
      yend =
        court$
          corner_transition_plot_y,
      linewidth = line_width
    ) +
    ggplot2::annotate(
      "segment",
      x = NBA_CORNER_THREE_Y,
      xend = NBA_CORNER_THREE_Y,
      y = 0,
      yend =
        court$
          corner_transition_plot_y,
      linewidth = line_width
    ) +

    # Three-point arc.
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

# ---- 4. Safe app.R patch helper ---------------------------------------------

app_path <- "app.R"

app_text <- paste(
  readLines(
    app_path,
    warn =
      FALSE
  ),
  collapse =
    "\n"
)

replace_once <- function(
    text,
    old,
    new,
    label) {

  positions <- gregexpr(
    old,
    text,
    fixed =
      TRUE
  )[[1]]

  count <- if (
    length(
      positions
    ) ==
      1L &&
    positions[[1]] ==
      -1L
  ) {
    0L
  } else {
    length(
      positions
    )
  }

  if (
    count !=
      1L
  ) {
    stop(
      paste0(
        "Could not safely patch ",
        label,
        ": expected exactly 1 match, found ",
        count,
        "."
      )
    )
  }

  sub(
    old,
    new,
    text,
    fixed =
      TRUE
  )
}

# ---- 5. Patch app.R ----------------------------------------------------------

# 5A. Add team/player choices.
app_text <- replace_once(
  app_text,
  old = r"---(chart_shot_type_choices <- sort(
  unique(
    as.character(
      shot_chart_data$
        shot_type_group
    )
  )
)

xgb_feature_formula <-)---",
  new = r"---(chart_shot_type_choices <- sort(
  unique(
    as.character(
      shot_chart_data$
        shot_type_group
    )
  )
)

chart_team_choices <- sort(
  unique(
    shot_chart_data$
      team_abbrev_canonical[
        !is.na(
          shot_chart_data$
            team_abbrev_canonical
        )
      ]
  )
)

chart_player_choices <- sort(
  unique(
    shot_chart_data$
      shooter_name[
        !is.na(
          shot_chart_data$
            shooter_name
        )
      ]
  )
)

xgb_feature_formula <-)---",
  label =
    "team/player choices"
)

# 5B. Remove cluttered slider tick labels.
app_text <- replace_once(
  app_text,
  old = r"---(              sliderInput(
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
              ),)---",
  new = r"---(              sliderInput(
                "explorer_clock",
                "Seconds Remaining in Period",
                min =
                  0,
                max =
                  720,
                value =
                  360,
                step =
                  1,
                ticks =
                  FALSE
              ),)---",
  label =
    "explorer clock slider"
)

# 5C. Add team/player filter controls.
app_text <- replace_once(
  app_text,
  old = r"---(            selectInput(
              "chart_outcome",
              "Observed Result",)---",
  new = r"---(            selectInput(
              "chart_team",
              "Team",
              choices =
                c(
                  "All",
                  chart_team_choices
                ),
              selected =
                "All"
            ),

            selectInput(
              "chart_player",
              "Player",
              choices =
                c(
                  "All",
                  chart_player_choices
                ),
              selected =
                "All"
            ),

            selectInput(
              "chart_outcome",
              "Observed Result",)---",
  label =
    "team/player filter UI"
)

# 5D. Dynamic player choices.
app_text <- replace_once(
  app_text,
  old = r"---(  filtered_shots <- reactive({

    data <- shot_chart_data)---",
  new = r"---(  observeEvent(
    input$
      chart_team,
    {

      if (
        is.null(
          input$
            chart_team
        ) ||
        input$
          chart_team ==
          "All"
      ) {

        player_choices <- chart_player_choices

      } else {

        player_choices <- shot_chart_data %>%
          filter(
            team_abbrev_canonical ==
              input$
                chart_team
          ) %>%
          pull(
            shooter_name
          ) %>%
          unique() %>%
          sort()
      }

      player_choices <- player_choices[
        !is.na(
          player_choices
        )
      ]

      updateSelectInput(
        session,
        "chart_player",
        choices =
          c(
            "All",
            player_choices
          ),
        selected =
          "All"
      )
    },
    ignoreInit =
      TRUE
  )

  filtered_shots <- reactive({

    data <- shot_chart_data)---",
  label =
    "dynamic player filtering"
)

# 5E. Apply team/player filters.
app_text <- replace_once(
  app_text,
  old = r"---(    if (
      !is.null(
        input$
          chart_outcome
      ) &&
      input$
        chart_outcome ==
        "Make"
    ) {)---",
  new = r"---(    if (
      !is.null(
        input$
          chart_team
      ) &&
      input$
        chart_team !=
        "All"
    ) {
      data <- data %>%
        filter(
          team_abbrev_canonical ==
            input$
              chart_team
        )
    }

    if (
      !is.null(
        input$
          chart_player
      ) &&
      input$
        chart_player !=
        "All"
    ) {
      data <- data %>%
        filter(
          shooter_name ==
            input$
              chart_player
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
    ) {)---",
  label =
    "team/player reactive filters"
)

# 5F. Probability points: use color instead of alpha.
app_text <- replace_once(
  app_text,
  old = r"---(        base <- ggplot(
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
          court_plot_theme())---",
  new = r"---(        base <- ggplot(
          data,
          aes(
            x =
              plot_x,
            y =
              plot_y,
            colour =
              selected_model_probability
          )
        ) +
          geom_point(
            size =
              1.25,
            alpha =
              0.72
          ) +
          scale_colour_gradient(
            low =
              "#dbe7f3",
            high =
              "#174ea6",
            limits =
              c(
                0,
                1
              ),
            labels =
              scales::percent_format(
                accuracy =
                  1
              )
          ) +
          labs(
            colour =
              "Predicted Make Probability",
            caption =
              "Color encodes frozen-model make probability; click a point for details."
          ) +
          court_plot_theme())---",
  label =
    "probability chart encoding"
)

# 5G. Outcome points: use color for clearer result encoding.
app_text <- replace_once(
  app_text,
  old = r"---(        base <- ggplot(
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
          court_plot_theme())---",
  new = r"---(        base <- ggplot(
          plot_data,
          aes(
            x =
              plot_x,
            y =
              plot_y,
            colour =
              result
          )
        ) +
          geom_point(
            alpha =
              0.55,
            size =
              1.25
          ) +
          scale_colour_manual(
            values =
              c(
                "Miss" =
                  "#a8b3c2",
                "Make" =
                  "#174ea6"
              )
          ) +
          labs(
            colour =
              "Observed Result",
            caption =
              "Up to 20,000 matching shots shown; click a point for details."
          ) +
          court_plot_theme())---",
  label =
    "outcome chart encoding"
)

# 5H. Accent selected explorer shot point.
app_text <- replace_once(
  app_text,
  old = r"---(        geom_point(
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
        ) +)---",
  new = r"---(        geom_point(
          data =
            plot_location,
          aes(
            x =
              plot_x,
            y =
              plot_y
          ),
          size =
            5,
          colour =
            "#174ea6"
        ) +)---",
  label =
    "explorer selected point"
)

# 5I. Rename misleading "3PT Region" label.
app_text <- replace_once(
  app_text,
  old =
    '"3PT Region"',
  new =
    '"Court Region"',
  label =
    "court-region label"
)

# 5J. Add player/team to clicked-shot detail.
app_text <- replace_once(
  app_text,
  old = r"---(        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Date"
          ),)---",
  new = r"---(        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Player"
          ),
          div(
            class =
              "shot-detail-value",
            ifelse(
              is.na(
                shot$
                  shooter_name[[1]]
              ),
              "Unknown",
              shot$
                shooter_name[[1]]
            )
          )
        ),

        div(
          class =
            "shot-detail",
          div(
            class =
              "shot-detail-label",
            "Team"
          ),
          div(
            class =
              "shot-detail-value",
            ifelse(
              is.na(
                shot$
                  team_abbrev_canonical[[1]]
              ),
              "Unknown",
              shot$
                team_abbrev_canonical[[1]]
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
          ),)---",
  label =
    "clicked-shot player/team details"
)

writeLines(
  strsplit(
    app_text,
    "\n",
    fixed =
      TRUE
  )[[1]],
  app_path
)

# ---- 6. Parse patched files --------------------------------------------------

parse(
  file =
    "app.R"
)

parse(
  file =
    "R/court_helpers.R"
)

parse(
  file =
    "R/app_helpers.R"
)

# ---- 7. Frozen-model inference preflight ------------------------------------

source(
  "R/court_helpers.R"
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
    final_model_metadata$
      xgb_feature_formula,
  expected_feature_names =
    final_model_metadata$
      xgb_feature_names
)

# ---- 8. Verification ---------------------------------------------------------

stage6c_checks <- tibble::tibble(
  check = c(
    "Enriched shot-chart rows match frozen final-test population",
    "Shot-chart outcome remains complete",
    "Shot-chart probability remains complete",
    "Team exploration field is present",
    "Player exploration field is present",
    "At least 30 team values are available for exploration",
    "Player exploration contains non-missing names",
    "Frozen selected model remains restrained XGBoost",
    "One-row frozen-model inference still succeeds",
    "One-row probability is finite and strictly between 0 and 1",
    "app.R parses successfully",
    "R/court_helpers.R parses successfully",
    "R/app_helpers.R parses successfully",
    "Stage 5 model-insight bundle is preserved"
  ),

  passed = c(
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
    "team_abbrev_canonical" %in%
      names(
        shot_chart_data
      ),
    "shooter_name" %in%
      names(
        shot_chart_data
      ),
    dplyr::n_distinct(
      shot_chart_data$
        team_abbrev_canonical,
      na.rm =
        TRUE
    ) >=
      30L,
    sum(
      !is.na(
        shot_chart_data$
          shooter_name
      )
    ) >
      0L,
    identical(
      final_model_metadata$
        selected_model,
      "xgboost_restrained"
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
  stage6c_checks,
  "docs/validation/stage6c_final_app_checks.csv"
)

cat(
  "\n--- STAGE 6C FINAL APP VERIFICATION ---\n"
)

print(
  stage6c_checks,
  n = Inf
)

cat(
  "\n--- STAGE 6C EXPLORATION COVERAGE ---\n"
)

print(
  tibble::tibble(
    frozen_shots =
      nrow(
        shot_chart_data
      ),
    teams =
      dplyr::n_distinct(
        shot_chart_data$
          team_abbrev_canonical,
        na.rm =
          TRUE
      ),
    shooters =
      dplyr::n_distinct(
        shot_chart_data$
          shooter_name,
        na.rm =
          TRUE
      ),
    example_probability =
      preflight_probability
  ),
  n = Inf
)

if (!all(
  stage6c_checks$
    passed
)) {
  stop(
    "One or more Stage 6C final-app checks failed."
  )
}

if (requireNamespace(
  "renv",
  quietly =
    TRUE
)) {
  renv::snapshot(
    prompt =
      FALSE
  )

  cat(
    "\nUpdated renv.lock after Stage 6C final app polish.\n"
  )
}

cat(
  "\nSTAGE 6C COMPLETE.\n",
  "Stage 6 application construction is complete. Launch with shiny::runApp() ",
  "and perform a final visual/interactivity review. Next: Stage 7 QA, mobile ",
  "responsiveness, reproducibility checks, and deployment readiness.\n",
  sep = ""
)
