
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
                  1,
                ticks =
                  FALSE
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
            5,
          colour =
            "#174ea6"
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
            "Court Region"
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

  observeEvent(
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

      shiny::validate(
        shiny::need(
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
          court_plot_theme()

      } else {

        base <- ggplot(
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
