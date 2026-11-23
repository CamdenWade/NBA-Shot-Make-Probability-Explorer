# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 6A — Shiny Foundation + Evaluation/Insight Tabs
# File: scripts/06a_build_shiny_foundation.R
#
# PURPOSE:
#   Build the first working Shiny application shell using ONLY frozen,
#   precomputed Stage 5 app artifacts.
#
# Stage 6A implements:
#   1. Professional app shell / navigation
#   2. Shared KPI cards
#   3. Model Comparison tab
#   4. Calibration & Validation tab
#   5. Model Insights tab
#   6. Placeholder tabs for:
#        - Shot Probability Explorer
#        - Interactive Shot Chart
#
# IMPORTANT:
#   - No model fitting occurs in the Shiny app.
#   - No 2025-26 retuning occurs.
#   - All charts load precomputed Stage 5 artifacts.
# =============================================================================

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "ggplot2",
  "scales",
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

# ---- 2. Required inputs ------------------------------------------------------

required_files <- c(
  bundle =
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
      "Missing Stage 6A input(s): ",
      paste(
        missing_files,
        collapse = ", "
      ),
      ". Run scripts/05c_build_model_insight_artifacts.R first."
    )
  )
}

bundle <- readRDS(
  required_files[[
    "bundle"
  ]]
)

required_bundle_sections <- c(
  "performance",
  "performance_long",
  "calibration",
  "segment_diagnostics",
  "largest_segment_gaps",
  "feature_importance",
  "top_feature_importance",
  "insight_cards",
  "key_findings",
  "methodology"
)

if (!all(
  required_bundle_sections %in%
    names(
      bundle
    )
)) {
  stop(
    "model_insights_bundle.rds is missing one or more required sections."
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
  "docs/validation",
  recursive = TRUE,
  showWarnings = FALSE
)

# ---- 4. Write R/app_helpers.R -----------------------------------------------

app_helpers <- r"---(
# =============================================================================
# NBA Shot-Make Probability Explorer
# Shiny helpers
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

placeholder_panel <- function(
    title,
    text) {

  shiny::div(
    class =
      "placeholder-panel",

    shiny::div(
      class =
        "placeholder-eyebrow",
      "COMING IN THE NEXT BUILD STAGE"
    ),

    shiny::h2(
      title
    ),

    shiny::p(
      text
    )
  )
}
)---"

writeLines(
  app_helpers,
  "R/app_helpers.R"
)

# ---- 5. Write www/custom.css -------------------------------------------------

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
  max-width: 900px;
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

.placeholder-panel {
  background: var(--surface);
  border: 1px dashed #b8c6d9;
  border-radius: 16px;
  padding: 46px 34px;
  text-align: center;
  margin-top: 22px;
  box-shadow: var(--shadow);
}

.placeholder-panel h2 {
  margin-top: 5px;
}

.placeholder-panel p {
  color: var(--muted);
  max-width: 680px;
  margin: 10px auto 0 auto;
  line-height: 1.65;
}

.placeholder-eyebrow {
  color: var(--accent);
  font-size: 11px;
  font-weight: 800;
  letter-spacing: 0.1em;
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
  .kpi-grid {
    grid-template-columns: 1fr;
  }

  .app-hero h1 {
    font-size: 25px;
  }
}
)---"

writeLines(
  custom_css,
  "www/custom.css"
)

# ---- 6. Write app.R ----------------------------------------------------------

app_code <- r"---(
# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 6A Shiny Foundation
# =============================================================================

required_packages <- c(
  "dplyr",
  "ggplot2",
  "scales",
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
  library(scales)
  library(shiny)
  library(tibble)
})

source(
  "R/app_helpers.R"
)

bundle_path <-
  "data/app/model_insights_bundle.rds"

if (!file.exists(
  bundle_path
)) {
  stop(
    "Missing data/app/model_insights_bundle.rds. Run Stage 5C first."
  )
}

insights <- readRDS(
  bundle_path
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

insight_cards <-
  insights$
  insight_cards

key_findings <-
  insights$
  key_findings

selected_perf <- performance %>%
  filter(
    selected_model
  )

if (
  nrow(
    selected_perf
  ) !=
    1L
) {
  stop(
    "Expected exactly one selected model in the app performance artifact."
  )
}

top_feature <- feature_importance %>%
  arrange(
    shap_rank
  ) %>%
  slice(
    1
  )

largest_gap <- insights$
  largest_segment_gaps %>%
  slice(
    1
  )

segment_choices <- sort(
  unique(
    segment_diagnostics$
      segment
  )
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
          "NBA Shot-Make Probability Explorer"
        ),
        p(
          "A probability-first NBA shot model built with public hoopR data, ",
          "chronological validation, an untouched 2025–26 holdout, and a ",
          "frozen restrained XGBoost model."
        )
      ),

      div(
        class =
          "kpi-grid",

        metric_card(
          "Selected Model",
          selected_perf$
            model_label[[1]],
          "Chosen before the final holdout reveal.",
          "success"
        ),

        metric_card(
          "2025–26 Log-Loss",
          format_metric(
            selected_perf$
              test_log_loss[[1]]
          ),
          "Primary probability metric; lower is better."
        ),

        metric_card(
          "Overall Calibration",
          paste0(
            format_pct(
              selected_perf$
                test_mean_prediction[[1]]
            ),
            " vs ",
            format_pct(
              selected_perf$
                test_observed_rate[[1]]
            )
          ),
          "Predicted make probability vs. observed make rate."
        ),

        metric_card(
          "Top Driver",
          top_feature$
            feature_label[[1]],
          paste0(
            format_pct(
              top_feature$
                shap_share[[1]]
            ),
            " of global SHAP importance."
          )
        )
      ),

      placeholder_panel(
        "Interactive Probability Controls",
        paste0(
          "Stage 6B will add user-controlled shot location, shot type, ",
          "clock context, and pre-shot action inputs, then score the scenario ",
          "with the frozen final XGBoost model."
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
          "Explore shot locations and frozen model probabilities on an NBA half court."
        )
      ),

      placeholder_panel(
        "Interactive Court Visualization",
        paste0(
          "Stage 6B will add a filterable court view using the precomputed ",
          "2025–26 shot probabilities and shot context."
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
            7,

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
                "520px"
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
          "Interpretation Notes"
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
        coord_flip() +
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
        coord_flip() +
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
            round(
              validation_log_loss,
              3
            ),

          `Holdout Log-Loss` =
            round(
              test_log_loss,
              3
            ),

          `Validation Brier` =
            round(
              validation_brier,
              3
            ),

          `Holdout Brier` =
            round(
              test_brier,
              3
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

  output$
    calibration_plot <- renderPlot({

      ggplot(
        calibration_bins,
        aes(
          x =
            mean_predicted_probability,
          y =
            observed_make_rate,
          size =
            shots
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
          alpha =
            0.9
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
          size =
            "Shots",
          caption =
            "Dashed line = perfect calibration."
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

# ---- 7. Syntax checks --------------------------------------------------------

parse(
  file =
    "app.R"
)

parse(
  file =
    "R/app_helpers.R"
)

# ---- 8. App preflight --------------------------------------------------------

app_files <- c(
  "app.R",
  "R/app_helpers.R",
  "www/custom.css",
  "data/app/model_insights_bundle.rds"
)

app_file_status <- tibble::tibble(
  file =
    app_files,
  exists =
    file.exists(
      app_files
    )
)

bundle_checks <- tibble::tibble(
  check = c(
    "Performance table has four model families",
    "Exactly one selected model exists",
    "Selected model is restrained XGBoost",
    "20-bin calibration table is available",
    "Segment diagnostics are available",
    "Feature importance is available",
    "Insight cards are available",
    "Key findings are available"
  ),

  passed = c(
    nrow(
      bundle$
        performance
    ) ==
      4L,

    sum(
      bundle$
        performance$
        selected_model
    ) ==
      1L,

    bundle$
      performance %>%
      dplyr::filter(
        selected_model
      ) %>%
      dplyr::pull(
        model
      ) %>%
      identical(
        "xgboost_restrained"
      ),

    nrow(
      bundle$
        calibration$
        reliability_20bin
    ) ==
      20L,

    nrow(
      bundle$
        segment_diagnostics
    ) >
      0L,

    nrow(
      bundle$
        feature_importance
    ) >
      0L,

    nrow(
      bundle$
        insight_cards
    ) >
      0L,

    nrow(
      bundle$
        key_findings
    ) >
      0L
  )
)

stage6a_checks <- dplyr::bind_rows(
  tibble::tibble(
    check =
      paste0(
        "File exists: ",
        app_file_status$
          file
      ),
    passed =
      app_file_status$
        exists
  ),

  bundle_checks,

  tibble::tibble(
    check = c(
      "app.R parses successfully",
      "R/app_helpers.R parses successfully"
    ),

    passed = c(
      TRUE,
      TRUE
    )
  )
)

readr::write_csv(
  stage6a_checks,
  "docs/validation/stage6a_shiny_foundation_checks.csv"
)

cat(
  "\n--- STAGE 6A SHINY FOUNDATION VERIFICATION ---\n"
)

print(
  stage6a_checks,
  n = Inf
)

if (!all(
  stage6a_checks$
    passed
)) {
  stop(
    "One or more Stage 6A Shiny foundation checks failed."
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
    "\nUpdated renv.lock after Stage 6A Shiny foundation.\n"
  )
}

cat(
  "\nSTAGE 6A COMPLETE.\n",
  "Launch the app with shiny::runApp(). Review the Model Comparison, ",
  "Calibration & Validation, and Model Insights tabs. The Shot Probability ",
  "Explorer and Interactive Shot Chart remain placeholders until Stage 6B.\n",
  sep = ""
)
