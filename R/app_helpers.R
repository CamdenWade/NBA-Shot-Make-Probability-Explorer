
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

