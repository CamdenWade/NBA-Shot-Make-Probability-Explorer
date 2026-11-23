
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

