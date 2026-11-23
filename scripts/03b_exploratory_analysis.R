# =============================================================================
# NBA Shot-Make Probability Explorer
# Stage 3B — Exploratory Data Analysis
# File: scripts/03b_exploratory_analysis.R
#
# Purpose:
#   Build professional, reproducible EDA summaries and basketball visuals from
#   the Stage 3A feature-engineered datasets.
#
# Rules:
#   - Outcome is used only for 2022-23 through 2024-25 development data.
#   - 2025-26 outcome remains completely unavailable.
#   - The 14 "No Shot (Default Shot)" development rows are audited explicitly.
#   - The 2025-26-only "Heave Jump Shot" taxonomy change is audited structurally.
#   - No model is trained in this script.
# =============================================================================

required_packages <- c(
  "dplyr",
  "ggplot2",
  "purrr",
  "readr",
  "scales",
  "tibble",
  "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(scales)
  library(tibble)
  library(tidyr)
})

development_feature_path <- "data/processed/shots_development_features.rds"
final_test_feature_path <- "data/processed/shots_final_test_features.rds"

if (!file.exists(development_feature_path)) {
  stop("Missing shots_development_features.rds. Run 03a_feature_engineering.R first.")
}

if (!file.exists(final_test_feature_path)) {
  stop("Missing shots_final_test_features.rds. Run 03a_feature_engineering.R first.")
}

development_features <- readRDS(development_feature_path)
final_test_features <- readRDS(final_test_feature_path)

if (!("outcome" %in% names(development_features))) {
  stop("Development feature data do not contain outcome.")
}

if ("outcome" %in% names(final_test_features)) {
  stop("Final-test feature data unexpectedly contain outcome.")
}

if (!dir.exists("docs/eda")) {
  dir.create("docs/eda", recursive = TRUE)
}

theme_nba_explorer <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = base_size + 4),
      plot.subtitle = element_text(size = base_size),
      plot.caption = element_text(size = base_size - 2, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# -----------------------------------------------------------------------------
# Court helper
# -----------------------------------------------------------------------------

left_rim_x <- -41.75
left_rim_y <- 0
three_arc_radius_ft <- 23.75
corner_three_y_ft <- 22
corner_three_transition_dx_ft <- sqrt(
  three_arc_radius_ft^2 - corner_three_y_ft^2
)

make_arc <- function(center_x, center_y, radius, theta_start, theta_end, n = 250) {
  theta <- seq(theta_start, theta_end, length.out = n)
  tibble(
    x = center_x + radius * cos(theta),
    y = center_y + radius * sin(theta)
  )
}

three_theta_limit <- asin(corner_three_y_ft / three_arc_radius_ft)

three_arc <- make_arc(
  left_rim_x, left_rim_y, three_arc_radius_ft,
  -three_theta_limit, three_theta_limit
)

restricted_arc <- make_arc(
  left_rim_x, left_rim_y, 4,
  -pi / 2, pi / 2
)

hoop_circle <- make_arc(
  left_rim_x, left_rim_y, 0.75,
  0, 2 * pi
)

free_throw_arc <- make_arc(
  -28, 0, 6,
  -pi / 2, pi / 2
)

add_half_court <- function(p, line_width = 0.55) {
  p +
    geom_segment(
      aes(x = -47, xend = -47, y = -25, yend = 25),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_segment(
      aes(x = -47, xend = 0, y = -25, yend = -25),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_segment(
      aes(x = -47, xend = 0, y = 25, yend = 25),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_segment(
      aes(x = 0, xend = 0, y = -25, yend = 25),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_rect(
      aes(xmin = -47, xmax = -28, ymin = -8, ymax = 8),
      inherit.aes = FALSE, fill = NA, linewidth = line_width
    ) +
    geom_segment(
      aes(x = -43, xend = -43, y = -3, yend = 3),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_path(
      data = hoop_circle,
      aes(x = x, y = y),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_path(
      data = restricted_arc,
      aes(x = x, y = y),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_path(
      data = free_throw_arc,
      aes(x = x, y = y),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_segment(
      aes(
        x = -47,
        xend = left_rim_x + corner_three_transition_dx_ft,
        y = -corner_three_y_ft,
        yend = -corner_three_y_ft
      ),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_segment(
      aes(
        x = -47,
        xend = left_rim_x + corner_three_transition_dx_ft,
        y = corner_three_y_ft,
        yend = corner_three_y_ft
      ),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    geom_path(
      data = three_arc,
      aes(x = x, y = y),
      inherit.aes = FALSE, linewidth = line_width
    ) +
    coord_fixed(
      xlim = c(-47, 0),
      ylim = c(-25, 25),
      expand = FALSE,
      clip = "off"
    )
}

# -----------------------------------------------------------------------------
# Audit the 14 default/no-shot labels
# -----------------------------------------------------------------------------

default_shot_audit <- development_features %>%
  filter(
    type_id == 91L |
      type_text == "No Shot (Default Shot)"
  ) %>%
  select(
    shot_id,
    hoopr_season,
    nba_season,
    game_id,
    game_date,
    period_number,
    clock_display_value,
    team_abbrev_canonical,
    shooter_name,
    type_id,
    type_text,
    coordinate_x,
    coordinate_y,
    distance_to_rim_check_ft,
    shot_zone_geometry,
    outcome
  )

write_csv(
  default_shot_audit,
  "docs/eda/default_shot_type_audit.csv"
)

cat("\n--- DEFAULT / NO-SHOT TYPE AUDIT ---\n")
print(default_shot_audit, n = Inf)

# -----------------------------------------------------------------------------
# Audit 2025-26-only Heave Jump Shot taxonomy
# -----------------------------------------------------------------------------

heave_type_audit <- final_test_features %>%
  filter(
    type_id == 282L |
      type_text == "Heave Jump Shot"
  ) %>%
  summarise(
    shots = n(),
    min_distance_ft = min(distance_to_rim_check_ft, na.rm = TRUE),
    p25_distance_ft = quantile(distance_to_rim_check_ft, 0.25, na.rm = TRUE, names = FALSE),
    median_distance_ft = median(distance_to_rim_check_ft, na.rm = TRUE),
    p75_distance_ft = quantile(distance_to_rim_check_ft, 0.75, na.rm = TRUE, names = FALSE),
    max_distance_ft = max(distance_to_rim_check_ft, na.rm = TRUE),
    geometry_heave_pct = mean(is_heave),
    geometry_three_pct = mean(is_three_geometry),
    last_5_seconds_period_pct = mean(is_last_5_seconds_period)
  )

heave_type_by_period <- final_test_features %>%
  filter(
    type_id == 282L |
      type_text == "Heave Jump Shot"
  ) %>%
  count(period_group, name = "shots") %>%
  mutate(pct = shots / sum(shots))

write_csv(
  heave_type_audit,
  "docs/eda/final_test_heave_jump_shot_audit.csv"
)

write_csv(
  heave_type_by_period,
  "docs/eda/final_test_heave_jump_shot_by_period.csv"
)

cat("\n--- FINAL-TEST HEAVE JUMP SHOT STRUCTURAL AUDIT ---\n")
print(heave_type_audit, n = Inf)
print(heave_type_by_period, n = Inf)

# -----------------------------------------------------------------------------
# Season summary
# -----------------------------------------------------------------------------

season_summary <- development_features %>%
  group_by(
    hoopr_season,
    nba_season
  ) %>%
  summarise(
    shots = n(),
    makes = sum(outcome == 1L),
    make_rate = mean(outcome),
    mean_distance_ft = mean(distance_to_rim_check_ft),
    three_attempt_rate = mean(is_three_geometry),
    corner_three_rate = mean(is_corner_three_geometry),
    home_shot_rate = mean(is_home_shooter),
    overtime_rate = mean(is_overtime),
    .groups = "drop"
  )

write_csv(
  season_summary,
  "docs/eda/season_summary.csv"
)

cat("\n--- DEVELOPMENT SEASON SUMMARY ---\n")
print(season_summary, n = Inf)

# -----------------------------------------------------------------------------
# Make rate by distance
# -----------------------------------------------------------------------------

distance_efficiency <- development_features %>%
  mutate(
    distance_bin_ft = pmin(
      floor(distance_to_rim_check_ft),
      50
    )
  ) %>%
  group_by(distance_bin_ft) %>%
  summarise(
    shots = n(),
    makes = sum(outcome == 1L),
    make_rate = mean(outcome),
    .groups = "drop"
  ) %>%
  filter(shots >= 100)

write_csv(
  distance_efficiency,
  "docs/eda/make_rate_by_distance.csv"
)

p_distance <- ggplot(
  distance_efficiency,
  aes(x = distance_bin_ft, y = make_rate)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "NBA Make Rate Declines Sharply With Shot Distance",
    subtitle = "Development seasons 2022–23 through 2024–25; 1-foot bins with at least 100 attempts",
    x = "Distance from rim (feet)",
    y = "Observed make rate",
    caption = "Source: SportsDataverse / hoopR public NBA shot data."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/eda/make_rate_by_distance.png",
  p_distance,
  width = 10,
  height = 6,
  dpi = 180
)

# -----------------------------------------------------------------------------
# Zone efficiency
# -----------------------------------------------------------------------------

zone_efficiency <- development_features %>%
  group_by(shot_zone_geometry) %>%
  summarise(
    shots = n(),
    makes = sum(outcome == 1L),
    make_rate = mean(outcome),
    share_of_attempts = n() / nrow(development_features),
    .groups = "drop"
  ) %>%
  arrange(desc(make_rate))

write_csv(
  zone_efficiency,
  "docs/eda/zone_efficiency.csv"
)

p_zone <- zone_efficiency %>%
  mutate(
    shot_zone_geometry = reorder(
      shot_zone_geometry,
      make_rate
    )
  ) %>%
  ggplot(
    aes(x = shot_zone_geometry, y = make_rate)
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed Make Rate by Geometry-Based Shot Zone",
    subtitle = "Development seasons 2022–23 through 2024–25",
    x = NULL,
    y = "Observed make rate",
    caption = "Zones are derived from public court coordinates; associations are descriptive, not causal."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/eda/make_rate_by_zone.png",
  p_zone,
  width = 9,
  height = 6,
  dpi = 180
)

# -----------------------------------------------------------------------------
# Shot-type group efficiency
# -----------------------------------------------------------------------------

shot_group_efficiency <- development_features %>%
  group_by(shot_type_group) %>%
  summarise(
    shots = n(),
    makes = sum(outcome == 1L),
    make_rate = mean(outcome),
    share_of_attempts = n() / nrow(development_features),
    .groups = "drop"
  ) %>%
  arrange(desc(shots))

write_csv(
  shot_group_efficiency,
  "docs/eda/shot_type_group_efficiency.csv"
)

p_shot_group <- shot_group_efficiency %>%
  filter(shot_type_group != "Other") %>%
  mutate(
    shot_type_group = reorder(
      shot_type_group,
      make_rate
    )
  ) %>%
  ggplot(
    aes(x = shot_type_group, y = make_rate)
  ) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed Make Rate Varies by Shot Family",
    subtitle = "Grouped ESPN shot types; development seasons only",
    x = NULL,
    y = "Observed make rate",
    caption = "Descriptive only; shot family is confounded with location, player skill, and game context."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/eda/make_rate_by_shot_type_group.png",
  p_shot_group,
  width = 9,
  height = 6.5,
  dpi = 180
)

# -----------------------------------------------------------------------------
# Zone stability by season
# -----------------------------------------------------------------------------

zone_by_season <- development_features %>%
  group_by(
    nba_season,
    shot_zone_geometry
  ) %>%
  summarise(
    shots = n(),
    make_rate = mean(outcome),
    .groups = "drop"
  )

write_csv(
  zone_by_season,
  "docs/eda/zone_by_season.csv"
)

p_zone_season <- ggplot(
  zone_by_season,
  aes(
    x = nba_season,
    y = make_rate,
    group = shot_zone_geometry,
    linetype = shot_zone_geometry
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Shot-Zone Efficiency Across Development Seasons",
    subtitle = "Cross-season stability check before modeling",
    x = "NBA season",
    y = "Observed make rate",
    linetype = "Shot zone",
    caption = "Source: SportsDataverse / hoopR public NBA shot data."
  ) +
  theme_nba_explorer()

ggsave(
  "docs/eda/zone_efficiency_by_season.png",
  p_zone_season,
  width = 10,
  height = 6.5,
  dpi = 180
)

# -----------------------------------------------------------------------------
# Game-clock context
# -----------------------------------------------------------------------------

end_period_summary <- development_features %>%
  mutate(
    clock_context = case_when(
      is_last_5_seconds_period ~ "Last 5 seconds",
      is_last_24_seconds_period ~ "6–24 seconds",
      is_last_2_minutes_period ~ "25–120 seconds",
      TRUE ~ "More than 2 minutes"
    )
  ) %>%
  group_by(clock_context) %>%
  summarise(
    shots = n(),
    make_rate = mean(outcome),
    mean_distance_ft = mean(distance_to_rim_check_ft),
    three_attempt_rate = mean(is_three_geometry),
    .groups = "drop"
  )

clock_order <- c(
  "More than 2 minutes",
  "25–120 seconds",
  "6–24 seconds",
  "Last 5 seconds"
)

end_period_summary <- end_period_summary %>%
  mutate(
    clock_context = factor(
      clock_context,
      levels = clock_order
    )
  ) %>%
  arrange(clock_context)

write_csv(
  end_period_summary,
  "docs/eda/end_period_context_summary.csv"
)

p_clock <- ggplot(
  end_period_summary,
  aes(x = clock_context, y = make_rate)
) +
  geom_col() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed Shooting Efficiency by Time Remaining in the Period",
    subtitle = "Game-clock context; not shot-clock data",
    x = NULL,
    y = "Observed make rate",
    caption = "Late-period differences may reflect shot distance, selection, and game context."
  ) +
  theme_nba_explorer() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

ggsave(
  "docs/eda/make_rate_by_period_clock_context.png",
  p_clock,
  width = 9,
  height = 6,
  dpi = 180
)

# -----------------------------------------------------------------------------
# Home / away summary
# -----------------------------------------------------------------------------

home_away_summary <- development_features %>%
  group_by(venue_side) %>%
  summarise(
    shots = n(),
    make_rate = mean(outcome),
    mean_distance_ft = mean(distance_to_rim_check_ft),
    three_attempt_rate = mean(is_three_geometry),
    .groups = "drop"
  )

write_csv(
  home_away_summary,
  "docs/eda/home_away_summary.csv"
)

cat("\n--- HOME / AWAY DESCRIPTIVE SUMMARY ---\n")
print(home_away_summary, n = Inf)

# -----------------------------------------------------------------------------
# Spatial shot density grid
# -----------------------------------------------------------------------------

grid_size_ft <- 2

shot_density_grid <- development_features %>%
  mutate(
    x_grid =
      floor((court_x_folded + 47) / grid_size_ft) *
      grid_size_ft - 47 + grid_size_ft / 2,
    y_grid =
      floor((court_y_folded + 25) / grid_size_ft) *
      grid_size_ft - 25 + grid_size_ft / 2
  ) %>%
  count(
    x_grid,
    y_grid,
    name = "shots"
  )

write_csv(
  shot_density_grid,
  "docs/eda/spatial_shot_density_grid.csv"
)

p_density <- ggplot(
  shot_density_grid,
  aes(
    x = x_grid,
    y = y_grid,
    fill = shots
  )
) +
  geom_tile(
    width = grid_size_ft,
    height = grid_size_ft
  ) +
  scale_fill_viridis_c(
    trans = "sqrt",
    labels = comma
  ) +
  labs(
    title = "NBA Shot Attempt Density",
    subtitle = "2-foot court grid; development seasons 2022–23 through 2024–25",
    x = NULL,
    y = NULL,
    fill = "Attempts",
    caption = "Both attacking halves are folded onto one half-court."
  ) +
  theme_nba_explorer() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p_density <- add_half_court(p_density)

ggsave(
  "docs/eda/shot_density_half_court.png",
  p_density,
  width = 8,
  height = 8,
  dpi = 200
)

# -----------------------------------------------------------------------------
# Spatial observed make-rate grid
# -----------------------------------------------------------------------------

spatial_efficiency_grid <- development_features %>%
  mutate(
    x_grid =
      floor((court_x_folded + 47) / grid_size_ft) *
      grid_size_ft - 47 + grid_size_ft / 2,
    y_grid =
      floor((court_y_folded + 25) / grid_size_ft) *
      grid_size_ft - 25 + grid_size_ft / 2
  ) %>%
  group_by(
    x_grid,
    y_grid
  ) %>%
  summarise(
    shots = n(),
    make_rate = mean(outcome),
    .groups = "drop"
  ) %>%
  filter(shots >= 100)

write_csv(
  spatial_efficiency_grid,
  "docs/eda/spatial_make_rate_grid.csv"
)

p_spatial_fg <- ggplot(
  spatial_efficiency_grid,
  aes(
    x = x_grid,
    y = y_grid,
    fill = make_rate
  )
) +
  geom_tile(
    width = grid_size_ft,
    height = grid_size_ft
  ) +
  scale_fill_viridis_c(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Observed NBA Make Rate Across the Half-Court",
    subtitle = "2-foot cells with at least 100 development-season attempts",
    x = NULL,
    y = NULL,
    fill = "Make rate",
    caption = "Observed efficiency is descriptive; cell composition differs by shooter, shot type, and context."
  ) +
  theme_nba_explorer() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p_spatial_fg <- add_half_court(p_spatial_fg)

ggsave(
  "docs/eda/spatial_make_rate_half_court.png",
  p_spatial_fg,
  width = 8,
  height = 8,
  dpi = 200
)

# -----------------------------------------------------------------------------
# Predictor-distribution shift: 2024-25 vs 2025-26
# -----------------------------------------------------------------------------

dev_structural <- development_features %>%
  filter(hoopr_season == 2025L) %>%
  summarise(
    dataset = "2024-25 development",
    shots = n(),
    mean_distance_ft = mean(distance_to_rim_check_ft),
    median_distance_ft = median(distance_to_rim_check_ft),
    three_attempt_rate = mean(is_three_geometry),
    corner_three_rate = mean(is_corner_three_geometry),
    restricted_area_rate = mean(
      shot_zone_geometry == "Restricted Area"
    ),
    midrange_rate = mean(
      shot_zone_geometry == "Mid-Range"
    ),
    last_5_seconds_rate = mean(
      is_last_5_seconds_period
    ),
    home_shot_rate = mean(
      is_home_shooter
    )
  )

test_structural <- final_test_features %>%
  summarise(
    dataset = "2025-26 final test",
    shots = n(),
    mean_distance_ft = mean(distance_to_rim_check_ft),
    median_distance_ft = median(distance_to_rim_check_ft),
    three_attempt_rate = mean(is_three_geometry),
    corner_three_rate = mean(is_corner_three_geometry),
    restricted_area_rate = mean(
      shot_zone_geometry == "Restricted Area"
    ),
    midrange_rate = mean(
      shot_zone_geometry == "Mid-Range"
    ),
    last_5_seconds_rate = mean(
      is_last_5_seconds_period
    ),
    home_shot_rate = mean(
      is_home_shooter
    )
  )

structural_shift_summary <- bind_rows(
  dev_structural,
  test_structural
)

write_csv(
  structural_shift_summary,
  "docs/eda/development_vs_final_test_structural_summary.csv"
)

cat("\n--- DEVELOPMENT VS FINAL-TEST STRUCTURAL SUMMARY ---\n")
print(structural_shift_summary, n = Inf)

shot_group_shift <- bind_rows(
  development_features %>%
    filter(hoopr_season == 2025L) %>%
    count(
      shot_type_group,
      name = "shots"
    ) %>%
    mutate(dataset = "2024-25 development"),

  final_test_features %>%
    count(
      shot_type_group,
      name = "shots"
    ) %>%
    mutate(dataset = "2025-26 final test")
) %>%
  group_by(dataset) %>%
  mutate(pct = shots / sum(shots)) %>%
  ungroup()

write_csv(
  shot_group_shift,
  "docs/eda/shot_type_group_structural_shift.csv"
)

zone_shift <- bind_rows(
  development_features %>%
    filter(hoopr_season == 2025L) %>%
    count(
      shot_zone_geometry,
      name = "shots"
    ) %>%
    mutate(dataset = "2024-25 development"),

  final_test_features %>%
    count(
      shot_zone_geometry,
      name = "shots"
    ) %>%
    mutate(dataset = "2025-26 final test")
) %>%
  group_by(dataset) %>%
  mutate(pct = shots / sum(shots)) %>%
  ungroup()

write_csv(
  zone_shift,
  "docs/eda/shot_zone_structural_shift.csv"
)

# -----------------------------------------------------------------------------
# Player/team coverage for future Shiny filters
# -----------------------------------------------------------------------------

filter_coverage <- tibble(
  metric = c(
    "Development unique shooters",
    "Final-test unique shooters",
    "Final-test shooters seen in development",
    "Final-test shooters unseen in development",
    "Development unique teams",
    "Final-test unique teams"
  ),
  value = c(
    n_distinct(
      development_features$shooter_id,
      na.rm = TRUE
    ),
    n_distinct(
      final_test_features$shooter_id,
      na.rm = TRUE
    ),
    n_distinct(
      final_test_features$shooter_id[
        final_test_features$shooter_id %in%
          development_features$shooter_id
      ],
      na.rm = TRUE
    ),
    n_distinct(
      final_test_features$shooter_id[
        !(
          final_test_features$shooter_id %in%
            development_features$shooter_id
        )
      ],
      na.rm = TRUE
    ),
    n_distinct(
      development_features$team_abbrev_canonical,
      na.rm = TRUE
    ),
    n_distinct(
      final_test_features$team_abbrev_canonical,
      na.rm = TRUE
    )
  )
)

write_csv(
  filter_coverage,
  "docs/eda/player_team_filter_coverage.csv"
)

cat("\n--- PLAYER / TEAM FILTER COVERAGE ---\n")
print(filter_coverage, n = Inf)

# -----------------------------------------------------------------------------
# Output inventory + verification
# -----------------------------------------------------------------------------

eda_plot_files <- c(
  "docs/eda/make_rate_by_distance.png",
  "docs/eda/make_rate_by_zone.png",
  "docs/eda/make_rate_by_shot_type_group.png",
  "docs/eda/zone_efficiency_by_season.png",
  "docs/eda/make_rate_by_period_clock_context.png",
  "docs/eda/shot_density_half_court.png",
  "docs/eda/spatial_make_rate_half_court.png"
)

eda_csv_files <- c(
  "docs/eda/default_shot_type_audit.csv",
  "docs/eda/final_test_heave_jump_shot_audit.csv",
  "docs/eda/season_summary.csv",
  "docs/eda/make_rate_by_distance.csv",
  "docs/eda/zone_efficiency.csv",
  "docs/eda/shot_type_group_efficiency.csv",
  "docs/eda/zone_by_season.csv",
  "docs/eda/end_period_context_summary.csv",
  "docs/eda/home_away_summary.csv",
  "docs/eda/spatial_shot_density_grid.csv",
  "docs/eda/spatial_make_rate_grid.csv",
  "docs/eda/development_vs_final_test_structural_summary.csv",
  "docs/eda/shot_type_group_structural_shift.csv",
  "docs/eda/shot_zone_structural_shift.csv",
  "docs/eda/player_team_filter_coverage.csv"
)

default_rows_expected <- nrow(default_shot_audit) == 14L

heave_taxonomy_detected <-
  nrow(heave_type_audit) == 1L &&
  heave_type_audit$shots == 1099L

all_plots_exist <- all(file.exists(eda_plot_files))
all_csvs_exist <- all(file.exists(eda_csv_files))

stage3b_checks <- tibble(
  check = c(
    "Development dataset contains outcome",
    "Final-test dataset still contains no outcome",
    "The 14 development default/no-shot labels are explicitly audited",
    "The 2025-26 Heave Jump Shot taxonomy shift is explicitly audited",
    "Development season summary was created",
    "Distance-efficiency summary was created",
    "Zone-efficiency summary was created",
    "Shot-type-group efficiency summary was created",
    "Structural development/test comparison was created without test outcome",
    "All expected EDA PNG files were written",
    "All expected EDA CSV files were written"
  ),
  passed = c(
    "outcome" %in% names(development_features),
    !("outcome" %in% names(final_test_features)),
    default_rows_expected,
    heave_taxonomy_detected,
    nrow(season_summary) == 3L,
    nrow(distance_efficiency) > 0,
    nrow(zone_efficiency) > 0,
    nrow(shot_group_efficiency) > 0,
    nrow(structural_shift_summary) == 2L,
    all_plots_exist,
    all_csvs_exist
  )
)

cat("\n--- STAGE 3B EDA VERIFICATION ---\n")
print(stage3b_checks, n = Inf)

if (!all(stage3b_checks$passed)) {
  stop(
    "One or more Stage 3B EDA checks failed. Review the audit outputs before continuing."
  )
}

if (requireNamespace("renv", quietly = TRUE)) {
  renv::snapshot(prompt = FALSE)
  cat("\nUpdated renv.lock after Stage 3B EDA.\n")
}

cat(
  "\nSTAGE 3B COMPLETE.\n",
  "Next: review the EDA tables and PNGs, then build 03c_validation_freeze.R ",
  "to finalize the modeling population and approved feature set before Stage 4.\n",
  sep = ""
)
