library(tidyverse)
library(ggtext)
library(here)
# devtools::install_github("doehm/cropcircles")
library(cropcircles)
# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)


base_path <- here("2025", "18")


file_path <- here(base_path, "fbref-standard-stats-esp-2025.rds")
if (FALSE) {
  df_standard_stats <- fb_league_stats(
        country = "ESP",
        gender = "M",
        season_end_year = 2025,
        tier = "1st",
        stat_type = "standard",
        team_or_player = "player"
      )
  write_rds(df_standard_stats, file_path)
} else {
  df_standard_stats <- read_rds(file_path)
}

df_standard_stats <- df_standard_stats |> 
  janitor::clean_names()


# Threshold for the matches played to be included
min_matches <- 10
min_minutes <- min_matches * 30


df_standard_stats_prep <- df_standard_stats |> 
  filter(pos == "FW", mp_playing_time >= min_matches, 
         min_playing_time >= min_minutes) |> 
  select(player, squad, npx_g_per_90_minutes, g_minus_pk_per_90_minutes, gls)


# El Pais style elements
color_text_base <- "#181818"
color_text_axis_title <- rgb(116, 116, 116, maxColorValue = 255)
color_points <- "#AE3122"

theme_elpais <- function(...) {
  theme_minimal(base_family = "Roboto", base_size = 10, ...) +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      text = element_text(color = color_text_base),
      axis.title = element_blank(),
      axis.text = element_text(color = color_text_base, size = 7.5),
      legend.position = "right",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6),
      axis.line = element_line(color = color_text_base, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_textbox(
        width = 1, size = 8, lineheight = 1.25,
        margin = margin(t = 2, b = 12)),
      plot.title.position = "plot",
      plot.caption = element_textbox(
        width = 1, height = 0.1, hjust = 0, size = 7, color = color_text_axis_title,
        lineheight = 1.1),
      plot.caption.position = "plot",
      plot.margin = margin(rep(4, 4))
    )
}

# Dimensions of the plot - used to place the axis titles
min_x <- 0
min_y <- 0
max_x <- 1
max_y <- 1

# Players to highlight
highlighted_players <- c(
  "Robert Lewandowski", "Alexander Sørloth", "Kylian Mbappé", "Ayoze Pérez",
  "Vinicius Júnior", "Lamine Yamal", "Umar Sadiq", "Roberto Férnandez",
  "Ante Budimir")

df_standard_stats_prep |> 
  ggplot(aes(npx_g_per_90_minutes, g_minus_pk_per_90_minutes)) +
  geom_abline(slope = 1, intercept = 0, col = color_text_axis_title) +
  geom_point(
    aes(size = gls),
    shape = 21, color = "white", fill = alpha(color_points, 0.7)
  ) +
  ggrepel::geom_label_repel(
    data = ~filter(., player %in% highlighted_players),
    aes(label = str_wrap(player, 6)),
    family = "Roboto Medium", size = 2, hjust = 0.5,
    fill = alpha("white", 0.2), label.size = 0, lineheight = 0.8,
    col = color_text_base, segment.size = 0.2, min.segment.length = 0.5,
    arrow = arrow(angle = 30, length = unit(1.5, "mm"), type = "open"),
    label.padding = unit(0, "mm"), force_pull = 10,
    seed = 123
  ) +
  annotate(
    "label",
    x = 1, y = 0.95,
    label = "Players above the line\noutperform xG",
    family = "Roboto Medium", size = 2.5, hjust = 1,
    fill = "white", label.size = 0, col = color_text_base,
    label.padding = unit(0, "mm")
  ) +
  # Axis titles
  annotate(
    "label",
    x = c(max_x, min_x + 0.01),
    y = c(min_y + 0.025, max_y),
    label = c(
      "Expected non-penalty goals per 90 min",
      "Non-penalty goals per 90 min"
    ),
    hjust = c(1, 0),
    color = color_text_axis_title, family = "Roboto Medium", size = 3,
    fill = "white", label.size = 0
  ) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_y_continuous(
    breaks = seq(0.2, 1, 0.2),
    labels = scales::label_number(accuracy = 0.01)) +
  scale_size_area(max_size = 8) +
  coord_equal(xlim = c(min_x, 1), ylim = c(min_y, 1), expand = FALSE, clip = "off") +
  guides(size = guide_legend(
    override.aes = list(fill = NA, stroke = 0.2, color = color_text_base))) +
  labs(
    title = "Who are the most efficient strikers in LaLiga in 2024-25?",
    subtitle = "Comparing expected non-penalty goals (i.e. predicted based on shot
    quality) with actual non-penalty goals",
    caption = sprintf("Note: Analysis limited to strikers with at least %d matches 
    and %d minutes played in LaLiga in the 2024-25 season (31 matchdays).
    <br><br>
    Chart: Ansgar Wolsing. Source: FBRef.com", min_matches, min_minutes),
    size = "Total goals"
  ) +
  theme_elpais()
ggsave(here(base_path, "18-el-pais.png"), width = 5, height = 5)
