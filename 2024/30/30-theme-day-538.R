library(tidyverse)
library(ggtext)
library(worldfootballR)
library(here)

base_path <- here("2024", "30")


#' Inspiration:
# https://projects.fivethirtyeight.com/2020-election-forecast/


# Retrieve final tables data 
filename <- here(base_path, "bundesliga-tables-1996to2023.rds")

if (FALSE) {
  final_tables <- fb_season_team_stats(
    "GER", gender = "M", tier = "1st", 
    season_end_year = 1996:2023, 
    stat_type = "league_table")
  write_rds(final_tables, filename)
} else {
  final_tables <- read_rds(filename)
}


#' Assumptions:
#' GOOD: Points to win the title: at least as many as the 2nd spot, 
#' assuming a better goal difference for the champions
#' BAD: Points that are not enough avoid relegation: the 17th spot - 1 point

final_tables_title_relegation <- final_tables |> 
  tibble() |>
  arrange(Season_End_Year, Rk) |> 
  select(Season_End_Year, Rk, Squad, Pts, GD) |> 
  # Calculate the point and goal difference to the next better team in the table
  mutate(
    pts_to_better_spot = Pts - lag(Pts, 1, default = max(Pts)),
    gd_to_better_spot = GD - lag(GD, 1, default = max(GD)),
    .by = Season_End_Year
  ) |> 
  # Only keep the first 2 places (title race) and place 17 and 18 (relegation battle)
  filter(Rk %in% 1:2 | Rk %in% 15:16) |> 
  #' Title race: if the champion has a better goal difference than the runner-up,
  #' then the same number of points would be suffficient to secure the title
  #' Relegation battle: just assume the points of the 17th
  mutate(
    pts_secure_title = ifelse(
      Rk == 2,
      ifelse(gd_to_better_spot >= 0, Pts + 1, Pts),
      NA_integer_),
    pts_not_relegated = ifelse(
      Rk == 15,
      Pts,
      NA_integer_)
  )

df_plot <- final_tables_title_relegation |> 
  filter(Rk == 2 | Rk == 15) |> 
  group_by(Season_End_Year) |> 
  summarize(
    pts_secure_title = min(pts_secure_title, na.rm = TRUE),
    pts_not_relegated = min(pts_not_relegated, na.rm = TRUE)
  ) |> 
  pivot_longer(cols = c(pts_secure_title, pts_not_relegated),
               names_to = "result", values_to = "points_needed") |> 
  mutate(result = case_match(
    result,
    "pts_secure_title" ~ "Win title",
    "pts_not_relegated" ~ "Avoid relegation"
  ),
  result = factor(result, levels = c("Win title", "Avoid relegation"))
  )


ragg::agg_png(here(base_path, "30-theme-day-538.png"), width = 5, height = 5,
              units = "in", scaling = 1, res = 300)
df_plot |> 
  ggplot(aes(result, points_needed)) +
  # Median of points needed
  stat_summary(
    aes(color = result),
    fun = median, shape = 22, fill = NA, size = 1.25, stroke = 0.5,
    show.legend = FALSE) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = result),
    shape = 21, stroke = 0.2, 
    color = "white",
    width = 0.2,
    size = 3
  ) +
  # Annotations for the results
  annotate(
    "label",
    x = c(1, 2),
    y = c(56, 43), 
    vjust = c(1, 0),
    label = c("Win title", "Avoid relegation /\nrelegation play-off"),
    family = "Roboto", fontface = "bold", color = "#222222", label.size = 0,
    size = 3.5, fill = NA, label.r = unit(0, "mm"), lineheight = 0.9
  ) +
  # Highlight outcomes
  annotate(
    GeomTextBox,
    x = c(1.05, 1.05, 1.3, 1.85),
    y = c(78, 58.5, 68, 35),
    label = c(
      "In 2016, at least **78 points** would have been needed to win the title",
      "In 2003, only **59 points** would have been sufficient",
      "In half of the seasons,<br>**68 points** would have been enough to secure the
      championship",
      "**40 points** have always been enough to avoid relegation
      (or relegation play-offs). In 2009, Borussia M'gladbach only needed
      **31 points** to stay in the league."
    ),
    hjust = c(0, 0, 0, 1),
    family = "Libre Franklin", color = "#222222", 
    size = 2, box.r = unit(0, "mm"), fill = NA,
    box.size = 0, width = 0.25, lineheight = 0.9
  ) +
  annotate(
    GeomCurve,
    x = 1.32, xend = 1.05, y = 68.75, yend = 68,
    color = "white", linewidth = 0.5, 
    arrow = arrow(angle = 20, length = unit(0.1, "cm")),
    curvature = 0.2
  ) +
  annotate(
    GeomCurve,
    x = 1.32, xend = 1.05, y = 68.75, yend = 68,
    color = "#222222", linewidth = 0.3, 
    arrow = arrow(angle = 20, length = unit(0.1, "cm")),
    curvature = 0.2
  ) +
  scale_y_continuous(
    breaks = seq(0, 34 * 3, 10),
    minor_breaks = seq(0, 34 * 3, 5)
  ) +
  scale_fill_manual(
    values = c("#4C589F", "#F64847"),
    aesthetics = c("color", "fill")
  ) +
  guides(fill = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title = "How many points does it take to
    <span style='color:#4C589F'>win the title</span><br>
    and to
    <span style='color:#F64847'>avoid relegation</span> in the Bundesliga?",
    subtitle = "Bundesliga seasons since 1995-96, the season when the three-point rule
    rule was introduced.
    The squares show the median of points needed, i.e. in half of the seasons 
    teams needed more and in the other half they needed less points to 
    win the title / avoid relegation.<br><br>
    <span style='font-family:\"DM Mono Medium\"; color:#999999'>\U2193 Points needed</span>",
    caption = "<i>Note: Win title: If the champion has a better goal difference than the runner-up,
    then the same number of points would be sufficient to secure the title. 
    Avoid relegation: assume the points of the 15th are needed to avoid relegation.
    Since 2008-09, the 16th of the Bundesliga and the 3rd of the Second Bundesliga
    compete in 2 play-off matches for the last Bundesliga spot in the next season.</i>
    <br><br>
    Source: FBRef.com. Visualization: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = "#EFEFEF", fill = "#EFEFEF"),
    text = element_text(color = "#222222"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "DM Mono Medium", color = "#999999"),
    plot.title = element_markdown(
      face = "bold", hjust = 0.5, lineheight = 1),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, size = 8, hjust = 0, margin = margin(t = 4, b = 8),
      lineheight = 1.2),
    plot.caption = element_textbox(
      width = 1, margin = margin(t = 4, b = 6), hjust = 0.5, size = 6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#CECECE", linewidth = 0.15),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(color = "#CECECE", linewidth = 0.05)
  )
dev.off()


final_tables_title_relegation |> 
  filter(Rk == 1) |> 
  count(Pts) |> 
  mutate(n_cumul = cumsum(n))
  
median(df_plot$points_needed[df_plot$result == "Win title"])
median(df_plot$points_needed[df_plot$result == "Avoid relegation"])
