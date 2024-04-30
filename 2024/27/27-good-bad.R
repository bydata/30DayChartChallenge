library(tidyverse)
library(ggtext)
library(worldfootballR)
library(here)

base_path <- here("2024", "27")

final_tables <- fb_season_team_stats(
  "ENG", gender = "M", tier = "1st", 
  season_end_year = 1993:2023, 
  stat_type = "league_table")
write_rds(final_tables, here(base_path, "premier-league-tables-1992to2023.rds"))

# Only keep the seasons with 20 teams / 38 matches
final_tables |> 
  count(Season_End_Year) # >> starting from season end year 1996

final_tables_38matches <- final_tables |> 
  filter(MP == 38)

#' Assumptions:
#' GOOD: Points to win the title: at least as many as the 2nd spot, 
#' assuming a better goal difference for the champions
#' BAD: Points that are not enough avoid relegation: the 17th spot - 1 point

final_tables_38matches_title_relegation <- final_tables_38matches |> 
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
  filter(Rk %in% 1:2 | Rk %in% 17:18) |> 
  #' Title race: if the champion has a better goal difference than the runner-up,
  #' then the same number of points would be suffficient to secure the title
  #' Relegation battle: just assume the points of the 17th
  mutate(
    pts_secure_title = ifelse(
      Rk == 2,
      ifelse(gd_to_better_spot >= 0, Pts + 1, Pts),
      NA_integer_),
    pts_not_relegated = ifelse(
      Rk == 17,
      Pts,
      NA_integer_)
  )



# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title = element_text(size = 8),
      axis.text = element_markdown(family = "Source Code Pro", size = 7),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 7.5, width = 0.9, lineheight = 1.2,
        margin = margin(b = 8)),
      plot.caption = element_textbox(
        width = 1, hjust = 0, lineheight = 1.1, size = 6.5),
      plot.margin = margin(rep(4, 4)),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.height = unit(6, "mm"),
      legend.key.width = unit(10, "mm"),
      legend.box.spacing = unit(0, "mm"),
      legend.margin = margin(0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.08),
      strip.text = element_markdown(
        family = "Libre Franklin SemiBold", size = 10,
        margin = margin(t = 4, b = 1))
    ) 
)



final_tables_38matches_title_relegation |> 
  filter(Rk == 2 | Rk == 17) |> 
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
  ) |> 
  ggplot(aes(result, points_needed)) +
  # Median of points needed
  stat_summary(
    aes(fill = result),
    fun = median,
    shape = 22, color = "white", size = 1.25) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, color = "grey10", stroke = 0.2, fill = "grey70", width = 0.3, 
    size = 2
  ) +
  # Annotations for the results
  annotate(
    "label",
    x = c(1, 2),
    y = c(57, 50), 
    label = c("\U2191 Win title", "\U2193 Avoid relegation"),
    family = "Libre Franklin SemiBold", color = "grey40", label.size = 0,
    size = 5, fill = "#FFFFFF", label.r = unit(0, "mm")
  ) +
  # Highlight outcomes
  annotate(
    ggrepel::GeomLabelRepel,
    x = c(0.95, 1.6, 1.6),
    y = c(97, 92, 65),
    label = c(
      str_wrap("Liverpool got 97 points in 2019, but needed 1 more to overtake Man City", 24),
      str_wrap("Liverpool in 2022, again 1 point, again Man City won", 24),
      str_wrap("Manchester Utd. won the title in 1997, only 69 points would have
               been enough to secure the title in 1997", 32)
    ),
    family = "Libre Franklin Medium", color = "grey40", label.size = 0,
    size = 2, fill = "#FFFFFF22", label.r = unit(0, "mm"), label.padding = unit(0.2, "mm"),
    direction = "x", lineheight = 0.9, 
    min.segment.length = 0.1, point.padding = unit(1, "cm"),
    hjust = 0, segment.size = 0.2, segment.color = "grey40",
    force_pull = 0.5, 
    seed = 43
  ) +
  scale_y_continuous(
    breaks = seq(0, 38 * 3, 10),
    minor_breaks = seq(0, 38 * 3, 2)
  ) +
  scale_fill_manual(
    values = c("#26C6DA", "#EC407A"),
    aesthetics = c("color", "fill")
  ) +
  guides(fill = "none") +
  coord_cartesian(clip = "off") +
  labs(
    title = "How many points does it take to
    <span style='color:#26C6DA'>win the title</span><br>
    and to
    <span style='color:#EC407A'>avoid relegation</span> in the Premier League?",
    subtitle = "English Premier League seasons since 1995-96, the first season with 20 teams.
    The squares show the median of points needed, i.e. in half of the seasons 
    teams needed more and in the other half they needed less points to 
    win the title / avoid relegation.",
    caption = "<i>Note: Win title: If the champion has a better goal difference than the runner-up,
    then the same number of points would be suffficient to secure the title. 
    Avoid relegation: assume the points of the 17th are needed to avoid relegation.</i>
    <br><br>
    Source: FBRef.com. Visualization: Ansgar Wolsing",
    y = "Points needed"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.subtitle = element_textbox(width = 1, size = 7.5, 
                                    margin = margin(t = 4, b = 12)),
    plot.caption = element_textbox(margin = margin(t = 4, b = 6))
  )
ggsave(here(base_path, "27-good-bad.png"), width = 5, height = 5)
