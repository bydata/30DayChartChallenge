library(tidyverse)
library(ggtext)
library(here)
library(worldfootballR)

base_path <- here("2023", "09")

# 42 matchweeks only to season 1994-1995, after that 38 
start_years <- 1992:2022
last_matchdays <- c(rep(42, 3), rep(38, length(start_years) - 3))

final_tables <- map2(start_years, last_matchdays, 
     ~tm_matchday_table(country_name = "England", start_year = .x, matchday = .y))
final_tables <- set_names(final_tables, start_years)


final_tables_df <- final_tables %>% 
  bind_rows(.id = "season") %>% 
  tibble() %>% 
  filter(season != "2022") %>% 
  distinct()

seasons_summary_df <- final_tables_df %>% 
  group_by(squad) %>% 
  summarize(
    n_seasons = n(),
    best_season = season[which.max(pts)],
    worst_season = season[which.min(pts)],
    best_season_pts = pts[which.max(pts)],
    worst_season_pts = pts[which.min(pts)],
    # rank in the season with the most points 
    # (not necessarily the best rank of a club)
    best_season_rk = rk[which.max(pts)],
    worst_season_rk = rk[which.min(pts)]
  ) %>% 
  mutate(pts_diff = best_season_pts - worst_season_pts) 

# Custom annotation function
annotate_richtext <- function(x, y, label, hjust = 0, ...) {
  annotate(
    "richtext",
    x = x, y = y, label = label,
    size = 2.5, family = "Helvetica Neue", hjust = hjust, fill = "grey94", label.size = 0
  )
}

# Premier League colours
pl_colors <- c("#FF0082", "#37003C", "white", "#000000")

min_seasons_threshold <- 4

plot_titles <- list(
  "title" = "Premier League teams' Highs and Lows",
  "subtitle" = sprintf("Best and worst season in terms of points for each Premier 
  League team (at least 4 seasons).<br>
  Seasons highlighted when a team was
  <b style='color:%s'>relegated</b> or <b style='color:%s'>won the title</b>.
  ", pl_colors[2], pl_colors[1]),
  caption = "Data: Transfermarkt.de. Visualisation: Ansgar Wolsing"
)

seasons_summary_df %>% 
  filter(n_seasons >= min_seasons_threshold) %>% 
  mutate(
    # change Hull City to Hull due to small point difference
    # squad = ifelse(squad == "Hull City", "Hull", squad),
    squad = case_match(
      squad,
      "Hull City" ~ "Hull", 
      "Sheff Utd" ~ "Sheffield Utd",
      "Sheff Wed" ~ "Sheffield Wed",
      "Nottm Forest" ~ "Nottingham Forest",
      .default = squad),
    squad = fct_reorder(squad, worst_season_pts),
    ) %>% 
  ggplot(aes(squad)) +
  geom_segment(aes(xend = squad, y = best_season_pts, yend = worst_season_pts)) +
  # highlight title and relegation
  geom_point(
    data = ~subset(., best_season_rk == 1),
    aes(y = best_season_pts),
    size = 4, col = pl_colors[1]
  ) + 
  geom_point(
    data = ~subset(., (worst_season <= "1994" & worst_season_rk >= 20 | 
                         worst_season > "1994" & worst_season_rk >= 18)),
    aes(y = worst_season_pts),
    size = 4, col = pl_colors[2]
  ) + 
  geom_point(aes(y = best_season_pts), size = 2) +
  geom_point(aes(y = worst_season_pts), 
             shape = 21, size = 2, col = "grey40", fill =  "white") +
  # team labels
  geom_label(
    aes(y = (best_season_pts + worst_season_pts) / 2,
        label = toupper(squad)),
    size = 2.2, hjust = 0.5, family = "Helvetica Neue", fontface = "bold", label.size = 0.2
  ) +
  # annotations
  annotate_richtext(x = 39.65, y = 91, label = "**Arsenal's** 'Invicibles'<br>season ('03/'04)") + 
  annotate_richtext(x = 34.3, y = 93, label = "**Man City's** record<br>season ('17/'18)") + 
  annotate_richtext(
    x = 33, y = 16, label = "**Man City** have the highest<br>difference in points between<br>
    their best and worst season") + 
  annotate_richtext(
    x = 23, y = 12, label = "**Hull City** have the smallest <br>difference in points between<br>
    their best and worst season<br>(5 points)") +
  annotate_richtext(
    x = 39.5, y = 20, label = "**Tottenham** have the most points<br>
    to *never* win the title. *(They won<br>2 titles before Premier League, though.)*") +
  annotate_richtext(
    x = 9, y = 82, label = "Well, that was a surprise, **Leicester**!") +
  scale_y_continuous(position = "right") +
  coord_flip(clip = "off") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
    y = "Points"
  ) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = "grey94", fill = "grey94"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey84"),
    panel.grid.minor.x = element_line(color = "grey84"),
    # remove axis labels (club names)
    axis.title.y = element_blank(),
    axis.text.y.left = element_blank(),
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_textbox(width = 1, lineheight = 1.2),
    plot.caption = element_markdown(hjust = 1)
  )
ggsave(here(base_path, "09-high-low-premier-league-points.png"), width = 8, height = 9)


# Which club had the fewest points to ever win the PL?
final_tables_df %>% 
  filter(rk == 1) %>% 
  arrange(pts)

# Which club had the most points to never win the PL?
final_tables_df %>% 
  filter(rk >= 2) %>% 
  select(season, rk, squad, pts) %>% 
  arrange(-pts)

# Which club had the most points to ever be relegated?
final_tables_df %>% 
  filter(rk >= 18) %>% 
  select(season, rk, squad, pts) %>% 
  arrange(-pts)
