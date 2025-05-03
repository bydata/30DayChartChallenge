library(tidyverse)
library(ggtext)
library(here)
library(hoopR)

base_path <- here("2025", "11")

leaderboards_pergame <- nba_alltimeleadersgrids(
  per_mode = "PerGame",
  season_type = "Regular Season",
  top_x = 10000
)

leaderboards_total <- nba_alltimeleadersgrids(
  per_mode = "Totals",
  season_type = "Regular Season",
  top_x = 10000
)


leaderboards_pergame |> 
  pluck("FG3MLeaders")

segment_y <- 0.5

leaderboards_total_fg3m <- leaderboards_total |> 
  pluck("FG3MLeaders") |> 
  janitor::clean_names() |> 
  mutate(across(c(fg3m, fg3m_rank), as.integer)) 

# How many players made 1000/2000+ 3-pointers?
n_players_made2000 <- sum(leaderboards_total_fg3m$fg3m >= 2000, na.rm = TRUE)
n_players_made1000 <- sum(leaderboards_total_fg3m$fg3m >= 1000, na.rm = TRUE)

# Top 10 - how many are still active?
leaderboards_total_fg3m |> 
  slice_max(order_by = fg3m, n = 10) |> 
  count(is_active_flag)

# Top 20 - how many are still active?
leaderboards_total_fg3m |> 
  slice_max(order_by = fg3m, n = 20) |> 
  count(is_active_flag)


p <- leaderboards_total_fg3m |> 
  mutate(is_active_flag = ifelse(is_active_flag == "Y", "active", "inactive")) |> 
  ggplot(aes(fg3m)) +
  geom_segment(
    aes(xend = fg3m, y = -segment_y, yend = segment_y,
        col = is_active_flag),
    alpha = 0.5, linewidth = 0.5,
    key_glyph = "point"
  ) +
  ggrepel::geom_text_repel(
    data = ~slice_max(., order_by = fg3m, n = 5),
    aes(y = segment_y,
        label = player_name, col = is_active_flag),
    direction = "y", nudge_y = 0.2,
    segment.size = 0.15, min.segment.length = 0,
    family = "Roboto Condensed Medium", size = 2.5,
    seed = 1, show.legend = FALSE
  ) +
  annotate(
    "richtext",
    x = c(1500, 2500),
    y = -segment_y - 0.1,
    label = c(
      sprintf("**%d players** made<br>1000+ 3-pointers", n_players_made1000),
      sprintf("**%d players** made<br>2000+ 3-pointers", n_players_made2000)
    ), 
    family = "Roboto Condensed", size = 3, hjust = 0.5, lineheight = 0.9,
    fill = "#F8F8F8", label.size = 0
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("inactive" = "grey50", "active" = "#8338ec")) +
  coord_cartesian(ylim = c(-segment_y - 0.2, segment_y + 0.3)) +
  guides(
    color = guide_legend(
      override.aes = list(shape = 15, size = 3))
  ) +
  labs(
    title = "6 of the 10 NBA players with the most career 3-pointers 
    are still <span style='color:#8338ec'>active</span> today",
    subtitle = "Career 3-point field goals made in the NBA regular season",
    caption = "***Note:** The 3-point line was introduced in 1979. Players whose 
    career ended before 1979 are excluded.*<br><br>
    **Source:** NBA. **Visualization:** Ansgar Wolsing",
    col = "Player is"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 8),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.1),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95, 
      lineheight = 1.1),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.9),
    legend.direction = "horizontal",
    legend.key.width = unit(1, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
  )
ggsave(here(base_path, "11-stripes-nba-fg3m.png"), width = 5, height = 5)


bins <- 20
leaderboards_total_fg3m |> 
  mutate(is_active_flag = ifelse(is_active_flag == "Y", "active", "inactive")) |> 
  filter(fg3m > 0 ) |> 
  ggplot(aes(fg3m)) +
  geom_histogram(fill = "grey70", bins = bins) +
  geom_histogram(
    data = ~mutate(., is_active_flag2 = is_active_flag),
    fill = "#8338ec", bins = bins
  ) +
  scale_x_continuous(
    breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000),
    transform = "pseudo_log") +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(vars(paste(is_active_flag2, "NBA players"))) +
  labs(
    title = "Career 3-point field goals made in the NBA regular season",
    caption = "***Note:** Players with at least 1 3-point shot made.*
    **Source:** NBA. **Visualization:** Ansgar Wolsing",
    x = "# of 3-points field goals made (pseudo-log scale)",
    y = NULL,
    subtitle = "Number of players"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "grey40", size = 9),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.1),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95, 
      lineheight = 1.1),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.9),
    legend.direction = "horizontal",
    legend.key.width = unit(1, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
    strip.text = element_text(family = "Roboto Condensed Medium", size = 11)
  )
ggsave(here(base_path, "11-stripes-nba-fg3m-histo.png"), width = 6.5, height = 5)


breaks <- c(0, 1, 10, 20, 50, 100, 200, 1000, 5000)
labels <- paste0("(", head(breaks, -1), ", ", tail(breaks, -1), "]")

leaderboards_total_fg3m |> 
  filter(!is.na(fg3m)) |> 
  mutate(
    is_active_flag = ifelse(is_active_flag == "Y", "active", "inactive"),
    fg3m_grp = cut(fg3m, breaks = breaks, labels = labels,
                   include.lowest = TRUE, right = TRUE)) |> 
  count(is_active_flag, fg3m_grp) |> 
  mutate(share = n / sum(n), .by = is_active_flag) |> 
  ggplot(aes(fg3m_grp, share)) +
  geom_col(
    data = ~mutate(., is_active_flag2 = is_active_flag),
    fill = "#8338ec"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = c(0, 0)) +
  facet_wrap(vars(paste(is_active_flag2, "NBA players")), nrow = 2) +
  labs(
    title = "Career 3-point field goals made in the NBA regular season",
    subtitle = "Share of players (%)",
    caption = "***Note:** The 3-point line was introduced in 1979. Players whose 
    career ended before 1979 are excluded.*<br><br>
    **Source:** NBA. **Visualization:** Ansgar Wolsing",
    x = "Career 3-points field goals made (grouped)",
    y = ""
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "grey40", size = 9),
    axis.title.x = element_text(margin = margin(t = 6)),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.1),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95, 
      lineheight = 1.1),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "inside",
    legend.position.inside = c(0.2, 0.9),
    legend.direction = "horizontal",
    legend.key.width = unit(1, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
    strip.text = element_text(family = "Roboto Condensed Medium", size = 11),
    panel.spacing.y = unit(5, "mm")
  )
ggsave(here(base_path, "11-stripes-nba-fg3m-barchart.png"), width = 6, height = 5)

