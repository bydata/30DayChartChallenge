library(tidyverse)
library(ggtext)
library(here)
library(hoopR)

base_path <- here("2025", "14")

leaderboards_pergame <- nba_alltimeleadersgrids(
  per_mode = "PerGame",
  season_type = "Regular Season",
  top_x = 6000
)

leaderboards_pergame_fg3m <- leaderboards_pergame |> 
  pluck("FG3MLeaders") |> 
  janitor::clean_names() |> 
  mutate(across(c(fg3m, fg3m_rank), as.numeric)) 

leaderboards_pergame_fg3a <- leaderboards_pergame |> 
  pluck("FG3ALeaders") |> 
  janitor::clean_names() |> 
  mutate(across(c(fg3a, fg3a_rank), as.numeric)) 

df_leaderboards_combined <- leaderboards_pergame_fg3a |> 
  inner_join(leaderboards_pergame_fg3m, by = "player_id",
             suffix = c("", ".y")) |> 
  select(-is_active_flag.y)

# The Curry family
family <- c("Dell Curry", "Seth Curry", "Stephen Curry")

# Placement of the custom axis titles
min_x <- min(df_leaderboards_combined$fg3a)
max_x <- max(df_leaderboards_combined$fg3a)
min_y <- min(df_leaderboards_combined$fg3m)
max_y <- max(df_leaderboards_combined$fg3m)


p <- df_leaderboards_combined |> 
  ggplot(aes(fg3a, fg3m)) +
  geom_smooth(method = "lm", col = "grey20", linetype = "dashed",
              linewidth = 0.5, se = FALSE) +
  geom_point(
    alpha = 0.2, col = "grey40") +
  geom_point(
    data = ~filter(., player_name %in% family),
    col = "#BD2D87"
  ) +
  ggrepel::geom_label_repel(
    data = ~filter(., player_name %in% family),
    aes(label = player_name),
    col = "#BD2D87", fill = alpha("#F8F8F8", 0.7), label.size = 0,
    family = "Roboto Condensed Medium", seed = 1234
  ) +
  annotate(
    "label",
    x = 6.5,
    y = 2.1,
    label = "Other NBA players",
    fill = "#F8F8F8", label.size = 0, family = "Roboto Condensed",
    size = 2.5, color = "grey40", hjust = 0
  ) +
  # Custom axis titles
  annotate(
    "label",
    x = c(max_x, min_x),
    y = c(min_y, max_y),
    label = c("3-point attempts per game \U2192", "\U2191 3-point made per game"),
    hjust = c(1, 0.1),
    fill = "#F8F8F8", label.size = 0, family = "Roboto Condensed Medium",
    size = 3.5, color = "grey40"
  ) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
  labs(
    title = "3-Pointers - A <span style='color:#BD2D87'>Family</span> Business",
    subtitle = "Dell Curry is the father of Stephen and Seth Curry.
    <br>All three of them are famous for their 3-point shooting skills.",
    caption = "***Note:** Per game career stats.* 
    **Source:** NBA. **Visualization:** Ansgar Wolsing"
  ) + 
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 9),
    panel.grid.major = element_line(
      linetype = "dashed", linewidth = 0.1, color = "grey40"),
    panel.grid.minor = element_blank(),
    plot.title = element_markdown(
      family = "Bangers", size = 18, hjust = 0.5, lineheight = 1.1),
    plot.subtitle = element_markdown(
      margin = margin(t = 4, b = 16), lineheight = 1.2, hjust = 0.5
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
ggsave(here(base_path, "14-kinship.png"), width = 5, height = 5)
