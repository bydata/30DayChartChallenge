# devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "29")

Sys.getenv("VROOM_CONNECTION_SIZE")
Sys.setenv("VROOM_CONNECTION_SIZE" = 4 * 131072)
player_stats_current <- bref_players_stats(seasons = 2024, tables = c("advanced", "per_game"))
player_stats_early <- bref_players_stats(seasons = 2004, tables = c("advanced", "per_game"))
colnames(player_stats_current)

# rosters <- teams_rosters(seasons = c(2004, 2024))

games_played_min_threshold <- 20
fg3aPerGame_min_threshold <- 0.25

player_stats_current |> 
  select(namePlayer, groupPosition, slugPosition, slugSeason, yearSeason, pctFG3, countGames) |> 
  filter(countGames >= games_played_min_threshold) |> 
  mutate(slugPosition_first = str_extract(slugPosition, "([A-Z]{1,2})-?", group = 1)) |> 
  group_by(slugSeason, yearSeason, slugPosition_first) |> 
  summarize(
    mean_pctFG3 = mean(pctFG3),
    n_players = n(),
    sd_pctFG3 = sd(pctFG3),
    .groups = "drop"
  ) |> 
  mutate(
    pctFG3_95_lower = mean_pctFG3 - 1.96 * sd_pctFG3,
    pctFG3_95_upper = mean_pctFG3 + 1.96 * sd_pctFG3
  )



player_stats_current |> 
  filter(groupPosition == "C") |> 
  filter(countGames >= games_played_min_threshold) |> 
  select(namePlayer, pctFG3) |> 
  arrange(-pctFG3)

player_stats_early |> 
  filter(slugPosition == "C") |> 
  filter(countGames >= games_played_min_threshold) |> View
  select(namePlayer, pctFG3) |> 
  arrange(-pctFG3)


player_stats_early |> 
  mutate(slugPosition = str_extract(slugPosition, "([A-Z]{1,2})-?", group = 1)) |> 
  filter(slugPosition == "SG") |> View()
  filter(countGames >= games_played_min_threshold) |> 
  select(namePlayer, pctFG3, fg3aPerGame) |> 
  arrange(-pctFG3)


player_stats_current |> 
  mutate(slugPosition = str_extract(slugPosition, "([A-Z]{1,2})-?", group = 1)) |> 
  filter(slugPosition == "PG") |> 
  select(namePlayer, fg3aPerGame)  |> 
  arrange(- fg3aPerGame)
  
  

# Combine current and previous 3 pt FG %
player_stats_prep <- player_stats_current |> 
  bind_rows(player_stats_early) |> 
  mutate(
    slugSeason = fct_reorder(slugSeason, desc(slugSeason)),
    slugPosition = str_extract(slugPosition, "([A-Z]{1,2})-?", group = 1),
    slugPosition = factor(slugPosition, levels = c("PG", "SG", "SF", "PF", "C"))
  ) |> 
  filter(countGames >= games_played_min_threshold, fg3aPerGame >= fg3aPerGame_min_threshold) |> 
  select(namePlayer, groupPosition, slugPosition, slugSeason, yearSeason, 
         fg3aPerGame, pctFG3, countGames) 


player_stats_prep |> 
  group_by(slugSeason, slugPosition) |> 
  group_split() |> 
  map(function(x) sample(x$pctFG3, size = 1000, replace = TRUE)) |> 
  map(function(x) list("mean" = mean(x), "sd" = sd(x)))



player_stats_current |> 
  bind_rows(player_stats_early) |> 
  mutate(
    slugSeason = fct_reorder(slugSeason, desc(slugSeason)),
    slugPosition = str_extract(slugPosition, "([A-Z]{1,2})-?", group = 1),
    slugPosition = factor(slugPosition, levels = c("PG", "SG", "SF", "PF", "C"))
  ) |> 
  filter(countGames >= games_played_min_threshold) |> 
  select(namePlayer, groupPosition, slugPosition, slugSeason, yearSeason, 
         fg3aPerGame, pctFG3, countGames) |> 
  ggplot(aes(slugSeason, fg3aPerGame)) +
  # stat_summary() +
  ggdist::stat_histinterval(
    breaks = 15, point_interval = "mean_qi", outline_bars = TRUE) +
  geom_label(
    aes(y = 0.005, label = slugSeason),
    stat = "unique", family = "Roboto Condensed", fontface = "bold", size = 2.5, 
    hjust = 0
  ) +
  # scale_x_discrete(expand = c(0, 1)) +
  scale_y_continuous(position = "right") +
  # coord_flip(ylim = c(0.15, 0.4), clip = "off") +
  coord_flip(clip = "off") +
  facet_grid(rows = vars(slugPosition), switch = "y") +
  labs(
    title = "3pt Shooting 2004 vs. 2024",
    subtitle = "3pt shooting percentage in the NBA has increased for Centers
    (**C**) especially, 
    Power Forwards (**PF**), Shooting Guards (**SG**), and Point Guards (**PG**).",
    caption = sprintf("*Note: Players with at least %d games played and %0.2f
    3pt attempts per game*<br><br>
      Data: Basketball-Reference.com, via {nbaStatR}. Visualization: Ansgar Wolsing",
    games_played_min_threshold, fg3aPerGame_min_threshold),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2, color = "grey80"),
    panel.grid.minor.x = element_line(linewidth = 0.1, color = "grey80"),
    panel.spacing.y = unit(0, "mm"),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "Bangers", hjust = 0.5, size = 18),
    plot.subtitle = element_textbox(
      width = 1, hjust = 0.5, halign = 0, margin = margin(b = 12)),
    plot.caption = element_textbox(
      width = 1, hjust = 0.5, halign = 0)
  )
ggsave(here(base_path, "29-black-n-white.png"), width = 3, height = 4, scale = 1.5)



player_stats_prep |> 
  ggplot(aes(slugSeason, fg3aPerGame)) +
  # stat_summary() +
  ggdist::stat_histinterval(
    breaks = 15, point_interval = "mean_qi", outline_bars = TRUE) +
  geom_label(
    aes(y = 0.005, label = slugSeason),
    stat = "unique", family = "Roboto Condensed", fontface = "bold", size = 2.5, 
    hjust = 0
  ) +
  # scale_x_discrete(expand = c(0, 1)) +
  scale_y_continuous(
    labels = scales::percent_format(), position = "right") +
  # coord_flip(ylim = c(0.15, 0.4), clip = "off") +
  coord_flip(ylim = c(0, NA), clip = "off") +
  facet_grid(rows = vars(slugPosition), switch = "y") +
  labs(
    title = "3pt Shooting 2004 vs. 2024",
    subtitle = "3pt shooting percentage in the NBA has increased for Centers
    (**C**) especially, 
    Power Forwards (**PF**), Shooting Guards (**SG**), and Point Guards (**PG**).",
    caption = sprintf("*Note: Players with at least %d games played and %0.2f
    3pt attempts per game*<br><br>
      Data: Basketball-Reference.com, via {nbaStatR}. Visualization: Ansgar Wolsing",
                      games_played_min_threshold, fg3aPerGame_min_threshold),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2, color = "grey80"),
    panel.grid.minor.x = element_line(linewidth = 0.1, color = "grey80"),
    panel.spacing.y = unit(0, "mm"),
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "Bangers", hjust = 0.5, size = 18),
    plot.subtitle = element_textbox(
      width = 1, hjust = 0.5, halign = 0, margin = margin(b = 12)),
    plot.caption = element_textbox(
      width = 1, hjust = 0.5, halign = 0)
  )
ggsave(here(base_path, "29-black-n-white.png"), width = 3, height = 4, scale = 1.5)



player_stats_contour_prep <- player_stats_current |> 
  bind_rows(player_stats_early) |> 
  mutate(
    slugPosition = str_extract(slugPosition, "([A-Z]{1,2})-?", group = 1),
    slugPosition = factor(slugPosition, levels = c("PG", "SG", "SF", "PF", "C"))
  ) |> 
  filter(countGames >= games_played_min_threshold) |> 
  select(namePlayer, groupPosition, slugPosition, slugSeason, yearSeason, 
         fg3aPerGame, pctFG3, countGames) 


player_stats_contour_prep |> 
  ggplot(aes(fg3aPerGame, pctFG3)) +
  geom_density_2d(
    linewidth = 0.2, color = "black", contour_var = "ndensity",
    h = c(MASS::bandwidth.nrd(player_stats_contour_prep$fg3aPerGame),
          MASS::bandwidth.nrd(player_stats_contour_prep$pctFG3)),
    n = 100, bins = 8) +
  geom_point(size = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(ylim = c(0, 0.6)) +
  facet_grid(cols = vars(slugSeason), rows = vars(slugPosition))  +
  labs(
    title = "3pt Shooting 2004 vs. 2024",
    subtitle = "3pt attempts have increased for all positions in the NBA, 
    and shooting percentage has improved especially for Power Forwards and Centers",
    caption = sprintf("(**PG**) Point Guards, (**SG**) Shooting Guards, 
    (**SF**) Small Forwards, (**PF**) Power Forwards, (**C**) Centers.
    <br><br>*Note: Players with at least %d games played.*<br><br>
      Data: Basketball-Reference.com, via {nbaStatR}. Visualization: Ansgar Wolsing",
                      games_played_min_threshold, fg3aPerGame_min_threshold),
    x = "3pt attempts per game", y = "3pt percentage"
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 11) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_line(linewidth = 0.15, color = "grey70"),
    panel.grid.major.y = element_line(linewidth = 0.15, color = "grey70"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 6.5),
    axis.text = element_text(size = 6.5),
    strip.text = element_text(angle = 0, face = "bold"),
    strip.text.y.right = element_text(angle = 0, face = "bold"),
    plot.title = element_text(family = "Bangers", hjust = 0.5, size = 18),
    plot.subtitle = element_textbox(
      width = 1.2, hjust = 0.5, halign = 0.5, margin = margin(b = 12)),
    plot.caption = element_textbox(size = 7, width = 1, hjust = 0.5, halign = 0)
  )
ggsave(here(base_path, "29-black-n-white.png"), width = 3, height = 4, scale = 1.5)
