library(tidyverse)
library(hoopR)
library(ggtext)
library(here)


# Retrieve NBA leaderboard for the 2025-26 regular season
# Per Game + Totals to filter outliers
data_pergame <- nba_leagueleaders(
  per_mode = "PerGame",
  season = "2025-26",
  season_type = "Regular Season"
)

data_totals <- nba_leagueleaders(
  per_mode = "Totals",
  season = "2025-26",
  season_type = "Regular Season"
)

dfs <- map(
  list(data_pergame, data_totals),
  function(x) {
    x[["LeagueLeaders"]] |> 
      janitor::clean_names() |> 
      mutate(
        gp = as.integer(gp),
        across(min:eff, as.numeric)
      ) |> 
      select(-c(rank, team_id))
  }
)

df_leaders <- dfs[[1]] |> 
  inner_join(dfs[[2]], by = "player_id", suffix = c(".pg", ".tot")) |> 
  rename(gp = gp.pg, player = player.pg, team = team.pg) |> 
  select(-c(gp.tot, player.tot, team.tot))

colnames_sorted <- c(
  colnames(df_leaders)[1:4],
  colnames(df_leaders)[5:ncol(df_leaders)][order(colnames(df_leaders)[5:ncol(df_leaders)])]
)

# Combine totals and per game columns so that we have separate columns for both modes
df_leaders_prep <- df_leaders |> 
  select(all_of(colnames_sorted), -c(ast_tov, stl_tov, pf)) |> 
  pivot_longer(cols = -c(player_id, player, team, gp),
    names_to = "stat") |> 
  separate_wider_delim(stat, delim = ".", names = c("stat", "per_mode")) |> 
  pivot_wider(id_cols = c(player_id, player, team, gp, stat),
    names_from = "per_mode", values_from = "value")


stat_mapping <- c(
  "ast" = "Assists",
  "reb" = "Rebounds",
  "pts" = "Points"
)

selected_player <- "Nikola Jokić"

df_plot <- df_leaders_prep |> 
  filter(stat %in% c("ast", "reb", "pts")) |> 
  mutate(
    stat_label = stat_mapping[stat],
    stat_label = factor(stat_label, levels = c("Rebounds", "Assists", "Points"))) |> 
  arrange(player == selected_player)

df_selected_player_stats <- df_plot |> 
  filter(player == selected_player) |> 
  select(stat, stat_label, pg) |> 
  mutate(stat_label = as.character(stat_label))

facet_labels <- df_selected_player_stats |> 
  mutate(label = paste(
    scales::number(pg, accuracy = 0.1),
    paste0("**", stat_label, "**"), "per game")) |> 
  select(stat_label, label) |> 
  deframe() 

df_label <- data.frame(
  stat_label = "Assists",
  x = df_selected_player_stats$pg[df_selected_player_stats$stat_label == "Assists"],
  xend = df_selected_player_stats$pg[df_selected_player_stats$stat_label == "Assists"],
  y = 1.8,
  yend = 1.1)

p <- df_plot |> 
  ggplot(aes(pg, y = 1)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = player == selected_player),
    shape = 21, col = "grey8", stroke = 0.1,
    size = 3, method = "quasirandom"
  ) +
  geom_richtext(
    data = df_label,
    aes(x, yend, 
      label = sprintf("<img src='%s' width=50>", here("2026", "07", "jokic.png"))),
      nudge_x = -0.5, nudge_y = 1, fill = NA, label.size = 0
  ) +
  geom_curve(
    data = df_label,
    aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.3, linewidth = 0.2, col = "grey50",
    arrow = arrow(angle = 25, length = unit(0.15, "cm"))
  ) +
  scale_fill_manual(values = c("TRUE" = "#1D428A", "FALSE" = "grey86")) +
  facet_wrap(
    vars(stat_label), scales = "free_x", ncol = 1,
    labeller = as_labeller(facet_labels)
  ) +
  coord_cartesian(xlim = c(0, NA), clip = "off") +
  guides(fill = "none") +
  labs(
    title = "<span style='color: #1D428A'>Nikola Jokić</span> not only clinches 
      a triple-double season on average,<br>but also leads two of the three stats categories",
    subtitle = "Each dot represents one NBA player with at least 55 games played in the 
    2025-26 regular season.
    <br><br>
    *Per game stats (as of April 6th, 2026)*<br>",
    caption = "**Source:** NBA.com (via {hoopR} R package.
    **Photo:** All-Pro Reels (CC BY-SA 2.0).
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans") +
  theme(
    plot.title = element_markdown(
      family = "Instrument Sans SemiBold", size = 16, lineheight = 1.25),
    plot.subtitle = element_textbox(
      width = 0.95, lineheight = 1.25),
    plot.caption = element_markdown(hjust = 0),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_markdown(
      color = "#1D428A", size = 14, hjust = 0),
    panel.spacing.y = unit(0.5, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave(here("2026", "07", "07-multiscale.png"), width = 6, height = 6, scale = 1.2)

#' Photo: All-Pro Reels (CC BY-SA 2.0)
#' https://de.wikipedia.org/wiki/Nikola_Joki%C4%87#/media/Datei:Nikola_Jokic_free_throw_(cropped).jpg

