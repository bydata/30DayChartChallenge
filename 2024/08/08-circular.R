# devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(tidyverse)
library(ggtext)
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(here)

base_path <- here("2024", "08")

wemby <- "Victor Wembanyama"

Sys.getenv("VROOM_CONNECTION_SIZE")
Sys.setenv("VROOM_CONNECTION_SIZE" = 4 * 131072)
player_stats <- bref_players_stats(seasons = 2024, tables = c("advanced", "per_game"))
colnames(player_stats)


# Minimum minutes / games played
games_played_min_treshold <- 10

player_stats_min_games_played <- player_stats |> 
  filter(countGames >= games_played_min_treshold)

# Percentile positions Wemby

# Calculate the percentile rank 
calculate_player_percentile_rank <- function(metric, 
                                             name = wemby, 
                                             data = player_stats_min_games_played,
                                             round = TRUE) {
  value <- data |> 
    filter(namePlayer == name) |> 
    pull({{metric}})
  percentile_position <- sum(data[, metric] <= value) / nrow(data[, metric]) * 100
  if (round) {
    percentile_position <- floor(percentile_position)
  }
  percentile_position
}

metrics <- c("blkPerGame", "ptsPerGame", "astPerGame", "pctUSG", "fgaPerGame",
             "pctFG2", "pctFG3", "pctEFG",
             "stlPerGame", "trbPerGame", "orbPerGame",
             "drbPerGame")
metrics_labels <- 
  c("Blocks", "Points", "Assists", "Usage %", "FG Attempts", "2-pt FG %", 
    "3-pt FG %", "Effective FG %",
    "Steals", "Total Rebounds", "Offensive Rebounds", 
    "Defensive Rebounds")

percentile_ranks <- map_int(metrics, calculate_player_percentile_rank)
names(percentile_ranks) <- metrics
percentile_ranks

df_radar <- data.frame(matrix(percentile_ranks / 100, nrow = 1))
metrics_labels_values <- sprintf("%s\n(%dth)", metrics_labels, percentile_ranks)
# replace "100th" with "leader"
metrics_labels_values <- gsub("100th", "leader", metrics_labels_values)
colnames(df_radar) <- metrics_labels_values
df_radar$group <- wemby
df_radar <- df_radar[, c("group", metrics_labels_values)]


# Custom theme elements
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)

colors_circle <- c("#888888", "#FFFFFF")
gradient_fill_circle <- grid::linearGradient(colors_circle, group = FALSE)

ggradar(df_radar,
        grid.min = 0,
        grid.max = 1,
        gridline.mid.colour = "#666666",
        values.radar = c("0th", "50%", "100%"),
        axis.label.size = 3,
        font.radar = "Libre Franklin",
        gridline.min.linetype = "solid",
        gridline.max.linetype = "solid",
        grid.line.width	= 0.8,
        grid.label.size = 4,
        background.circle.colour = gradient_fill_circle,
        group.colours = "#EE6730",
        group.line.width = 1.2,
        group.point.size = 5,
        fill = TRUE,
        fill.alpha = 0.1
        ) +
  coord_equal(clip = "off") +
  # coord_cartesian(clip = "off") +
  labs(
    title = "Exceptionally Wemby",
    subtitle = "Per game statistics of rookie
    <b style='color:#EE6730'>Victor Wembanyama</b> compared to the other 
    NBA players in the 2023/24 season.
    Wembanyama's percentile rank is shown in brackets.
    The higher the percentile rank, the better the player compares to his peers.",
    caption = sprintf("Note: Minimum %d games played.<br><br>
                      Source: Basketball-Reference (as of April 07, 2024).
                      Visualization: Ansgar Wolsing",
                      games_played_min_treshold)) +
  theme_void(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(color = "#090909"),
    plot.title = element_markdown(
      color = "grey8", lineheight = 1.2,
      # family = "Libre Franklin SemiBold", 
      family = "Bangers",
      hjust = 0.5, size = 20,
      margin = margin(t = 0, b = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      hjust = 0.5, halign = 0, color = "grey20", width = 1, size = 10,
      lineheight = 1.2, margin = margin(t = 6, b = 12)),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 1.1, size = 7),
    plot.margin = margin(t = 4, b = 4, l = 30, r = 30),
    legend.position = "none"
  )
ggsave(here(base_path, "08-circular.png"), width = 5, height = 5, scale = 1.25,
       bg = "white")
