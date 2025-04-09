library(tidyverse)
library(ggtext)
library(patchwork)
library(grid)
library(here)

base_path <- here("2025", "08")

#' Manually download the dataset from 
#' https://www.kaggle.com/datasets/tushar5harma/billboard-hot-100-songs-spotify-data-1946-2022

df <- read_csv(here(base_path, "Billboard_Hot100_Songs_Spotify_1946-2022.csv"),
               name_repair = janitor::make_clean_names)

df <- df |> 
  mutate(song_length_min = song_length_ms / 60 / 1000)

# What's the first year with 100 year top songs?
df |> 
  group_by(hot100_ranking_year) |> 
  summarize(max_rank = max(hot100_rank)) |> 
  filter(max_rank == 100)


df |> 
  select(song, hot100_ranking_year, hot100_rank, song_length_min, artist_names) |> 
  arrange(-song_length_min)

# Custom theme
theme_set(
  theme_minimal(base_family = "Roboto Condensed") +
    theme(
      plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
      strip.text = element_text(family = "Roboto Condensed", face = "bold", size = 10),
      plot.title = element_markdown(
        family = "Roboto Condensed SemiBold", lineheight = 1.05),
      plot.title.position = "plot",
      plot.subtitle = element_markdown(
        margin = margin(t = 12, b = 12)),
      plot.caption = element_markdown(
        hjust = 0, size = 8, margin = margin(t = 12, b = 4)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(hjust = 0)
    )
)

df_plot <- df |> 
  select(song, hot100_ranking_year, hot100_rank, song_length_ms, artist_names) |> 
  mutate(song_length_min = song_length_ms / 60 / 1000) |> 
  filter(hot100_ranking_year >= 1959)

median_song_length <- median(df_plot$song_length_min)


p1 <- df_plot |> 
  ggplot(aes(song_length_min)) +
  geom_histogram(
    fill = alpha("red", 0.3), binwidth = 0.25) +
  geom_vline(
    aes(xintercept = median_song_length),
    linewidth = 0.4
  ) +
  # Rectangle enclosing the 
  annotate(
    "rect",
    xmin = 0, xmax = 8, ymin = 0, ymax = Inf,
    fill = NA, color = "red", linewidth = 0.2
  ) +
  # Labels and arrows
  annotate(
    "richtext", 
    x = c(27, 8.1, 6), y = c(340, 150, 600),
    label = c(
    "<i>The longest song in the<br>**year-end Hot 100**:</i><br><br>
    **Tubular Bells - Pt. I**<br>(Mike Oldfield)<br>26:00 min",
    "<i>The longest **number 1 song**<br>in the **weekly Hot 100**:<br><br>
    **All Too Well (Taylor's Version)**<br>Taylor Swift<br>10:13 min",
    sprintf("The **median song**<br>is %d:%d min long",
            floor(median_song_length), round(median_song_length %% 1 * 60))
    ),
    hjust = c(1, 0, 0),
    family = "Roboto Condensed", size = 3, vjust = 0,
    label.size = 0, fill = "#F8F8F8"
  ) +
  annotate(
    "segment",
    x = c(26, 10.21, 6),
    xend = c(26, 10.21, 4),
    y = c(340, 150, 650),
    yend = c(9, 9, 650),
    linewidth = 0.2,
  ) +
  annotate(
    "point",
    x = c(26, 10.21), 
    y = c(0),
    shape = 21, fill = NA, size = 3, stroke = 0.3
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Length of songs in the Year-End Billboard Hot 100 1959-2023",
    subtitle = "All songs",
    caption = "**Source:** Billboard Hot 100, Spotify (via Kaggle).
    **Visualization:** Ansgar Wolsing",
    x = "Song length (minutes) \U2192", y = "# songs"
  ) 
p1

p2 <- df_plot |> 
  filter(song_length_min <= 8) |> 
  ggplot(aes(song_length_min)) +
  geom_histogram(
    fill = alpha("red", 0.3), binwidth = 0.25) +
  geom_vline(
    aes(xintercept = median_song_length),
    linewidth = 0.4
  ) +
  annotate(
    "richtext", 
    x = c(6), y = c(650),
    label = c(
      "**Median song** length"),
    hjust = c(0),
    family = "Roboto Condensed", size = 3, vjust = 0.5,
    label.size = 0, fill = "#F8F8F8"
  ) +
  annotate(
    "segment",
    x = c(6),
    xend = c(4),
    y = c(650),
    yend = c(650),
    linewidth = 0.2,
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "Only songs with up to 8 min length",
    x = NULL, y = NULL
  ) +
  theme(
    # remove y axis labels
    axis.text.y = element_blank()
  )
p2


ragg::agg_png(here(base_path, "08-histogram.png"), width = 7.5, height = 5,
              units = "in", res = 300)
p1 + p2 +
  plot_layout() &
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8")
  )
grid.lines(x = c(0.67, 0.812), y = c(0.86, 0.86), gp = gpar(col = "red"))
dev.off()
