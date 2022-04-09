library(tidyverse)
library(here)
library(glue)
library(lubridate)
# devtools::install_github("charlie86/spotifyr")
library(spotifyr)
library(furrr) 
library(tictoc)
library(ggtext)


base_path <- here("2022", "09")

#' Download data from Kaggle:
#' https://www.kaggle.com/dhruvildave/billboard-the-hot-100-songs

charts <- read_csv(here(base_path, "data", "charts.csv"))
glimpse(charts)

no1 <- charts %>% 
  filter(rank == 1) %>% 
  select(date, song, artist)

max(no1$date)

# Latest date in the Kaggle dataset is 2021-11-06
# "All Too Well" made no. 1 on 2021-11-27
# Adele - Easy On Me was no. 1 on 2021-11-13 and 2021-11-20
# Source: https://www.billboard.com/charts/hot-100/2021-11-27/
# https://www.billboard.com/music/chart-beat/taylor-swift-all-too-well-hot-100-debut-1235001340/

no1_ext <- no1 %>% 
  add_row(date = as_date("2021-11-13"), song = "Easy On Me", artist = "Adele") %>% 
  add_row(date = as_date("2021-11-20"), song = "Easy On Me", artist = "Adele") %>% 
  add_row(date = as_date("2021-11-27"), song = "All Too Well (10 Minute Version) (Taylor's Version)", artist = "Taylor Swift") %>% 
  arrange(desc(date))


discard_featuring_artist_name <- function(x) {
  regex <- "(?i)\\s(Feat(\\.|uring)?|x|&|With\\s|And The|And His|Starring|\\(.+).+"
  str_remove_all(x, regex)
}

cleanup_song_name <- function(x) {
  str_remove(x, "(\\s\\(|/).+")
}


no1_uniq <- no1_ext %>% 
  # distinct(song, artist) %>% 
  group_by(song, artist) %>% 
  summarize(first_no1_date = min(date), .groups = "drop") %>% 
  mutate(artist2 = discard_featuring_artist_name(artist),
         song2 = cleanup_song_name(song),
         artist_song = glue("{artist2} - {song2}")) %>% 
  # manually clean up some song names
  mutate(
    artist_song = case_when(
      artist == "Barbra Streisand/Donna Summer" &
       song == "No More Tears (Enough Is Enough)" ~ "Barbra Streisand - No More Tears (Enough Is Enough)",
      artist == "Joey Dee & the Starliters" & 
        song == "Peppermint Twist - Part I" ~ 
        "Joey Dee & the Starliters - Peppermint Twist",
      TRUE ~ as.character(artist_song)  # glue returns an object of class glue which doesn't work with case_when
    )
  )


## Spotify API

## Authenticate -----
source(here(base_path, "spotify_credentials.R"))
Sys.setenv(SPOTIFY_CLIENT_ID = cred$client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = cred$client_secret)

access_token <- get_spotify_access_token()

song1 <- search_spotify("Barbra Streisand - No More Tears (Enough Is Enough)", 
                        type = "track", limit = 1)

# initialize parallel computing with purrr
#' Takes some time. Only run if needed
if (FALSE) {
  # http://zevross.com/blog/2019/02/12/dramatically-speed-up-your-r-purrr-functions-with-the-furrr-package/
  future::plan(multisession)
  
  tic("Get tracks")
  tracks <- map(no1_uniq$artist_song, search_spotify, type = "track", limit = 1)
  tracks <- set_names(tracks, glue("{no1_uniq$artist} - {no1_uniq$song}"))
  # count the missing songs
  tracks %>% keep(is_empty) %>% names()
  toc()
  write_rds(tracks, here(base_path, "data", "tracks.rds"))
} else {
  tracks <- read_rds(here(base_path, "data", "tracks.rds"))
}



empty_tracks <- tracks %>% 
  keep(is_empty) %>% 
  names()

tracks_df <- bind_rows(tracks, .id = "artist_song") %>% 
  inner_join(mutate(no1_uniq, 
                    artist_song = glue("{no1_uniq$artist} - {no1_uniq$song}"), 
                    by = "artist_song")) %>% 
  mutate(duration_s = duration_ms / 1000,
         decade = str_sub(first_no1_date, 1, 4) %>% 
           as.numeric() %/% 10 * 10)


tracks_df %>% 
  ggplot(aes(duration_s)) +
  geom_histogram(fill = "deeppink", alpha = 0.6) +
  theme_light()

tracks_df %>% 
  ggplot(aes(x = 1, y = duration_s)) +
  ggbeeswarm::geom_beeswarm(col = "grey30", 
                            alpha = 0.6, size = 2) +
  coord_flip() +
  theme_light()

# identify songs which are the longest in their decade
longest_no1_per_decade <- tracks_df %>% 
  filter(duration_ms < 615000) %>% 
  group_by(decade) %>% 
  slice_max(order_by = duration_s, n = 1, with_ties = TRUE) %>% 
  ungroup() %>% 
  mutate(
    label = str_replace(artist_song, " - ", "\n"),
    label = ifelse(str_detect(label, "Taylor Swift"), 
                   "Taylor Swift\nAll Too Well (Taylor's Version)", 
                   label))


## Custom ggplot2 theme
theme_set(theme_minimal(base_family = "Raleway", base_size = 9))
theme_update(
  plot.background = element_rect(color = NA, fill = "white"),
  plot.margin = margin(t = 10, b = 10, l = 6, r = 6),
  panel.grid = element_blank(),
  axis.title.x = element_text(size = 7),
  axis.title.y = element_blank(),
  axis.text.x = element_text(size = 6),
  axis.text.y = element_blank(),
  axis.ticks.x = element_line(color = "grey60", size = 0.3),
  text = element_text(color = "grey35"),
  plot.title = element_markdown(
    color = "black", family = "Raleway SemiBold", size = 16),
  plot.subtitle = element_textbox_simple(
    margin = margin(t = 6, b = 6), width = unit(0.9, "npc"), hjust = 0
  ),
  plot.caption = element_markdown(size = 6)
)


# Greyscale palette function
greyscale_pal <- function(n, lower = 30, upper = 80, reverse = FALSE) {
  stopifnot("n must not exceed the difference of `upper` and `lower` + 1." = 
              n <= abs(upper - lower) + 1)
  stepsize <- ceiling(abs(upper - lower) / n)
  colors <- paste0("grey", seq(lower, upper, stepsize))
  if (reverse) {
    rev(colors)
  }
  colors
}

highlight_color <- "#B14625"
color_palette <- c(rev(greyscale_pal(7, 40, 70)), highlight_color)

plot_titles <- list(
  title = glue("<b style='color:{highlight_color}'>\"All Too Well\"</b> 
  is the Longest Song in U.S. Chart History"),
  subtitle = "**Taylor Swift's** new version of
  \"All Too Well\" is the longest track to ever top the Billboard Hot 100 with a duration of 
  10 minutes and 13 seconds. The song replaces **Don McLean's song \"American Pie\"** 
  (8 minutes and 36 seconds), which became no. 1 on 15 January 1972, after nearly 50 years.
  <br>
  The longest no. 1 song in each decade is highlighted.",
  caption = "Source: **Billboard Hot 100 (Kaggle), Spotify API** |
    Visualization: **Ansgar Wolsing**")

tracks_df %>% 
  ggplot(aes(x = factor(decade), y = duration_s)) +
  ggbeeswarm::geom_beeswarm(
    aes(fill = factor(decade),
        size = ifelse(str_detect(artist_song, "Taylor Swift - All Too Well"), 3.5, 2)),
    cex = 0.75, shape = 21, col = "white", alpha = 0.95, show.legend = FALSE) +
  # Annotations for songs
  ggrepel::geom_text_repel(
    data = filter(longest_no1_per_decade, !str_detect(label, "Taylor Swift")),
    aes(label = label),
    size = 2.5, family = "Raleway", color = "grey30", lineheight = 0.8,
    min.segment.length = unit(0.1, "mm"), segment.size = 0.2, segment.color = "grey50",
    direction = "y", hjust = 0, nudge_y = 40
  ) +
  ggrepel::geom_text_repel(
    data = filter(longest_no1_per_decade, str_detect(label, "Taylor Swift")),
    aes(label = label),
    size = 3, family = "Raleway", color = "grey30", lineheight = 0.8,
    min.segment.length = unit(0.1, "mm"), segment.size = 0.2, segment.color = "grey50",
    direction = "y", hjust = 0, nudge_y = -180,
    fontface = "bold"
  ) +
  # Labels for decades
  annotate("text",
           x = seq_along(seq(1950, 2020, 10)) + 0.3,
           y = 100, label = paste0(seq(1950, 2020, 10), "s"),
           family = "Raleway SemiBold", color = "grey50", size = 4
           ) +
  scale_y_continuous(position = "right",
                     breaks = seq(120, 600, 120),
                     labels = paste(seq(120, 600, 120) / 60, "00 min", sep = ":")
  ) +
  scale_fill_manual(values = color_palette) +
  scale_size_identity() +
  coord_flip() +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
    y = "Track duration"
  )
ggsave(here(base_path, "09-all-too-well.png"),
       dpi = 300, width = 6.5, height = 7)


## With distribution strips instead of individual points -------------------

library(ggdist)

tracks_df %>% 
  ggplot(aes(x = factor(decade), y = duration_s)) +
  geom_point(data = longest_no1_per_decade,
             aes(fill = factor(decade), 
                 size = ifelse(str_detect(artist_song, "Taylor Swift - All Too Well"), 3.5, 2)),
             shape = 21, stroke = 1, color = "grey80", show.legend = FALSE) +
  # stat_interval() +
  stat_halfeye(aes(fill = factor(decade)), 
               show.legend = FALSE) +
  # Annotations for songs
  ggrepel::geom_text_repel(
    data = filter(longest_no1_per_decade, !str_detect(label, "Taylor Swift")),
    aes(label = label),
    size = 2.5, family = "Raleway", color = "grey30", lineheight = 0.8,
    min.segment.length = unit(0.1, "mm"), segment.size = 0.2, segment.color = "grey50",
    direction = "y", hjust = 0, nudge_y = 40
  ) +
  ggrepel::geom_text_repel(
    data = filter(longest_no1_per_decade, str_detect(label, "Taylor Swift")),
    aes(label = label),
    size = 3, family = "Raleway", color = "grey30", lineheight = 0.8,
    min.segment.length = unit(0.1, "mm"), segment.size = 0.2, segment.color = "grey50",
    direction = "y", hjust = 0, nudge_y = -180,
    fontface = "bold"
  ) +
  # Labels for decades
  annotate("text",
           x = seq_along(seq(1950, 2020, 10)) + 0.3,
           y = 100, label = paste0(seq(1950, 2020, 10), "s"),
           family = "Raleway SemiBold", color = "grey50", size = 4
  ) +
  
  scale_y_continuous(position = "right",
                     breaks = seq(120, 600, 120),
                     labels = paste(seq(120, 600, 120) / 60, "00 min", sep = ":")
  ) +
  scale_fill_manual(values = color_palette, aesthetics = c("fill", "color")) +
  scale_size_identity() +
  coord_flip() +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
    y = "Track duration"
  )
ggsave(here(base_path, "09-all-too-well-dist.png"),
       dpi = 300, width = 6.5, height = 7)


library(magick)
img1 <- image_read(here(base_path, "09-all-too-well-dist.png"))
img2 <- image_read(here(base_path, "09-all-too-well.png"))
image_morph(c(img1, img2))
