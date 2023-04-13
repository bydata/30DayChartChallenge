library(tidyverse)
library(ggtext)
library(spotifyr)
library(here)

source("credentials.R") # set credentials here
Sys.setenv(SPOTIFY_CLIENT_ID = credentials$client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = credentials$secret)
access_token <- get_spotify_access_token()

artist <- "Bloc Party"
audio_features <- get_artist_audio_features(artist = artist)

# Albums
unique(audio_features$album_name)
# Songs
unique(audio_features$track_name)
# Keys
table(audio_features$key_name)
# Mode
table(audio_features$mode_name)
# Key Mode
table(paste(audio_features$key_name, audio_features$mode_name))


studio_albums <- c("Silent Alarm", "A Weekend in the City", "Intimacy", "Four",
                   "Hymns", "Alpha Games")

tracks <- audio_features %>%
  tibble() %>%
  filter(album_name %in% studio_albums) %>%
  # there are multiple versions of the albums, select the UK version
  unnest(cols = available_markets) %>%
  filter(available_markets == "GB" |
           (available_markets == "US" & album_name == "A Weekend in the City")) %>%
  mutate(album_name = factor(album_name, levels = studio_albums)) %>%
  group_by(album_name, track_name) %>%
  summarize(
    across(c(danceability, liveness, energy, loudness, speechiness,
         valence, tempo, duration_ms), mean),
    across(c(key_name, mode_name), first),
  .groups = "drop") %>%
  mutate(mode_name = ifelse(mode_name == "minor", "Minor key", "Major key"))


count(tracks, album_name)

# Colors from the album covers
album_colors <- c(
  "#6D615F",
  "#E1E8D3",
  "#82485C",
  "#38468D",
  "#BAE77F",
  "#DD6266"
)
album_bgcolors <- c(
  "white",
  "#51432C",
  "#C4B8BA",
  "black",
  "#0F0706",
  "#202020"
)

tracks %>%
  ggplot(aes(valence, energy)) +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = album_name)
  ) +
  geom_hline(aes(yintercept = 0.5), linewidth = 0.25, color = "grey60", lty = "dashed") +
  geom_vline(aes(xintercept = 0.5), linewidth = 0.25, color = "grey60", lty = "dashed") +
  geom_point(
    aes(col = album_name, shape = mode_name),
    size = 1.5) +
  scale_x_continuous(
    limits = c(0, 1), breaks = c(0.25, 0.75),
    labels = c("More negative", "More positive")) +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0.25, 0.75),
    labels = c("Less energy", "More energy")) +
  scale_color_manual(values = album_colors) +
  scale_fill_manual(values = album_bgcolors) +
  scale_shape_manual(values = c(15, 16)) +
  facet_wrap(vars(album_name)) +
  coord_fixed() +
  guides(col = "none", fill = "none",
         shape = guide_legend(override.aes = list(size = 2.5))) +
  labs(
    title = "Bloc Party's Songs' Valence and Energy",
    subtitle = "*Valence* describes the musical positiveness conveyed by a track.
    *Energy* represents a perceptual measure of intensity and activity. Typically,
    energetic tracks feel fast, loud, and noisy.
    Each *studio album* is shown separately.",
    caption = "Source: Spotify API. Visualisation: Ansgar Wolsing",
    x = "Valence",
    y = "Energy",
    shape = NULL
  ) +
  theme_minimal(base_family = "Gill Sans") +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = "grey96", fill = "grey96"),
    panel.background = element_rect(color = NA, fill = NA),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_textbox(width = 0.9, lineheight = 1.2),
    plot.title.position = "plot"
  )
ggsave(here("plots", "13-pop-culture-bloc-party.png"), width = 6, height = 6)
