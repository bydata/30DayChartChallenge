library(tidyverse)
library(ggtext)
library(here)
library(spotifyr)
library(waffle)
library(grid)

base_path <- here("2024", "09")

## Spotify API

## Authenticate -----
Sys.setenv(SPOTIFY_CLIENT_ID = cred$client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = cred$client_secret)
access_token <- get_spotify_access_token()

# Get audio features
beatles <- get_artist_audio_features("The Beatles")

# Select relevant album names and order by release date
beatles_album_names <- beatles |> 
  count(album_name, album_release_date) |> 
  filter(str_detect(album_name, "\\(Remastered\\)$") &
           !str_detect(album_name, "Naked \\(Remastered\\)$") & 
           album_name != "Live At The BBC (Remastered)") |> 
  # order albums by release date
  mutate(album_name = fct_reorder(album_name, album_release_date)) |> 
  arrange(album_name) |> 
  pull(album_name)

beatles_prep <- beatles |> 
  tibble() |> 
  filter(album_name %in% beatles_album_names) |> 
  mutate(
    album_name = str_remove(album_name, "\\s\\(Remastered\\)$"),
    album_name = factor(album_name,
                        levels = str_remove(beatles_album_names, "\\s\\(Remastered\\)$"))
    )

# Custom theme
colors <- c("#FCFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_void(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      plot.title = element_text(
        color = "grey8", family = "AHDN", hjust = 0.5, size = 24,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0.5, halign = 0.5, color = "grey35", lineheight = 1.25, width = 1,
        margin = margin(b = 6)),
      plot.caption = element_markdown(hjust = 1),
      plot.margin = margin(t = 4, b = 4, l = 9, r = 9),
      legend.position = "top",
      panel.grid = element_blank(),
      strip.text = element_text(
        family = "Libre Franklin SemiBold", size = 9, color = "grey35", hjust = 0,
        margin = margin(t = 3, b = 1))
    )
)

beatles_font_mapping <- c(
  "John" = "J",
  "Paul" = "H",
  "George" = "G",
  "Ringo" = "I"
)

ragg::agg_png(here(base_path, "09-major-minor.png"), width = 6, height = 4.6,
              bg = "grey90", scaling = 1/1.3, units = "in", res = 300)
beatles_prep |> 
  count(album_name, mode_name) |> 
  ggplot(aes(fill = mode_name, values = n)) +
  geom_waffle(n_rows = 4, width = 0.8, height = 0.8, radius = unit(0.2, "npc")) +
  scale_fill_manual(values = c("#FFD600", "#512DA8")) +
  coord_equal() +
  facet_wrap(
    vars(album_name), ncol = 4, labeller = as_labeller(function(x) str_wrap(x, 20))) +
  guides(fill = "none") +
  labs(
    title = "The Beatles Modes",
    subtitle = "Each box represents a song on an album. 
    The colour indicates whether a song is in a 
    <b style='color:#FFD600'>major</b> or <b style='color:#512DA8'>minor</b>
    key.",
    caption = "Source: Spotify API. Visualization: Ansgar Wolsing"
  )

# Add the Beatles picture
img_x_pos <- seq(0.30, 0.60, 0.09)
walk2(
  img_x_pos, beatles_font_mapping,
  function(x, label) {
    grid.text(
      label = label, 
      x = x, y = 0.14,
      gp = gpar(fontfamily = "JAMON del MAR", cex = 3))
  }
)
dev.off()

## FONTS
# AHDN font: https://www.fontspace.com/ahdn-font-f5443
# Beatles icons font: https://www.fontspace.com/jamon-del-mar-font-f5444
