library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "10")

lyrics <- read_lines(here(base_path, "physical-lyrics.txt"))
# remove empty lines
lines <- tibble(line = lyrics[str_length(lyrics) > 0])

# Full lyrics in one string
full_lyrics <- toupper(paste(lines$line, collapse = " "))
# Highlight the word "physical"
highlight_color <- "#e809a5"
highlight_str <- sprintf("<span style='color:%s'>physical</span>", highlight_color)
highlight_str <- "<b>PHYSICAL</b>"
full_lyrics_highlighted <- gsub("PHYSICAL", highlight_str, full_lyrics)

gradient_fill <- grid::linearGradient(c("#f2e018", "#5406a1"), group = FALSE) 

ggplot() +
  geom_textbox(
    aes(x = 0, y = 1, label = full_lyrics_highlighted),
    width = 0.8, box.size = 0, family = "Roboto Condensed", size = 2.75,
    fill = NA, color = "white"
  ) +
  labs(
    title = "Let's Get Physical",
    caption = "Lyrics written by Steve Kipner & Terry Shaddick. Performed by Olivia Newton-John.") +
  theme_void(base_family = "Roboto Condensed Light") +
  theme(
    plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
    text = element_text(color = "white"),
    plot.title = element_text(family = "Bangers", size = 28, hjust = 0.5),
    plot.caption = element_text(size = 5, hjust = 0.5),
    plot.margin = margin(t = 2, b = 4)
  )
ggsave(here(base_path, "10-physical-lyrics-text.png"), width = 4, height = 4)


lines |> 
  filter(str_detect(line, "[Ll]et's get")) |> 
  mutate(activity = str_extract(line, "[Ll]et's get (?:into )?([^,]+)", group = 1)) |> 
  count(activity) |> 
  mutate(activity = fct_reorder(activity, -n)) |> 
  ggplot(aes(activity, n)) +
  geom_col(fill = "white", width = 0.6)  +
  geom_text(
    aes(y = 0, label = toupper(activity)),
    col = "grey58", family = "Bangers", vjust = -0.5, size = 5
  ) +
  geom_text(
    aes(label = n),
    vfamily = "Roboto Condensed", fontface = "bold", vjust = -0.5, color = "white"
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "LET'S GET...",
    subtitle = "# of lines",
    caption = "Lyrics written by Steve Kipner & Terry Shaddick. Performed by Olivia Newton-John.") +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
    text = element_text(color = "white"),
    plot.title = element_text(
      family = "Bangers", size = 28, hjust = 0.5, margin = margin(t = 4, b = 12)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(size = 5, hjust = 0.5),
    plot.margin = margin(t = 2, b = 4)
  )
ggsave(here(base_path, "10-physical-lyrics-barchart.png"), width = 4, height = 4)
