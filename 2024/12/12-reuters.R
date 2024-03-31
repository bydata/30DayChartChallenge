library(tidyverse)
library(ggtext)
library(here)
library(schrute)
library(ggdist)
library(shadowtext)

base_path <- here("2024", "12")

data(theoffice)

ratings <- theoffice |> 
  distinct(season, episode, imdb_rating)

ratings |> 
  ggplot(aes(imdb_rating)) +
  geom_histogram()

# Determine the height of the mode value
max_height <- ratings |> 
  count(imdb_rating, sort = TRUE) |> 
  head(1) |> 
  pull(n)


ratings |> 
  ggplot(aes(imdb_rating)) +
  ggdist::geom_dots(
    aes(col = imdb_rating < 7 | imdb_rating > 9.0),
    shape = 15,
    dotsize = 1.07, stackratio = 1.1, scale = 1, height = max_height + 1,
    show.legend = FALSE) +
  annotate(
    # GeomShadowtext,
    "text",
    x = 6.5, y = 8, label = "Only 2 episodes have\na rating below 7.0",
    color = "#F3B145", family = "Source Sans Pro", lineheight = 1.2, size = 3,
    hjust = 0, vjust = 0.5
  ) +
  annotate(
    "segment",
    x = 6.7, xend = 6.7, y = 6.6, yend = 1,
    color = "#C7C7C7", linewidth = 0.2
  ) +
  # Custom y axis labels
  annotate(
    GeomTextBox,
    x = 6.1, y = seq(0, 25, 5),
    label = seq(0, 25, 5),
    halign = 1, hjust = 0, vjust = 0, family = "Source Sans Pro Light", size = 3,
    box.size = 0, box.padding = unit(0, "mm"), color = "#666666", fill = NA, 
    width = 0.025
  ) +
  # ... add "episodes"
  annotate(
    GeomTextBox,
    x = 6.22, y = 25,
    label = "episodes",
    halign = 0, hjust = 0, vjust = 0, family = "Source Sans Pro Light", size = 3,
    box.size = 0, box.padding = unit(0, "mm"), color = "#666666", fill = NA, 
    width = 0.05
  ) +
  scale_x_continuous(
    limits = c(NA, 10), breaks = seq(0, 10, 0.5),
    expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(breaks = seq(0, 25, 5), expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = c("grey70", "#F3B145")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "",
    subtitle = "Avg. IMDb rating per episode. Each square represents one episode.",
    caption = "Source: {schrute} R package. Visualization: Ansgar Wolsing
    (replicating the style of Reuters Graphics).",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Source Sans Pro Light") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#C7C7C7", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_line(color = "#31312F", linewidth = 0.3),
    plot.title = element_text(family = "Source Sans Pro SemiBold"),
    plot.title.position = "plot",
    plot.caption = element_textbox(width = 0.9, hjust = 0, size = 7),
    plot.caption.position = "plot"
  )
ggsave(here(base_path, "12-reuters.png"), width = 6, height = 4)  
