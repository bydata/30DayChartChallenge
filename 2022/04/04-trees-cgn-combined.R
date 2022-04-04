library(ggplot2)
library(magick)
library(here)
library(glue)

base_path <- here("2022", "04")

# run the R scripts to create the treemap and the wordcloud
source(here(base_path, "04-flora-trees-cologne.R"))
# Open the resulting wordcloud in the browser and make a screenshot
source(here(base_path, "04-flora-trees-wordcloud.R"))

# Create a blank image with titles
ggplot() +
  labs(title = "Tree Genera and Species in Cologne",
       subtitle = "*Note:* Trees of unknown genera not shown.<br>
       *Source:* Open Data Cologne, Tree Cadastre<br>
       *Visualization:* Ansgar Wolsing") +
  theme_void() +
  theme(
   plot.background = element_rect(color = NA, fill = "white"),
   plot.margin = margin(20, 20, 20, 20),
   plot.title = element_text(
     size = 32, family = "Source Serif Pro SemiBold",
     margin = margin(b = 24)),
   plot.subtitle = ggtext::element_markdown(
     size = 12, color = "grey20", family = "Source Serif Pro", lineheight = 1.4)
  )
ggsave(here(base_path, "04-trees-title.png"), width = 8, height = 2)

images <- c(
  "treemap" = image_read(here(base_path, "04-treemap.png")),
  "wordcloud" = image_read(here(base_path, "tree-wordcloud.png")),
  "barchart" = image_read(here(base_path, "04-trees-bar-chart.png")),
  "title" = image_read(here(base_path, "04-trees-title.png"))
)
print(images)

# Combine the images
img_left <- image_append(c(images[4], image_scale(images[2], "2200x")), stack = TRUE) 
img_combined <- image_append(c(img_left, image_scale(images[1], "x2800")), stack = FALSE)
image_write(img_combined, here(base_path, "04-trees-combined.png"))

