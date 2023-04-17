library(tidyverse)
library(ggtext)
library(tidytext)
library(here)
library(circlize)
# install.packages("schrute")
library(schrute)

base_path <- here("2023", "17")

data(theoffice)
head(theoffice)
count(theoffice, season, episode)

# Speaker names clean up
theoffice <- theoffice %>% 
  mutate(
    character = str_remove_all(character, "[\":]"),
    character = str_remove_all(character, "\\\""),
    character = case_match(
      character,
      "(Pam's mom) Heleen" ~ "Pam's mom",
      "AJ" ~ "A.J.",
      "abe" ~ "Gabe",
      .default = character
    )) 

top_character_names <- theoffice %>% 
  count(character, sort = TRUE) %>% 
  slice_max(order_by = n, n = 10) %>% 
  arrange(-n) %>% 
  pull(character)

theoffice_mentions <- 
  theoffice %>% 
  filter(character %in% top_character_names) %>% 
  unnest_tokens(word, text, token = "words", to_lower = FALSE) %>%
  filter(word %in% top_character_names) %>% 
  # order characters by total lines
  mutate(
    character = factor(character, levels = top_character_names),
    character_mentioned = factor(word, levels = top_character_names)) %>% 
  select(index, season, episode, character, character_mentioned)
    
theoffice_mentions_agg <- theoffice_mentions %>% 
  group_by(character, character_mentioned) %>% 
  summarize(
    total_mentions = n(),
    mentions_lines = n_distinct(index),
    .groups = "drop"
  ) %>% 
  complete(character, character_mentioned, fill = list(total_mentions = 0, mentions_lines = 0)) %>% 
  arrange(character, character_mentioned)

write_csv(theoffice_mentions_agg, here(base_path, "theoffice-mentions-agg.csv"))


#' CHORD DIAGRAM DOCUMENTATION:
#' https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html

# matrix of character connections as input for chordDiagram function
mat <- matrix(theoffice_mentions_agg$total_mentions, ncol = length(top_character_names))
rownames(mat) <- top_character_names
colnames(mat) <- top_character_names

# Colors
pal_office <- 
  c("#FBA93A", "#cc2d36", "#93BFE5", "#D8ACD8", "#f0813c", "#F0F4EC", "#2AA3A6",
             "#4BD9EF", "#4bad6d", "#5D77AA")
bg_color <- colorspace::darken("#435774", 0.2)


# All in 1 chart
ragg::agg_png(here(base_path, "17-network-the-office-mentions.png"),
              res = 500, width = 8, height = 8, units = "in", bg = bg_color)
par(
  family = "Outfit", cex = 2, col = "white", # font family, size, color
  bg = bg_color, 
  mai = rep(0.5, 4) # plot margin in inches
  ) 
chordDiagram(mat, transparency = 0.3, 
             grid.col = pal_office[1:10], # color the connections
             link.border = "white", link.lwd = 0.2, # color the connection borders + border size
             annotationTrack = c("name", "grid"), # remove axis labels (default is: name, grid, axis)
             annotationTrackHeight = mm_h(c(3, 5)),
             link.largest.ontop = TRUE
             )
title(
  main = "Who speaks about whom?",
  sub = "top 10 characters in The Office\nSource: {schrute} R package. Visualisation: Ansgar Wolsing",
  col.main = "white", cex.main = 1.3)
dev.off()

       

# A function to print the chord diagram and the title
plot_chordDiagram <- function(name, rank, color = NULL) {
  # row color with highlighting of focussed character
  row_colors <- rep("#EEEEEE33", 10)
  stopifnot(rank <= length(pal_office))
  row_colors[rank] <- pal_office[rank]
  # grid colors
  grid_colors <- alpha(pal_office, 0.2)
  # names(grid_colors) <- top_character_names
  stopifnot(rank <= length(pal_office))
  grid_colors[rank] <- pal_office[rank]
  
  chordDiagram(mat,
               grid.col = grid_colors, # color the connections
               # link.border = "white", link.lwd = 0.1, # color the connection borders + border size
               annotationTrack = c("name", "grid"), # remove axis labels (default is: name, grid, axis)
               annotationTrackHeight = mm_h(c(2, 2)),
               link.largest.ontop = TRUE,
               row.col = row_colors
  )
  title(main = name, col.main = "white", cex.main = 1.3)  
}



filepath_facets_chart <- here(base_path, "17-network-the-office-mentions-facets.png")
ragg::agg_png(filepath_facets_chart,
              res = 500, width = 10, height = 10, units = "in", bg = bg_color)
par(
  mfrow = c(3, 3),
  family = "Outfit", cex = 0.8, col = "white", # font family, size, color
  bg = bg_color, 
  mai = rep(0.5, 4) # plot margin in inches
) 

# Create chord diagram for each character
for (i in 1:9) {
  plot_chordDiagram(top_character_names[i], i)  
}

dev.off()


# Add titles 

library(magick)

img_chart <- image_read(filepath_facets_chart) 
img_chart_info <- image_info(img_chart)
img_new_geometry <- sprintf("%sx%s", img_chart_info$height, 1.11 * img_chart_info$height)

img_chart %>% 
  image_extent(img_new_geometry, gravity = "south", color = bg_color) %>% 
  # Title
  image_annotate("Who speaks about whom in The Office",
                 gravity = "north", size = 200, color = "white", 
                 font = "American Typewriter") %>% 
  # Subtitle
  image_annotate("The width of the connecting lines indicates how often a character
  mentions another character's name",
                 location = "+0+250",
                 gravity = "north", size = 100, color = "white", 
                 font = "Outfit") %>% 
  # Caption
  image_annotate("Source: {schrute} R package. Visualisation: Ansgar Wolsing",
                 location = "+0+50",
                 gravity = "south", size = 80, color = "white", font = "Outfit") %>% 
  image_write(here(base_path, "17-network-who-speaks-office.png"))

