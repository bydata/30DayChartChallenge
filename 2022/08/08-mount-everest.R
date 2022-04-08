library(tidyverse)
library(here)
library(glue)
library(ggtext)
library(jsonlite)
library(lubridate)
library(magick)

base_path <- here("2022", "08")

#' Source: The Himalayan Database
#' https://haexpeditions.com/advice/list-of-mount-everest-climbers/
#' As of 2022-04-08, 6,014 persons have climbed the Mt. Everest
#' Since the API provides records in chunks of 3,000, we need to fetch chunks 0 to 2.
chunk_ids <- 0:2
api_urls <- glue(
  "https://haexpeditions.com/wp-admin/admin-ajax.php?action=wp_ajax_ninja_tables_public_action&table_id=1084&target_action=get-all-data&default_sorting=old_first&skip_rows=0&limit_rows=0&chunk_number={chunk_ids}&ninja_table_public_nonce=4421ea962c"
)

# Pull the data
climbers_raw <- map_dfr(api_urls, read_json, simplify = TRUE)
write_rds(climbers_raw, here(base_path, "data", "mteverest_climbers_raw.rds"))
str(climbers_raw)

# Prepare the data
climbers <- tibble(climbers_raw$value) %>% 
  mutate(date = mdy(date),
         year = year(date),
         gender = factor(gender)) %>% 
  select(-`___id___`)

climbers %>% 
  count(year) %>% 
  arrange(desc(year))

climbers %>% 
  ggplot(aes(year)) +
  geom_bar() +
  scale_y_continuous(position = "right") +
  theme_minimal()
ggsave(here(base_path, "08-mteverest-barchart.png"), width = 6, height = 5)

climbers %>% 
  ggplot(aes(year)) +
  stat_count(geom = "area")


#' Image credits:
#' https://commons.wikimedia.org/wiki/File:Mount_Everest_as_seen_from_Drukair2.jpg
#' by shrimpo1967
img_everest <- image_read(here(base_path, "Mount_Everest_as_seen_from_Drukair2_PLW_edit.jpg"))
img_everest <- image_scale(img_everest, "x1200")
img_everest_info <- image_info(img_everest)
# desaturate image
img_everest_edited <- image_modulate(img_everest, saturation = 25, brightness = 60) 


# Annotations to be presented in plot with geom_richtext and geom_segment
#' 2014: https://en.wikipedia.org/wiki/2014_Mount_Everest_ice_avalanche
#' 2015: https://en.wikipedia.org/wiki/2015_Mount_Everest_avalanches
annotations <- tibble(
  year = c(1953, 1980, 2000, 2014, 2020),
  y    = c( 100,  200,  400,  500,  300),
  label = c(
    "<b>1953</b> | First successful ascent",
    "<b>1980</b> | Reinhold Messer - first ascent<br>without supplementary oxygen",
    "Climbing Mt. Everest becomes<br>increasingly popular",
    "<b>2014/15</b> | Due to avalanches<br>with multiple people killed,<br>
    there were no ascents in 2015",
    "Coronavirus<br>pandemic")
)
# determine yend for geom_segment from climbers data
annotations <- climbers %>% 
  count(year, name = "yend") %>% 
  right_join(annotations) %>% 
  mutate(yend = yend + 5, 
         y = y - 1)


# Create the ggplot2 graph with image_graph as graphics device
img_plot <- image_graph(res = 300, width = img_everest_info$width, 
                        height = img_everest_info$height, 
                        bg = "transparent")
climbers %>% 
  ggplot(aes(year)) +
  geom_bar(fill = "white", color = "grey50", size = 0.05) +
  geom_segment(
    data = annotations,
    aes(x = year, xend = year, y = y, yend = yend),
    inherit.aes = FALSE,
    col = alpha("white", 0.9), size = 0.2) + 
  geom_richtext(
    data = annotations,
    aes(x = year - 1, y, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0.9), color = "grey4", label.color = NA,
    family = "PT Serif", size = 2, hjust = 0, # halign = 0, 
    vjust = 0, label.size = 0, label.r = unit(0, "mm")) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(NA, 2025)) +
  scale_y_continuous(position = "right", limits = c(0, 600), breaks = seq(0, 500, 100)) +
  labs(
    title = "Mount Everest Climbers",
    subtitle = "6,014 different people have climbed the Mount Everest since 
    Tenzing Norgay and Edmund Hillary reached its summit in 1953. 
    Some, like Nawang Gombu and Reinhold Messner, even climbed Mount Everest twice.
    The graph counts each climber only once and with the date of the first ascent.",
    caption = "**Source:** The Himalayan Database, haexpeditions.com. **Image credit:** shrimpo1967, Wikipedia.
    **Visualization:** Ansgar Wolsing"
  ) + 
  theme_void(base_family = "PT Serif", base_size = 8) +
  theme(
    text = element_text(color = "white"),
    axis.text = element_text(),
    axis.text.y = element_text(
      vjust = 0.2, hjust = 0, margin = margin(l = 3), size = 6),
    axis.line.x = element_line(color = "white"),
    axis.ticks.y.right = element_line(color = "white"),
    axis.ticks.length.y.right = unit(2, "mm"),
    plot.margin = margin(3, 10, 10, 10),
    plot.title = element_markdown(
      size = 20, fill = alpha("grey10", 0.9), face = "bold", 
      padding = margin(t = 2, b = 2, l = 6, r = 6)),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 6, b = 24), width = unit(0.6, "npc"), hjust = 0
    ),
    plot.caption = element_markdown(
      hjust = 1, margin = margin(t = 8))
  )
dev.off()

# combine plot and Mt. Everest photo
img_combined <- image_composite(img_everest_edited, img_plot)
image_write(img_combined, here(base_path, "08-mteverest.png"))
