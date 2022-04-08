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


#' 2014: 
#' 2015: https://en.wikipedia.org/wiki/2015_Mount_Everest_avalanches


# annotate_richtext <- function(...) {
#   annotate("richtext",
#            fill = "red", color = "grey12",
#            family = "PT Serif", size = 3, hjust = 0,
#            label.size = 0, label.r = unit(2, "mm"),
#            ...)
# } 




#' Image credits:
#' https://commons.wikimedia.org/wiki/File:Mount_Everest_as_seen_from_Drukair2.jpg
#' by shrimpo1967
img_everest <- image_read(here(base_path, "Mount_Everest_as_seen_from_Drukair2_PLW_edit.jpg"))
img_everest <- image_scale(img_everest, "x1200")
img_everest_info <- image_info(img_everest)
img_everest_desaturated <- image_modulate(img_everest, saturation = 30)


img_plot <- image_graph(res = 300, width = img_everest_info$width, 
                        height = img_everest_info$height, 
                        bg = "transparent")
climbers %>% 
  ggplot(aes(year)) +
  geom_bar(fill = "white", color = "grey50", size = 0.05) +
  # annotate_text(label = "foo foo foo foo foo ", x = 1953, y = 100) +
  annotate("richtext", 
           label = "<b>1953</b> First successful ascent", x = 1953, y = 100,
           fill = "white", color = "grey12", label.color = NA,
           family = "Helvetica", size = 2, hjust = 0,
           label.size = 0, label.r = unit(0, "mm")) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_y_continuous(position = "right") +
  labs(
    title = "Mount Everest Climbers",
    subtitle = "6,014 different people have climbed the Mount Everest since 
    Tenzing Norgay and Edmund Hillary reached its summit in 1953. 
    Some, like Nawang Gombu and Reinhold Messner, even climbed Mount Everest twice.
    The graph counts each climber only once and with the date of the first ascent.",
    caption = "**Source:** The Himalayan Database. **Image credit:** shrimpo1967, Wikipedia.
    **Visualization:** Ansgar Wolsing"
  ) + 
  theme_void(base_family = "PT Serif", base_size = 8) +
  theme(
    text = element_text(color = "white"),
    axis.text.x = element_text(),
    axis.line.x = element_line(color = "white"),
    plot.margin = margin(3, 10, 10, 10),
    plot.title = element_markdown(
      size = 20, fill = alpha("grey10", 0.9),
      # family = "Helvetica Neue Bold",
      face = "bold", padding = margin(t = 2, b = 2, l = 6, r = 6)),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 6, b = 12), width = unit(0.6, "npc"), hjust = 0
    ),
    plot.caption = element_markdown(hjust = 0)
  )
dev.off()

img_combined <- image_composite(img_everest_desaturated, img_plot)
image_write(img_combined, here(base_path, "08-mteverest-combined.png"))

