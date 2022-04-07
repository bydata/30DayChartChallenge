library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(ggdist)
library(patchwork)

base_path <- here("2022", "07")

library(DBI)
library(dbplyr)
con <- dbConnect(RSQLite::SQLite(), here(base_path, "basketball.sqlite"))

dbListTables(con)

query <- "
SELECT 
  FIRST_NAME, LAST_NAME, POSITION, HEIGHT, WEIGHT, FROM_YEAR, TO_YEAR 
FROM
  Player_Attributes
"
res <- dbSendQuery(con, query)
players <- dbFetch(res) %>% as_tibble()
colnames(players) <- tolower(colnames(players))

players %>% 
  filter(position == "Guard-Forward")

players <- players %>% 
  filter(!is.na(height), height > 0, position != "",
         !is.na(weight), weight > 0) %>% 
  # recode positions
  mutate(position = fct_recode(position, "Guard-Forward" = "Forward-Guard",
                               "Center-Forward" = "Forward-Center"
  ))



## custom ggplot2 theme
text_color <- "grey83"
theme_set(theme_void(base_family = "Poppins", base_size = 8))
theme_update(
  plot.background = element_rect(color = NA, fill = "grey12"),
  text = element_text(color = text_color),
  strip.text = element_text(face = "bold",
                            margin = margin(t = 4, b = 4, l = 4, r = 4)),
  strip.text.y = element_text(angle = 90),
  strip.background = element_rect(color = NA, fill = alpha("grey20", 0.8)),
  panel.background = element_rect(color = NA, fill = "grey12"),
  axis.text = element_text(color = text_color, size = 6),
  axis.title.x = element_text(color = text_color, margin = margin(t = 4),
                              size = 8, family = "Poppins SemiBold"),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  plot.margin = margin(6, 6, 6, 6),
  axis.line.x = element_line(color = text_color),
  axis.ticks.x = element_line(color = text_color),
  axis.ticks.length.x = unit(2, "mm"),
  plot.title = element_text(color = "white", size = 14, family = "Poppins SemiBold"),
  plot.subtitle = element_markdown(),
  plot.caption = element_textbox_simple(
    hjust = 0, margin = margin(t = 8, b = 2)
  )
)


p_height <- players %>% 
  ggplot(aes(position, height)) +
  stat_halfeye(aes(fill = position), col = "white") +
  geom_text(aes(position, y = min(height), label = position, col = position), 
            stat = "unique", size = 4, hjust = 0, nudge_x = 0.3, family = "Bangers") +
  paletteer::scale_color_paletteer_d("miscpalettes::jojo") +
  paletteer::scale_fill_paletteer_d("miscpalettes::jojo") +
  coord_flip() +
  guides(color = "none", fill = "none")+
  labs(y = "Height (in inches)")


p_weight <- players %>% 
  ggplot(aes(position, weight, fill = position, col = position)) +
  stat_halfeye(col = "white") +
  geom_text(aes(position, y = max(weight), label = position), stat = "unique",
            hjust = 1, size = 4, nudge_x = 0.3, family = "Bangers") +
  scale_y_continuous(breaks = seq(150, 350, 50)) +
  paletteer::scale_color_paletteer_d("miscpalettes::jojo") +
  paletteer::scale_fill_paletteer_d("miscpalettes::jojo") +
  coord_flip() +
  guides(color = "none", fill = "none") +
  labs(y = "Weight (in pounds)")

n_players <- nrow(players)

p_height + p_weight +
  plot_annotation(
    title = "NBA Player Heights and Weights",
    subtitle = glue::glue("Height and weight attributes of
                          {scales::number(n_players, big.mark = ',')}
                          NBA professionals from 1951 to 2019"),
    caption = "Positions Guard-Forward and Forward-Guard as well as Forward-Center and Center-Forward 
    merged.<br>Source: NBA, Kaggle. Visualization: Ansgar Wolsing")
ggsave(here(base_path, "07-physical-nba-height-weight.png"), width = 7, height = 4)

