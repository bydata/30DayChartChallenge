library(tidyverse)
library(gganimate)
library(ggtext)
library(here)
library(glue)

base_path <- here("2022", "01")

champions <- tribble(
  ~year, ~champion,
  1964, "1. FC Köln",
  1965, "Werder Bremen",
  1966, "TSV 1860 München",
  1967, "Eintracht Braunschweig",
  1968, "1. FC Nürnberg",
  1969, "Bayern München",
  1970, "Borussia Mönchengladbach",
  1971, "Borussia Mönchengladbach",
  1972, "Bayern München",
  1973, "Bayern München",
  1974, "Bayern München",
  1975, "Borussia Mönchengladbach",
  1976, "Borussia Mönchengladbach",
  1977, "Borussia Mönchengladbach",
  1978, "1. FC Köln",
  1979, "Hamburger SV",
  1980, "Bayern München",
  1981, "Bayern München",
  1982, "Hamburger SV",
  1983, "Hamburger SV",
  1984, "VfB Stuttgart",
  1985, "Bayern München",
  1986, "Bayern München",
  1987, "Bayern München",
  1988, "Werder Bremen",
  1989, "Bayern München",
  1990, "Bayern München",
  1991, "1. FC Kaiserslautern",
  1992, "VfB Stuttgart",
  1993, "Werder Bremen",
  1994, "Bayern München",
  1995, "Borussia Dortmund",
  1996, "Borussia Dortmund",
  1997, "Bayern München",
  1998, "1. FC Kaiserslautern",
  1999, "Bayern München",
  2000, "Bayern München",
  2001, "Bayern München",
  2002, "Borussia Dortmund",
  2003, "Bayern München",
  2004, "Werder Bremen",
  2005, "Bayern München",
  2006, "Bayern München",
  2007, "VfB Stuttgart",
  2008, "Bayern München",
  2009, "VfL Wolfsburg",
  2010, "Bayern München",
  2011, "Borussia Dortmund",
  2012, "Borussia Dortmund",
  2013, "Bayern München",
  2014, "Bayern München",
  2015, "Bayern München",
  2016, "Bayern München",
  2017, "Bayern München",
  2018, "Bayern München",
  2019, "Bayern München",
  2020, "Bayern München",
  2021, "Bayern München"
)
champions

nrow(champions)
unique(champions$champion)
length(unique(champions$champion))

## prepare dataframe into to grids (time, title count) for the plot  -----------

#' Manually download club logos as pngs (filenames according to club names) 
#' and place them in the "icons" directory.

# how many logos per row?
max_cols <- 10
# shift start by n
offset_start <- 4
# width and height for club logos
image_size <- 15

df_plot <- champions %>% 
  # create grid by year
  mutate(id = row_number(),
         grid_time_x = (row_number() + offset_start - 1) %% max_cols + 1,
         grid_time_y = (row_number() + offset_start + (max_cols - 1)) %/% max_cols) %>% 
  # create grid by number of titles
  add_count(champion, name = "total_titles") %>% 
  arrange(-total_titles, year) %>% 
  mutate(
    champion = fct_reorder(champion, -total_titles),
    grid_count_x = (row_number() - 1) %% max_cols + 1,
    grid_count_y = (row_number() + (max_cols - 1)) %/% max_cols) %>% 
  # reshape to long format to switch between order by year and order by title count
  pivot_longer(cols = starts_with("grid_"), names_to = c("order", "name"), 
               names_pattern = "grid_(.+)_(.+)") %>% 
  pivot_wider(id_cols = c(year, champion, id, total_titles, order),
              names_from = "name", values_from = "value") %>% 
  # make order a factor with time first, then title count
  mutate(order = factor(order, levels = c("time", "count"))) %>% 
  arrange(year) %>% 
  mutate(club_logo = sprintf(
    "<img src='%s' width='%d' height='%d'>",
    here(base_path, "icons", paste0(champion, ".png")),
    image_size, ifelse(champion == "Hamburger SV", round(image_size * 192/260), image_size)))


## Static plot -----------------------------------------------------------------

# with facets
df_plot %>% 
  ggplot(aes(x, y)) +
  geom_richtext(aes(label = club_logo)) +
  scale_y_reverse() +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(vars(order)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.spacing.x = unit(1.5, "cm")
  )

## BASE plot -----------------------------------------------------------------

p_base <- df_plot %>% 
  ggplot(aes(x, y)) +
  geom_richtext(aes(label = club_logo),
                label.size = 0, color = "grey60", fill = NA) +
  geom_text(
    data = data.frame(x = rep(-1, 6), y = 2:7,label = seq(1970, 2020, 10),
                      order = rep("time", 6)),
    aes(x, y, label = label),
    family = "Fira Sans", color = "grey60", fontface = "bold", hjust = 0
  ) +
  scale_x_continuous(expand = expansion(add = c(0, 0.5))) +
  scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) + 
  theme_minimal(base_family = "Fira Sans") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    text = element_text(color = "grey30"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 4, b = 8)),
    plot.caption = element_markdown(color = "grey45", size = 6)
  )


## ENGLISH PLOT ----------------------------------------------------------------

# Add some more annotations
annotations_en <- tribble(
  ~x, ~y, ~order, ~hjust, ~label,
   5,  6.75, "count", 0,  "4 clubs won the title once",
  4,  1, "time", 1,       str_wrap("1. FC Köln won the first Bundesliga title", width = 30),
  2.5,  7, "time", 0,     "The last 9 titles went to Munich",
  1,  0.25, "count", 0,   "30 titles"
)

p_with_annotations_en <- p_base + 
  geom_text(data = annotations_en,
                  aes(x, y, label = label, hjust = hjust, group = order),
              family = "Fira Sans Light", size = 3) +
  geom_rect(data = data.frame(order = c("count", "count"), 
                              xmin = c(0.5, 4.5), 
                              xmax = c(max_cols + 0.5, 8.5),
                              ymin = c(0.5, 5.5), 
                              ymax = c(3.5, 6.5)),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax =  ymax), inherit.aes = FALSE,
            color = "grey40", fill = NA, size = 0.2
            ) +
  labs(
    title = "All Champions in the German Bundesliga",
    subtitle = "1963/'64 to 2020/'21",
    caption = "**Visualization:** Ansgar Wolsing"
  )

p_anim_en <- p_with_annotations_en +
  transition_states(order, state_length = 3) +
  enter_appear(early = FALSE, exclude_layer = "text")
animate(p_anim_en, res = 300, detail = 2, width = 4.5, height = 4, units = "in",
        rewind = FALSE, start_pause = 2, end_pause = 5)
anim_save(here(base_path, "01-bundesliga-en.gif"))



## GERMAN PLOT -----------------------------------------------------------------

# Add some more annotations
annotations_de <- tribble(
  ~x, ~y, ~order, ~hjust, ~label,
  5,  6.75, "count", 0,  "4 Vereine gewannen den Titel einmal",
  4,  1, "time", 1,       str_wrap("1. FC Köln erster Bundesliga-Meister", width = 20),
  2.5,  7, "time", 0,     "Die vergangenen 9 Titel gingen nach München",
  1,  0.25, "count", 0,   "30 Titel"
)

p_with_annotations_de <- p_base + 
  geom_text(data = annotations_de,
            aes(x, y, label = label, hjust = hjust, group = order),
            family = "Fira Sans Light", size = 3) +
  geom_rect(data = data.frame(order = c("count", "count"), 
                              xmin = c(0.5, 4.5), 
                              xmax = c(max_cols + 0.5, 8.5),
                              ymin = c(0.5, 5.5), 
                              ymax = c(3.5, 6.5)),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax =  ymax), inherit.aes = FALSE,
            color = "grey40", fill = NA, size = 0.2
  ) +
  labs(
    title = "Alle Meister der deutschen Fußball-Bundesliga",
    subtitle = "1963/'64 bis 2020/'21",
    caption = "**Visualisierung:** Ansgar Wolsing"
  )

p_anim_de <- p_with_annotations_de +
  transition_states(order, state_length = 3) +
  enter_appear(early = FALSE, exclude_layer = "text")
animate(p_anim_de, res = 300, detail = 2, width = 4.5, height = 4, units = "in",
        rewind = FALSE, start_pause = 2, end_pause = 5)
anim_save(here(base_path, "01-bundesliga-de.gif"))
