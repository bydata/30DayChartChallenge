library(tidyverse)
library(gganimate)
library(ggtext)
library(here)

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
  2018, "Bayern München"
)
champions

nrow(champions)

# prepare dataframe into to grids (time, title count) for the plot 
max_cols <- 8
df_plot <- champions %>% 
  # create grid by year
  mutate(id = row_number(),
         grid_time_x = (row_number() - 1) %% max_cols + 1,
         grid_time_y = (row_number() + (max_cols - 1)) %/% max_cols) %>% 
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
  arrange(year) 

# with facets
df_plot %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(color = champion), size = 5) +
  scale_y_reverse() +
  facet_wrap(vars(order)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.spacing.x = unit(1.5, "cm")
  )

# animated
df_plot %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(color = champion), size = 8) +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  transition_states(order)

