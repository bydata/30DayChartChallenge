library(tidyverse)
library(gganimate)
library(ggtext)
library(here)
library(grid)

#' HANS ROSLING'S BUBBLE CHART 
#'  https://www.youtube.com/watch?v=hVimVzgtD6w
#'  With the drama and urgency of a sportscaster, statistics guru Hans Rosling 
#'  uses an amazing new presentation tool, Gapminder, to present data that debunks 
#'  several myths about world development. Rosling is professor of international 
#'  health at Sweden's Karolinska Institute, and founder of Gapminder, a nonprofit 
#'  that brings vital global data to life. 
#'  (Recorded February 2006 in Monterey, CA.)

base_path <- here("2022", "22")

## Data preparation ------------------------------------------------------------

child_mortality <- read_csv(here(base_path, "child_mortality_0_5_year_olds_dying_per_1000_born.csv"))
life_expectancy <- read_csv(here(base_path, "life_expectancy_years.csv"))
population <- read_csv(here(base_path, "population_total.csv"))
children_per_woman <- read_csv(here(base_path, "children_per_woman_total_fertility.csv"))

end_year <- 2021
population_prep <- population %>% 
  pivot_longer(cols = -country, names_to = "year", names_transform = as.numeric,
               values_to = "population") %>% 
  mutate(scale_unit = str_extract(population, "[kMB]"),
         population_num = str_remove(population, scale_unit),
         population_num = as.numeric(population_num),
         population_num = case_when(
           scale_unit == "B" ~ population_num * 10^9,
           scale_unit == "M" ~ population_num * 10^6,
           scale_unit == "k" ~ population_num * 10^3
         )) %>% 
  filter(year <= end_year) %>% 
  select(country, year, population = population_num)

child_mortality_prep <-  child_mortality %>% 
  pivot_longer(cols = -country, names_to = "year", names_transform = as.numeric,
               values_to = "child_mortality_per_1k_born")

life_expectancy_prep <-  life_expectancy %>% 
  pivot_longer(cols = -country, names_to = "year", names_transform = as.numeric,
               values_to = "life_expectancy")

children_per_woman_prep <-  children_per_woman %>% 
  pivot_longer(cols = -country, names_to = "year", names_transform = as.numeric,
               values_to = "children_per_woman")

# Combine datasets
df_prep <- population_prep %>% 
  inner_join(children_per_woman_prep, by = c("country", "year")) %>% 
  inner_join(life_expectancy_prep, by = c("country", "year")) %>% 
  mutate(continent = countrycode::countrycode(country, origin = "country.name", 
                                              destination = "continent"))


## Plot & Animation ------------------------------------------------------------

line_color <- "grey41"
highlighted_countries <- c("Bangladesh", "China", "Turkey", "Morocco",
                           "South Korea", "Netherlands", "United States")

p <- df_prep %>% 
  # original animated bubble plot starts at 1962
  filter(year >= 1962) %>% 
  ggplot(aes(children_per_woman, life_expectancy)) +
  geom_text(aes(label = scales::number(year, accuracy = 1, big.mark = ""), 
                x = 5, y = 35), 
            stat = "unique", color = "grey85", size = 16,
            family = "Chivo",
            hjust = 0.5, vjust = 0.5) +
  geom_point(
    data = ~filter(., !country %in% highlighted_countries),
    aes(size = population, 
        col = continent,
        fill = stage(continent, after_scale = alpha(colorspace::desaturate(fill, 0.4), 0.2))
        ), shape = 21, stroke = 0.15) + 
  geom_point(
    data = ~filter(., country %in% highlighted_countries),
    aes(size = population, 
        fill = continent),
    shape = 21, col = "white", stroke = 0.15) + 
  ggrepel::geom_label_repel(
    data = . %>% filter(country %in% highlighted_countries),
    aes(label = country), fill = alpha("grey92", 0.5), segment.color = "grey52",
    size = 2, label.size = 0, family = "Fira Sans Condensed", label.padding = 0.1,
    seed = 1, point.padding = 0.25, min.segment.length = 0, segment.size = 0.25) +
  scale_x_continuous(position = "top", breaks = 1:9) +
  scale_y_continuous(expand = expansion(add = c(0, 2))) +
  scale_fill_manual(values = c(
    "Africa" = "#F9C31F", "Americas" = "#F97A1F", "Asia" = "#E2365B",
    "Europe" = "#36E2BD", "Oceania" = "#2E45B8"),
    aesthetics = c("fill", "col")) +
  scale_size_continuous(
    breaks = c(10^6, 10^7, 5 * 10^7, 10^8, 5 * 10^8, 10^9),
    labels = scales::number_format(accuracy = 1, scale = 10^-6, suffix = "M")) +
  coord_cartesian(ylim = c(20, 85)) +
  guides(
    fill = guide_legend(order = 1, title.position = "top", title.hjust = 0.5,
                        override.aes = list(size = 3)),
    size = guide_legend(order = 2, title.position = "top", title.hjust = 0.5,
                        nrow = 1,
                        override.aes = list(color = "white", fill = "grey50")),
    color = "none"
  ) +
  labs(
    title = "LONGER LIVES IN SMALLER FAMILIES",
    subtitle = "",
    caption = "Based on free material from GAPMINDER.ORG, CC-BY LICENSE.
    Visualization: Ansgar Wolsing",
    x = "Fertility rate (children per woman)",
    y = "Life expectancy at birth (years)",
    fill = "Continents",
    size = "Population"
  ) +
  theme_void(base_family = "Fira Sans Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey97"),
    legend.position = "bottom",
    legend.box = "vertical",
    text = element_text(color = "grey12"),
    plot.title = element_text(color = "grey2", family = "Fira Sans Condensed SemiBold", 
                              size = 14, hjust = 0.5),
    plot.subtitle = element_textbox(width = 0.8, size = 9, hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5),
    axis.line = element_line(color = line_color),
    axis.ticks = element_line(color = line_color),
    axis.ticks.length = unit(1, "mm"),
    axis.title = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.text = element_text(color = line_color),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.margin = margin(6, 6, 6, 6)
  )
p

p_anim <- p  +
  transition_states(year) +
  ease_aes("cubic-out")

animate(p_anim, res = 200, width = 6, duration = 18, fps = 16, height = 5, 
        units = "in", end_pause = 32, bg = "grey97")
anim_save(here(base_path, "22-global-change.gif"))


# summary statistics per country
df_prep %>% 
  filter(year >= 1962) %>% 
  group_by(country, continent) %>% 
  summarize(across(c(children_per_woman, life_expectancy), list(min = min, max = max)),
            .groups = "drop") %>% View()
