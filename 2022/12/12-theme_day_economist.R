library(tidyverse)
library(grid)
library(ggtext)
library(here)

base_path <- here("2022", "12")

#' Manually download data source from OWID:
#' GDP per capita: https://ourworldindata.org/grapher/gdp-per-capita-worldbank
#' Population: https://ourworldindata.org/grapher/population-past-future

# Prepare GDP data
gdp <- read_csv(here(base_path, "gdp-per-capita-worldbank.csv"))
gdp <- gdp %>% 
    rename(gdpPercap = 4) %>%
    filter(!is.na(Code))
glimpse(gdp)
gdp_2020 <- gdp %>% 
  filter(Year == 2020)

# Prepare population data
pop <- read_csv(here(base_path, "population-past-future.csv"))
pop <- pop %>% 
  rename(pop = 4) %>%
  filter(!is.na(Code))
glimpse(pop)
pop_2020 <- pop %>% 
  filter(Year == 2020)

# merge dataframes and add continent
df_2020 <- gdp_2020 %>% 
  inner_join(pop_2020, by = c("Year", "Entity", "Code")) %>% 
  rename(country = Entity, code = Code, year = Year)  %>% 
  mutate(
    continent = countrycode::countrycode(
      code, origin = "iso3c", destination = "continent"),
    ountry = fct_reorder(country, -pop),
    continent = factor(continent, 
                       levels = rev(c("Africa", "Americas", "Asia", "Europe", "Oceania")))) %>% 
  filter(!is.na(continent))

p <- df_2020 %>% 
  ggplot(aes(continent, gdpPercap, fill = continent)) +
  ggbeeswarm::geom_beeswarm(aes(size = pop), 
                            cex = 3.1, alpha = 0.6,
                            shape = 21, stroke = 0.2, col = "white",
                            show.legend = FALSE) +
  # y axis labels above the grid lines
  geom_text(aes(x = continent, y = min(gdpPercap) - 300, label = continent),
            stat = "unique", nudge_x = 0.1, family = "Fira Sans Condensed",
            hjust = 0, size = 3.5) + 
  # no expansion on the left-hand side so that the geom_text appears as y-axis labels
  scale_y_log10(labels = scales::number_format(), 
                breaks = c(1000, 3000, 10000, 30000, 100000),
                expand = expansion(mult = c(0, 0.05))) +
  # scale_size_area(max_size = 20) +
  scale_size_continuous(range = c(1, 20)) +
  scale_fill_manual(values = c(
    "Europe" = "#36E2BD", "Oceania" = "#141F52", "Asia" = "#E2365B", 
    "Africa" = "#1F2E7A", "Americas" = "#F97A1F")) + 
  coord_flip(clip = "off")  +
  labs(
    title = "GDP per capita varies between and within continents",
    subtitle = "Measured in constant international US-$ (logarithmic scale).<br>
    The size of the bubbles indicates population. Data from 2020",
    caption = "Source: Our World in Data, Worldbank. Theme: The Economist.", 
    x = NULL,
    y = "GDP per capita (log scale)",
    col = NULL
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.3, color = "#DAD9D9"),
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(),
    axis.line.x = element_line(color = "black", size = 0.3),
    axis.ticks.x = element_line(color = "black", size = 0.3), 
    axis.ticks.length.x = unit(2, "mm"), 
    axis.title = element_text(family = "Fira Sans Condensed Medium"),
    # axis.text.y = element_text(),
    axis.text.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_markdown(),
    plot.title = element_markdown(
      face = "bold", margin = margin(t = 16, b = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(b = 8), lineheight = 1),
    plot.caption = element_text(hjust = 0, size = 7, 
                                family = "Fira Sans Condensed Light"),
    plot.caption.position = "plot"
  )
p


# Add theme elements using {grid} ---------------
ragg::agg_png(here(base_path, "12-economist_gdpercap.png"), res = 300, width = 6.5, height = 6, units = "in")
p
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#D13223", lwd = 2)
)
grid.rect(
  x = 0,
  y = 1,
  width = 0.2, # TODO 10 % of line / image width
  height = 0.05,  # TODO ~2 % of line / image height
  gp = gpar(fill = "#D13223", col = NA)
)
invisible(dev.off())


#' Inspiration:
#' Bar chart: https://www.economist.com/the-economist-explains/2022/02/10/how-does-america-calculate-inflation
#' Line chart: https://www.economist.com/graphic-detail/2022/03/17/russian-soldiers-appear-to-be-dying-in-ukraine-at-a-remarkably-high-rate
#' https://design-system.economist.com/foundations/typography/line-height#multipliers
