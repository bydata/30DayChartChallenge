library(tidyverse)
library(grid)
library(ggtext)
library(here)
library(gapminder)
library(ggdist)


base_path <- here("2022", "12")

data(gapminder)
glimpse(gapminder)

gap_2007 <- gapminder %>% 
  filter(year == 2007)

p <- gap_2007 %>% 
  mutate(country = fct_reorder(country, -pop),
         continent = factor(continent, levels = rev(levels(continent)))) %>% 
  ggplot(aes(continent, gdpPercap, fill = continent)) +
  ggbeeswarm::geom_beeswarm(aes(size = pop), 
                            cex = 3.1,
                            shape = 21, stroke = 0.5,
                            col = "white",
                            show.legend = FALSE) +
  scale_y_log10() +
  scale_size_area(max_size = 18) +
  #' Colors:
  # Hong Kong 55 	#36E2BD
  # Chicago 30  #1F2E7A
  # Chicago 20  	#141F52
  # London 5   	#F97A1F
  # Tokyo 55 	#E2365B
  scale_fill_manual(values = c(
    "Europe" = "#36E2BD", "Oceania" = "#141F52", "Asia" = "#E2365B", 
    "Africa" = "#1F2E7A", "Americas" = "#F97A1F")) + 
  coord_flip()  +
  labs(
    title = "GDP per capita varies between and within continents",
    subtitle = "Logarithmic scale",
    caption = "Source: Gapminder. Theme: The Economist.", 
    x = NULL,
    y = "GDP per capita (log scale)",
    col = NULL
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, color = "#DAD9D9"),
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(),
    axis.line.x = element_line(color = "black", size = 0.3),
    axis.ticks.x = element_line(color = "black", size = 0.3), 
    axis.ticks.length.x = unit(2, "mm"), 
    axis.title = element_text(family = "Fira Sans Condensed Medium"),
    axis.text.y = element_text(),
    legend.position = "top",
    legend.justification = "left",
    legend.text = ggtext::element_markdown(),
    plot.title = ggtext::element_markdown(face = "bold", 
                                          margin = margin(t = 16, b = 4)),
    plot.title.position = "plot"
  )
p

unique(gap_2007$country[gap_2007$continent == "Europe"])

# Add theme elements using {grid} ---------------

ragg::agg_png(here(base_path, "economist_gdpercap.png"), res = 300, width = 6, height = 6, units = "in")
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