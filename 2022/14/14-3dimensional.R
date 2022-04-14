library(tidyverse)
library(ggtext)
library(here)
library(grid)

base_path <- here("2022", "14")

# Source: https://ourworldindata.org/time-use#do-workers-in-richer-countries-work-longer-hours

df_raw <- read_csv(here(base_path, "annual-working-hours-vs-gdp-per-capita-pwt.csv"))

df <- df_raw %>% 
  rename(country = Entity, code = Code, year = Year, working_hours = 4, gdp_per_cap = 5,
         population = 6) %>% 
  select(-Continent) %>% 
  mutate(continent = countrycode::countrycode(code, origin = "iso3c", destination = "continent")) %>% 
  na.omit() %>% 
  filter(year == max(year))

# Correlation between working hours and GDP
cor(df$working_hours, df$gdp_per_cap)
corr_log <- cor(df$working_hours, log10(df$gdp_per_cap))


# Number of countries per continent
count(df, continent)

with(subset(df, continent == "Europe"), cor(working_hours, log10(gdp_per_cap)))
with(subset(df, continent == "Asia"), cor(working_hours, log10(gdp_per_cap)))
with(subset(df, continent == "Americas"), cor(working_hours, log10(gdp_per_cap)))

relevant_continents <- c("Americas", "Asia", "Europe")

continent_correlations <- df %>% 
  filter(continent %in% relevant_continents) %>% 
  group_by(continent) %>% 
  summarize(corr = cor(working_hours, log10(gdp_per_cap)),
            min_gdp = min(gdp_per_cap))

highlight_countries <- c(
  "Ireland", "Germany", "Greece", "Bulgaria", "Hungary", "France",
  "United States", "Mexico", "Argentina", "Brazil", "Uruguay",
  "Cambodia", "Singapore", "India", "China", "Japan", "Turkey", "Sri Lanka")


ragg::agg_png(here(base_path, "14-3dimensional.png"), res = 300, width = 8, height = 6, units = "in")
df %>% 
  filter(continent %in% relevant_continents) %>% 
  # add correlation coefficients per continent
  inner_join(continent_correlations, by = "continent") %>% 
  ggplot(aes(gdp_per_cap, working_hours)) +
  # geom_smooth(method = lm, col = "#141F52", fill = "grey72", size = 1.5,
  #             fullrange = TRUE) +
  geom_smooth(aes(size = abs(corr)), method = lm, col = "#141F52", fill = "grey72",
              fullrange = TRUE, show.legend = FALSE) +
  geom_point(aes(fill = continent),
             shape = 21, size = 3, color = "white", alpha = 0.7) +
  ggrepel::geom_text_repel(
    data = subset(df, country %in% highlight_countries),
    aes(label = country),
    family = "Fira Sans Condensed Light", size = 2.5, min.segment.length = 0,
    segment.size = 0.2
  ) +
  geom_label(
    data = continent_correlations,
    aes(x = min_gdp, y = 1400, label = paste("r =", scales::number(corr, accuracy = 0.01))),
    fill = alpha("grey12", 0.8), color = "white", hjust = 0,
    family = "Fira Sans Condensed", size = 3) +
  scale_x_log10() +
  scale_size_area(max_size = 1.5) +
  scale_fill_manual(values = c(
    "Asia" = "#E2365B", "Americas" = "#2E45B8", "Europe" = "#36E2BD")) + 
  facet_wrap(vars(factor(continent, levels = c("Asia", "Americas", "Europe"))), scales = "free_x") +
  guides(fill = "none") +
  labs(
    title = "Workers in poorer countries tend to work more",
    subtitle = "But the relationship between GDP per capita and annual average working 
    hours is stronger in Europe than in Asia and very weak in the Americas.<br>
    Trend lines indicate the direction, the thickness indicates the strength of the relationship.<br>
    *Note: Different GDP scales (x-axis) between continents*",
    caption = "Source: Our World in Data, Feenstra et al. (2015). Theme: The Economist.
    Visualization: Ansgar Wolsing",
    x = "GDP per capita (adjusted US-$, log scale)",
    y = "Average annual working hours per worker",
    fill = NULL
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
    strip.text = element_text(family = "Fira Sans Condensed Medium", size = 11),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_markdown(),
    plot.title = element_markdown(
      face = "bold", margin = margin(t = 16, b = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(margin = margin(t = 4, b = 8), lineheight = 1),
    plot.caption = element_markdown(
      hjust = 0, size = 7, family = "Fira Sans Condensed Light"),
    plot.caption.position = "plot"
  )
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#D13223", lwd = 2)
)
grid.rect(
  x = 0,
  y = 1,
  width = 0.2,
  height = 0.05,
  gp = gpar(fill = "#D13223", col = NA)
)
invisible(dev.off())
