library(tidyverse)
library(ggtext)
library(here)
library(grid)

base_path <- here("2022", "13")

# Source: https://ourworldindata.org/time-use#do-workers-in-richer-countries-work-longer-hours

df_raw <- read_csv(here(base_path, "annual-working-hours-vs-gdp-per-capita-pwt.csv"))

df <- df_raw %>% 
  rename(country = Entity, code = Code, year = Year, working_hours = 4, gdp_per_cap = 5,
         population = 6) %>% 
  select(-Continent) %>% 
  mutate(continent = countrycode::countrycode(code, origin = "iso3c", destination = "continent")) %>% 
  na.omit() %>% 
  # group_by(country) %>% 
  filter(year == max(year)) # %>% 
  # ungroup()


cor(df$working_hours, df$gdp_per_cap)
corr_log <- cor(df$working_hours, log10(df$gdp_per_cap))

with(subset(df, continent == "Europe"), cor(working_hours, log10(gdp_per_cap)))
with(subset(df, continent == "Asia"), cor(working_hours, log10(gdp_per_cap)))
with(subset(df, continent == "Americas"), cor(working_hours, log10(gdp_per_cap)))

highlight_countries <- c("Cambodia", "Germany", "United States", "South Africa",
                         "Singapore", "Mexico", "Argentina", "Brazil", "India",
                         "China", "Nigeria")

ragg::agg_png(here(base_path, "13-correlation.png"), res = 300, width = 6.5, height = 6, units = "in")
df %>% 
  ggplot(aes(gdp_per_cap, working_hours)) +
  geom_smooth(method = lm, col = "#141F52", fill = "grey72", size = 1.5,
              fullrange = TRUE) +
  geom_point(aes(size = population, fill = continent),
             shape = 21, color = "white", alpha = 0.7) +
  ggrepel::geom_text_repel(
    data = subset(df, country %in% highlight_countries),
    aes(label = country),
    family = "Fira Sans Condensed Light", size = 2.5, min.segment.length = 0,
    segment.size = 0.2
  ) +
  annotate("label",
           x = 4500, y = 2350, label = paste("r =", scales::number(corr_log, accuracy = 0.01)),
           fill = alpha("grey12", 0.8), color = "white",
           family = "Fira Sans Condensed", size = 3) +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 18)) +
  scale_fill_manual(values = c(
    "Africa" = "#F9C31F", "Americas" = "#F97A1F", "Asia" = "#E2365B",
    "Europe" = "#36E2BD", "Oceania" = "#2E45B8")) + 
  guides(
    size = "none",
    fill = guide_legend(override.aes = list(size = 4))
    ) +
  labs(
    title = "Workers in poorer countries tend to work more",
    subtitle = "Average working hours are 40 % higher in Cambodia than in the U.S.,
    with the U.S.'s GDP per capita 17 times larger than Cambodia's.<br>
    *Bubbles sized by population*",
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
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_markdown(),
    plot.title = element_markdown(
      face = "bold", margin = margin(t = 16, b = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(margin = margin(t = 4, b = 8), lineheight = 1),
    plot.caption = element_markdown(hjust = 0, size = 7, 
                                family = "Fira Sans Condensed Light"),
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
  width = 0.2, # TODO 10 % of line / image width
  height = 0.05,  # TODO ~2 % of line / image height
  gp = gpar(fill = "#D13223", col = NA)
)
invisible(dev.off())
