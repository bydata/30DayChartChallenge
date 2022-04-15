library(tidyverse)
library(ggtext)
library(here)
library(grid)

base_path <- here("2022", "15")

tuesdata <- tidytuesdayR::tt_load('2022-04-12')
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)

# join these dataframes by Entity and Year
# fuel_gdp, indoor_pollution, death_timeseries

df <- tuesdata$fuel_gdp %>% 
  inner_join(tuesdata$indoor_pollution, by = c("Entity", "Code", "Year")) %>% 
  inner_join(tuesdata$death_source, by = c("Entity", "Code", "Year")) %>% 
  filter(!is.na(Code)) %>% 
  select(-Continent) %>% 
  mutate(continent = countrycode::countrycode(Code, origin = "iso3c", destination = "continent"))

# rename columns and drop duplicate column
colnames(df) 
colnames(df) <- c("country", "code", "year", "access_clean_fuels", "gdp_per_cap",
                  "population", "deaths_household_air_pollution", "X1", "continent")
df$X1 <- NULL


## PARALLEL COORDINATES --------------------------------------------

variable_names <- c("gdp_per_cap" = "GDP per capita<br>(US-$, log)", 
                    "access_clean_fuels" = "Access to<br>clean fuels", 
                    "deaths_household_air_pollution" = "Share of deaths due to<br>indoor air pollution")

df_filtered <- df %>% 
  filter(continent != "Oceania") %>% 
  select(-population) %>% 
  na.omit() %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  mutate(gdp_per_cap = log(gdp_per_cap))

# Correlation between the 3 variables
cor(df_filtered[c(
  "access_clean_fuels",
  "gdp_per_cap",
  "deaths_household_air_pollution"
)])

unique(df_filtered$continent)

df_filtered %>% 
  select(continent, access_clean_fuels, gdp_per_cap, deaths_household_air_pollution) %>% 
  group_split(continent, .keep = FALSE) %>% 
  map(cor) %>% 
  set_names(unique(df_filtered$continent))

# Calculate the values based on share of the min-max range
get_min_max_breaks <- function(x, breaks = seq(0, 1, 0.25), accuracy = c(1, 0.1, 0.01)) {
  min_val <- min(x)
  max_val <- max(x)
  range <- max_val - min_val
  # break_vals <- round(breaks * range, log10(accuracy^-1))
  break_vals <- round(breaks * range, 0)
  break_vals
}

# Create the break labels 
breaks_df <-
  map_dfr(
    c(
      "access_clean_fuels",
      "gdp_per_cap",
      "deaths_household_air_pollution"
    ),
    ~ data.frame(
      variable = .x,
      # "access_clean_fuels",
      step = seq(0, 1, 0.25),
      value = get_min_max_breaks(df_filtered[.x])
    )
  ) %>%
  mutate(
    variable = variable_names[variable],
    variable = factor(variable, levels = variable_names),
    value = case_when(
      variable %in% variable_names[c("access_clean_fuels", "deaths_household_air_pollution")] ~
        paste0(value, "%"),
      TRUE ~ scales::number(value, accuracy = 1, big.mark = ",")
    )
  )

# Annotations
plot_titles <- list(
  title = "Indoor air pollution - \"the world's largest single environmental health risk\"*",
  subtitle = "
  Indoor air pollution is caused by burning solid fuel sources 
  (e.g. firewood, crop waste, dung) for cooking and heating.
  The burning of such fuels, results in air pollution that leads to respiratory 
  diseases which can result in premature death. **Income** is a strong driver for the 
  **access to cleaner methods** (natural gas, ethanol, electric).<br><br>
  <i style='color:grey40'>The purpose of the chart is to detect patterns and similarities: 
  Each line connecting the parallel axis represents a country.
  Many parallel lines indicate a positive relationship, lots of crossing lines 
  indicate a negative correlation.
  Randomly directed lines indicate the absence of a (linear) relationship.</i>
  ",
  caption = "\\* WHO 2014. Data from 2016. Source: Our World in Data, #TidyTuesday | Visualization: Ansgar Wolsing"
)


df_filtered %>% 
  mutate(
    across(-c(country, code, year, continent), function(x) (x - min(x)) / (max(x) - min(x)))
  )  %>% 
  pivot_longer(cols = -c(country, code, year, continent), names_to = "variable") %>% 
  mutate(variable = variable_names[variable],
         variable = factor(variable, levels = variable_names)) %>% 
  ggplot(aes(variable, value, group = country)) +
  # create separate "pseudo" y-axes
  geom_vline(aes(xintercept = variable), color = "grey40", size = 0.8) +
  geom_text(
    data = breaks_df,
    aes(variable, step, label = value),
    inherit.aes = FALSE, color = "grey30",
    size = 2.5, family = "Roboto Slab", hjust = 1, vjust = 0, nudge_x = -0.05
  ) +
  geom_text(
    data = breaks_df,
    aes(variable, step, label = "-"),
    inherit.aes = FALSE,
    size = 4, family = "Roboto Slab", hjust = 1, color = "grey40"
  ) +
  # country lines
  geom_line(size = 0.2, alpha = 0.9, col = "#3295a8") +
  geom_point(size = 0.2, col = "#3295a8") +
  scale_x_discrete(position = "bottom") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  facet_wrap(vars(continent), scales = "free_x") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
  ) +
  # ggthemes::theme_fivethirtyeight(base_family = "Fira Sans Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    axis.text = element_text(),
    # axis.text.y = element_markdown(family = "Roboto Slab"),
    axis.text.x.bottom = element_markdown(
      hjust = 0.5, halign = 0.5, size = 8, family = "Fira Sans Condensed SemiBold"),
    strip.text = element_text(size = 10, family = "Fira Sans Condensed SemiBold"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.y = unit(8, "mm"),
    text = element_text(color = "grey19", family = "Fira Sans Condensed"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.subtitle = element_textbox_simple(
      width = 0.9, size = 9, hjust = 0, margin = margin(t = 4, b = 8)
    ),
    plot.caption = element_markdown(size = 7, hjust = 0, color = "grey25",
    margin = margin(t = 16))
  )
ggsave(here(base_path, "15-parallel-coordinates.png"), width = 9, height = 6)  



# Scatterplots --------

df_filtered %>% 
  ggplot(aes(gdp_per_cap, access_clean_fuels)) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_wrap(vars(continent))

df_filtered %>% 
  ggplot(aes(access_clean_fuels, deaths_household_air_pollution)) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_wrap(vars(continent))
