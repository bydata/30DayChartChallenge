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

# http://dwoll.de/rexrepos/posts/diagMultivariate.html
library(ellipse)
df_no_na <- select_if(df, is.numeric) %>% na.omit()
corr_mat <- cor(df_no_na)
# Relabel the correlation matrix
variable_names <- c("Year", "Access to clean fuels", "GDP per capita", 
                    "Population", "Deaths by household air pollution")
rownames(corr_mat) <- variable_names
colnames(corr_mat) <- variable_names
png(here(base_path, "15-multivariate-ellipse.png"), res = 300, width = 6, height = 6, units = "in")
plotcorr(corr_mat, type = "lower", diag = FALSE, main = "Bivariate correlations",
         col = "steelblue")
invisible(dev.off())


## PARALLEL COORDINATES --------------------------------------------

variable_names <- c("gdp_per_cap" = "GDP per capita<br>(log)", 
                    "access_clean_fuels" = "Access to<br>clean fuels", 
                    "deaths_household_air_pollution" = "Share of deaths due to<br>household air pollution")

df_filtered <- df %>% 
  filter(continent != "Oceania") %>% 
  select(-population) %>% 
  na.omit() %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  ungroup() 

# quantiles_list <- map(select(df_filtered, access_clean_fuels, gdp_per_cap, deaths_household_air_pollution),
#            quantile)
# quantiles_df <- bind_rows(quantiles_list, .id = "variable") %>% 
#   pivot_longer(-variable, names_to = "quantile")  %>% 
#   bind_cols(quantile_num = rep(seq(0, 1, 0.25), 3))


df_filtered %>% 
  summarize(across(-c(country, code, year, continent), list(min = min, max = max)))


get_min_max_breaks <- function(x, breaks = seq(0, 1, 0.25), accuracy = c(1, 0.1, 0.01)) {
  min_val <- min(x)
  max_val <- max(x)
  range <- max_val - min_val
  # break_vals <- round(breaks * range, log10(accuracy^-1))
  break_vals <- round(breaks * range, 0)
  break_vals
}

get_min_max_breaks(df_filtered$access_clean_fuels)



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
  mutate(variable = variable_names[variable],
         variable = factor(variable, levels = variable_names)) 


df_filtered %>% 
  mutate(
    gdp_per_cap = log(gdp_per_cap),
    # center + scale
    # across(-c(country, code, year, continent), function(x) (x - mean(x)) / sd(x))
    # min - max scaling
    across(-c(country, code, year, continent), function(x) (x - min(x)) / (max(x) - min(x)))
  )  %>% 
  pivot_longer(cols = -c(country, code, year, continent), names_to = "variable") %>% 
  mutate(variable = variable_names[variable],
         variable = factor(variable, levels = variable_names)) %>% 
  ggplot(aes(variable, value, group = country)) +
  geom_vline(aes(xintercept = variable), color = "grey80", size = 1) +
  geom_text(
    data = breaks_df,
    aes(variable, step, label = value),
    inherit.aes = FALSE
  ) +
  geom_line(size = 0.2, col = "#3295a8") +
  geom_point(size = 0.2, col = "#3295a8") +
  scale_x_discrete(position = "top") +
  facet_wrap(vars(continent), scales = "free_x") +
  ggthemes::theme_fivethirtyeight(base_family = "Familjen Grotesk") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    axis.text = element_text(),
    axis.text.y = element_markdown(family = "Roboto Slab"),
    axis.text.x.top = element_markdown(hjust = 0.5, halign = 0.5),
    strip.text = element_text(size = 12)
  )
ggsave(here(base_path, "15-parallel-coordinates.png"), width = 10, height = 6)  

