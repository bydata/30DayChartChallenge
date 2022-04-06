library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(ggdist)

base_path <- here("2022", "07")

#' Download historical weather data (DWD) 

# Air temperature
data_urls_temp <- paste0(
  "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/air_temperature_mean/regional_averages_tm_",
  str_pad(1:12, 2, "left", "0"),
  ".txt")
temperature_raw <- read_delim(data_urls_temp, delim = ";", trim_ws = TRUE,
                     locale = locale(decimal_mark = "."), skip = 1, id = NULL)

# Precipitation
data_urls_precipitation <- paste0(
  "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/precipitation/regional_averages_rr_",
  str_pad(1:12, 2, "left", "0"),
  ".txt")
precipitation_raw <- read_delim(data_urls_precipitation, delim = ";", trim_ws = TRUE,
                       locale = locale(decimal_mark = "."), skip = 1, id = NULL)


prepare_data <- function(df) {
  df %>% 
    mutate(Monat = as.numeric(str_remove(Monat, "^0"))) %>% 
    select(-`...20`) %>% 
    rename(year = 1, month = Monat) %>% 
    pivot_longer(cols = -c(year, month), names_to = "territory", values_to = "avg") %>% 
    # calculate avg. value for each year
    group_by(year) %>% 
    mutate(annual_avg = mean(cur_data()$avg)) %>% 
    ungroup() %>% 
    arrange(year, month) %>% 
    mutate(month_name = fct_inorder(month.name[month]))
}

temperature <- prepare_data(temperature_raw)
precipitation <- prepare_data(precipitation_raw)


df <- temperature %>% 
  inner_join(precipitation, by = c("year", "month", "territory", "month_name"), 
             suffix = c(".temp", ".precip")) %>% 
  mutate(season = case_when(
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Autumn",
    month %in% c(12, 1, 2) ~ "Winter",
  ),
  season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
  period = ifelse(year >= 1990, "1990 and later", "before 1990")) 

df %>% 
  filter(territory == "Brandenburg/Berlin") %>% 
  ggplot(aes(avg.temp, avg.precip)) +
  geom_point(aes(col = period), alpha = 0.7, size = 0.3) +
  facet_grid(cols = vars(season), rows = vars(period)) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

library(ggridges)

temperature %>% 
  bind_rows(precipitation, .id = "measure") %>% 
  mutate(measure = ifelse(measure == 1, "temperature", "precipitation"),
         season = case_when(
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Autumn",
    month %in% c(12, 1, 2) ~ "Winter",
  ),
  season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
  period = ifelse(year >= 1990, "1990 and later", "before 1990")) %>% 
  filter(territory == "Deutschland") %>% 
  ggplot(aes(avg, season)) +
  geom_density_ridges(aes(fill = period), alpha = 0.5) +
  facet_wrap(vars(measure), scales = "free_x") +
  theme_ridges()
  

temperature %>% 
  bind_rows(precipitation, .id = "measure") %>% 
  mutate(measure = ifelse(measure == 1, "temperature", "precipitation"),
         season = case_when(
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Autumn",
           month %in% c(12, 1, 2) ~ "Winter",
         ),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         period = ifelse(year >= 1990, "1990 and later", "before 1990")) %>% 
  filter(territory == "Nordrhein-Westfalen") %>% 
  ggplot(aes(period, avg)) +
  stat_dotsinterval(aes(fill = period), alpha = 0.4, p_limits = c(0.05, 0.95)) +
  coord_flip() +
  facet_grid(cols = vars(measure), rows = vars(season), scales = "free_x") +
  theme_bw()


temperature %>% 
  bind_rows(precipitation, .id = "measure") %>% 
  mutate(measure = ifelse(measure == 1, "temperature", "precipitation"),
         season = case_when(
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Autumn",
           month %in% c(12, 1, 2) ~ "Winter",
         ),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         period = ifelse(year >= 1990, "1990 and later", "before 1990")) %>% 
  filter(territory == "Deutschland") %>% 
  group_by(season, measure, period) %>% 
  summarize(avg = mean(avg))


temperature %>% 
  bind_rows(precipitation, .id = "measure") %>% 
  mutate(measure = ifelse(measure == 1, "temperature", "precipitation"),
         season = case_when(
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Autumn",
           month %in% c(12, 1, 2) ~ "Winter",
         ),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         period = ifelse(year >= 1990, "1990 and later", "before 1990")) %>% 
  filter(territory == "Deutschland", season == "Winter", measure == "temperature") %>% 
  t.test(avg ~ period, data = .)



foo <- temperature %>% 
  bind_rows(precipitation, .id = "measure") %>% 
  mutate(measure = ifelse(measure == 1, "temperature", "precipitation"),
         season = case_when(
           month %in% 3:5 ~ "Spring",
           month %in% 6:8 ~ "Summer",
           month %in% 9:11 ~ "Autumn",
           month %in% c(12, 1, 2) ~ "Winter",
         ),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
         period = ifelse(year >= 1990, "1990 and later", "before 1990")) %>% 
  filter(territory == "Deutschland") 

means_before_1990 <- foo %>% 
  filter(period == "before 1990") %>% 
  group_by(season, measure) %>% 
  summarize(avg_period = mean(avg), .groups = "drop")
means_before_1990  

kelvin_zero <- 273.15

foo %>% 
  filter(year < 2022) %>% 
  inner_join(means_before_1990, by = c("season", "measure")) %>% 
  group_by(measure, year, season) %>% 
  summarize(avg_season = mean(avg), avg_period = first(avg_period), .groups = "drop") %>% 
  group_by(measure) %>% 
  mutate(diff_avg = avg_season - avg_period,
         diff_avg_rel = diff_avg / max(diff_avg)) %>% 
  ungroup() %>% 
  ggplot(aes(year, diff_avg)) +
  # geom_vline(xintercept = 1990, col = "grey70", lty = "dotted") + 
  annotate("rect", xmin = 1990, xmax = 2022, ymin = -Inf, ymax = Inf,
           fill = "grey24") +
  geom_col(aes(fill = diff_avg_rel)) +
  scale_y_continuous(position = "right", expand = expansion(mult = c(0.1, 0.1))) +
  # paletteer::scale_fill_paletteer_c("ggthemes::Orange-Blue Diverging", direction = -1) +
  paletteer::scale_fill_paletteer_c("ggthemes::Gold-Purple Diverging") +
  facet_grid(rows = vars(measure), cols = vars(season), 
             labeller = as_labeller(toupper),
             scales = "free_y",
             switch = "y") +
  guides(fill = "none") +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = NULL, y = "Absolute deviation from baseline"
  ) +
  theme_void(base_family = "Avenir") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey4", size = 0.2),
    text = element_text(color = "grey89"),
    strip.text = element_text(face = "bold",
                              margin = margin(t = 4, b = 4, l = 4, r = 4)),
    strip.text.y = element_text(angle = 90),
    strip.background = element_rect(color = NA, fill = alpha("grey20", 0.8)),
    panel.background = element_rect(color = NA, fill = "grey12"),
    axis.text.y.right = element_text(size = 6),
    axis.title.y.right = element_text(angle = 90),
    plot.margin = margin(6, 6, 6, 6)
  )
ggsave(here(base_path, "07-physical-weather-de.png"), width = 8, height = 6)
