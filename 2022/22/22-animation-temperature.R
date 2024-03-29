library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(gganimate)

#' https://commons.wikimedia.org/wiki/File:20171231_Climate_spiral_(HadCRUT4.6_1850-_)_Ed_Hawkins.gif

base_path <- here("2022", "22")

#' Download historical weather data (DWD) 
data_urls <- paste0(
  "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly/air_temperature_mean/regional_averages_tm_",
  str_pad(1:12, 2, "left", "0"),
  ".txt")
df_raw <- read_delim(data_urls, delim = ";", trim_ws = TRUE,
                     locale = locale(decimal_mark = "."), skip = 1, id = NULL)


df_long <- df_raw %>% 
  mutate(Monat = as.numeric(str_remove(Monat, "^0"))) %>% 
  select(-`...20`) %>% 
  rename(year = 1, month = Monat) %>% 
  pivot_longer(cols = -c(year, month), names_to = "territory", values_to = "avg_temp") %>% 
  filter(territory == "Deutschland") %>% 
  # exclude 2022
  filter(year < 2022) %>% 
  # calculate avg. temperature for each year
  group_by(year) %>% 
  mutate(annual_avg_temp = mean(cur_data()$avg_temp)) %>% 
  ungroup() %>% 
  arrange(year, month) %>% 
  mutate(month_name = fct_inorder(month.name[month]))

first_year <- min(df_long$year)
last_year <- max(df_long$year)

# Baseline temperature
baseline_temp <- df_long %>% 
  filter(year >= 1881, year <= 1910) %>% 
  summarize(baseline_temp = mean(avg_temp)) %>% 
  pull(baseline_temp)


p <- df_long %>% 
  ggplot(aes(month_name, avg_temp, group = year, col = annual_avg_temp)) +
  geom_line(size = 0.6, alpha = 1) +
  # geom_text(data = . %>% filter(month == 1), 
  #           aes(label = year),
  #           hjust = 1, family = "Fira Sans", fontface = "bold",
  #           nudge_y = 2) +
  geom_text(data = . %>% filter(month == 1), 
            aes(x = 0.5, y = 2, label = year),
            hjust = 0.5, family = "Fira Sans", fontface = "bold",
            nudge_y = 2) +
  scale_color_gradient2(low = "blue", mid = "grey95", high = "red", midpoint = baseline_temp) +
  coord_polar() +
  guides(col = guide_colorbar(title.position = "top")) +
  labs(
    title = glue::glue("Average monthly temperature in Germany {first_year} to {last_year}"),
    caption = "Baseline: 1881-1910 **Source:** DWD CDC | **Visualization:** Ansgar Wolsing",
    y = "Avg. monthly temperature (°C)",
    col = "Avg. annual temperature (°C)"
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey1"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey30", size = 0.2),
    text = element_text(color = "grey89"),
    plot.subtitle = element_text(size = 20),
    legend.text = element_text(color = "grey87"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.7, color = "grey60"),
    axis.text = element_text(color = "grey40"),
    plot.title = element_markdown(color = "grey99", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.key.height = unit(2, "mm"),
    legend.key.width = unit(12, "mm"),
    legend.title.align = 0.5,
    axis.ticks.y = element_line(size = 0.1),
  ) 


## Version with deviations from baseline instead of absolute temperatures ------

# Baseline temperature (1881-1910)
baseline_temp_month <- df_long %>% 
  filter(year >= 1881, year <= 1910) %>% 
  group_by(month) %>% 
  summarize(baseline_temp = mean(avg_temp)) %>% 
  pull(baseline_temp)

p2 <- df_long %>% 
  mutate(avg_temp_deviation = avg_temp - baseline_temp_month[month]) %>% 
  ggplot(aes(month_name, avg_temp_deviation, group = year, col = annual_avg_temp)) +
  # geom_hline(yintercept = c(0, 1, 2), color = "green") +
  geom_line(size = 0.6, alpha = 1) +
  geom_text(data = . %>% filter(month == 1), 
            aes(x = 0.5, y = 2, label = year),
            hjust = 0.5, family = "Fira Sans", fontface = "bold",
            nudge_y = 2) +
  scale_color_gradient2(low = "blue", mid = "grey95", high = "red", midpoint = baseline_temp) +
  coord_polar() +
  guides(col = guide_colorbar(title.position = "top")) +
  labs(
    title = glue::glue("Average monthly temperature in Germany {first_year} to {last_year}"),
    caption = "Baseline: 1951-1980. **Source:** DWD CDC | **Visualization:** Ansgar Wolsing",
    y = "Avg. monthly temperature (°C)",
    col = "Avg. annual temperature  (°C)"
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey1"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey30", size = 0.2),
    text = element_text(color = "grey89"),
    plot.subtitle = element_text(size = 20),
    legend.text = element_text(color = "grey87"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0.7, color = "grey60"),
    axis.text = element_text(color = "grey40"),
    plot.title = element_markdown(color = "grey99", hjust = 0.5),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.key.height = unit(2, "mm"),
    legend.key.width = unit(12, "mm"),
    legend.title.align = 0.5,
    axis.ticks.y = element_line(size = 0.1),
  ) 


## Animation -------------------------------------------------------------------

p_anim <- p +
  transition_time(year) +
  shadow_mark(size = size / 5, alpha = 0.75 * alpha, exclude_layer = 2) # exclude geom_text

animate(p_anim, res = 200, width = 1000, height = 1200,
        fps = 6, end_pause = 20, bg = "grey1")
anim_save(here(base_path, "22-temperature-de.gif"))

