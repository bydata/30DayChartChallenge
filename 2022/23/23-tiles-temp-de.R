library(tidyverse)
library(lubridate)
library(here)
library(ggtext)

base_path <- here("2022", "23")

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
    mutate(month_name = fct_inorder(month.name[month]),
           month_abb = fct_inorder(month.abb[month]))
}

temperature <- prepare_data(temperature_raw)
precipitation <- prepare_data(precipitation_raw)


df <- temperature %>% 
  inner_join(precipitation, by = c("year", "month", "territory", "month_name", "month_abb"), 
             suffix = c(".temp", ".precip")) %>% 
  mutate(season = case_when(
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Autumn",
    month %in% c(12, 1, 2) ~ "Winter",
  ),
  season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))) 


# Custom Red-Blue color palette
pal <- c(rev(RColorBrewer::brewer.pal(7, "Blues")), "white", RColorBrewer::brewer.pal(7, "Reds"))

# calculate baseline temperature per month
# see https://showyourstripes.info/c/europe/germany/all
baseline_temp_month <- df %>% 
  filter(territory == "Deutschland", year >= 1881 & year <= 1910) %>% 
  group_by(month) %>% 
  summarize(mean = mean(avg.temp)) %>% 
  pull(mean)

df %>% 
  filter(territory == "Deutschland") %>% 
  mutate(month_abb = fct_rev(month_abb),
         avg.temp.deviation = avg.temp - baseline_temp_month[month]) %>% 
  filter(year < 2022) %>% 
  ggplot(aes(year, month_abb)) +
  geom_tile(aes(fill = avg.temp.deviation), col = "white") +
  scale_x_continuous(position = "top", expand = c(0, add = 1),
                     breaks = seq(1880, 2030, 10)) +
  scale_y_discrete() +
  # scale_fill_distiller(palette = "RdBu") +
  scale_fill_gradient2(high = pal[length(pal)], mid = "white",
                       low = pal[1], midpoint = 0) +
  # scale_fill_stepsn(colours = pal, n.breaks = length(pal)) +
  guides(
    fill = guide_colorbar(title.position = "top", title.hjust = 0.5)
  ) +
  labs(
    title = glue::glue("Average monthly temperature in Germany 1881 to 2021"),
    subtitle = "Each month since 1881 is represented in a tile. 
    Its color indicates by how much it deviates from the baseline temperature for this month. 
    The baseline is calculated as the average temperature of the years 1881 to 1910.",
    caption = "Baseline: 1881-1910. **Source:** DWD CDC | **Visualization:** Ansgar Wolsing",
    y = NULL,
    fill = "Deviation from baseline<br>for the respective month (°C)"
  ) +
  theme_void(base_family = "Avenir") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey19", size = 0.2),
    text = element_text(color = "grey89"),
    plot.title = element_text(size = 24, color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_textbox(
      hjust = 0.5, halign = 0, width = 0.7, margin = margin(t = 6, b = 12),
      lineheight = 1.1),
    plot.caption = element_markdown(hjust = 0.5),
    panel.background = element_rect(color = NA, fill = "grey12"),
    axis.text = element_text(size = 8, face = "bold", color = "grey79"),
    axis.text.y = element_text(hjust = 0, margin = margin(r = 2)),
    axis.title.y = element_text(angle = 90),
    plot.margin = margin(6, 6, 6, 6),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_markdown(hjust = 0),
    legend.text.align = 0.5,
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(3, "mm")
  )
ggsave(here(base_path, "23-tiles-temp-de.png"), width = 12, height = 5)
