library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(gganimate)

base_path <- here("2022", "22")

#' Download historical weather data from 
#' https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/

df_raw <- read_csv2(here(base_path, "produkt_klima_tag_19570701_20201231_02667.txt"))
colnames(df_raw) <- tolower(colnames(df_raw))

precipitation <- df_raw %>% 
  transmute(
    date = lubridate::as_date(as.character(mess_datum)),
    rsk = as.numeric(rsk),
    rsk = na_if(rsk, -999)
  ) %>% 
  na.omit() 

temperature <- df_raw %>% 
  transmute(
    date = lubridate::as_date(as.character(mess_datum)),
    # Tagesmaximum der Lufttemperatur in 2m Höhe	
    txk = as.numeric(txk),
    txk = na_if(txk, -999),
    # Tagesmittelwert der Lufttemperatur in 2m Höhe
    tmk = as.numeric(txk),
    tmk = na_if(tmk, -999)
  ) %>% 
  na.omit() 


precipitation %>% 
  ggplot(aes(rsk)) +
  geom_histogram()

precipitation %>% 
  filter(rsk > 0) %>% 
  ggplot(aes(rsk)) +
  geom_histogram()

precipitation %>% 
  mutate(day_of_year = yday(date)) %>% 
  ggplot(aes(day_of_year, rsk)) +
  geom_point(alpha = 0.5, size = 0.4)


temperature_plot_df <- temperature %>% 
  mutate(day_of_year = yday(date),
         year = year(date)) %>% 
  filter(year > min(year)) %>% 
  group_by(year) %>% 
  mutate(tmk_year = mean(cur_data()$tmk)) %>% 
  ungroup()

# average temperature for 1961-1990
avg_tmp_30yrs <- mean(temperature_plot_df$tmk[temperature_plot_df$year >= 1961 & temperature_plot_df$year <= 1990])

temperature_plot_df %>% 
  ggplot(aes(day_of_year, tmk, group = year, col = tmk_year)) +
  geom_line(size = 0.6, alpha = 1) +
  # geom_smooth(se = FALSE, span = 0.2,
  #             size = 0.4, alpha = 0.8) +
  # geom_text(data = . %>% filter(day_of_year == 1),
  #           aes(y = 40, label = year),
  #           col = "grey50", size = 12, family = "Fira Sans", fontface = "bold",
  #           hjust = 0) + 
  scale_color_gradient2(low = "blue", mid = "grey80", high = "red", midpoint = avg_tmp_30yrs) +
  labs(
    subtitle = "{closest_state}"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = "grey1"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey30", size = 0.2),
    text = element_text(color = "grey93"),
    plot.subtitle = element_text(size = 20),
    legend.text = element_text(color = "grey87")
  ) +
  transition_states(year) +
  shadow_mark(size = size / 5, alpha = 0.75 * alpha)
