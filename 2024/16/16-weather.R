library(tidyverse)
library(ggtext)
library(jsonlite)
library(sf)
library(here)

base_path <- here("2024", "16")

## Functions to retrieve weather data  =========================================
#' Source: https://open-meteo.com/

get_weather_conditions <- function(latitude, longitude, date, timezone = "America/New_York") {
  url <- sprintf("https://archive-api.open-meteo.com/v1/era5?latitude=%s&longitude=%s&start_date=%s&end_date=%s&hourly=temperature_2m,snowfall&timezone=%s",
                 latitude, longitude, date, date, timezone)
  res_json <- read_json(url, simplifyVector = TRUE)
  res_df <- as_tibble(res_json$hourly)
  res_df
}

## NFL Schedule 2023 & Stadium locations =======================================

team_abbr <- read_tsv(here(base_path, "nfl-team-abbreviations.tsv"))
#' Stadium locations
#' Source: https://gist.github.com/rajinwonderland/80b3ac9c7dc75337594fb5e711e461a7?short_path=cd04ea5
stadiums <- st_read("https://gist.githubusercontent.com/rajinwonderland/80b3ac9c7dc75337594fb5e711e461a7/raw/6cf57ddbe377b652345a5a44bdf0c420693ef32d/stadiums.geojson")

# Correct incorrect franchise names
stadiums <- stadiums %>%
  rename(stadium_name = name1) %>%
  mutate(team = case_match(
    team,
    "Chicago Bers" ~ "Chicago Bears",
    "Washington Redskins" ~ "Washington Commanders",
    .default = team
  ))

# Reduced set of variables
colnames(stadiums)
stadiums_reduced <- stadiums %>%
  select(city, state, team, stadium_name, longitude, latitude) %>% 
  mutate(across(c(longitude, latitude), as.numeric))

crs <- st_crs(stadiums_reduced)
stadiums_updated <- stadiums_reduced %>%
  filter(!team %in% c("Oakland Raiders", "St. Louis Rams", "San Diego Chargers")) %>%
  # split the combined record for Giants and Jets
  separate_rows(team, sep = "/ ") %>%
  mutate(team = ifelse(team == "NewYork Jets", "New York Jets", team)) %>%
  add_row(
    city = "Paradise", state = "NV", team = "Las Vegas Raiders",
    stadium_name = "Allegiant Stadium",
    longitude = -115.183889, latitude = 36.090556,
    geometry = st_sfc(st_point(c(-115.183889, 36.090556)), crs = crs)
  ) %>%
  add_row(
    city = "Inglewood", state = "CA", team = "Los Angeles Rams",
    stadium_name = "SoFi Stadium",
    longitude = -118.339, latitude = 33.953,
    geometry = st_sfc(st_point(c(-118.339, 33.953)), crs = crs)
  ) %>%
  add_row(
    city = "Inglewood", state = "CA", team = "Los Angeles Chargers",
    stadium_name = "SoFi Stadium",
    longitude = -118.339, latitude = 33.953,
    geometry = st_sfc(st_point(c(-118.339, 33.953)), crs = crs)
  ) 


#' Manually download the schedule from 
#' https://www.pro-football-reference.com/years/2023/games.htm
#' Game time is Eastern (EST)
schedule <- read_csv(here(base_path, "nfl-schedule-2023-pro-football-ref.csv"))
schedule <- schedule[, 1:7]
colnames(schedule) 
colnames(schedule) <- c("Week", "Day", "Date", "Time", "Winner_or_tie",
                        "Winner_or_tie_home_away", "Loser_or_tie")

europe_games <- tribble(
  ~team_code, ~home_team, ~week, ~opponent_code, ~opponent_name, ~home_away, ~stadium_name,
  "JAX", "Jacksonville Jaguars", 4, "ATL", "Atlanta Falcons", "home", "Wembley Stadium",
  "BUF", "Buffalo Bills", 5, "JAX", "Jacksonville Jaguars", "home", "Tottenham Hotspur Stadium",
  "TEN", "Tennessee Titans", 6, "BAL", "Baltimore Ravens", "home", "Tottenham Hotspur Stadium",
  "KC", "Kansas City Chiefs", 9, "MIA", "Miami Dolphins", "home", "Waldstadion",
  "NE", "New England Patriots", 10, "IND", "Indianapolis Colts", "home", "Waldstadion")


schedule_locations <- schedule %>% 
  mutate(
    Time_hour = as.numeric(str_sub(Time, 1, 2)),
    home_team = ifelse(is.na(Winner_or_tie_home_away), Winner_or_tie, Loser_or_tie)
    ) %>% 
  # remove Europe games
  anti_join(europe_games, by = join_by(Week == week, home_team)) %>% 
  # add stadiums
  inner_join(stadiums_updated, by = join_by(home_team == team))
  
# Keep only games on Sundays 
schedule_locations_sundays <- schedule_locations %>% 
 filter(wday(Date, week_start = 1) == 7) 

schedule_weather <- schedule_locations_sundays %>% 
  rowwise() %>% 
  mutate(weather_conditions = list(get_weather_conditions(latitude, longitude, Date))) %>% 
  ungroup()

schedule_weather %>% 
   unnest(cols = weather_conditions)

schedule_weather_kickoff <- schedule_weather %>% 
  mutate(date_time = as_datetime(paste(Date, paste(Time_hour, "00", "00", sep = ":")))) %>%
  mutate(
    weather_conditions_kickoff = map2(weather_conditions, date_time, 
                                      function(x, y) subset(x, ymd_hm(time) == y))) %>% 
  select(-weather_conditions) %>% 
  unnest(cols = weather_conditions_kickoff) 

month_weeks <- schedule_weather_kickoff %>% 
  transmute(Week, year = year(Date), month = month(Date)) %>% 
  count(year, month, Week) %>% 
  group_by(year, month) %>% 
  summarize(xmin = min(Week), xmax = max(Week), .groups = "drop") %>% 
  mutate(
    xmin = lag(xmax + 0.5, 1, default = 0.5),
    xmax = lead(xmin, 1, default = max(.$xmax) + 0.5))

# Minimal and maximum temperature per week
min_max_temp_per_week <- schedule_weather_kickoff %>% 
  group_by(Week) %>% 
  summarize(
    min_temp = min(temperature_2m),
    max_temp = max(temperature_2m),
    range = max_temp - min_temp
  )


# Temperature ranges within weeks
schedule_weather_kickoff %>% 
  group_by(Week) %>% 
  summarize(
    temp_max = max(temperature_2m),
    temp_min = min(temperature_2m),
    temp_range = temp_max - temp_min) %>% 
  arrange(-temp_range) # largest range: 34.3 in week 8

# Largest range: cities
schedule_weather_kickoff %>% 
  filter(Week == 8) %>% 
  filter(temperature_2m %in% c(max(temperature_2m), min(temperature_2m))) %>% 
  select(Week, Date, city, temperature_2m)
# W8, 2023-10-29, Denver, -6.2 vs. Miami 28.1

# Highest temperature:
schedule_weather_kickoff %>%
  filter(temperature_2m == max(temperature_2m)) %>% 
  select(Week, Date, home_team, city, temperature_2m)
# W2, Arizona Cardinals, Glendale, 38.6

# Lowest temperature:
schedule_weather_kickoff %>%
  filter(temperature_2m == min(temperature_2m)) %>% 
  select(Week, Date, home_team, city, temperature_2m)
# W12, Minnesota Vikings, Minneapolis, -9.7


## Plot ========================================================================

# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title.x.top = element_markdown(size = 7, hjust = 0),
      axis.title.y = element_text(size = 7, hjust = 1),
      axis.text = element_text(family = "Source Code Pro", size = 6),
      axis.text.x.top = element_text(hjust = 0.5, vjust = -0.2),
      axis.text.y = element_text(),
      axis.line.x = element_line(linewidth = 0.15),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 7.5, width = 1, lineheight = 1.2,
        margin = margin(b = 8)),
      plot.caption = element_markdown(hjust = 0, lineheight = 1.1, size = 7),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 10, color = "grey35",
        margin = margin(t = 4, b = 1))
    )
)

# Custom annotations
annotate_text_custom <- function(x, y, label, hjust = 0, vjust = 1, ...) {
  annotate(
    "text",
    x = x, y = y, label = label,
    size = 2, color = "grey40", 
    family = "Libre Franklin", fontface = "italic",
    hjust = hjust, vjust = vjust, lineheight = 0.9
  )
}


# Gradient color for the segment
low_color <- "#0000d6"
high_color <- "#D32F2F"
# Gradient for the connecting lines
# keep x1 and x2 at zero to have the gradient ranging only on the y-dimension
gradient_segment_color <- grid::linearGradient(
  c(low_color, high_color), 
  x1 = 0, y1 = 0, x2 = 0, y2 = 1,
  group = TRUE)


schedule_weather_kickoff %>% 
  ggplot(aes(factor(Week), temperature_2m)) +
  # Annotations / shaded backgrounds for months
  annotate(
    "rect",
    xmin = month_weeks$xmin, xmax = month_weeks$xmax, 
    ymin = -Inf, ymax = Inf,
    fill = rep(c("white", "grey80"), 4)[1:nrow(month_weeks)],
    alpha = 0.2
  ) +
  annotate(
    "segment",
    x = seq(1.5, 17.5, 1), xend = seq(1.5, 17.5, 1), 
    y = -Inf, yend = Inf,
    linewidth = 0.1, color = "grey30"
  ) +
  geom_label(
    data = month_weeks,
    aes(x = xmin + (xmax - xmin) / 2, y = -12,
        label = month.abb[month]),
    family = "Libre Franklin", fontface = "bold", color = "grey60", size = 3, 
    fill = "#FEFEFE", label.size = 0
  ) +
  # Line for 0°C
  geom_hline(aes(yintercept = 0), size = 0.2, color = "grey40") +
  geom_segment(
    data = min_max_temp_per_week,
    aes(xend = factor(Week), y = 45, yend = 45 - range / 5),
    linewidth = 1.3, color = "#BDBDBD"
  ) +
  geom_text(
    data = min_max_temp_per_week,
    aes(factor(Week), y = 45, 
        label = scales::number(range, accuracy = 1, prefix = "(", suffix = "°C)")),
    family = "Source Code Pro",
    size = 1.5, color = "grey40", vjust = -0.2, hjust = 0.5
  ) +
  annotate_text_custom(
    x = 14, y = 37,
    label = "Temperature range between\nlowest and highest temperature"
  ) +
  annotate(
    GeomCurve,
    x = 13.95, xend = 13.9, y = 37, yend = 41.6,
    color = "grey40", curvature = -0.2, linewidth = 0.2
  ) +
  annotate_text_custom(
    x = 7.75, y = -5.9,
    label = "-5.9°C in Denver",
    hjust = 1
  ) +
  annotate_text_custom(
    x = 8.25, y = 31,
    label = "28.7°C in Miami",
    hjust = 0.5
  ) +
  geom_rect(
    data = min_max_temp_per_week,
    aes(
      xmin = Week - 0.033, xmax = Week + 0.033,
      ymin = min_temp,
      ymax = max_temp
    ),
    size = 1, fill = gradient_segment_color, inherit.aes = FALSE,
    linejoin = "round"
  ) +
  geom_point(
    data = min_max_temp_per_week,
    aes(y = min_temp, color = min_temp),
    size = 1.5,
  ) +
  geom_point(
    data = min_max_temp_per_week,
    aes(y = max_temp, color = max_temp),
    size = 1.5,
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(
    labels = function(x) case_when(
     x == 40 ~ paste0(x, "°C"),
      x == -10 ~ paste0(x, "°C"),
      TRUE ~ as.character(x)
    ),
    expand = expansion(add = c(3, 0)), breaks = seq(-10, 40, 10)
  ) +
  scale_color_gradient(
    low = low_color, high = high_color,
    limits = c(min(schedule_weather_kickoff$temperature_2m), 
               max(schedule_weather_kickoff$temperature_2m))) +
  scale_size_area(max_size = 6) +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(
    title = "Miami October Heat, Denver Freeze",
    subtitle = "Temperature and temperature differences at kickoff per week
    in the 2023 NFL regular season.
    The connected dots show the range from the minimum to the maximum temperature,
    the smaller bars going from top to bottom indicate the temperature difference
    between the weekly extremes.",
    caption = "Only games on Sundays are included. Data: open-meteo.com. Visualization: Ansgar Wolsing",
    x = "Week \U2192<br>
    <span style='color:grey40;font-size:5pt;'>(temperature difference in brackets)</span>",
    y = "Temperature (in °C)"
  )
ggsave(here(base_path, "16-weather.png"), width = 5, height = 4, scale = 1.2)
