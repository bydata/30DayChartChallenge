library(tidyverse)
library(lubridate)
library(here)
library(ggdist)

base_path <- here("2022", "11")

#' Download historical weather data from 
#' until 2020: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/
#' recent: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/

# Cologne
df_raw_k <- vroom::vroom(here(base_path, 
                              c("produkt_klima_tag_19570701_20201231_02667.txt", 
                                "produkt_klima_tag_20200924_20220327_02667.txt")),
                         na = c("-999"))
colnames(df_raw_k) <- tolower(colnames(df_raw_k))



# recode dataset
data_prep <- function(df) {
  df %>% 
    # there is a certain overlap between the two data sources, remove those duplicates
    group_by(mess_datum) %>% 
    filter(rank(mess_datum) == 1) %>% 
    ungroup() %>% 
    transmute(
      date = lubridate::as_date(as.character(mess_datum)),
      sdk
    ) %>% 
    na.omit() %>% 
    mutate(month = month(date, label = TRUE),
           year = year(date),
           decade = year %/% 10 * 10)
}

df_k <- data_prep(df_raw_k)


df_k %>% 
  ggplot(aes(sdk)) +
  geom_histogram()


font_family <- "Noto Serif"
start_value_y <- -7
sun_color <- "#EAC76A"

# sun_color_palette <- c("Spring" = "#E37B31", "Summer" = "#D36E2A", 
#                        "Autumn" = "#DA9D42", "Winter" = "#CC9738")

#' Sun God Color Scheme
#' https://www.schemecolor.com/sun-god.php
#' The Sun God Color Scheme palette has 6 colors which are Sunburnt Cyclops (#FF3E4A), 
#' Smashed Pumpkin (#FD6838), Deep Saffron (#FD8F34), Maximum Yellow (#FDF13D), 
#' School Bus Yellow (#FDD900) and Orange-Yellow (#FDB725).

sun_color_palette <- c("#FF3E4A", "#FD6838", "#FD8F34", "#FDF13D", "#FDD900", "#FDB725")
sun_color <- sun_color_palette[6]
sun_color_palette_seasons <- sun_color_palette[c(4, 2, 6, 5)]
names(sun_color_palette_seasons) <- c("Spring", "Summer", "Autumn", "Winter")
  

# p_base <- df_k %>% 
#   group_by(month) %>% 
#   summarize(mean_daily_sun = mean(sdk), .groups = "drop") %>% 
#   mutate(season = case_when(
#     as.numeric(month) %in% 3:5 ~ "Spring",
#     as.numeric(month) %in% 6:8 ~ "Summer",
#     as.numeric(month) %in% 9:11 ~ "Autumn",
#     as.numeric(month) %in% c(12, 1, 2) ~ "Winter",
#   )) %>% 
#   ggplot(aes(month, mean_daily_sun)) +
#   geom_label(aes(y = 0, label = scales::number(mean_daily_sun, accuracy = 0.1)), 
#              vjust = 0.5, size = 2, nudge_y = 5, family = font_family,
#              label.size = 0, label.r = unit(0.25, "mm")) +
#   geom_hline(yintercept = 0, color = "#E37B31", size = 0.1) +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = start_value_y, ymax = 0,
#            fill = sun_color) + 
#   # geom_text(aes(label = month, y = start_value_y + 2), family = font_family, size = 3)  +
#   scale_x_discrete(position = "left") +
#   scale_y_continuous(limits = c(start_value_y, NA)) +
#   scale_color_manual(values = sun_color_palette) +
#   coord_polar(theta = "x", start = -0.2) +
#   guides(fill = "none") +
#   labs(
#     title = "Average daily sunshine hours in Cologne, Germany",
#     color = NULL
#   ) +
#   theme_void(base_family = font_family) +
#   theme(
#     plot.background = element_rect(color = NA, fill = "grey96"),
#     plot.margin = margin(10, 10, 10, 10),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "bottom"
#   )
# 
# p_base + 
#   geom_segment(aes(xend = month, y = 0, yend = mean_daily_sun,  col = season), size = 2)
# ggsave(here(base_path, "11-circular-sushine-cgn.png"), width = 6, height = 5)
# 
# 

p_base <- df_k %>% 
  mutate(season = case_when(
    as.numeric(month) %in% 3:5 ~ "Spring",
    as.numeric(month) %in% 6:8 ~ "Summer",
    as.numeric(month) %in% 9:11 ~ "Autumn",
    as.numeric(month) %in% c(12, 1, 2) ~ "Winter",
  )) %>% 
  ggplot(aes(month, sdk)) +
  geom_hline(yintercept = 0, color = "#E37B31", size = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = start_value_y, ymax = 0,
           fill = sun_color) + 
  # geom_text(aes(label = month, y = start_value_y + 2), family = font_family, size = 3)  +
  scale_x_discrete(position = "left") +
  scale_y_continuous(limits = c(start_value_y, NA)) +
  coord_polar(theta = "x", start = -0.2) +
  guides(fill = "none") +
  labs(
    title = "Daily sunshine hours in Cologne, Germany",
    color = NULL
  ) +
  theme_void(base_family = font_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "#00C0F0"), # #00B5E2
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, color = "black"),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

p_base + 
  geom_point(aes(col = season), 
             size = 0.01, alpha = 0.8, 
             position = position_jitter(width = 0.1, height = 0))  +
  scale_color_manual(values = sun_color_palette_seasons) 
ggsave(here(base_path, "11-circular-sushine-cgn-dots.png"), width = 6, height = 5)


# Custom summary function for stat_summary, returning a data frame with relevant information
summary_func <- function(x, y_position = 17) {
  mean_val <- mean(x, na.rm = TRUE)
  data.frame(
    label = paste(scales::number(mean_val, accuracy = 0.1), "h"),
    y = y_position
  )
}

geom_interval_size <- 2.5

# version with stat_gradient_interval
p_base + 
  stat_interval(color = "white", alpha = 0.5, size = geom_interval_size) +
  stat_gradientinterval(aes(alpha = after_stat(level)), 
              fill = colorspace::desaturate(sun_color, 0.3), size = geom_interval_size, stroke = 0.5,
              fill_type = "gradient",
              shape = 21, color = sun_color) +
  stat_summary(geom = "label", 
               fun.data = summary_func,
               vjust = 0.5, size = 2, fill = alpha("white", 0.8),
               family = font_family, label.size = 0, label.r = unit(0.4, "mm")) +
  annotate("text", 
           x = 1:12, y = 21, label = month.abb, color = "white", alpha = 0.5, 
           size = 2, family = font_family)
ggsave(here(base_path, "11-circular-sushine-cgn-intervals1.png"), width = 6, height = 5)

# version with stat_interval
p_base + 
  stat_interval(color = "white", alpha = 0.5, size = geom_interval_size) +
  stat_interval(aes(alpha = after_stat(level)), 
                        size = geom_interval_size, color = sun_color) +
  stat_summary(geom = "label", 
               fun.data = summary_func,
               vjust = 0.5, size = 2, fill = alpha("white", 0.8),
               family = font_family, label.size = 0, label.r = unit(0.4, "mm")) +
  # stat_summary(geom = "point", color = "grey20", fun = mean, size = 0.5) +
  annotate("text", 
           x = 1:12, y = 21, label = month.abb, color = "white", alpha = 0.5, 
           size = 2, family = font_family)
ggsave(here(base_path, "11-circular-sushine-cgn-intervals2.png"), width = 6, height = 5)



# https://de.weatherspark.com/y/54495/Durchschnittswetter-in-K%C3%B6ln-Deutschland-das-ganze-Jahr-%C3%BCber

