library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

#' Source: National Office for Statistics, UK
#' RPI: Percentage change over 12 months - Petrol and Oil incl Fuel Oil
#' Source dataset: Consumer price inflation time series (MM23)
#' https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/dogq/mm23

base_path <- here("2022", "21")


df <- read_csv(here(base_path, "series-180422.csv"), skip = 8,
               col_names = c("time_interval", "value"))

start_date <- as_date("2002-01-01")  

df_monthly <- df %>%
  # remove annual and quarterly data
  filter(!str_detect(time_interval, "^\\d{4}(\\sQ[1-4])?$")) %>% 
  transmute(date = parse_date(time_interval, format = "%Y %b"),
            value) %>% 
  filter(date >= start_date)

df_annual <- df %>%
  # remove annual and quarterly data
  filter(str_detect(time_interval, "^\\d{4}$")) %>% 
  transmute(date = parse_date(time_interval, format = "%Y"),
            value) %>% 
  filter(date >= start_date)


df_annual$index <- NA
for (i in 1:nrow(df_annual)) {
  if (i == 1) {
    df_annual$index[i] <- 100
  } else {
    df_annual$index[i] <- df_annual$index[i-1] * (100 + df_annual$value[i]) / 100
  }
}
df_annual$index <- round(df_annual$index, 1)


df_monthly$index <- NA
for (i in 1:nrow(df_monthly)) {
  if (i <= 12) {
    df_monthly$index[i] <- 100
  } else {
    df_monthly$index[i] <- df_monthly$index[i-12] * (100 + df_monthly$value[i]) / 100
  }
}
df_monthly$index <- round(df_monthly$index, 1)


lyrics <- c("The price of gas", "Keeps on rising", "Nothing comes for free")
subtitle <- paste(paste(lyrics, collapse = "<br>"),
                  "<br>*(Bloc Party, Price of Gasoline)*")
bp_color <- "#CDC4BF"
text_color <- colorspace::lighten("#3B3330", 0.3)

df_annual %>% 
  ggplot(aes(date, index)) +
  geom_line(size = 1.2, col = "#82746C") +
  annotate("richtext", label = subtitle, x = start_date, y = 190,
           hjust = 0, vjust = 1, label.size = 0, fill = "white", family = "Gill Sans", color = text_color) +
  annotate("richtext", label = "Release of Silent Alarm (2005)", x = as_date("2005-01-01"), y = 100,
           hjust = 0, vjust = 0, label.size = 0, fill = NA, family = "Gill Sans", color = text_color) +
  annotate("segment", x = as_date("2005-01-01"), xend = as_date("2005-01-01"),
           y = 103, yend = 120, size = 0.2, color = text_color) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(100, 180, 20), position = "right") +
  labs(
    title = toupper("Price of Gasoline."),
    # subtitle = subtitle,
    caption = "**Source:** Own calculations based on Percentage change over 12 months -
    *Petrol and Oil incl Fuel Oil*, National Office for Statistics, UK<br> 
    **Visualization:** Ansgar Wolsing",
    x = NULL, y = "Price index (2002 = 100)"
  ) +
  theme_minimal(base_family = "Gill Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey82", size = 0.3, linetype = "dotted"),
    text = element_text(color = text_color),
    plot.title.position = "plot",
    plot.title = element_markdown(color = bp_color, size = 20),
    plot.subtitle = element_markdown(lineheight = 1.2),
    plot.caption = element_textbox_simple(size = 7,
                                          margin = margin(t = 8, b = 2))
  )
ggsave(here(base_path, "21-price-of-gasoline.png"), width = 6, height = 4)
  

# Monthly index
df_monthly %>% 
  ggplot(aes(date, index)) +
  geom_line(size = 1.2, col = "#82746C") +
  annotate("richtext", label = subtitle, x = start_date, y = 240,
           hjust = 0, vjust = 1, label.size = 0, fill = "white", family = "Gill Sans", color = text_color) +
  annotate("richtext", label = "Release of Silent Alarm (2005)", x = as_date("2005-01-01"), y = 100,
           hjust = 0, vjust = 0.5, label.size = 0, fill = NA, family = "Gill Sans", color = text_color) +
  annotate("segment", x = as_date("2005-01-01"), xend = as_date("2005-01-01"),
           y = 103, yend = 114, size = 0.2, color = text_color) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(100, 240, 20), position = "right") +
  coord_cartesian(clip = "off") +
  labs(
    title = toupper("Price of Gasoline."),
    # subtitle = subtitle,
    caption = "**Source:** Own calculations based on Percentage change over 12 months -
    *Petrol and Oil incl Fuel Oil*, National Office for Statistics, UK<br> 
    **Visualization:** Ansgar Wolsing",
    x = NULL, y = "Price index (2002 = 100)"
  ) +
  theme_minimal(base_family = "Gill Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey82", size = 0.3, linetype = "dotted"),
    text = element_text(color = text_color),
    plot.title.position = "plot",
    plot.title = element_markdown(color = bp_color, size = 20),
    plot.subtitle = element_markdown(lineheight = 1.2),
    plot.caption = element_textbox_simple(size = 7,
                                          margin = margin(t = 8, b = 2))
  )
ggsave(here(base_path, "21-price-of-gasoline-monthly.png"), width = 6, height = 4)
