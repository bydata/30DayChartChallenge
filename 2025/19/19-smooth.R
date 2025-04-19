library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2025", "19")

#' Source: U.S. Bureau of Labor Statistics
#' Manually download CSV from 
#' https://data.bls.gov/dataViewer/view/timeseries/APU0000708111

df <- read_csv(here(base_path, "egg-cpi-us.csv"), 
               name_repair = janitor::make_clean_names)

df_prep <- df |> 
  mutate(
    month = str_extract(period, "M0?(\\d{1,2})", group = 1),
    month = as.numeric(month),
    date = ymd(paste(year, month, "01", sep = "-"))
  ) |> 
  select(year, month, date, value)

# For the placement for the custom axis titles
min_x <- min(df_prep$date)
max_x <- max(df_prep$date)
min_y <- min(df_prep$value)
max_y <- max(df_prep$value)

df_prep |> 
  ggplot(aes(date, value)) +
  geom_smooth(
    method = "loess", span = 0.1,
    col = "#bc6c25") +
  geom_point(
    shape = 21, col = "grey20", fill = "grey80", alpha = 0.8, size = 1,
    stroke = 0.2
  ) +
  # Annotate trendline and individual points
  annotate(
    "label",
    x = as_date("2024-12-01"), y = 5.5,
    label = "Smoothed trendline",
    hjust = 1, col = "#bc6c25", size = 3, family = "Roboto Condensed Medium",
    fill = "#F8F8F8", label.size = 0
  ) +
  annotate(
    "label",
    x = as_date("2020-01-01"), y = 2.5,
    label = "Monthly value",
    hjust = 1, vjust = 0, col = "grey20", size = 3, family = "Roboto Condensed Medium",
    fill = "#F8F8F8", label.size = 0, label.padding = unit(1, "mm")
  ) +
  annotate(
    "segment",
    x = as_date("2019-11-01"), xend = as_date("2020-03-15"),
    y = 2.5, yend = 2.1,
    linewidth = 0.2,
    arrow = arrow(angle = 20, length = unit(1.5, "mm"))
  ) +
  # Axis titles
  annotate(
    "label",
    x = min_x,
    y = floor(max_y),
    label = "Price in US $",
    hjust = 0.2,
    color = color_text_axis_title, family = "Roboto Condensed Medium", size = 3,
    fill = "#F8F8F8", label.size = 0, label.padding = unit(0, "mm")
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(
    title = "U.S. egg prices hit new high ahead of Easter holidays",
    subtitle = "Average price of eggs, grade A, large, per dozen in U.S. city average,
    not seasonally adjusted, smoothed trendline (Loess, bandwidth = 0.10)",
    caption = "**Source:** U.S. Bureau of Labor Statistics.
    **Visualization:** Ansgar Wolsing",
    x = NULL
  ) +
  coord_cartesian(ylim = c(NA, floor(max_y)), clip = "off") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 8),
    axis.text.y.right = element_text(
      family = "Roboto Condensed Medium", color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.1),
    panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.05),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95,
      lineheight = 1.2),
    plot.subtitle = element_textbox(
      margin = margin(t = 2, b = 16), lineheight = 1.2, width = 1, size = 10
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      lineheight = 1.1, margin = margin(t = 10, b = 6), hjust = 0, size = 7, width = 1),
    legend.position = "bottom",
    plot.margin = margin(t = 4, r = 20, b = 4, l = 8)
  )
ggsave(here(base_path, "19-smooth.png"), width = 5, height = 5)
