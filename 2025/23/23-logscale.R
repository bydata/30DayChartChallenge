library(tidyverse)
library(ggtext)
library(here)
library(tidyfinance)
library(patchwork)

base_path <- here("2025", "23")

df <- download_data(
  type = "stock_prices",
  symbols = "NVDA",
  start_date = "1999-01-01",
  end_date = "2025-04-23"
)

df |> 
  summarize(min(date), max(date))

# Annotations: significant events
df_events <- tribble(
  ~date, ~description,
  "1999-10-11", "Launch of the GeForce 256",
  "2002-09-24", "Dot-com bubble",
  "2006-02-16", "Release of CUDA",
  "2008-09-15", "Lehman Brothers bankruptcy",
  "2016-01-05", "Increasing importance of Deep Learning",
  "2020-03-20", "Covid-19",
  "2022-11-30", "Release of ChatGPT by OpenAI",
  "2025-04-22", "Trump trade war"
) |> 
  mutate(date = as_date(date))

df_events_close <- df_events |> 
  inner_join(df, by = join_by(date)) |> 
  select(date, description, adjusted_close)

# Custom theme
theme_set(
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_text(size = 9),
    axis.text = element_text(
      family = "Roboto Condensed Medium", color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.1),
    # panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.05),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 18, width = 0.95,
      lineheight = 1.2),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      lineheight = 1.1, margin = margin(t = 10, b = 6), hjust = 0, size = 7, width = 1),
    plot.margin = margin(t = 8, r = 8, b = 4, l = 8)
  )
)

max_y = max(df$adjusted_close)
annotation_y_pos <- max_y * 1.1

p1 <- df |> 
  ggplot(aes(date, adjusted_close)) +
  geom_line(col = "#D14081", size = 0.5) +
  geom_segment(
    data = df_events_close,
    aes(xend = date,
        y = ifelse(year(date) < 2025, adjusted_close + 58,
                   (adjusted_close - 0.01) / 1.5),
        yend = adjusted_close),
    col = "grey50", linewidth = 0.25) +
  geom_point(
    data = df_events_close,
    shape = 21, col = "white", fill = "grey50"
  ) +
  geom_label(
    data = df_events_close,
    aes(
      x = date - ifelse(year(date) < 2025, 0, 400),
      y = ifelse(year(date) < 2025, adjusted_close + 60,
                 (adjusted_close - 0.01) / 1.5),
      label = str_wrap(description, 14),
      hjust = ifelse(year(date) < 2025, 1, 0)),
    family = "Roboto Condensed", size = 2.5,
    fill = "#F8F8F8", label.size = 0, nudge_x = 100,
    label.padding = unit(1, "mm"), lineheight = 0.8
  ) +
  # y-axis title
  annotate(
    "text",
    x = max(df$date),
    y = max(df$adjusted_close) * 1.09,
    label = "Adjusted close (US-$)",
    family = "Roboto Condensed Medium", size = 2.5, hjust = 0.4
  ) +
  # Custom subtitle
  annotate(
    "richtext",
    x = min(df$date),
    y = max(df$adjusted_close),
    label = "**Linear scale:**<br>Shows absolute price jumps and recent market surges",
    family = "Roboto Condensed", size = 4, hjust = 0,
    fill = "#F8F8F8", label.size = 0,
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(
    position = "right",
    breaks = seq(0, 150, 25),
    expand = expansion(mult = c(0, 0.1))) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL, y = NULL
  )


p2 <- df |> 
  ggplot(aes(date, adjusted_close)) +
  geom_line(col = "#D14081", size = 0.5) +
  geom_segment(
    data = df_events_close,
    aes(xend = date,
        y = ifelse(year(date) < 2020, adjusted_close + 9,
                   (adjusted_close - 0.01) / 4),
        yend = adjusted_close),
    col = "grey50", linewidth = 0.25) +
  geom_point(
    data = df_events_close,
    shape = 21, col = "white", fill = "grey50"
  ) +
  geom_label(
    data = df_events_close,
    aes(
      x = date - ifelse(year(date) < 2025, 0, 400),
      y = ifelse(year(date) < 2020, adjusted_close + 15,
                 (adjusted_close - 0.01) / 4),
      label = str_wrap(description, 14),
      hjust = ifelse(year(date) < 2025, 1, 0)),
    family = "Roboto Condensed", size = 2.5, 
    fill = "#F8F8F8", label.size = 0, nudge_x = 90,
    label.padding = unit(1, "mm"), lineheight = 0.8
  ) +
  # Annotation to explain the log scale
  annotate(
    "richtext",
    x = max(df$date) - 3.5 * 365,
    y = 0.25,
    label = "**Each step** of the scale<br>represents a **doubling**<br>of the stock price",
    family = "Roboto Condensed", fontface = "italic", size = 3, fill = "#F8F8F8",
    label.size = 0, lineheight = 0.9, hjust = 0
  ) +
  annotate(
    GeomCurve,
    x = max(df$date),
    xend = max(df$date) + 500,
    y = 0.12, yend = 0.12,
    arrow = arrow(angle = 20, length = unit(1, "mm")),
    linewidth = 0.2, col = "grey30", curvature = 0.3
  ) +
  # y-axis title
  annotate(
    "text",
    x = max(df$date),
    y = max(df$adjusted_close) * 1.9,
    label = "Adjusted close (US-$)",
    family = "Roboto Condensed Medium", size = 2.5, hjust = 0.2
  ) +
  # Custom subtitle
  annotate(
    "richtext",
    x = min(df$date),
    y = max(df$adjusted_close),
    label = "**Logarithmic scale:**<br>
    Highlights percentage-based gains and long-term trends",
    family = "Roboto Condensed", size = 4, hjust = 0,
    fill = "#F8F8F8", label.size = 0,
  ) +
  scale_x_date(
    date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(
    breaks = c(0.04, 0.08, 0.15, 0.3125, 0.625, 1.25, 2.5, 5, 10, 20, 40, 80, 160),
    labels = as_labeller(function(x) {
      x <- as.numeric(x)
      ifelse(x < 2, round(x, 2), round(x, 0))
      }),
    transform = "log",
    position = "right",
    expand = expansion(mult = c(0, 0.1))
  ) +
  coord_cartesian(xlim = c(min(df$date), max(df$date)), clip = "off") +
  labs(
    x = NULL, y = NULL
  )

p1 / p2 &
  plot_annotation(
    title = "The scale(s) of NVIDIA's growth",
    caption = "**Source:** Yahoo Finance (via {tidyfinance} R package).
    **Visualization:** Ansgar Wolsing")
ggsave(here(base_path, "23-logscale.png"), width = 9, height = 7.5,
       scale = 0.8)
