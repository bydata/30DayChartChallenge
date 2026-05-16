library(tidyverse)
library(ggtext)
library(here)

base_url <- "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/seasonal"
feature_stub <- "air_temperature_mean"
seasons <- c("spring", "summer", "autumn", "winter")
file_names <- sprintf("regional_averages_tm_%s.txt", seasons)

urls <- file.path(base_url, feature_stub, file_names)

dfs_airtemp <- map(urls, read_delim, delim = ";", skip = 1)
dfs_airtemp <- set_names(dfs_airtemp, seasons)

df_airtemp_prep <- dfs_airtemp |> 
  map(
    function(x) {
      select(x, year = Jahr, value = Deutschland) |> 
        mutate(value = as.numeric(value))
    }
  ) |> 
  bind_rows(.id = "season") |> 
  mutate(season = factor(season, levels = seasons)) |> 
  # Exclude incomplete year
  filter(year <= 2025)

df_airtemp_prep |> 
  ggplot(aes(season, value)) +
  geom_jitter()

df_airtemp_prep |> 
  ggplot(aes(season, value)) +
  geom_point(
    aes(fill = year >= 2000),
    position = position_jitter(seed = 1, width = 0.25, height = 0),
    shape = 21, stroke = 0.2, size = 2, col = "white",
    alpha = 0.6
  ) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey60")) +
  theme_minimal(base_family = "Instrument Sans")


# Calculate longterm averages 1961-1990
df_airtemp_reference <- df_airtemp_prep |> 
  filter(year >= 1961 & year <= 1990) |> 
  group_by(season) |> 
  summarize(value = mean(value)) |> 
  ungroup()

df_airtemp_anomaly <- df_airtemp_prep |> 
  inner_join(df_airtemp_reference, by = "season") |> 
  mutate(anomaly = value.x - value.y)

bg_color <- "#fefbf2fb"
p_base <- df_airtemp_anomaly |> 
  ggplot(aes(year, anomaly, group = season)) +
  scale_x_continuous(
    breaks = seq(1880, 2040, 20),
    position = "top"
  ) +
  scale_y_continuous(
    breaks = seq(-6, 4, 2),
    labels = c(-6, -4, -2, " 0", "+2", "+4°C"),
    position = "right") +
  coord_cartesian(ylim = c(-4, 4), clip = "off") +
  scale_fill_manual(values = c("TRUE" = "#BA5624", "FALSE" = "#465362")) +
  facet_wrap(
    vars(season), ncol = 1, labeller = as_labeller(str_to_title),
    strip.position = "left") +
  guides(fill = "none") +
  labs(
    title = "The Warming of the Seasons",
    subtitle = "Annual seasonal temperature anomalies from 1881-2025 relative to the
    1961–1990 baseline (in °C).
    Saturated color represent the 10 warmest and coldest seasons on record.",
    caption = "**Source:** Deutscher Wetterdienst (DWD) Open Data. 
      **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans", base_size = 10,
    paper = bg_color, ink = "black") +
  theme(
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      lineheight = 1.4, width = 1, hjust = 0, halign = 0,
      margin = margin(b = 12)),
    plot.caption = element_markdown(
      hjust = 0, margin = margin(t = 20)),
    plot.caption.position = "plot",
    axis.title = element_blank(),
    axis.text.x = element_text(
      family = "Instrument Sans Medium", size = 7, vjust = 0),
    axis.text.y = element_text(
      family = "Roboto Mono Medium", size = 7, hjust = 0),
    axis.ticks.x = element_line(linewidth = 0.2, color = "grey40"),
    axis.ticks.length.x = unit(1, "mm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey50", linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.y = unit(6, "mm"),
    plot.margin = margin(t = 6, r = 4, b = 4, l = 4),
    strip.text.y.left = element_text(
      family = "Instrument Sans SemiBold", hjust = 0, size = 10, 
      color = "grey40", angle = 0)
  )

p_base + 
  geom_col(
    aes(fill = anomaly > 0),
    col = "white", linewidth = 0.1
  ) +
  geom_hline(yintercept = 0, col = "grey40", linewidth = 0.33) 


# Top 10 coldest and hottest years per season
df_airtemp_hottest <- df_airtemp_anomaly |> 
  slice_max(order_by = anomaly, n = 10, with_ties = TRUE, by = season) 

df_airtemp_coldest <- df_airtemp_anomaly |> 
  slice_min(order_by = anomaly, n = 10, with_ties = TRUE, by = season)

df_airtemp_anomaly_ranking_by_season <- df_airtemp_anomaly |> 
  group_by(season) |> 
  mutate(
    in_top_10_hottest = rank(-anomaly) <= 10,
    in_top_10_coldest = rank(anomaly) <= 10
  ) |> 
  ungroup()


df_annotations <- data.frame(
      season = factor(
        c(rep("spring", 2), 
        rep("summer", 2),
        "winter"),
      levels = seasons),
      x = c(1883, 1999, 1986, 1935, 1988), 
      y = c(-4.5, -3.5, -3, -3.5, -4.1),
      label = c(
        "Saturated blue bars\n= top 10 coldest years",
        "Saturated red bars\n= top 10 hottest years",
        "The last summer that was\nmore than 0.5°C below\nthe long-term average\nwas in 1987",
        "Only one of the top 10\nhottest summers was\nbefore 1990 (1947)",
        "Winters exhibit the highest\nseasonal variability; recent\nyears have seen temperatures\nup to 4°C above average."
      )
    )

df_annotation_arrows <- data.frame(
      season = factor(
        c(rep("spring", 2), "summer"), 
        levels = seasons),
      x = c(1883, 1999, 1947), 
      xend = c(1883, 1999, 1947), 
      y = c(-4, -3, -2.3),
      yend = c(-2.2, 0, -0.1)
     )

p_base + 
  geom_col(
    data = df_airtemp_anomaly_ranking_by_season,
    aes(
      fill = anomaly > 0,
      alpha = in_top_10_hottest | in_top_10_coldest),
    col = "grey10", linewidth = 0.1
  ) +
  geom_label(
    data = df_annotations,
    aes(x, y, label = label),
    family = "Instrument Sans Medium", size = 2.5, lineheight = 0.8,
    hjust = 0, fill = alpha(bg_color, 0.9), linewidth = 0
  ) +
  geom_curve(
     data = df_annotation_arrows,
     aes(x = x, xend = xend, y = y, yend = yend),
     linewidth = 0.2, 
     arrow = arrow(angle = 25, length = unit(0.1, "cm"), type = "closed"),
     curvature = -0.2
  ) +
  geom_hline(yintercept = 0, col = "grey40", linewidth = 0.33) +
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.33)) +
  guides(alpha = "none")
ggsave(here("2026", "23", "23-seasons.png"), width = 6, height = 6)
