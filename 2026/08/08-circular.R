library(tidyverse)
library(ggtext)
library(here)

base_url <- "https://opendata.dwd.de/climate_environment/CDC/regional_averages_DE/monthly"
feature_stub <- "air_temperature_mean"
file_names <- sprintf("regional_averages_tm_%02d.txt", 1:12)

urls <- file.path(base_url, feature_stub, file_names)

dfs_airtemp <- map(urls, read_delim, delim = ";", skip = 1)
dfs_airtemp <- set_names(dfs_airtemp, month.name)

df_airtemp_prep <- dfs_airtemp |> 
  map(
    function(x) {
      select(x, year = Jahr, value = Deutschland) |> 
        mutate(value = as.numeric(value))
    }
  ) |> 
  bind_rows(.id = "month") |> 
  mutate(
    month = factor(month, levels = month.name)
  )

df_airtemp_prep |> 
  ggplot(aes(month, value)) +
  geom_jitter()

df_airtemp_prep |> 
  ggplot(aes(month, value)) +
  geom_point(
    aes(fill = year >= 2000),
    position = position_jitter(seed = 1, width = 0.25, height = 0),
    shape = 21, stroke = 0.2, size = 2, col = "white",
    alpha = 0.6
  ) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey60")) +
  coord_radial() +
  theme_minimal(base_family = "Instrument Sans")

df_airtemp_prep |> 
  ggplot(aes(month, value, group = year)) +
  geom_line(
    aes(col = year >= 2000),
    alpha = 0.5, linewidth = 0.2
  ) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey60")) +
  coord_radial(expand = TRUE) +
  theme_minimal(base_family = "Instrument Sans")


# Calculate longterm averages 1961-1990
df_airtemp_reference <- df_airtemp_prep |> 
  filter(year >= 1961 & year <= 1990) |> 
  group_by(month) |> 
  summarize(value = mean(value)) |> 
  ungroup()

bg_color <- "#fdf4de"
df_airtemp_prep |> 
  ggplot(aes(month, value)) +
  geom_hline(
    aes(yintercept = 0),
    linewidth = 0.2
  ) +
  geom_label(
    data = data.frame(
      month = factor("January", levels = month.name),
      y = seq(-10, 20, 5),
      label = c(as.character(seq(-10, 15, 5)), "20°C")
    ),
    aes(
      month, y, label = label,
      hjust = ifelse(y == max(y), 0.5, 1)),
    nudge_x = -0.55, family = "Roboto Mono",
    label.size = 0, fill = bg_color, 
    label.padding = unit(0, "mm")
  ) +
  geom_line(
    aes(group = 1),
    data = df_airtemp_reference,
    col = "black", linewidth = 1, linetype = "solid"
  ) +
  geom_point(
    aes(
      fill = year >= 2016,
      alpha = ifelse(year >= 2010, 1, 0.6),
      size = ifelse(year >= 2010, 1.5, 1)
    ),
    position = position_jitter(seed = 1, width = 0.25, height = 0),
    shape = 21, stroke = 0.2, col = "white"
  ) +
  annotate(
    "label",
    x = 7,
    y = 6,
    label = "Long-term monthly\naverage (1961-1990)",
    family = "Instrument Sans Medium", size = 2.5, lineheight = 0.9,
    fill = alpha(bg_color, 0.6), linewidth = 0, 
    label.padding = unit(0, "mm")
  ) +
  annotate(
    "segment",
    x = 7.2, xend = 7.5,
    y = 9.5, yend = 16,
    linewidth = 0.2,
    arrow = arrow(angle = 25, length = unit(0.15, "cm"), type = "closed") 
  ) +
  scale_fill_manual(values = c("TRUE" = "#7303fc", "FALSE" = "grey60")) +
  scale_alpha_identity() +
  scale_size_identity() +
  coord_radial() +
  guides(fill = "none") +
  labs(
    title = "Average monthly temperature in Germany 1881-2026",
    subtitle = "The majority of the monthly average air temperatures (2m) in Germany 
    <b style='color:#7303fc'>since 2016</b> have been 
    higher than the <b>long-term average</b> (1961-1990).
    Each dot represents the monthly average for a year.",
    caption = "**Source:** Deutscher Wetterdienst (DWD) Open Data. 
      **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans", base_size = 10,
    paper = bg_color, ink = "black") +
  theme(
    plot.title = element_text(
      family = "Instrument Sans SemiBold", hjust = 0.5, size = 14),
    plot.subtitle = element_textbox(
      lineheight = 1.33, width = 1.25, hjust = 0.5, halign = 0.5,
      margin = margin(t = 4, b = 10)),
    plot.caption = element_markdown(hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "Instrument Sans SemiBold"),
    axis.text.y = element_blank(),
    panel.grid = element_line(color = "grey30", linewidth = 0.1),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
  )
ggsave(here("2026", "08", "08-circular.png"), width = 5, height = 5)
