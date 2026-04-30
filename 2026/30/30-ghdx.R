library(tidyverse)
library(ggtext)
library(here)

#' Source: 
#' Suggested Citation:
#' Institute for Health Metrics and Evaluation (IHME). Global Expected Health Spending 2023-2050. Seattle,
#' United States of America: Institute for Health Metrics and Evaluation (IHME), 2025.
#' 
#' Log in and download zip file from
#' https://ghdx.healthdata.org/record/ihme-data/global-expected-health-spending-2023-2050-20251114

df <- read_csv(here("2026", "30", "IHME_EXPECTED_HEALTH_SPENDING_2023_2050_Y2025M11D06.CSV"))

# Country selection: Top 10 in 2023, excluding small states (pop < 1M)
selected_countries <- df |>
  filter(
    year == min(year), level == "Country",
    !location_name %in% c("Monaco", "Greenland", "Luxembourg")
  ) |>
  arrange(desc(ghes_per_cap_mean)) |> 
  slice_max(order_by = ghes_per_cap_mean, n = 10) |> 
  pull(iso3)


df |> 
  filter(iso3 %in% selected_countries) |> 
  mutate(
    location_name = ifelse(!is.na(iso3) & iso3 == "USA", "United States", location_name),
    location_name = fct_reorder(location_name, -ghes_per_cap_ppp_mean, .fun = max)) |> 
  ggplot(aes(year, ghes_per_cap_ppp_mean, group = location_name)) +
  geom_line(
    col = "grey80", layout = "fixed", linewidth = 0.2) +
  geom_ribbon(
    aes(ymin = ghes_per_cap_ppp_lower, ymax = ghes_per_cap_ppp_upper),
    alpha = 0.1, linewidth = 0.1, fill = "#285185"
  ) +
  geom_line(linewidth = 1.2, col = "#285185") +
  coord_cartesian(ylim = c(5e3, NA)) +
  facet_wrap(vars(location_name), ncol = 5) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Title",
    subtitle = "*Total Health Spending per person
    (constant 2023 purchasing parity power dollars)*",
    caption = "**Source:** Institute for Health Metrics and Evaluation (IHME).
    Global Expected Health Spending 2023-2050 via Global Health Data Exchange.
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white", ink = "grey30") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.95, margin = margin(t = 2, b = 10),
      lineheight = 1.4, size = rel(0.9)),
    plot.caption =  element_textbox(
      width = 1, hjust = 0, lineheight = 1.1, size = rel(0.75),
      margin = margin(t = 10)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 4, r = 60, b = 4, l = 10),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.length.x = unit(1.5, "mm")
  )
ggsave(here("2026", "30", "30-ghdx.png"), width = 9, height = 5)


selected_countries <- c("USA", "DEU")
df |> 
  filter(iso3 %in% selected_countries) |> 
  mutate(
    location_name = ifelse(!is.na(iso3) & iso3 == "USA", "United States", location_name),
    location_name = fct_reorder(location_name, -ghes_per_cap_ppp_mean, .fun = max)) |> 
  ggplot(aes(year, ghes_per_cap_ppp_mean, group = location_name)) +
  geom_ribbon(
    aes(ymin = ghes_per_cap_ppp_lower, ymax = ghes_per_cap_ppp_upper),
    alpha = 0.1, linewidth = 0.1, fill = "#D4AA7D"
  ) +
  geom_line(linewidth = 1.2, col = "#D4AA7D") +
  geom_ribbon(
    aes(ymin = the_per_cap_ppp_lower, ymax = the_per_cap_ppp_upper),
    alpha = 0.1, linewidth = 0.1, fill = "#285185"
  ) +
  geom_line(aes(y = the_per_cap_ppp_mean), linewidth = 1.2, col = "#285185") +
  geom_label(
    data = data.frame(
      location_name = factor("United States", levels = c("United States", "Germany")),
      x = 2045, y = c(7.5e3, 17.5e3),
      label = c("Government", "Total\nHealth\nSpending"),
      col = c("#D4AA7D", "#285185")
    ),
    aes(x, y, label = label, col = col),
    family = "Instrument Sans Medium", size = 3,
    fill = "white", label.size = 0, label.padding = unit(0, "mm"),
    hjust = 0, lineheight = 0.8
  ) +
  # y axis label
  geom_label(
    data = data.frame(
      location_name = factor("United States", levels = c("United States", "Germany")),
      x = 2023, y = 3e4,
      label = "Constant 2023 purchasing parity power dollars"
    ),
    aes(x, y, label = str_wrap(label, 16)),
    family = "Inconsolata Bold", size = 2.75,
    fill = "white", label.size = 0, label.padding = unit(0, "mm"),
    hjust = 0, vjust = 0.8, lineheight = 0.8, col = "grey30"
  ) +  
  scale_y_continuous(breaks = seq(0, 3e4, 5e3)) +
  scale_color_identity() +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(vars(location_name), nrow = 1) +
  guides(fill = "none", color = "none") +
  labs(
    title = "The German health system is expensive, but no comparison to the U.S.",
    subtitle = "Projected <b style='color:#285185'>total health spending</b> per person
      and the <b style='color:#D4AA7D'>government's</b> contribution, 2023-2050",
    caption = "**Source:** Institute for Health Metrics and Evaluation (IHME).
    Global Expected Health Spending 2023-2050 via Global Health Data Exchange.
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white", ink = "grey30") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", color = "black", 
      size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.95, margin = margin(t = 2, b = 10),
      lineheight = 1.4, size = rel(0.9)),
    plot.caption =  element_textbox(
      width = 1, hjust = 0, lineheight = 1.25, size = rel(0.75),
      margin = margin(t = 10)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 4, r = 4, b = 4, l = 10),
    axis.text = element_text(family = "Inconsolata"),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.length.x = unit(1.5, "mm"),
    strip.text = element_text(
      family = "Instrument Sans SemiBold", size = 12)
  )
ggsave(here("2026", "30", "30-ghdx.png"), width = 7.5, height = 5)
