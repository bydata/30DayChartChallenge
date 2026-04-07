library(tidyverse)
library(ggtext)
library(here)

#' Source: https://kvb.koeln/service/open_data.html

# Elevators
urls <- c(
  "Elevators, total" = "https://data.webservice-kvb.koeln/service/opendata/aufzuege/csv",
  "Elevators, out of order" = "https://data.webservice-kvb.koeln/service/opendata/aufzugsstoerung/csv",
  "Escalators, total" = "https://data.webservice-kvb.koeln/service/opendata/fahrtreppen/csv",
  "Escalators, out of order" = "https://data.webservice-kvb.koeln/service/opendata/fahrtreppenstoerung/csv"
)

dfs <- map(urls, read_csv2)
counts <- map_int(dfs, nrow)
timestamp <- max(dfs[[2]]$timestamp)

df_counts <- tibble(
  type = names(counts),
  n = unname(counts)
) |> 
  separate_wider_delim(
    type, delim = ", ", names = c("type", "status")) |> 
  mutate(
    n = ifelse(status == "total", n - min(n), n),
    .by = type) |> 
  mutate(
    status = ifelse(status == "total", "okay", status),
    icon = case_when(
      type == "Elevators" & status == "out of order" ~ here("2026", "02", "aufzug-rot.png"),
      type == "Elevators" & status == "okay" ~ here("2026", "02", "aufzug-grau.png"),
      type == "Escalators" & status == "out of order" ~ here("2026", "02", "rolltreppe-rot.png"),
      type == "Escalators" & status == "okay" ~ here("2026", "02", "rolltreppe-grau.png")
    ),
    icon_html = sprintf("<img src='%s' width='15'>", icon)
  )

df_uncount <- df_counts |>
  uncount(n) |> 
  group_by(type) |> 
  mutate(
    col = (row_number() - 1) %% 10 + 1,
    row = (row_number() - 1) %/% 10 + 1,
  ) |> 
  ungroup()

df_uncount |> 
  ggplot(aes(col, row)) +
  geom_richtext(
    aes(label = icon_html),
    fill = NA, col = NA
  ) +
  geom_richtext(
    data = data.frame(
      type = "Elevators",
      label = "Elevators out of order",
      row = 9.5, col = 1
    ),
    aes(label = label),
    family = "Fira Sans Italic", col = "#BB271A",
    fill = NA, label.size = 0, hjust = 0
  ) +
  geom_curve(
    data = data.frame(
      type = "Elevators",
      x = 1.5, xend = 1.5,
      y = 9, yend = 7.5
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    col = "#BB271A",
    curvature = 0.2, linewidth = 0.25,
    arrow = arrow(angle = 25, length = unit(0.1, "cm"))
  ) +
  coord_equal() +
  facet_wrap(vars(type), strip.position = "top") +
  labs(
    title = "Out of Order",
    subtitle = sprintf("In the underground stations in Cologne, **%d elevators (%s)** and
    **%d escalators (%s)** are currently <b style='color:#BB271A'>out of order</b>.",
      counts["Elevators, out of order"], 
      scales::percent(counts["Elevators, out of order"] / counts["Elevators, total"]),
      counts["Escalators, out of order"],
      scales::percent(counts["Escalators, out of order"] / counts["Escalators, total"])
    ),
    caption = sprintf("**Data:** Kölner Verkehrsbetriebe (KVB) as of %s.<br>
    **Image credit:** DinosoftLabs, Icon Media.
    **Visualization:** Ansgar Wolsing", 
    format(timestamp, "%B %d, %Y")
    )
  ) +
  theme_void(base_family = "Fira Sans", paper = "white") +
  theme(
    plot.title = element_markdown(
      family = "Fira Sans", size = 24, hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 1.1, hjust = 0.5, halign = 0.5, lineheight = 1.33,
      size = 14, margin = margin(t = 8, b = 15)
    ),
    plot.caption = element_textbox(
      lineheight = 1.25, halign = 0.5, width = 1),
    strip.text = element_text(
      family = "Fira Sans SemiBold", size = 14, color = "grey40"),
    panel.spacing.x = unit(1.5, "cm"),
    plot.margin = margin(t = 4, r = 10, b = 4, l = 10)
  )
ggsave(here("2026", "02", "02-pictogram.png"), width = 6, height = 7.8)

# Image credit:
# Elevator: Icon Media
# Escalator: DinosoftLabs