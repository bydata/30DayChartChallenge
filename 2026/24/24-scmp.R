library(tidyverse)
library(ggtext)
library(here)

#' Source: Destatis
#' https://www-genesis.destatis.de/datenbank/online/url/dd31ba18
#' Inspiration: https://cdn3.i-scmp.com/sites/default/files/images/methode/2018/04/11/8cfb9394-0736-11e8-82e3-6b95ccc67ee3_image_hires_232400.jpg

df <- read_csv2(here("2026", "24", "51000-0007_de_flat.csv"), na = "-")

df_prep <- df |> 
  select(
    year = time, country = `2_variable_attribute_label`,
    value, import_export = value_variable_label) |> 
  mutate(import_export = ifelse(import_export == "Einfuhr: Wert", "Import", "Export")) |> 
  arrange(year, country)

df_annual_totals <- df_prep |> 
  summarize(value = sum(value, na.rm = TRUE), .by = c(year, import_export)) 

df_annual_china <- df_prep |> 
  filter(country == "China") |> 
  inner_join(
    df_annual_totals, by = join_by(year, import_export),
  suffix = c(".china", ".total")) |> 
  mutate(share = value.china / value.total)

# Which rank?
df_prep |> 
  filter((year == min(year) | year == max(year)) & import_export == "Import") |> 
  mutate(rank = rank(-value), .by = year) |> 
  filter(country == "China")

df_prep |> 
  filter((year == 2022| year == max(year)) & import_export == "Export") |> 
  mutate(rank = rank(-value), .by = year) |> 
  filter(country == "China")

color_pal <- c("#021146", "#FFCA05")
y_axis_breaks <- seq(0, 3e7, 5e6)
y_axis_labels <- scales::number(y_axis_breaks, suffix = " bn", scale = 1e-6) # data is in thousands --> billion
y_axis_labels[length(y_axis_labels)] <- paste0(y_axis_labels[length(y_axis_labels)], "\nEUR")

# Annotations
df_annotations <- tibble(
  x = c(2010, 2023, 2020),
  y = c(9e6, 29e6, 10.5e6),
  label = c(
    "Between 2008 and 2012, German car exports to China almost quadrupled.",
    "Exports reach its peak in 2022, but have dropped by 55 in just 3 years.",
    "Imports from China have tripled between 2020 and 2023."  
  )
)

df_annual_china |> 
  ggplot(aes(year, value.china, col = import_export)) +
  geom_line(
    linewidth = 1.2) +
  geom_label(
    data = ~filter(., year == max(year), import_export == "Import"),
    aes(
      label = ifelse(import_export == "Import", "Imports from China", "Exports to China"),
      fill = import_export),
    family = "Public Sans Medium", size = 3,
    color = "black", linewidth = 0, hjust = 0, nudge_x = 0.2
  ) +
  geom_label(
    data = ~filter(., year == max(year), import_export == "Export"),
    aes(
      label = ifelse(import_export == "Import", "Imports from China", "Exports to China"),
      fill = import_export),
    family = "Public Sans Medium", size = 3,
    color = "white", linewidth = 0, hjust = 0, nudge_x = 0.2
  ) +
  geom_point(
    data = ~filter(., year == max(year)),
    size = 2
  ) +
  geom_point(
    data = ~filter(., 
      year %in% c(2008, 2012, 2022) & import_export == "Export" |
      year %in% c(2020, 2023) & import_export == "Import"
    ),
    shape = 21, fill = "white", stroke = 1, size = 2.5
  ) +
  # Annotations
  geom_label(
    data = df_annotations,
    aes(x, y, label = str_wrap(label, 20)),
    inherit.aes = FALSE,
    family = "Public Sans", size = 2.5, col = "grey40",
    fill = "white", linewidth = 0, hjust = 0, 
    lineheight = 0.9, label.padding = unit(0, "mm")
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = y_axis_breaks,
    labels = y_axis_labels,
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_color_manual(values = color_pal, aesthetics = c("fill", "color")) +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  guides(color = "none", fill = "none") +
  labs(
    title = "German car exports to China have dropped sharply since 2022",
    subtitle = "While since 2020, imports from China have more than tripled.
    <br><br>
    <i>Annual import and export volumes (in billion EUR)</i>",
    caption = "Source: Destatis.
    Visualization: Ansgar Wolsing (theme adapted from the South China Morning Post)"
  ) +
  theme_minimal(base_family = "Public Sans", paper = "white", ink = "grey36") +
  theme(
    plot.title = element_text(family = "Merriweather Bold", color = color_pal[1]),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(
      hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot",
    axis.title = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.length.x = unit(1.5, "mm"),
    plot.margin = margin(t = 6, r = 90, b = 4, l = 10),
    panel.grid = element_line(color = "grey80"),
    panel.grid.major.y = element_line(linewidth = 0.22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linewidth = 0.08)
  )
ggsave(here("2026", "24", "24-scmp.png"), width = 6, height = 4, scale = 1.1)
