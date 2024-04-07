library(tidyverse)
library(ggtext)
library(patchwork)
library(sf)
library(giscoR)
library(here)

base_path <- here("2024", "07")

#' Source: 
#' https://earthquake.usgs.gov/earthquakes/search/
#' API endpoint documentation: https://earthquake.usgs.gov/fdsnws/event/1/
api_url <- "https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=2009-12-31T00:00:00UTC&endtime=2023-12-31T12:00:00UTC&minmagnitude=1&minlatitude=-47.517&maxlatitude=-33.87&minlongitude=165.938&maxlongitude=179.297"
earthquakes <- read_csv(api_url)


# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin Light") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.text = element_text(family = "Libre Franklin", hjust = 1, size = 5.5),
      axis.line.x = element_line(linewidth = 0.2),
      axis.line.y = element_blank(),
      plot.title = element_markdown(
        color = "grey8",
        family = "Libre Franklin SemiBold", hjust = 0, size = 16, 
        lineheight = 1.15, margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_markdown(
        hjust = 0, color = "grey35", size = 7.5,
        margin = margin(t = 6, b = 0)),
      plot.caption = element_textbox(
        width = 1, hjust = 0, lineheight = 1, size = 7),
      plot.caption.position = "plot",
      plot.margin = margin(t = 2, b = 2, l = 4, r = 2),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05),
      strip.text = element_blank()
    )
)

## Histogram -------------------------------------------------------------------

p <- earthquakes |> 
  ggplot(aes(mag)) +
  geom_histogram(binwidth = 0.2, fill = "#6002ee", color = "white", size = 0.05) +
  scale_x_continuous(breaks = seq(1, 8, 1)) +
  scale_y_continuous(expand = expansion(mult = c(0.005, 0.1))) +
  labs(
    title = "Magnitude of Earthquakes in New Zealand",
    subtitle = "\\# of earthquakes between 2009 and 2023",
    caption = "Source: USGS Earthquake Hazards Program. Visualization: Ansgar Wolsing",
    x = "Magnitude", 
    y = NULL
  )


## Inset map -------------------------------------------------------------------

# Load shape of New Zealand
nz <- giscoR::gisco_get_countries(epsg = "4326", country = "New Zealand", resolution = "20")
nz <- st_shift_longitude(nz) |> 
  st_crop(xmin = 165.93, xmax = 179.297, ymin = -47.517, ymax = -33.87)

# Max. magnitude
earthquake_max_magnitude <- earthquakes |> 
  filter(mag == max(mag)) |> 
  select(time, latitude, longitude, mag, place)

earthquake_max_magnitude[rep(1, 3), ]

p_map <- ggplot(nz) +
  geom_sf() +
  geom_point(
    data = earthquake_max_magnitude[rep(1, 3), ] |> 
      bind_cols(marker_size = 1:3),
    aes(longitude, latitude, size = marker_size),
    color = "#6002ee", shape = 21, stroke = 0.3, show.legend = FALSE
  ) +
  scale_size_identity() +
  theme_void() +
  theme(
    plot.margin = margin(0)
  )
  

## Combine plots ---------------------------------------------------------------

p + 
  annotate(
    "segment",
    x = earthquake_max_magnitude$mag, xend = earthquake_max_magnitude$mag,
    y = 320, yend = 10,
    linewidth = 0.2, color = "#666666",
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    GeomTextBox,
    x = earthquake_max_magnitude$mag - 0.05, y = 220, width = 0.3,
    label = "2016<br>Kaikōura<br>earthquake", family = "Libre Franklin SemiBold", 
    size = 3, box.size = 0, fill = NA, hjust = 1, halign = 1, lineheight = 0.9
  ) +
  inset_element(p_map, left = 0.85, right = 1, t = 0.6, b = 0.3)

ggsave(here(base_path, "07-hazards.png"), width = 5, height = 5)  
