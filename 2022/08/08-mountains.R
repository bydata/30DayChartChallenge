library(tidyverse)
library(osmdata)
library(here)
library(ggtext)
library(sf)
library(glue)

base_path <- here("2022", "08")

shape_de <- getbb("Germany", featuretype = "country", format_out = "sf_polygon")$multipolygon
st_crs(shape_de) <- "EPSG:4326"


# Query the OSM API
# https://wiki.openstreetmap.org/wiki/Tag:natural%3Dpeak
mountain_feature_values <- c("peak", "hill", "ridge")
feature_query <- glue("\"natural\"=\"{mountain_feature_values}\"" )
osm_features <- opq(bbox = st_bbox(shape_de), timeout = 1200) %>% 
  add_osm_features(features = feature_query) %>% 
  osmdata_sf()
write_rds(osm_features, here(base_path, "data", glue("osm_features_{Sys.Date()}.rds")))


# Make sure there are only features within DE in the dataset
osm_features_intersect <- st_intersection(osm_features$osm_points, shape_de)
write_rds(osm_features_intersect, here(base_path, "data", glue("osm_features_intersect_{Sys.Date()}.rds")))

osm_features_intersect %>% 
  transmute(elevation = as.numeric(ele)) %>% 
  ggplot() +
  geom_sf(aes(col = elevation), size = 0.1, alpha = 0.8) +
  coord_sf(crs = 4326)

osm_features_intersect %>% 
  sf::st_drop_geometry() %>% 
  count(natural)

elevations <- osm_features_intersect %>% 
  sf::st_drop_geometry() %>% 
  transmute(name, elevation = as.numeric(ele)) %>% 
  na.omit() %>% 
  as_tibble() %>% 
  arrange(-elevation)
elevations
  

elevations %>% 
  ggplot(aes(elevation)) +
  stat_density(geom = "area", col = "green", fill = alpha("green", 0.8))


font_family <- "Playfair Display"
 
# Zugspitze
zugspitze_elevation <- elevations$elevation[elevations$name == "Zugspitze"]
# How many peaks above 1000 / 2000 m?
peaks_above_1000m <- length(which(elevations$elevation >= 1000))
peaks_above_2000m <- length(which(elevations$elevation >= 2000))


elevations %>% 
  ggplot(aes(elevation)) +
  geom_histogram(bins = 50, fill = "grey30", col = "white") +
  annotate("text", x = zugspitze_elevation, y = 1000, label = glue("Zugspitze ({zugspitze_elevation} m)"),
           size = 3, angle = 90, hjust = 0, family = font_family) +
  annotate("segment", x = zugspitze_elevation, xend = zugspitze_elevation, y = 960, yend = 20,
           lty = "solid", size = 0.3) +
  annotate("text", x = 1000, y = 1000, 
           label = glue("{peaks_above_1000m} peaks at or above 1,000 m"),
           size = 3, angle = 90, hjust = 0, family = font_family, lineheight = 0.8) +
  annotate("segment", x = 1000, xend = 1000, y = 960, yend = 200,
           lty = "solid", size = 0.3) +
  annotate("text", x = 2000, y = 1000, 
           label = glue("{scales::number(peaks_above_2000m)} peaks at or above 2,000 m"),
           size = 3, angle = 90, hjust = 0, family = font_family, lineheight = 0.8) +
  annotate("segment", x = 2000, xend = 2000, y = 960, yend = 100,
           lty = "solid", size = 0.3) +
  scale_y_continuous(expand = expansion(add = c(20, 100))) +
  labs(
    title = "Elevation of Mountains in Germany",
    subtitle = "Number of hills/mountains",
    caption = "Source: OpenStreetMap contributors. Visualization: Ansgar Wolsing",
    x = "elevation (m)", y = NULL
  ) +
  theme_minimal(base_family = font_family, base_size = 8) +
  theme(
    plot.background = element_rect(color = NA, fill = "grey98"),
    panel.grid = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    text = element_text(color = "grey17"),
    plot.title = element_text(size = 16),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
ggsave(here(base_path, "08-mountains-de.png"), width = 5, height = 4)  
  