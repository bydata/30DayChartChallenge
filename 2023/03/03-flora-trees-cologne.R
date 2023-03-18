pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "tidygraph", "ggraph")

base_path <- here("2023", "03")

## GET DATA ====================================================================
#' Tree population in Cologne, 2020
#' Source: Offene Daten Köln,
#' https://www.offenedaten-koeln.de/dataset/baumkataster-koeln

url_trees_2020 <- "https://www.offenedaten-koeln.de/sites/default/files/20200610_Baumbestan_Koeln.zip"
data_dir <- here(base_path, "cologne_trees")
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
  download.file(url_trees_2020, destfile = here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020.zip"))
  unzip(here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020.zip"),
        exdir = here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020"))
}

crs <- "EPSG:4326"
trees_2020_sf <- st_read(here(data_dir,
                              "Shapes_Bestand_Einzelbaeume_Koeln_2020",
                              "Baumbestand.shp"))
trees_2020_sf <- st_transform(trees_2020_sf, crs)
st_crs(trees_2020_sf)


## Basic EDA ===================================================================


# Replace NA with 1 for ANZAHL (number of trees)
trees_2020_sf_prep <- trees_2020_sf %>%
  mutate(ANZAHL2 = replace_na(ANZAHL, 1),
         ANZAHL2 = ifelse(ANZAHL2 == 0, 1, ANZAHL2))


trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  count(Gattung, sort = TRUE) %>% 
  head(10)

trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  filter(!is.na(Gattung), Gattung != "unbekannt") %>% 
  transmute(Gattung, gattung_art = paste(Gattung, ifelse(!is.na(Art), Art, "Other")),
            ANZAHL2) %>% 
  count(gattung_art, sort = TRUE) %>% 
  head(10)

trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  filter(!is.na(Gattung), Gattung != "unbekannt") %>% 
  count(Gattung, sort = TRUE) %>% 
  slice_max(n, n = 10) %>% 
  mutate(Gattung = fct_reorder(Gattung, n)) %>% 
  ggplot(aes(Gattung, n)) +
  geom_segment(aes(xend = Gattung, y = 0, yend = n, size = n  / 4),
               col = "#4C332C") +
  geom_point(aes(size = n, col = n)) +
  colorspace::scale_color_continuous_sequential(
    "Greens", trans = "pseudo_log", aesthetics = list("fill", "color")) +
  scale_size_area(max_size = 16) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  )


# https://www.pinterest.de/pin/498562621254655108/
flora_palette <- c(
  "#BDA679", "#162231", "#6F7C47", "#8B7356", "#A1B36F", "#6E5542", 
           "#4F5F6F", "#3B191E", "#576148", "#30302E", "#8799A8", "#434318"
)


gattung_art_connections <- trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  transmute(Gattung = ifelse(Gattung == "Verschiedene", "Other", Gattung),
            gattung = ifelse(is.na(Gattung) | Gattung == "unbekannt", "Unknown", Gattung),
            gattung = fct_lump_min(gattung, min = 1000, other_level = "Other"),
            art = ifelse(!is.na(Art), Art, ""),
            art = fct_lump_min(art, min = 1000, other_level = "Other"),
            gattung_art = paste(Gattung, art, sep = " "),
            n = ANZAHL2) %>% 
  # count(gattung, art, gattung_art, wt = n)
  count(from = gattung, to = gattung_art, wt = n)

origin_gattung_connections <- data.frame(
  from = "origin",
  to = unique(gattung_art_connections$from))

edges <- bind_rows(gattung_art_connections, origin_gattung_connections) %>% 
  tibble()

nodes <- distinct(edges, name = to, group = from, size = n) %>% 
  add_row(name = "origin") %>% 
  mutate(
    is_main = group == "origin",
    label = ifelse(is_main | size > 1000, name, NA))

nodes$id <- NA
is_leaf <- nodes$group != "origin" & nodes$name != "origin"
nleaves <- nrow(nodes[is_leaf, ]) 
nodes$id[is_leaf] <- seq(1 : nleaves)
nodes$angle <- 90 - 360 * nodes$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
nodes$hjust <- ifelse(nodes$angle < -90, 1, 0)

# flip angle BY to make them readable
nodes$angle <- ifelse(nodes$angle < -90, nodes$angle + 180, nodes$angle)
nodes <- nodes %>% 
  mutate(angle = replace_na(angle, 0),
         hjust = replace_na(hjust, 0)
         )

graph <- igraph::graph_from_data_frame(edges, vertices = nodes)
# as_tbl_graph(graph) %>%
#   activate(nodes) %>%
#   as_tibble() %>%
#   View()

ggraph(graph, layout = "dendrogram", circular = TRUE) +
  geom_edge_diagonal(
    # aes(color = factor(ifelse(from == 378, to, from))), 
    #                  edge_width = 0.4, show.legend = FALSE
    color = "grey70", edge_width = 0.33
    ) +
  geom_node_point(
    aes(fill = group, size = size), 
    shape = 21, color = "white", stroke = 0.1, alpha = 0.9,
    show.legend = FALSE) +
  geom_node_text(
    aes(label = label, angle = angle, hjust = hjust,
        fontface = ifelse(is_main, "bold", "plain")),
    size = 3, family = "Georgia"
  ) +
  scale_edge_color_manual(values = c(flora_palette, "grey40", "grey80")) +
  # scale_fill_manual(values = c(flora_palette, "grey40", "grey80")) +
  scale_size_area(max_size = 15) +
  coord_fixed(clip = "off") +
  guides(color = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(color = "grey90", fill = "grey90"),
    plot.margin = margin(t = 10, b = 12, l = 5, r = 10)
  )
ggsave(here(base_path, "03-flora-cologne-trees-dendrogram.png"),
       width = 6.5, height = 7)
