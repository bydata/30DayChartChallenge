pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf", "tidygraph", "ggraph", "here")

base_path <- here("2023", "03")

## GET DATA ====================================================================
#' Tree Census in New York City, 2015
#' The 2015, 2005, and 1995 tree censuses were conducted by NYC Parks and Recreation staff,
#'  TreesCount! program staff, and hundreds of volunteers.
#' https://www.kaggle.com/datasets/nycparks/tree-census?select=new_york_tree_census_2015.csv

nyc_trees <- read_csv(here(base_path, "ny-tree-census", "new_york_tree_census_2015.csv"))

nyc_trees_counts <- nyc_trees %>% 
  select(spc_latin) %>% 
  mutate(
    spc_latin = replace_na(spc_latin, "Unknown"),
    gattung = str_extract(spc_latin, "[^\\s]+"),
    gattung = ifelse(spc_latin == "Unknown", "Other", gattung),
    art = str_remove(spc_latin, gattung),
    art = str_squish(art),
    gattung = fct_lump_min(gattung, min = 15000, other_level = "Other"),
    gattung_art = ifelse(art == "", paste0(spc_latin, "*"), spc_latin),
    gattung_art = if_else(gattung == "Other", 
                          fct_lump_min(gattung_art, min = 1000, other_level = "Other*"), 
                          gattung_art)
  ) %>% 
  count(gattung_art, gattung, sort = TRUE)

# Gattung count
length(unique(nyc_trees_counts$gattung))

# https://www.pinterest.de/pin/498562621254655108/
flora_palette <- c(
  "#BDA679", "#162231", "#6F7C47", "#8B7356", "#A1B36F", "#6E5542", 
           "#4F5F6F", "#3B191E", "#576148", "#30302E", "#8799A8", "#434318"
)
length(flora_palette)

gattung_art_connections <- nyc_trees_counts %>% 
  select(from = gattung, to = gattung_art, size = n)

origin_gattung_connections <- data.frame(
  from = "origin",
  to = unique(gattung_art_connections$from))

edges <- bind_rows(gattung_art_connections, origin_gattung_connections) %>% 
  tibble()

nodes <- distinct(edges, name = to, group = from, size) %>% 
  mutate(
    is_main = group == "origin",
    label = ifelse(is_main | size > 1000, name, NA)) %>% 
  add_row(name = "origin", is_main = FALSE) %>% 
  arrange(group, name)

nodes$id <- NA
is_leaf <- nodes$group != "origin" & nodes$name != "origin"
is_leaf <- nodes$group != "origin" & nodes$name != "origin"
nleaves <- nrow(nodes[is_leaf, ])
nodes$id[is_leaf] <- seq_len(nleaves)
nodes$angle <- 90 - 360 * nodes$id / nleaves

# nodes$group_id <- NA
# n_nonleaves <- nrow(nodes[!is_leaf, ])
# nodes$group_id[!is_leaf] <- seq_len(n_nonleaves)
# nodes$angle[!is_leaf] <- 90 - 360 * nodes$group_id / n_nonleaves

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

ggraph(graph, layout = "dendrogram", circular = TRUE) +
  geom_edge_diagonal(color = "grey70", edge_width = 0.33) +
  geom_node_point(
    aes(fill = group, size = size), 
    shape = 21, color = "white", stroke = 0.2, alpha = 0.8,
    show.legend = TRUE) +
  geom_node_text(
    aes(filter = !is_main, label = label, angle = angle, hjust = hjust,
        fontface = ifelse(is_main, "bold", "plain")),
    size = 3.5, family = "Georgia"
  ) +
  geom_node_text(
    aes(filter = is_main, label = label, angle = angle, hjust = hjust,
        fontface = "bold"),
    size = 3.5, family = "Georgia", repel = TRUE
  ) +
  scale_edge_color_manual(values = c(flora_palette, "grey40", "grey80")) +
  scale_fill_manual(values = c(flora_palette, "grey40", "grey80")) +
  scale_size_area(max_size = 24) +
  coord_fixed(clip = "off") +
  guides(
    color = "none", fill = "none",
    size = guide_legend(title = "# of trees", override.aes = list(color = "grey50"))
    ) +
  labs(
    title = "Trees of New York City",
    subtitle = "All tree species by genus with their Latin names.",
    caption = "\\* Species unknown. <br>
    Data: Kaggle, NYC Parks and Recreation staff,
    TreesCount!<br>program staff,and hundreds of volunteers, 2015. <br>
    Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Georgia") +
  theme(
    plot.background = element_rect(color = "grey90", fill = "grey90"),
    plot.margin = margin(t = 10, b = 80, l = 80, r = 80),
    plot.title = element_text(
      size = 32, face = "bold", hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 1, hjust = 0.5, halign = 0.5, margin = margin(b = 80)),
    plot.caption = element_markdown(
      hjust = 1.5, halign = 0, margin = margin(t = 60, b = -36)),
    legend.position = c(-0.22, 0),
    # move legend text slightly to the left
    legend.text = element_text(margin = margin(l = -6))
  )
ggsave(here(base_path, "03-flora-nyc-trees-dendrogram.png"),
       width = 10, height = 9, scale = 1.15)

