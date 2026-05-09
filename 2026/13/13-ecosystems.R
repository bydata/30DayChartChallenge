library(tidyverse)
library(ggtext)
library(rmangal)
# library(igraph)
library(ggraph)
library(tidygraph)
library(here)

all_datasets <- search_datasets("Antarctic")

data <- search_datasets(list("name" = "mackintosh_1967"))
data$references

network <- data |> get_collection()
head(network[[1]]$nodes)
network[[1]]$nodes$original_name

graph <-  as_tbl_graph(network[[1]])
graph |> 
  activate(edges) |> 
  as_tibble() |> 
  count(type)

graph |> 
  activate(nodes) |> 
  as_tibble() |> View()

graph <- graph |> 
  activate(nodes) |> 
  mutate(original_name = str_to_sentence(original))


bg_color <- "#fdf4de"

p <- ggraph(graph, layout = "stress") +
  geom_edge_link(
    end_cap = circle(0.5),
    start_cap = circle(0.5),
    color = "grey30", linewidth = 0.2,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
  ggfx::with_shadow(
    geom_node_point(
      shape = 21, col = "grey8", fill = "#bc92e8",
      size = 6
    ),
    x_offset = 5, y_offset = 5
  ) +
  geom_node_text(
    aes(label = str_wrap(original_name, 10)),
    family = "Instrument Sans Medium", lineheight = 0.75, repel = TRUE,
    size = 3
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Food web of Antarctic marine ecosystems",
    subtitle = "Mapping the trophic connections of the Southern Ocean.
    <br><br>
    ***Note:** These relationships represent historical observations that may not
    reflect recent knowledge in species distribution.*",
    caption = "**Source:** Mackintosh (1967). **Visualization:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Instrument Sans", paper = bg_color, ink = "grey30") +
  theme(
    plot.margin = margin(t = 8, r = 8, b = 8, l = 8),
    plot.title = element_text(
      family = "Playfair Display", face = "italic", size = 24, color = "grey2"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.8, lineheight = 1.4,
      margin = margin(t = 6, b = 16)),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )


# Placement custom legend
b <- ggplot_build(p)
x_range <- b$layout$panel_params[[1]]$x.range
y_range <- b$layout$panel_params[[1]]$y.range

# Where to start the segment
point_offset_x <- 0.02 * (x_range[2] - x_range[1])

p_with_legend <- p +
  annotate(
    "text",
    x = point_offset_x + x_range[1] + 0.1 * (x_range[2] - x_range[1]) / 2,
    y = y_range[2] + 0.06 * (y_range[2] - y_range[1]),
    label = "Food chain link",
    family = "Instrument Sans SemiBold", col = "grey40", size = 3.75,
    hjust = 0.5
  ) +
  ggfx::with_shadow(
    annotate(
      "point",
      x = c(x_range[1], 2 * point_offset_x + x_range[1] + 0.1 * (x_range[2] - x_range[1])),
      y = y_range[2],
      shape = 21, col = "grey8", fill = "#bc92e8",
      size = 4
    ), 
    x_offset = 3, y_offset = 3
  ) +
  annotate(
    "text",
    x = c(x_range[1], 2 * point_offset_x + x_range[1] + 0.1 * (x_range[2] - x_range[1])),
    y = y_range[2] - 0.042 * (y_range[2] - y_range[1]),
    label = c("Predator", "Prey"),
    col = "grey8", size = 3, family = "Instrument Sans Medium", hjust = 0.5
  ) +
  annotate(
    "segment",
    x = point_offset_x + x_range[1], 
    xend = point_offset_x + x_range[1] + 0.1 * (x_range[2] - x_range[1]),
    y = y_range[2], yend = y_range[2],
    color = "grey30", linewidth = 0.2,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  )
ggsave(here("2026", "13", "13-ecosystems.png"), width = 7, height = 7)
