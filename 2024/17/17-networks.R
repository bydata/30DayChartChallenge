library(tidyverse)
library(ggtext)
library(here)
library(ggraph)
library(tidygraph)
library(patchwork)

base_path <- here("2024", "17")

#' Source: Kaggle
#' https://www.kaggle.com/datasets/nasirkhalid24/the-office-us-complete-dialoguetranscript

# There are a couple of lines in which the line column does split into two columns
# Avoid this behaviour by guessing the columns only from the first row
df_file <- here(base_path, "The-Office-Lines-V4.csv")
theoffice <- read_csv(df_file, guess_max = 1) %>% 
  select(-7)

# speaker clean up
theoffice <- theoffice %>% 
  mutate(speaker = str_remove_all(speaker, "[\":]"),
         speaker = case_match(
           speaker,
           "(Pam's mom) Heleen" ~ "Pam's mom",
           "AJ" ~ "A.J.",
           "abe" ~ "Gabe",
           .default = speaker
         )) 

# Count the scenes in which characters appear together
characters_scenes <- theoffice %>% 
  distinct(season, episode, scene, speaker) |> 
  # exclude seasons 8 and 9, since Michael Scott left the show after the 7th season
  filter(season <= 7)
characters_scenes

michael_scene_count_by_season <- characters_scenes %>% 
  filter(speaker == "Michael") |> 
  count(season, speaker, name = "n_scenes_season")
michael_scene_count_by_season

connections <- characters_scenes |> 
  group_by(season) |> 
  widyr::pairwise_count(speaker, scene) |> 
  ungroup() |> 
  filter(item1 == "Michael") |> 
  inner_join(michael_scene_count_by_season, by = c("season", "item1" = "speaker")) |> 
  mutate(scene_share = n / n_scenes_season) |> 
  arrange(season, -scene_share)

connections_topn_per_season <- connections |> 
  slice_max(order_by = scene_share, n = 5, by = season)

length(unique(connections_topn_per_season$item2))
max_share <- max(connections_topn_per_season$scene_share)

graphs_per_season <- connections_topn_per_season |>
  rename(from = item1, to = item2) |> 
  select(season, from, to, weight = scene_share) |> 
  group_by(season) |> 
  group_split() |> 
  map(
    ~select(.x, -season) |> 
      as_tbl_graph()
  )


# Custom theme
theme_set(
  theme_minimal(base_family = "Outfit", base_size = 10) +
  theme(
    plot.background = element_rect(color = "#2B394D", fill = "#2B394D"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.text.x.top = element_text(angle = 50, hjust = 0),
    plot.title = element_text(
      family = "American Typewriter", size = 22, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.95, lineheight = 1.2, size = 12, hjust = 0.5, halign = 0.5,
      margin = margin(b = 24)),
    plot.caption = element_markdown(hjust = 0.5),
    plot.margin = margin(t = 4, b = 4, l = 12, r = 12),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(4, "mm")
  )
)

# Colour palette
pal_office <- 
  c("#cc2d36", "#D8ACD8", "#FBA93A", "#93BFE5",  "#f0813c", "#F0F4EC", "#2AA3A6",
    "#4BD9EF", "#4bad6d", "#5D77AA")
names(pal_office) <- unique(connections_topn_per_season$item2)


# Plot a graph per season
plot_graph <- function(graph, season, seed = 1) {
  set.seed(seed)
  p <- graph |> 
    ggraph(layout = "dh") +
    geom_edge_fan(
      aes(edge_width = weight),
      color = "white") +
    geom_node_point(
      aes(size = ifelse(name == "Michael", 4, 15),
          fill = name),
      color = "white", shape = 21, stroke = 1) +
    geom_node_text(
      aes(
        label = ifelse(name == "Michael", "", toupper(name)),
        color = name == "Dwight"),
      family = "Outfit SemiBold", size = 2.5
    ) +
    scale_edge_width_continuous(
      name = "Share of scenes",
      range = c(0.1, 5),
      limits = c(0, max_share),
      breaks = seq(0.05, 0.3, 0.05),
      labels = scales::percent_format()) +
    scale_size_identity() +
    scale_fill_manual(values = pal_office) +
    scale_color_manual(values = c("black", "white")) +
    coord_cartesian(clip = "off") +
    guides(color = "none", fill = "none") +
    labs(subtitle = sprintf("Season %s", season))
  p
}

# Create a plot for each season
plots <- map2(graphs_per_season, seq_along(graphs_per_season), plot_graph)

# Combine plots
(plots[[1]] + plots[[2]] + plots[[3]]) / 
  (plots[[4]] + plots[[5]] + plots[[6]]) /
  (ggplot() + plots[[7]] + ggplot()) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Michael Scott's Network in The Office",
    subtitle= "The thickness of the edges is proportional to the share of scenes
    the characters appear together with Michael. 
    The top 5 characters per season are shown.",
    caption = "Source: kaggle.com, The Office (US) - Complete Dialogue/Transcript.
    Visualization: Ansgar Wolsing") & 
  theme(plot.margin = margin(t = 20, b = 12, l = 20, r = 20)) 
ggsave(here(base_path, "17-networks.png"),
       width = 5, height = 6, scale = 1.4, dpi = 500)



