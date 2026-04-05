library(tidyverse)
library(ggtext)
library(gganimate)
library(here)

df <- tibble(
  type = c("E-Bike", "Bike"),
  n = c(81L, 32L)
)


df_plot <- df |> 
  mutate(max_n = max(n)) |> 
  uncount(max_n, .remove = FALSE) |>
  mutate(
    type,
    id_scene = row_number(),
    id = ifelse(id_scene > n, n, id_scene),
    img = sprintf("<img src='%s' width='%d'>",
      file.path("2026", "05", ifelse(type == "E-Bike", "e-bike.png", "bike.png")),
      ifelse(type == "E-Bike", 45, 50)
    ),
    label_pos_x = ifelse(id_scene == max(id_scene), n, ""),
    .by = type
  ) |> 
  select(-max_n)


p_anim <- df_plot |> 
  ggplot() +
  geom_segment(
    aes(x = 0, xend = id, y = type, yend = type),
    linewidth = 6, col = "white"
  ) +
  geom_vline(
    data = data.frame(x = seq(10, max(df_plot$id_scene) %/% 10 * 10, 10)),
    aes(xintercept = x),
    linewidth = 1.5, col = "#EFCA08"
  ) +
  geom_richtext(
    aes(x = id, y = type, label = img),
    fill = NA, label.size = 0, hjust = 0, 
    nudge_x = 1
  ) +
  geom_richtext(
    aes(x = 0, y = type, label = sprintf("%d %ss", n, type)),
    stat = "unique", col = "black", size = 10, 
    nudge_y = 0.2, hjust = 0, vjust = 0,
    family = "Fira Sans Medium",
    fill = NA, label.size = 0, label.padding = unit(0, "mm")
  ) +
  scale_x_continuous(
    expand = expansion(add = c(1, 12)),
    breaks = seq(10, 1000, 10)) +
  labs(
    title = "Power to the Pedal",
    subtitle = "On a short 5 km bike ride between Noordwelle and Port Zélande (NL),
    more than two in three bikes are motorized.",
    caption = "Data collected on a bike ride between Noordwelle and Port Zélande, April 4th, 2026.<br>
      <span style='font-family:\"Fira Sans SemiBold\"'>Image credit:</span> Freepik.
      <span style='font-family:\"Fira Sans SemiBold\"'>Visualization:</span> Caspar & Ansgar Wolsing"
  ) +
  theme_void(base_family = "Fira Sans Light", base_size = 14, 
    paper = "#EFCA08"
  ) +
  theme(
    plot.title = element_text(
      family = "Fira Sans", size = 36),
    plot.subtitle = element_textbox(
      lineheight = 1.25, width = 0.98,
      margin = margin(t = 4, b = 15)
    ),
    plot.caption = element_textbox(
      width = 0.9, hjust = 0, lineheight = 1.6,
      margin = margin(t = -20)),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  transition_states(id_scene)

animate(p_anim, width = 1000, height = 720, res = 150, units = "px", end_pause = 20)
anim_save(here("2026", "05", "05-experimental.gif"))
