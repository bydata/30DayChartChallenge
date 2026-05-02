library(tidyverse)
library(ggtext)
library(jsonlite)
library(here)

# Source: https://github.com/cosmos-book/cosmos-book.github.io/tree/master/human-spaceflight/data/processed
df_astronauts <- read_tsv(
  "https://raw.githubusercontent.com/cosmos-book/cosmos-book.github.io/refs/heads/master/human-spaceflight/data/processed/astronauts.tsv",
  name_repair = janitor::make_clean_names  
)

df_missions <- read_json("https://raw.githubusercontent.com/cosmos-book/cosmos-book.github.io/refs/heads/master/human-spaceflight/data/processed/missions.json") |> 
  bind_rows(.id = "mission") |> 
  add_row(mission = "Soyuz MS-23", launch = "2023-02-24T00:00:00Z", land = "2023-09-27T00:00:00Z")

df_astronauts_missions <- df_astronauts |> 
  select(name, country, gender, number_of_launches, missions) |> 
  filter(number_of_launches > 0) |> 
  separate_longer_delim(cols = missions, delim = ";") |> 
  rename(mission = missions) |> 
  inner_join(df_missions, by = "mission") |> 
  arrange(launch) |> 
  mutate(decade_of_mission = paste0(as.integer(str_sub(launch, 1, 4)) %/% 10 * 10, "s"))

df_decades <- df_astronauts_missions |> 
  count(decade_of_mission, gender) |> 
  mutate(
    share = n / sum(n), 
    total_n_decade = sum(n),
    .by = decade_of_mission)


df_plot <- df_decades |> 
  nest(-c(decade_of_mission, total_n_decade)) |> 
  mutate(
    xmin = lag(cumsum(total_n_decade), 1, default = 0),
    xmax = cumsum(total_n_decade),
    xcenter = xmin + (xmax - xmin) / 2
  ) |> 
  unnest(cols = data) |> 
  group_by(decade_of_mission) |> 
  mutate(
    ymin = lag(cumsum(share), 1, default = 0),
    ymax = cumsum(share)
  ) |> 
  ungroup() 


bg_color <- "white"
df_plot |> 
  mutate(decade_of_mission = ifelse(decade_of_mission == "2020s", "2020s<sup>1</sup>", decade_of_mission)) |> 
  ggplot() +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    fill = gender), col = bg_color, linewidth = 0.5
  ) +
  geom_vline(
    aes(xintercept = xmax),
    col = bg_color, linewidth = 1.25
  ) +
  geom_richtext(
    data = ~distinct(., decade_of_mission, total_n_decade, xcenter),
    aes(
      x = xcenter, y = 1, 
      label = sprintf("**%s**<br>(%d)", decade_of_mission, total_n_decade)
    ), # Achtung: Es sind alle Missionen, d.h. ein Astronaut, der in der 1960er Jahren zweimal im All war, ist hier zweimal drin
      vjust = 0, family = "Instrument Sans", nudge_y = 0.01, lineheight = 1.05,
      fill = NA, label.size = 0, label.padding = unit(0, "mm")
  ) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)), labels = scales::label_percent()) +
  scale_fill_manual(values = c("Female" = "#730C6E", "Male" = "#F4BB3B")) +
  coord_cartesian(clip = "off") +
  guides(fill = "none") +
  labs(
    title = "",
    caption = "<sup>1</sup> 2020 to 2023<br><br>
      **Source:** Stuart Lowe and Chris North (CC-BY-SA). **Visualization:** Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(paper = bg_color, ink = "grey8", base_family = "Instrument Sans") +
  theme(
    plot.caption = element_markdown(hjust = 0),
    panel.grid = element_blank()
  )


n_stars <- df_astronauts |> 
  filter(gender == "Female", number_of_launches > 0) |> 
  nrow()
set.seed(43)
df_random_stars <- data.frame(
  x = runif(n_stars, min(df_plot$xmax), max(df_plot$xmax)),
  y = runif(n_stars, 0.2, 1.0),
  size = filter(df_astronauts, gender == "Female", number_of_launches > 0)
    |> pull(number_of_launches)
)

bg_color <- "grey4"
p <- df_plot |> 
  mutate(decade_of_mission = ifelse(decade_of_mission == "2020s", "2020s<sup>1</sup>", decade_of_mission)) |> 
  ggplot() +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    fill = gender), col = bg_color, linewidth = 0.5
  ) +
  geom_text(
    data = ~filter(., gender == "Female" & decade_of_mission >= "1980s") |> 
      mutate(
        label = scales::percent(share, accuracy = 1),
        label = ifelse(
          decade_of_mission == "1990s",
          paste(label, "female crew members"),
          label)),
    aes(xcenter, share / 2, 
      label = label),
    family = "Instrument Sans SemiBold",
    col = "grey4"
  ) +
  geom_vline(
    aes(xintercept = xmax),
    col = bg_color, linewidth = 1.25
  ) +
  ggfx::with_outer_glow(
    geom_point(
      data = df_random_stars,
      aes(x, y, size = size)
    ),
    col = "#FFFF00CC", sigma = 7, expand = 1
  ) +
  geom_richtext(
    data = ~distinct(., decade_of_mission, total_n_decade, xcenter),
    aes(
      x = xcenter, y = 1, 
      label = sprintf("**%s**<br>(%d)", decade_of_mission, total_n_decade)
    ),
    vjust = 0, family = "Instrument Sans", nudge_y = 0.01, lineheight = 1.05,
    fill = NA, label.size = 0, label.padding = unit(0, "mm"),
    col = "white", size = 3.25
  ) +
  annotate(
    GeomTextBox,
    x = 75, y = 0.15,
    label = "Valentina Tereshkova was the first woman in space (1963)",
    width = 0.2, size = 3, col = "grey90", fill = NA, box.size = 0, hjust = 0,
    family = "Instrument Sans Medium"
  ) +
  annotate(
    GeomCurve,
    x = 75, xend = 33, y = 0.14, yend = 0.025,
    col = "grey90", linewidth = 0.2,
    curvature = 0.2,
    arrow = arrow(angle = 25, length = unit(0.15, "cm"))
  ) +
  annotate(
    GeomTextBox,
    x = 1250, y = 0.38,
    label = "One star for each woman in space",
    width = 0.15, size = 3, col = "grey90", fill = NA, box.size = 0, 
    hjust = 0, family = "Instrument Sans Medium"
  ) +
  annotate(
    GeomCurve,
    x = 1300, xend = 1290, y = 0.35, yend = 0.30,
    col = "grey90", linewidth = 0.2,
    curvature = 0.2,
    arrow = arrow(angle = 25, length = unit(0.15, "cm"))
  ) +
  annotate(
    GeomTextBox,
    x = 710, y = 1.18,
    label = "The width of the segments is proportional to 
      the number of crew members in a decade",
    width = 0.25, size = 3, col = "grey90", fill = NA, box.size = 0, hjust = 0,
    family = "Instrument Sans Italic"
  ) +
  annotate(
    GeomCurve,
    x = 750, xend = 750, y = 1.12, yend = 1.01,
    col = "grey90", linewidth = 0.2,
    curvature = 0.2,
    arrow = arrow(angle = 25, length = unit(0.15, "cm"))
  ) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)), labels = scales::label_percent()) +
  scale_fill_manual(values = c("Female" = "#FFFFFFDD", "Male" = "#EEEEEE33")) +
  scale_size_continuous(range = c(0.05, 0.5)) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  guides(size = "none") +
  labs(
    title = "Space, still mostly male",
    subtitle = "The proportion of female astronauts among the crew members on 
    space missions has risen slowly over the past few decades.
    Nevertheless, male crew members still make up a large majority.
    <br><br><i>Share of crew members (in %)</i>",
    caption = "<sup>1</sup> 2020 to 2023<br><br>
      **Source:** Stuart Lowe and Chris North (CC-BY-SA). **Visualization:** Ansgar Wolsing",
    x = "Total number of crew members", y = NULL,
    fill = "Gender"
  ) +
  theme_minimal(paper = bg_color, ink = "white", base_family = "Instrument Sans") +
  theme(
    plot.title = element_text(
      family = "Instrument Sans", size = 28),
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.25,
      margin = margin(t = 8, b = 40)),
    plot.caption = element_markdown(hjust = 0),
    plot.margin = margin(t = 10, r = 6, b = 6, l = 6),
    panel.grid = element_blank(),
    legend.position = "bottom",
    # legend.justification = "left"
    # legend.position = "inside",
    # legend.position.inside = c(0.85, 1.15),
    # legend.direction = "horizontal",
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave(here("2026", "03", "03-mosaic.png"), width = 8, height = 7.2)
