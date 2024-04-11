library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "10")
  
  
#' Source: Destatis
#' https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Gesundheit/Gesundheitszustand-Relevantes-Verhalten/Tabellen/liste-koerpermasse.html
df <- tribble(
  ~age_group, ~female, ~male,
  "18 to 20",167.6,181.7,
  "20 to 25",167.5,181.4,
  "25 to 30",167.3,180.4,
  "30 to 35",167.2,180.4,
  "35 to 40",167.3,180.2,
  "40 to 45",167.5,180.2,
  "45 to 50",167.1,180.2,
  "50 to 55",167.1,179.9,
  "55 to 60",166.9,179.4,
  "60 to 65",165.4,178.4,
  "65 to 70",164.5,177.4,
  "70 to 75",163.9,176,
  "75 and above",162.6,174.6
)

gradient_fill <- grid::linearGradient(c("#FAF1E3", "#FFFFFF"), group = FALSE) 

measure_min = 160
measure_max = 186

measure <- list(
  min = measure_min,
  max = measure_max,
  big_marks = seq(measure_min, measure_max, 10),
  cm_marks = seq(measure_min, measure_max, 1),
  mm5_marks = seq(measure_min, measure_max, 0.5),
  mm_marks = seq(measure_min, measure_max, 0.1),
  bg_color = "#FAE458",
  mark_color = "grey20",
  xmin = 3.0,
  xmax = 5.0
)

ys_plot <- ggplot() +
  annotate(
    "rect",
    xmin = measure$xmin, xmax = measure$xmax,
    ymin = 0, ymax = measure$max - measure$min,
    fill = measure$bg_color, color = measure$mark_color, size = 0.1
  ) +
  # mm left and right
  annotate(
    "segment",
    x = measure$xmin, xend = measure$xmin + 0.2,
    y = measure$mm_marks - measure$min,
    color = measure$mark_color, size = 0.1
  ) +
  annotate(
    "segment",
    x = measure$xmax, xend = measure$xmax - 0.2,
    y = measure$mm_marks - measure$min,
    color = measure$mark_color, size = 0.1
  ) +
  # 5 mm marks left and right
  annotate(
    "segment",
    x = measure$xmin, xend = measure$xmin + 0.25,
    y = measure$mm5_marks - measure$min,
    color = measure$mark_color, size = 0.2
  ) +
  annotate(
    "segment",
    x = measure$xmax, xend = measure$xmax - 0.25,
    y = measure$mm5_marks - measure$min,
    color = measure$mark_color, size = 0.2
  ) +
  # cm marks 
  annotate(
    "segment",
    x = measure$xmin, xend = measure$xmax,
    y = measure$cm_marks - measure$min,
    color = measure$mark_color, size = 0.3
  ) +
  # cm labels
  annotate(
    "text",
    x = measure$xmin + (measure$xmax - measure$xmin) / 2,
    y = measure$cm_marks[seq_along(measure$cm_marks) - 1] - measure$min,
    label = measure$cm_marks[seq_along(measure$cm_marks) - 1],
    family = "Roboto Condensed", fontface = "bold", size = 4, hjust = 0.5, vjust = -0.1
  ) +
  coord_cartesian(xlim = c(0, 8), expand = FALSE, clip = "off")

ys_plot +
  # text labels
  annotate(
    "richtext",
    x = measure$xmin - 0.5, y = measure$max - measure$min - 1,
    label = "**<span style='color:#6200EE'>Female</span> height**<br>by age group",
    family = "Libre Franklin", hjust = 1, lineheight = 0.9,
    label.size = 0, fill = NA
  ) +
  annotate(
    "richtext",
    x = measure$xmax + 0.5, y = measure$max - measure$min - 1,
    label = "**<span style='color:#03DAC5'>Male</span> height**<br>by age group",
    family = "Libre Franklin", hjust = 0, lineheight = 0.9,
    label.size = 0, fill = NA
  ) +
  geom_point(
    data = df,
    aes(x = measure$xmin + 0.2, y = female - measure$min, fill = "female"),
    shape = 21, color = "white", size = 3,
    position = position_jitter(width = 0.1, height = 0, seed = 1)
  ) +
  # male heights
  geom_point(
    data = df,
    aes(x = measure$xmax - 0.2, y = male - measure$min, fill = "male"),
    shape = 21, color = "white", size = 3,
    position = position_jitter(width = 0.1, height = 0, seed = 1)
  ) +
  # Data labels for female heights
  geom_text(
    data = subset(df, !age_group %in% df$age_group[2:8]),
    aes(x = measure$xmin - 0.1, y = female - measure$min, label = age_group),
    hjust = 1, family = "Roboto Condensed", size = 2.25
  ) +
  # Data labels for male heights
  geom_text(
    data = subset(df, !age_group %in% df$age_group[3:6]),
    aes(x = measure$xmax + 0.1, y = male - measure$min, label = age_group),
    hjust = 0, family = "Roboto Condensed", size = 2.25
  ) +
  annotate(
    "text",
    measure$xmin + (measure$xmax - measure$xmin) / 2,
    y = -0.5,
    label = "average height (cm)", hjust = 0.5,
     family = "Libre Franklin SemiBold", size = 3
  ) +
  scale_fill_manual(values = c("#6200EE", "#03DAC5")) +
  guides(fill = "none") +
  labs(
    title = "Younger people in Germany outgrow their elders ",
    caption = "**Source:** Destatis, Mikrozensus 2021.
    The Mikrozensus is an annual sample of 1% of the population in
    Germany conducted by the Federal Statistical Office.
    Body weight and height are collected in a subsample that covers up to 
    45 % of the overall sample. 
    **Visualization:** Ansgar Wolsing."
  ) +
  theme_void(base_family = "Libre Franklin") +
  theme(
    plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
    plot.title = element_markdown(
      family = "Libre Franklin SemiBold", size = 16, hjust = 0.5,
      margin = margin(b = 14)),
    plot.caption = element_textbox(
      width = 0.95, lineheight = 1, hjust = 0.5, margin = margin(t = 12)),
    plot.margin = margin(t = 10, b = 8, l = 5, r = 5)
  )
ggsave(here(base_path, "10-physical-measure-tape.png"), width = 5, height = 5,
       scale = 1.25)
