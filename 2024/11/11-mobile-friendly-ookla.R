library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "11")

#' Source: Ookla https://www.speedtest.net/global-index
df <- read_tsv(here(base_path, "ookla-mobile-speed-202402.tsv"),
               na = "-")

gradient_fill_bg <- grid::linearGradient(c("#F2F7FE", "#FFFFFF"), group = FALSE)
gradient_fill_bar <- grid::linearGradient(c("#4527A0", "#5E35B1"), group = FALSE)

p_smartphone <- ggplot() +
  ggfx::with_shadow(
    annotate(
      ggchicklet:::GeomRrect,
      xmin = 0, 
      xmax = 10,
      ymin = 0, 
      ymax = 20.7, 
      fill = "grey60",
      radius = unit(4, "mm")
    ),
    sigma = 6, colour = "grey30", x_offset = 4, y_offset = 4
  ) +
  annotate(
    ggchicklet:::GeomRrect,
    xmin = 0.1, 
    xmax = 9.9,
    ymin = 0.1, 
    ymax = 20.6,
    fill = "black",
    radius = unit(3.5, "mm")
  ) +
  annotate(
    ggchicklet:::GeomRrect,
    xmin = 0.4, 
    xmax = 9.6,
    ymin = 0.4, 
    ymax = 20.3,
    fill = "#FDFDFD",
    radius = unit(2, "mm")
  ) +
  coord_equal(xlim = c(-1, 22)) +
  theme_void() +
  theme(
    plot.background = element_rect(color = gradient_fill_bg, fill = gradient_fill_bg)
  )

bar_ymin <-  0.4
bar_ymax <- 20.3

min_mbps <- min(df$mbps)
max_mbps <- max(df$mbps)
selected_country <- "Germany"
df_subset <- subset(df, country == selected_country | mbps == min(mbps) | mbps == max(mbps))

p_smartphone +
  annotate(
    "rect",
    xmin = 6, xmax = 9,
    ymin = bar_ymin, ymax = bar_ymax,
    fill = gradient_fill_bar
  ) +
  # Marks for all countries
  geom_segment(
    data = df,
    aes(
      x = 6, xend = 9,
      y = 1 + mbps / max(mbps) * (bar_ymax - 3), 
      yend = 1 + mbps / max(mbps) * (bar_ymax - 3),
      color = country == selected_country),
    linewidth = 0.1
  ) +
  # Marks for selected country + min + max
  geom_segment(
    data = df_subset,
    aes(
      x = 6, xend = 9,
      y = 1 + mbps / max(mbps) * (bar_ymax - 3), 
      yend = 1 + mbps / max(mbps) * (bar_ymax - 3),
      color = country == selected_country,
      linewidth = country == selected_country)
  ) +
  # Labels for selected country + min + max
  geom_text(
    data = subset(df_subset, country != selected_country),
    aes(
      x = 5.8, y = 1 + mbps / max(df_subset$mbps) * (bar_ymax - 3),
      label = paste(country, rank, sep = " | #"),
    ),
    color = "grey20", 
    family = "Roboto Condensed", fontface = "bold", size = 3.5, hjust = 1
  ) +
  geom_label(
    data = subset(df_subset, country == selected_country),
    aes(
      x = 5.8, y = 1 + mbps / max(df_subset$mbps) * (bar_ymax - 3),
      label = paste(country, rank, sep = " | #")
    ),
    family = "Roboto Condensed", fontface = "bold", size = 3.5, hjust = 1,
    label.r = unit(0, "mm"), color = "white", fill = "#ee0290"
  ) +
  # # Data labels for the connection speed
  # geom_richtext(
  #   data = df_subset,
  #   aes(
  #     x = 7.5, y = 1 + mbps / max(mbps) * (bar_ymax - 3),
  #     label = sprintf("**%s**<br>mbit/s", round(mbps))
  #   ),
  #   family = "Roboto Condensed", lineheight = 0.8, color = "grey8",
  #   size = 3.25, fill = "#FFFFFFAA", vjust = 0, hjust = 0.5, label.size = 0,
  #   label.padding = unit(0.7, "mm")
  # ) +
  # Title
  annotate(
    GeomTextBox,
    x = 10.5, y = 20,
    label = "Mobile internet speed",
    hjust = 0, vjust = 1, family = "Libre Franklin SemiBold", size = 6,
    fill = NA, box.size = 0, width = 0.5
  ) +
  # Subtitle
  annotate(
    GeomTextBox,
    x = 10.5, y = 18.75,
    label = "(Download)",
    hjust = 0, vjust = 1, family = "Libre Franklin",
    size = 3.5, fill = NA, box.size = 0, width = 0.5
  ) +
  # Description
  annotate(
    GeomTextBox,
    x = 10.5, y = 16,
    label = sprintf("<b style='color:#ee0290'>%s</b> is ranked
    <b style='color:#ee0290'>%d<sup>th</sup></b> out of %d countries in terms of
    mobile internet speed (download) based on the Ookla speedtest taken by users.", 
                  selected_country, 
                  df$rank[df$country == selected_country],
                  nrow(df)),
    hjust = 0, vjust = 1, family = "Libre Franklin", size = 3.5,
    fill = NA, box.size = 0, width = 0.45, lineheight = 1.33
  ) +
  # Caption
  annotate(
    GeomTextBox,
    x = 10.5, y = 0,
    label = "**Source:** Ookla Global Speedtest Index<sup>TM</sup>,
    data from 02/2024.<br>
    **Visualization:** Ansgar Wolsing",
    hjust = 0, vjust = 0, family = "Libre Franklin", size = 2.5,
    fill = NA, box.size = 0, width = 0.5
  ) +
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "#ee0290")) +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#ee0290")) +
  scale_linewidth_manual(values = c("FALSE" = 0.9, "TRUE" = 1.5)) +
  guides(color = "none", linewidth = "none", fill = "none")
ggsave(here(base_path, "11-mobile-friendly-speed.png"), width = 5, height = 5,
       scale = 1.2, bg = gradient_fill_bg)
