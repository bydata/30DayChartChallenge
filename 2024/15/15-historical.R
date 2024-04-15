library(tidyverse)
library(ggtext)
library(grid)
library(geomtextpath)
library(here)

base_path <- here("2024", "15")

# width to height
aspect_ratio <- 1280 / 946

# data collected manually from the original plot
df <- tibble(
  year = seq(1700, 1780, 5),
  imports = c(70, 75, 82,  87,  97, 102,  98,  93,  93, 
              92, 90, 79,  79,  80,  84,  91,  91),
  exports = c(35, 41, 59,  78,  76,  73,  64,  60, 66,
              75, 78, 82, 120, 149, 163, 179, 187)
)

# apply lowess smoothing to the import / export data 
smooth_bw <- 0.25
df_smoothed <- tibble(
  year = seq(min(df$year), max(df$year), 1),
  imports = predict(loess(imports ~ year, data = df, span = smooth_bw), 
                    data.frame(year = seq(min(df$year), max(df$year), 1)), se = FALSE),
  exports = predict(loess(exports ~ year, data = df, span = smooth_bw), 
                    data.frame(year = seq(min(df$year), max(df$year), 1)), se = FALSE)
)

base_font_family <- "Abhaya Libre"
line_color <- "#ABA098"
font_color <- "#564D46" # "#45403E"

axis_labels_y <- as.character(seq(10, 190, 10))
axis_labels_y[axis_labels_y == "100"] <- "100,000"

geom_textline2 <- function(..., stat = "unique", linecolor = NA, 
                           color = "#554C49", family = base_font_family, 
                           fontface = "bold", size = 3) {
  geom_textline(...,
                stat = stat, linecolor = linecolor, color = color, alpha = 0.87,
                family = family, size = size)
}


p_base <- p <- df_smoothed %>% 
  ggplot(aes(year)) +
  scale_x_continuous(breaks = seq(1700, 1780, 10)) +
  labs(
    title = "Exports and Imports to and from 
    <span style='font-size: 14pt'>DENMARK & NORWAY</span> from 1700 to 1780.",
    caption = paste("The Bottom line is divided into Years,
    the Right hand line into L10,000 each.",
                    "<br><span style='font-size:4pt'>Published as the Act directs, 14<sup>t</sup> May 1786, by W. Playfair",
                    "<span style='color:transparent'>",
                    paste(rep(".", 200), collapse = ""),
                    "</span>",
                    "Neele Sculpt, 352, Strand, London</span>")) +
  theme_minimal(base_family = base_font_family, base_size = 9) +
  theme(
    plot.background = element_rect(color = NA, fill = "#FEFEFF"),
    axis.title = element_blank(),
    axis.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = line_color, size = 0.3),
    panel.grid.minor = element_blank(),
    text = element_text(color = font_color),
    plot.title = element_markdown(face = "bold", hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.6, size = 9, lineheight = 0.8,
                                    family = "Charm", face = "bold",
                                    margin = margin(t = 8)),
    plot.margin = margin(t = 16, b = 8, l = 12, r = 12))


ragg::agg_png(here(base_path, "03-historical-wframe.png"), res = 300, units = "in",
    width = 6, height = 6 / aspect_ratio)
p <- p_base +
  geom_hline(yintercept = 100, size = 1, col = "#ABA098") +
  # Highlighted area
  geom_ribbon(
    data = . %>% filter(year <= 1755),
    aes(x = year, ymin = exports, ymax = imports),
    fill = "#F2DBD8", alpha = 0.9) +
  geom_ribbon(
    data = . %>% filter(year >= 1755),
    aes(x = year, ymin = exports, ymax = imports),
    fill = "#E5D9B9", alpha = 0.9) +
  # smoothed lines (thick coloured lines + thin grey lines)
  geom_line(aes(y = exports),
              size = 1.5, alpha = 0.8, col = "#A3555B") +
  geom_line(aes(y = imports),
              size = 1.5, alpha = 0.8, col = "#D8A962") +
  geom_line(aes(y = exports),
              size = 0.2, alpha = 0.2, col = "grey40") +
  geom_line(aes(y = imports),
              size = 0.2, alpha = 0.2, col = "grey40") +
  # Annotations
  geom_textline2(aes(y = imports, label = "Line of Imports"),
                 vjust = 0, hjust = 0.3) +
  geom_textline2(aes(y = exports, label = "Line of Exports"),
                 vjust = 1, hjust = 0.3) +
  geom_textline2(aes(y = imports, label = "Imports"),
                 vjust = 1, hjust = 0.95) +
  geom_textline2(aes(y = exports, label = "Exports"),
                 vjust = 0, hjust = 0.85) +
  geom_textline2(aes(y = exports, label = "BALANCE AGAINST"),
                 vjust = -1.5, hjust = 0.38, size = 3.5
  ) +
  annotate("richtext", x = 1765, y = 140, 
           label = "BALANCE</span> 
           <i style='font-size: 7pt'>in</i> <br>
           FAVOUR
           <i style='font-size: 7pt'>of</i> <br>
           ENGLAND.", 
           size = 4, family = "Abhaya Libre",
           hjust = 0, vjust = 1,  color = "#554C49", label.size = 0, fill = NA) +
  scale_y_continuous(position = "right", breaks = seq(10, 190, 10),
                     labels = axis_labels_y) +
  coord_cartesian(ylim = c(0, 200), expand = FALSE, clip = "off")

p + annotation_custom(
  rectGrob(gp = gpar(col = "#4B4543", fill = NA, lwd = 3)),
  xmin = 1699.5, xmax = 1787, ymin = -2, ymax = 202
) + annotation_custom(
  rectGrob(gp = gpar(col = "#4B4543", fill = NA, lwd = 0.5)),
  xmin = 1700, xmax = 1786.5, ymin = 0, ymax = 200
)
invisible(dev.off())


## Version showing the difference between exports and imports ------------------

ragg::agg_png(here(base_path, "03-historical-difference-wframe.png"), res = 300, units = "in",
              width = 6, height = 6 / aspect_ratio)
p_remake <- p_base +
  geom_hline(yintercept = 0, size = 1, col = "#ABA098") +
  geom_area(
    aes(y = exports - imports, fill = exports - imports > 0),
    alpha = 0.9) +
  geom_line(
    aes(y = exports - imports),
    size = 1.2, alpha = 0.8, col = "grey40") +
  geom_textline2(aes(y = exports - imports, label = "BALANCE AGAINST"),
                 vjust = -1.5, hjust = 0.38, size = 3.5
  ) +
  annotate("richtext", x = 1765, y = 60, 
           label = "BALANCE</span> 
           <i style='font-size: 7pt'>in</i> <br>
           FAVOUR
           <i style='font-size: 7pt'>of</i> <br>
           ENGLAND.", 
           size = 4, family = "Abhaya Libre", fontface = "italic",
           hjust = 0, vjust = 1,  color = "#554C49", label.size = 0, fill = NA) +
  scale_y_continuous(position = "right", breaks = seq(-30, 90, 10)) +
  coord_cartesian(expand = FALSE, clip = "off", ylim = c(-38, 100)) +
  scale_fill_manual(values = c("FALSE" = "#F2DBD8", "TRUE" = "#E5D9B9")) +
  guides(fill = "none") 

p_remake + annotation_custom(
  rectGrob(gp = gpar(col = "#4B4543", fill = NA, lwd = 3)),
  xmin = 1699.5, xmax = 1784, ymin = -39, ymax = 101
) + annotation_custom(
  rectGrob(gp = gpar(col = "#4B4543", fill = NA, lwd = 0.5)),
  xmin = 1700, xmax = 1783.5, ymin = -37.5, ymax = 99.5
)
invisible(dev.off())
