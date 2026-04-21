library(tidyverse)
library(ggtext)
library(geomtextpath)
library(grid)
library(here)

#' Source: Destatis
#' https://www-genesis.destatis.de/datenbank/online/url/68d5bfb0

df <- read_csv2(here("2026", "21", "51000-0008_de_flat.csv"), na = "...")

df_prep <- df |> 
  select(
    year = time,
    month = `1_variable_attribute_code`,
    import_export = value_variable_label, value) |> 
  mutate(
    import_export = if_else(import_export == "Ausfuhr: Gewicht", "Export", "Import"),
    month_date = ym(paste(year, str_extract(month, "(\\d{2})", group = 1)))
  ) |> 
  arrange(month_date) 

df_prep_annual <- df_prep |> 
  filter(year < 2026) |> 
  group_by(year, import_export) |> 
  summarize(value = sum(value), .groups = "drop")

df_prep_annual_wide <- df_prep_annual |> 
  pivot_wider(id_cols = year, names_from = import_export, values_from = value)

df_prep |> 
  ggplot(aes(month_date, value, col = import_export)) +
  geom_line()

df_prep_annual |> 
  ggplot(aes(year, value, col = import_export)) +
  geom_line()


color_pal <- c("Export" = "#F2B705", "Import" = "#1B2631")
base_font_family <- "Abhaya Libre"
font_color <- "#564D46"
line_color <- "#ABA098"

# Custom y axis labels
min(df_prep_annual$value)
max(df_prep_annual$value)

axis_breaks_y <- seq(2e4, 1e5, 2e4)
axis_labels_y <- scales::number(axis_breaks_y)

# Write labels following a line
geom_textline2 <- function(..., stat = "unique", linecolor = NA, 
                           color = "#554C49", family = paste(base_font_family, "Bold"),
                           size = 5) {
  geom_textline(...,
                stat = stat, linecolor = linecolor, color = color, alpha = 0.87,
                family = family, size = size)
}


p <- df_prep_annual_wide |> 
  ggplot(aes(year, group = 1)) +
  geom_ribbon(
    aes(ymin = Export, ymax = Import, fill = ifelse(Export > Import, "Export", "Import")),
    alpha = 0.2
  ) +
  geom_line(
    aes(y = Export, col = "Export"),
    linewidth = 1.5) +
  geom_line(
    aes(y = Import, col = "Import"),
    linewidth = 1.5) +
  geom_textline2(
    aes(y = Export, label = "Line of Exports"),
    col = color_pal["Export"], vjust = -0.2, hjust = 0.5) +
  geom_textline2(
    aes(y = Import, label = "Line of Imports"),
    col = color_pal["Import"], vjust = 1.2, hjust = 0.5) +
  annotate(
    "richtext", x = 2007, y = 51e3,
    label = "BALANCE</span> 
    <i style='font-size: 7pt'>in</i> <br>
    FAVOUR
    <i style='font-size: 7pt'>of</i> <br>
    GERMANY", 
    size = 3, family = "Abhaya Libre",
    hjust = 0, vjust = 1, color = "#554C49",
    label.size = 0, fill = NA, lineheight = 0.9
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(
    position = "right", breaks = axis_breaks_y,
    labels = axis_labels_y) +
  scale_color_manual(values = color_pal) +
  scale_fill_manual(values = color_pal) +
  coord_cartesian(clip = "off") +
  guides(fill = "none", color = "none") +
  labs(
    title = "German Cheese Exports and Imports to and from 
    <span style='font-size: 14pt'>AUSTRIA</span> from 2001 to 2025.",
    caption = paste("The Bottom line is divided into Years,
    the Right hand line into 20,000 tonnes each.",
                    "<br><span style='font-size:8pt'>Source: Destatis",
                    "<span style='color:transparent'>",
                    paste(rep(".", 65), collapse = ""),
                    "</span>",
                    "Visualization: Ansgar Wolsing</span>")
  ) +
  theme_minimal(base_family = base_font_family, base_size = 9) +
  theme(
    plot.background = element_rect(color = NA, fill = "#FEFEFF"),
    axis.title = element_blank(),
    axis.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = line_color, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    text = element_text(color = font_color),
    plot.title = element_markdown(face = "bold", hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.6, size = 9, lineheight = 0.8,
                                    family = "Charm", face = "bold",
                                    margin = margin(t = 8)),
    plot.margin = margin(t = 16, b = 8, l = 12, r = 12))


# Add borders around the plot
ragg::agg_png(here("2026", "21", "21-historical.png"), 
  width = 6, height = 4, units = "in", res = 300)
p
grid::grid.rect(
  x = 0.5, y = 0.5,
  width = 0.995, height = 0.99, 
  gp = gpar(col = "#4B4543", fill = "transparent", lwd = 3))
grid::grid.rect(
  x = 0.5, y = 0.5,
  width = 0.98, height = 0.97, 
  gp = gpar(col = "#4B4543", fill = "transparent", lwd = 0.9))
dev.off()
