library(tidyverse)
library(ggtext)
library(tidygraph)
library(ggraph)
library(patchwork)
library(here)

df <- read_csv2(here("2026", "14", "51000-0007_de_flat.csv"), na = "-")

n_countries <- 10


df_prep <- df |> 
  select(
    country2 = `2_variable_attribute_label`,
    import_export = value_variable_label, value) |> 
  filter(value > 0, !country2 %in% c("Schiffs- und Luftfahrzeugbedarf", 
    "Nicht ermittelte Länder und Gebiete")) |> 
  mutate(
    import_export = if_else(import_export == "Ausfuhr: Gewicht", "Export", "Import"),
    country2 = countrycode::countrycode(
      country2, "country.name.de", "country.name.en"),
  )

# Determine the top N countries in import and export separately
top_countries <- map(
  c("Import", "Export"),
  function(x) {
    df_prep |> 
  filter(import_export == x) |> 
  slice_max(value, n = n_countries) |> 
  pull(country2)
  }
)
top_countries_combined <- unique(as.character(flatten(top_countries)))


color_pal <- c("Export" = "#F2B705", "Import" = "#1B2631")


df_prep_graph <- df |> 
  select(
    country1 = `1_variable_attribute_label`, country2 = `2_variable_attribute_label`,
    import_export = value_variable_label, value) |> 
  # filter(value > 0, !country2 %in% c("Schiffs- und Luftfahrzeugbedarf", 
  #   "Nicht ermittelte Länder und Gebiete")) |> 
  # mutate(
  #   country2 = fct_lump_n(country2, n = n_countries, w = value), .by = import_export
  # ) |> 
  filter(value > 0) |> 
  mutate(
    country1 = "Germany",
    country2 = countrycode::countrycode(
      country2, "country.name.de", "country.name.en", custom_match = c("Other" = "Other")),
    country2 = ifelse(country2 %in% top_countries_combined, country2, "Other"),
    import_export = if_else(import_export == "Ausfuhr: Gewicht", "Export", "Import"),
    from_country = if_else(import_export == "Export", country1, country2),
    to_country  = if_else(import_export == "Import", country1, country2)
  ) |> 
  count(from_country, to_country, wt = value, name = "value") |>
  filter(from_country != "Other" & to_country != "Other") |> 
  arrange(-value)

graph <- df_prep_graph |> 
  as_tbl_graph(directed = TRUE) |> 
  activate("edges") |>
  mutate(import_export = ifelse(.N()$name[from] == "Germany", "Export", "Import")) |> 
  activate("nodes") |> 
  mutate(name = factor(name, levels = c("Germany", top_countries_combined))) |> 
  arrange(desc(name))


set.seed(123)
p_graph <- graph |> 
  ggraph(layout = "linear") +
  geom_edge_arc(
    aes(edge_width = value, col = import_export)
  ) +
  geom_node_point(
    shape = 21, col = "grey10", fill = "white", size = 3) +
  geom_node_label(
    aes(label = name), 
    repel = TRUE, direction = "x",
    family = "Instrument Sans Semibold",
    size = 2.5, label.size = 0, fill = "#FFFFFFAA", segment.size = 0.2) +
  scale_edge_width_continuous(
    range = c(0.1, 6),
    breaks = c(10, 50, 100, 200, 300) * 1000,
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  scale_edge_color_manual(values = color_pal) +
  coord_flip() +
  guides(
    edge_width = guide_legend(title.position = "top"),
    edge_color = "none") +
  labs(edge_width = "Weight (in tonnes)") +
  theme_void(base_family = "Instrument Sans", paper = "white") +
  theme(
    legend.position = "top"
  )
p_graph
 

## Dot plot  

p_dotplot <- df_prep |> 
  filter(country2 %in% top_countries_combined) |> 
  mutate(country2 = fct_reorder(country2, value, .fun = median)) |> 
  ggplot(aes(value, country2, fill = import_export)) +
  geom_line(
    aes(group = country2),
    col = "grey60"
  ) +
  geom_point(
    shape = 21, col = "grey2", size = 3, alpha = 0.8
  ) +
  annotate(
    "richtext",
    x = c(20e4, 27.5e4),
    y = c(11.5, 10.5),
    label = 
       c("Germany <b style='color:#F2B705'>exports</b><br>most to Italy", 
        "... and <b style='color:#1B2631'>imports</b><br>most from the<br>Netherlands"),
    fill = "#FFFFFFaa", lineheight = 0.8,
    family = "Instrument Sans", size = 2.5, hjust = 0, vjust = 1,
    label.size = 0
  ) +
  annotate(
    GeomCurve,
    x = c(23e4, 31e4),
    xend = c(24.6e4, 32.4e4),
    y = c(11.5, 10.7),
    yend = c(11.8, 12.8),
    linewidth = 0.3, linetype = "dotted", col = "grey30",
    curvature = 0.1
  ) +
  scale_x_continuous(
    labels = c("0", "100K", "200K", "300k tonnes"),
    position = "top"
  ) +
  scale_fill_manual(values = color_pal) +
  labs(
    # x = "Weight (tonnes)", 
    x = NULL, y = NULL, fill = " "
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    axis.text.y = element_text(family = "Instrument Sans Medium"),
    panel.grid.major.y = element_line(linewidth = 0.2),
    panel.grid.major.x = element_line(linewidth = 0.2),
    panel.grid.minor.x = element_line(linewidth = 0.1)
  )
p_dotplot


# Combine plots 
p <- p_graph + p_dotplot & 
  # plot_layout(guides = "collect") &
  plot_annotation(
    title = "Where does Germany get its cheese? (And where do they sell it to?)",
    subtitle = sprintf("German cheese <b style='color:#F2B705'>exports</b>
      and <b style='color:#1B2631'>imports</b> by country (2025).<br>
    Showing the trading partners (in the top %d for exports or imports) by weight in tonnes.",
      n_countries
    ),
    caption = "**Source:** Destatis. **Visualization:** Ansgar Wolsing"
  ) &
  theme(
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      family = "Instrument Sans", 
      width = 1, lineheight = 1.4),
    plot.caption = element_markdown(
      family = "Instrument Sans", hjust = 0,
      margin = margin(t = 12)),
    plot.margin = margin(t = 8, r = 4, b = 4, l = 4),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  )
ggsave(here("2026", "14", "14-trade.png"), width = 9, height = 6, scale = 1)
