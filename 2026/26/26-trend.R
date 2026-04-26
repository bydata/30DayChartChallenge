#' https://github.com/vdeminstitute/vdemdata
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(tidyverse)
library(ggtext)
library(here)

#' Source:
#' Codebook: https://www.v-dem.net/documents/70/codebook_v16.pdf
#' Report: https://www.v-dem.net/documents/75/V-Dem_Institute_Democracy_Report_2026_lowres.pdf
#' 
#' This version has the suffixes: "codelow" and "codehigh" (e.g. v2elmulpar_codelow and
#' v2elmulpar_codehigh). These two kinds of variables ["code low" and "code high"] demarcate
#' the interval in which the measurement model places 68 percent of the probability mass for
#' each country-year score, which is approximately equivalent to one standard deviation upper
#' and lower bounds. If the underlying posterior distribution is skewed, the HPDs reflect this
#' with unequal distances between the point estimate and the high and low estimates. 
#' 
#' "The ribbon represent a range of uncertainty.
#' We are 68% confident that the estimated score falls within this window;
#' a wider range indicates more uncertainty."


df_vdem <- vdemdata::vdem 
colnames(df_vdem)

# Mapping index names
index_mapping <- c(
  "v2x_polyarchy" = "Electoral democracy", 
  "v2x_libdem" = "Liberal democracy",
  "v2x_partipdem" = "Participatory democracy",
  "v2x_delibdem" = "Deliberative democracy",
  "v2x_egaldem" = "Egalitarian democracy",
  "v2x_jucon" = "Judicial constraints",
  "v2xlg_legcon" = "Legislative constraints",
  "v2xcl_rol" = "Equality before the law",
  "v2x_freexp" = "Freedom of expression"
)

selected_indices <- c("v2x_libdem", "v2x_polyarchy",
    "v2x_jucon", "v2xlg_legcon", "v2xcl_rol", "v2x_freexp")

df_vdem_us <-  df_vdem |> 
  filter(country_text_id == "USA") |> 
  select(country_text_id, year, 
    all_of(selected_indices),
    all_of(paste0(selected_indices, "_codelow")),
    all_of(paste0(selected_indices, "_codehigh"))
  ) |> 
  pivot_longer(cols = -c(country_text_id, year), names_to = "index", values_to = "value") |> 
  mutate(
    upper = ifelse(str_detect(index, "_codehigh"), value, NA_real_),
    lower = ifelse(str_detect(index, "_codelow"), value, NA_real_),
    index = str_remove(index, "_code(high|low)"),
    index_label = index_mapping[index]
  ) |> 
  group_by(country_text_id, year, index, index_label) |> 
  summarize(
    value = mean(value, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    .groups = "drop"
  )

color_pal <- c("#b192b5", "#65a6a1")

plot_titles <- list(
  title = "Within a year, the Republicans have set US democracy back by decades",
  subtitle = sprintf("The <b style='color:%s'>Liberal Democracy Index</b>
    captures both electoral and liberal aspects of democracy, 
    such as checks and balances on the executive, respect for civil
    liberties, the rule of law, and the independence of the legislature and the judiciary.
    The <b style='color:%s'>Electoral Democracy Index</b> measures the quality of elections, 
    individual rights, as well as freedoms of expression, the media, and association.
    Both indices range from the lowest (0) to the highest (1) levels of democracy.
    <br><br>
    <span style='font-family:Inconsolata Medium'>Index (0-1)</span>",
  color_pal[1], color_pal[2]
  ),
  caption = "**Source:** Varieties of Democracy (V-Dem) Project, v16 - March 2026. 
    **Visualization:** Ansgar Wolsing"
)

annotation_ci <- "**What the shaded bands show:** 
  Each estimate comes with an uncertainty range capturing where the true value most likely falls.
  The bands contain 68% of the probability assigned by the model to each score.
  A wider ribbon indicates more uncertainty in the data."

start_year <- 1950
df_vdem_us |> 
  filter(year >= start_year, index %in% c("v2x_libdem", "v2x_polyarchy")) |> 
  mutate(index_label = factor(
    index_label, levels = c("Liberal democracy", "Electoral democracy"))) |> 
  ggplot(aes(year, value, col = index_label, fill = index_label)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2, linewidth = 0.1
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    data = ~filter(., year == max(year))
  ) +
  geom_hline(
    data = ~filter(., year == max(year)),
    aes(yintercept = value),
    linetype = "dashed", linewidth = 0.3
  ) +
  geom_label(
    data = ~filter(., year == max(year)),
    aes(
      x = (start_year + max(year)) / 2,
      y = value,
      label = "2025 value"
    ),
    inherit.aes = FALSE,
    family = "Instrument Sans Medium", size = 2.5,
    hjust = 0, fill = "white", linewidth = 0
  ) +
  geom_textbox(
    data = ~filter(., year == 1963, index_label == "Electoral democracy"),
    aes(y = 0.5, label = str_wrap(annotation_ci, 25)),
    family = "Instrument Sans", size = 2.5,
    hjust = 0, fill = "#FFFFFF55", box.size = 0, col = "grey30",
    lineheight = 1, width = 0.6
  ) +
  geom_label(
    data = ~filter(., year == 2018, index_label == "Liberal democracy"),
    aes(y = 0.65, label = "Trump I"),
    family = "Instrument Sans Medium", size = 2.5,
    hjust = 0.5, fill = "white", linewidth = 0, col = "grey30"
  ) +
  # geom_curve(
  #   data = ~filter(., year == 201, index_label == "Liberal democracy"),
  #   aes(xend = 2016, yend = 0.77),
  #   linewidth = 0.2, col = "grey30",
  #   arrow = arrow(angle = 24, length = unit(0.15, "cm"), type = "closed")
  # ) +
  geom_rect(
    data = ~filter(., year == 2017, index_label == "Liberal democracy"),
    aes(xmin = year, xmax = 2020, ymin = 0.67, ymax = 0.81),
    linewidth = 0.25, linetype = "dashed", col = "grey30", fill = NA
  ) +
  facet_wrap(vars(index_label)) +
  scale_y_continuous(breaks = seq(0.4, 1, 0.1)) +
  scale_color_manual(values = color_pal, aesthetics = c("fill", "color")) +
  coord_cartesian(ylim = c(0.4, NA)) +
  guides(fill = "none", color = "none") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white", ink = "grey30") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.92, margin = margin(t = 4),
      family = "FigTree", lineheight = 1.4, size = rel(0.9)),
    plot.caption =  element_markdown(
      hjust = 0, lineheight = 1.1, size = rel(0.8),
      margin = margin(t = 10)),
    strip.text = element_text(family = "Instrument Sans SemiBold", size = rel(1.2)),
    axis.text.x = element_text(vjust = 2),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.length.x = unit(1.5, "mm")
  )
ggsave(here("2026", "26", "26-trend.png"), width = 8, height = 6)
