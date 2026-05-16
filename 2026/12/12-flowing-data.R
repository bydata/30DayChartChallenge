#' https://github.com/vdeminstitute/vdemdata
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(tidyverse)
library(ggtext)
library(here)

## THEME ELEMENTS ***
#' Fonts
##' Title: FigTree (alternative to Gotham) size 130 % weight 500 #000
##' Subtitle: 90% line-height: 1.4 weight: 400
##' Axis labels and titles: Inconsolata 80-100 % weight 400 #000 
##' Axis titles capitalized, often left-aligned (x-axis) / top and 0° (y-axis)
#' Gridlines
##' often none, when used they are thin and #E6E6E6
#' Axis marks often visible
#' Direct labelling of lines replacing legend
#' Data labels same size as axis labels, text often capitalized
#' Colors
##' grey: rgb 170/170/170 -> #AAAAAA
##' orangered : #fd3e13
##' red: #e15759
##' yellow: #edc949
##' green: #76b7b2
##' orange: #f09a55
##' lilac: #d4a6c8
##' blue: #7796CB
##' male: #95D2CE
##' female: #e6d5ae
#' Geoms:
##' Points often shape 21 with empty/background color fill, rather thick stroke (e.g. 3.5 width at radius 4)
#' Background: white


# FlowingData theme
bg_color <- "white"
theme_flowingdata <- function(...) {
  theme_minimal(base_family = "Inconsolata Medium", paper = bg_color, ink = "black") +
    theme(
      plot.title = element_text(
        family = "FigTree SemiBold", size = rel(1.3)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        width = 1,
        family = "FigTree", lineheight = 1.4, size = rel(0.9)),
      plot.caption =  element_markdown(
        hjust = 0, lineheight = 1.1, size = rel(0.8)),
      plot.caption.position = "plot",
      plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
      axis.text = element_text(size = rel(0.9)),
      axis.ticks = element_line(linewidth = 0.2),
      axis.ticks.length = unit(0.1, "cm"),
      panel.grid = element_line(color = "#E6E6E6"),
      panel.grid.major = element_line(linewidth = 0.2),
      panel.grid.minor = element_line(linewidth = 0.1),
    )
}


#' Source:
#' Codebook: https://www.v-dem.net/documents/70/codebook_v16.pdf
#' Report: https://www.v-dem.net/documents/75/V-Dem_Institute_Democracy_Report_2026_lowres.pdf

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

df_vdem_hun <-  df_vdem |> 
  filter(country_text_id == "HUN") |> 
  select(country_text_id, year, 
    v2x_libdem, v2x_polyarchy,
    v2x_jucon, v2xlg_legcon, v2xcl_rol, v2x_freexp
  ) |> 
  pivot_longer(cols = -c(country_text_id, year), names_to = "index", values_to = "value") |> 
  mutate(
    index_label = index_mapping[index]
  )


plot_titles <- list(
  title = "Orbán's systematic autocratization in Hungary",
  subtitle = "The <b style='color:#b496b8'>Liberal Democracy Index</b>
    captures both electoral and liberal aspects of democracy, 
    such as checks and balances on the executive, respect for civil
    liberties, the rule of law, and the independence of the legislature and the judiciary.
    The <b style='color:#76b7b2'>Electoral Democracy Index</b> measures the quality of elections, 
    individual rights, as well as freedoms of expression, the media, and association.
    Both indices range from the lowest (0) to the highest (1) levels of democracy.
    <br><br>
    <span style='font-family:Inconsolata Medium'>Index (0-1)</span>",
  caption = "Source: Varieties of Democracy (V-Dem) Project, v16 - March 2026.<br>
    Visualization: Ansgar Wolsing (theme adapted from FlowingData)"
)

df_annotations <- data.frame(
  x = c(1990 - 3, 1998, 2010 + 1),
  y = c(0.87, 0.68, 0.8),
  label = c(
    "After the end of communist dictatorship, Hungary adopted liberal democracy",
    "Orbán's first term (1998-2002)",
    "Orbán returns to power and systematically dismantles democratic achievements."
  )
)


df_vdem_hun |> 
  filter(year >= 1980, index %in% c("v2x_libdem", "v2x_polyarchy")) |> 
  ggplot(aes(year, value, col = index_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(
    data = ~filter(., year %in% c(1990, 1998, 2010)),
    shape = 21, stroke = 0.7, fill = bg_color, size = 3
  ) +
  # Direct labels
  geom_label(
    data = ~filter(., year == max(year)),
    aes(label = str_wrap(toupper(index_label), 6)),
    family = "Inconsolata Semibold", size = 4,
    fill = bg_color, nudge_x = 0.5, hjust = 0, vjust = 0.5,
    label.padding = unit(0, "mm"), label.size = 0,
    lineheight = 0.8
  ) +
  # Annotations
    geom_label(
    data = df_annotations,
    aes(x, y, label = str_wrap(toupper(label), 20)),
    family = "Inconsolata", size = 3,
    fill = bg_color, hjust = 0, vjust = 0,
    label.padding = unit(0, "mm"), label.size = 0,
    lineheight = 0.8, col = "#555555",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("#76b7b2", "#b496b8")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  guides(color = "none") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
    x = NULL, y = NULL
  ) +
  theme_flowingdata() +
  theme(
    plot.margin = margin(t = 6, r = 40, b = 4, l = 4),
    plot.subtitle = element_textbox(
      width = 1.05, margin = margin(b = 12)
    )
  )
ggsave(here("2026", "12", "12-flowing-data.png"), width = 6.5, height = 6.5)
