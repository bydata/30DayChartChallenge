library(tidyverse)
library(ggtext)
library(patchwork)
library(xml2)
library(here)

base_path <- here("2024", "06")

#' Students enrolled in schools with high staff shortages
#' Source URL:
#' https://data-explorer.oecd.org/vis?fs[0]=Topic%2C0%7CSociety%23SOC%23&pg=0&fc=Topic&bp=true&snb=26&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_CWB%40DF_CWB&df[ag]=OECD.WISE.CWB&df[vs]=1.0&pd=%2C&dq=.B2_2.B2&ly[rw]=REF_AREA&to[TIME_PERIOD]=false&lo=5&lom=LASTNPERIODS
api_url <- "https://sdmx.oecd.org/public/rest/data/OECD.WISE.CWB,DSD_CWB@DF_CWB,1.0/.B2_2.B2?startPeriod=2017&dimensionAtObservation=AllDimensions"
raw <- read_xml(api_url)

# Extract the relevant nodes
obs_keys <- xml_find_all(raw, ".//generic:ObsKey")
obs_values <- xml_find_all(raw, ".//generic:ObsValue")

# Extract data from nodes
df <- tibble(
  time_period = xml_attr(xml_find_all(obs_keys, ".//generic:Value[@id='TIME_PERIOD']"), "value"),
  ref_area = xml_attr(xml_find_all(obs_keys, ".//generic:Value[@id='REF_AREA']"), "value"),
  obs_value = as.numeric(xml_attr(obs_values, "value"))
)

# Country names & continents
df$ref_area_label <- countrycode::countrycode(df$ref_area, "iso3c", "country.name")
df$continent <- countrycode::countrycode(df$ref_area, "iso3c", "continent")

# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin Light") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(family = "Libre Franklin", hjust = 1, size = 6),
      axis.line = element_blank(),
      plot.title = element_markdown(
        color = "grey8",
        family = "Libre Franklin SemiBold", hjust = 0, size = 16, 
        lineheight = 1.15, margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_markdown(
        hjust = 0, color = "grey35", # family = "Poppins",
        margin = margin(b = 8)),
      plot.caption = element_textbox(
        width = 1, hjust = 0, lineheight = 1, size = 7),
      plot.caption.position = "plot",
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid = element_blank(),
      panel.spacing.y = unit(4, "mm"),
      strip.text = element_blank()
    )
)

# color_pal <- c("#C2185B", "#0097A7", "#4527A0", "#FF8F00")
color_pal <- c("#009688", "#00897B", "#00796B", "#00695C")

df |> 
  mutate(ref_area_label = fct_reorder(ref_area_label, obs_value)) |> 
  ggplot(aes(obs_value, ref_area_label)) +
  # Light grey background for the bars
  geom_col(
    aes(x = Inf),
    fill = "grey93") +
  geom_col(
    aes(fill = continent), width = 0.75) +
  geom_text(
    aes(x = 0, label = scales::number(obs_value, accuracy = 0.1)),
    hjust = 0, nudge_x = 0.5, family = "Source Sans Pro Medium", color = "white",
    size = 2) +
  # print the continent as text instead of facet labels
  geom_text(
    data = df |> 
      add_count(continent, name = "n_countries"),
    aes(x = 0, y = n_countries + 1.5, label = continent),
    stat = "unique", family = "Libre Franklin SemiBold", color = "#666666",
    hjust = 0, size = 3
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = color_pal) +
  coord_cartesian(clip = "off") +
  facet_grid(rows = vars(continent), scales = "free_y", space = "free") +
  guides(fill = "none") + 
  labs(
    title = "Share of students aged 15 enrolled<br>in schools with high staff shortages",
    subtitle = "Share in %",
    caption = "Note: 15-year-old students in schools with high relative values 
    on the PISA Index of Staff Shortage.
    Source: OECD (2018).
    Visualization: Ansgar Wolsing"
  )
ggsave(here(base_path, "06-data-day-oecd.png"),
       width = 800, height = 1200, unit = "px", scale = 1.5)
