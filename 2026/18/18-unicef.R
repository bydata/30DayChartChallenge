library(tidyverse)
library(ggtext)
library(here)

#' Source: UNICEF
#' Adolescent population as proportion of total population (%) 1970-2025
#' https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=.DM_POP_ADLCNT_PROP.&startPeriod=1970&endPeriod=2026
#' https://data.unicef.org/topic/adolescents/overview/
#' 
#' Total population (thousands)
#' https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=.DM_POP_TOT.&startPeriod=2016&endPeriod=2026


df_adolescent <- read_csv(here("2026", "18", "fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.DM_POP_ADLCNT_PROP..csv"),
  name_repair = janitor::make_clean_names)


df_pop <- read_csv(here("2026", "18", "fusion_GLOBAL_DATAFLOW_UNICEF_1.0_.DM_POP_TOT..csv"),
  name_repair = janitor::make_clean_names)


prepare_data <- function(df) {
  df |> 
  select(geo_area = ref_area_geographic_area, sex = sex_sex,
   year = time_period_time_period, value = obs_value_observation_value
  ) |> 
  filter(sex == "_T: Total") |> 
  select(-sex) |> 
  separate_wider_delim(geo_area, delim = ": ", names = c("geo_code", "geo_name")) |> 
  filter(!str_detect(geo_name, "UNICEF|WORLD|UNSDG|SDGRC|WHO|WB_|AU_")) |> 
  mutate(
    continent = countrycode::countrycode(geo_code, "iso3c", "continent",
    custom_match = ("XKX" = "Europe")),
  ) |> 
  filter(!is.na(continent))
}

df_adolescent_prep <- prepare_data(df_adolescent)
df_pop_prep <- prepare_data(df_pop)


df_adolescent_prep |> 
  filter(!is.na(continent)) |> 
  distinct(geo_name) 



df_prep_wide <- df_adolescent_prep |> 
  filter(!is.na(continent)) |> 
  filter(year == min(year) | year == max(year)) |> 
  pivot_wider(
    id_cols = c(geo_code, geo_name, continent),
    names_from = year, values_from = value, names_prefix = "year_") |> 
  left_join(df_pop_prep, by = join_by(geo_code, geo_name, continent)) |> 
  rename(pop_2025 = value) |> 
  mutate(
    pop_2025 = 1000 * pop_2025,
    geo_name_short = case_when(
      geo_code == "HKG" ~ "Hong Kong",
      geo_code == "TWN" ~ "Taiwan",
      TRUE ~ geo_name
    ))


library(ggiraph)

p <- df_prep_wide |> 
  mutate(
    tooltip = sprintf(
        "<h4 style='border-bottom: 1px solid #CCC;
          padding: 0px; margin:0px'>%s %s</h4>
          <p style='margin-top: 5px; margin-bottom: 2px; font-family: Roboto Mono'>
          2025: %s<br>1970: %s</p>",
        countrycode::countrycode(geo_code, "iso3c", "unicode.symbol"),
        geo_name_short,
        scales::number(year_2025, accuracy = 0.1, suffix = "%"),
        scales::number(year_1970, accuracy = 0.1, suffix = "%")
      )
  ) |> 
  # Sort by population desc so that smaller countries are placed last
  arrange(-pop_2025) |> 
  ggplot(aes(year_1970, year_2025)) +
  geom_abline(
    slope = 1, intercept = seq(-20, 5, 5), linewidth = 0.2, linetype = "dashed") +
  annotate(
    "label",
    x = c(c(0, 5) + 25, rep(30, 4)),
    y = c(rep(30, 2), seq(25, 10, -5)),
    label = scales::number(seq(5, -20, -5), style_positive = "plus"),
    family = "Instrument Sans", size = 3,
    fill = "white", linewidth = 0
  ) +
  annotate(
    "label",
    x = 24,
    y = 30,
    label = "Change 1970-2025 (%p) \U2192",
    family = "Instrument Sans", size = 3,
    fill = "white", linewidth = 0, hjust = 1
  ) +
  # geom_point(
  geom_point_interactive(
    aes(
      fill = continent, size = pop_2025,
      tooltip = tooltip
    ),
    shape = 21, col = "grey8", stroke = 0.1,
    alpha = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = ~filter(., pop_2025 > 1e6),
    aes(label = str_wrap(geo_name_short, 12)), 
    size = 2.25, family = "Instrument Sans",
    min.segment.length = 0, segment.size = 0.2, segment.color = "grey30",
    max.overlaps = 12, lineheight = 0.8
  ) +
  scale_size_area(
    max_size = 15, breaks = c(5e7, 25e7, 1e9),
    labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  paletteer::scale_fill_paletteer_d("feathers::cassowary") + 
  coord_equal(xlim = c(10, 30), clip = "off") +
  guides(
    size = legendry::guide_circles(text_position = "right"),
    fill = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    title = "Adolescent population 1970 vs. 2025",
    subtitle = "The majority of the world has seen a drop in the share of adolescents since 1970.
    This does not apply to many countries in sub-Saharan Africa and a few in Asia where
    the share has increased by up to 5 %-points.
    The diagonal lines represent the percentage point change.
    Most countries fall below the 0 line, indicating the share of 
    adolescents among their population has decreased.
    <br><br>
    \U2193 Adolescent population (in % of total) **2025**",
    caption = "***Note:** Adolescents are defined by the United Nations as those
    between the ages of 10 and 19.*
    <br><br>
      **Source:** UNICEF. **Visualization:** Ansgar Wolsing",
    x = "Adolescent population (in % of total) **1970** \U2192",
    y = NULL,
    fill = "Continent", size = "Population 2025"
  ) +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white",
    ink = "grey30") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, margin = margin(t = 4, b = 0),
      lineheight = 1.4, size = rel(0.8)),
    plot.caption =  element_markdown(
      hjust = 0, lineheight = 1.1, size = rel(0.7),
      margin = margin(t = 10)),
    plot.caption.position = "plot",
    strip.text = element_text(
      family = "Instrument Sans", size = rel(1.2), hjust = 0),
    axis.title.x = element_markdown(hjust = 0, size = rel(0.8)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(vjust = 2),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10)),
    axis.ticks = element_line(linewidth = 0.1),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.height = unit(4, "mm"),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = 7),
    legend.ticks = element_line(color = "grey30", linewidth = 0.2, linetype = "dotted")
  )
ggsave(here("2026", "18", "18-unicef.png"), width = 6, height = 7.2, scale = 0.85)

g <- girafe(
  ggobj = p + theme(plot.subtitle = element_textbox(margin = margin(t = 4, b = 28))),
  options = list(opts_sizing(rescale = TRUE, width = 0.6)))
girafe_options(
  g,
  opts_tooltip(css = "font-family: 'Instrument Sans', Arial;
    font-size: 10pt;
    background-color: #000000BB; padding: 3px; border-radius: 5px; 
    box-shadow: 2px 2px 2px #00000033;  
    color: white;")
)


## Diverging bar chart - only change

df_prep_wide |> 
  mutate(
    change = year_2025 - year_1970,
    geo_name = fct_reorder(geo_name, change),
    geo_code = fct_reorder(geo_code, change)
  ) |> 
  arrange(change) |> 
  View()


df_prep_wide |> 
  mutate(
    change = year_2025 - year_1970,
    geo_name = fct_reorder(geo_name, change),
    geo_code = fct_reorder(geo_code, change)
  ) |> 
  arrange(change) |> 
  ggplot(aes(change, geo_code)) +
  geom_col(
    aes(fill = continent)
  ) +
  paletteer::scale_fill_paletteer_d("feathers::cassowary") +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white",
    ink = "grey30") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, margin = margin(t = 4, b = 0),
      lineheight = 1.4, size = rel(0.8)),
    plot.caption =  element_markdown(
      hjust = 0, lineheight = 1.1, size = rel(0.7),
      margin = margin(t = 10)),
    plot.caption.position = "plot",
    strip.text = element_text(
      family = "Instrument Sans", size = rel(1.2), hjust = 0),
    axis.title.x = element_markdown(hjust = 0, size = rel(0.8)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(vjust = 2),
    axis.text.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.1),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.7),
    legend.direction = "vertical",
    legend.key.height = unit(4, "mm"),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = 7)
  )
