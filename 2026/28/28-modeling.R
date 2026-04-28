library(tidyverse)
library(ggtext)
library(here)

#' Source: Destatis
#' https://www-genesis.destatis.de/datenbank/online/url/a7a6c3d4

df <- read_csv2(here("2026", "28", "12421-0001_de_flat.csv"))

df_prep <- df |> 
  mutate(
    year = year(time),
    value = value * value_unit) |> 
  select(
    year, fc_code = `2_variable_attribute_code`, fc_name = `2_variable_attribute_label`,
    value
  ) |> 
  arrange(year, fc_code)

df_prep |> 
  distinct(fc_code, fc_name)

df_max_year <- df_prep |> 
  filter(year == max(year)) |> 
  select(fc_code, max_year_value = value)

df_change <- df_prep |> 
  filter(year == min(year) | year == max(year)) |> 
  arrange(fc_code, year) |> 
  summarize(
    change_abs = last(value) - first(value),
    change_rel = change_abs / first(value),
    .by = fc_code
  )
df_change |> 
  filter(change_abs > 0)

df_change |> 
  filter(change_abs < 8e6)

df_max_year |> 
  filter(max_year_value <= 70e6)


selected_forecasts <- c("BEV-MODELL-01", "BEV-MODELL-03", "BEV-VARIANTE-10", "BEV-VARIANTE-11")

df_prep |> 
  inner_join(df_max_year, by = join_by(fc_code)) |> 
  mutate(
    fc_scenario_highlight = fc_code %in% selected_forecasts,
    fc_scenario_label = ifelse(
      fc_scenario_highlight,
      paste(
      sprintf("**%s**", scales::number(max_year_value, scale_cut = scales::cut_short_scale(), accuracy = 0.1)),
      case_match(
        fc_code,
        "BEV-MODELL-01" ~ "Moderate fertility rate &<br>life expectancy,<br>**no external migration**",
        "BEV-MODELL-03" ~ "**Constant**<br>at 2024 level",
        "BEV-VARIANTE-10" ~ "Minimum<br>population",
        "BEV-VARIANTE-11" ~ "Maximum<br>population",
        .default = "Other"
      ),
      sep = "<br>"),
      "Other"
    ),

  ) |> # View()
  ggplot(aes(year, value, group = fc_name, col = fc_scenario_highlight)) +
  geom_line(
    aes(linewidth = ifelse(fc_scenario_highlight, 1, 0.3))
  ) +
  geom_point(
    data = ~filter(., fc_scenario_highlight & year == max(year))
  ) +
  geom_richtext(
    data = ~filter(., fc_scenario_highlight & year == max(year)),
    aes(label = fc_scenario_label),
    family = "Instrument Sans", size = 2.5, hjust = 0, nudge_x = 0.5,
    lineheight = 1, label.size = 0, fill = "white"
  ) +
  # Annotation
  annotate(
    GeomTextBox,
    x = 2025, y = 64e6,
    label =  "30 projections of the population of Germany from 2025 to 2070:<br>
    In 20 scenarios, the population **decreases by more than 8 million** people.
    7 of these project a **population size below 70 million** in 2070.
    Only 2 projections estimate an **increased population** by 2070.",
    width = 0.4, box.size = 0, fill = "white", hjust = 0,
    family = "Instrument Sans", lineheight = 1.33, size = 3, 
    box.padding = unit(1, "mm"), col = "grey30"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_linewidth_identity() +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "#285185")) +
  coord_cartesian(ylim = c(55e6, 90e6), clip = "off") +
  guides(color = "none") +
  labs(
    title = "Germany's future: 30 population projections 2025-2070",
    subtitle = "<span style='font-family: Inconsolata; 
    font-size: 9pt'>Projected population (in million)</span>",
  caption = "***Note:** Projections assuming different scenarios for 
    life expectancy (83.2 to 89.3 years for females),
    fertility rate (between 1.29 and 2.10 births per woman) and long-term net migration
    (between 0 and 430,000 people per year).*
    <br><br>**Source:** Destatis. Results of the 16th coordinated population projection 
    (based on: 31 December 2024).
    **Visualization:** Ansgar Wolsing<br>"
  ) +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white", ink = "grey30") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.2, color = "grey85"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linewidth = 0.1, color = "grey85"),
    axis.title = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.95, margin = margin(t = 2, b = 0),
      lineheight = 1.33, size = rel(0.9)),
    plot.caption =  element_textbox(
      width = 1, hjust = 0, lineheight = 1.2, size = rel(0.75),
      margin = margin(t = 10, b = 10)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 4, r = 72, b = 4, l = 10),
    axis.text = element_text(family = "Inconsolata"),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.length.x = unit(1.5, "mm"),
    legend.position = "bottom"
  )
ggsave(here("2026", "28", "28-modeling.png"), width = 7, height = 5.5)
