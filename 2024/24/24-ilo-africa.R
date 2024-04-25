library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "24")

#' Source: 
#' https://rshiny.ilo.org/dataexplorer48/?region=AFRICA&lang=en&id=SDG_0131_SEX_SOC_RT_A
#' Select Sex=Total & Age group=Youth, adults 15+
#' 
#' SDG indicator 1.1.1 - Working poverty rate (percentage of employed living below US$2.15 PPP) (%) -- Annual

#' The series is part of the ILO modelled estimates and is harmonized to account 
#' for differences in national data and scope of coverage, collection and 
#' tabulation methodologies as well as for other country-specific factors. 
#' The working poverty rate conveys the percentage of employed persons living 
#' in poverty in spite of being employed. Poverty is defined using the 
#' international poverty line of US$2.15 per day in purchasing power parity (PPP).

df <- read_csv(here(base_path, "SDG_0111_SEX_AGE_RT_A-filtered-2024-03-24.csv"),
               name_repair = janitor::make_clean_names)

df <- df |> 
  mutate(ref_area_label = ifelse(
    ref_area_label == "Congo, Democratic Republic of the",
    "DR Congo", ref_area_label))

change_per_country <- df |> 
  arrange(ref_area_label, time) |> 
  group_by(ref_area_label) |> 
  filter(time == min(time) | time == max(time)) |> 
  mutate(change = obs_value - lag(obs_value)) |> 
  ungroup() |> 
  filter(!is.na(change)) |> 
  select(ref_area_label, change) 
change_per_country

# In how many countries has the share decreased or increased?
sum(change_per_country$change > 0)
sum(change_per_country$change < 0)

change_per_country |> 
  arrange(change) |> 
  mutate(change_grp = case_when(
    change < -30 ~ "Decrease: More than 30%",
    change < -20 ~ "Decrease: More than 20%",
    change < -10 ~ "Decrease: >10-20%",
    change < -5 ~ "Decrease: >5-10%",
    change < 0 ~ "Decrease: >0-5%",
    change > 20 ~ "Increase: More than 20%",
    change > 10 ~ "Increase: >10-20%",
    change > 5 ~ "Increase: >5-10%",
    change > 0 ~ "Increase: >0-5%",
  ),
  change_grp = fct_inorder(change_grp)) |>
  ggplot(aes(change_grp)) +
  geom_bar(aes(fill = str_detect(change_grp, "Decrease"))) +
  coord_flip()


# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title = element_text(size = 8),
      axis.text.x = element_text(family = "Source Code Pro", size = 7),
      axis.text.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.15),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 8.5, width = 1, lineheight = 1.2,
        margin = margin(b = 8)),
      plot.caption = element_markdown(
        hjust = 0, lineheight = 1.1, size = 7),
      plot.margin = margin(rep(4, 4)),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.height = unit(6, "mm"),
      legend.key.width = unit(10, "mm"),
      legend.box.spacing = unit(0, "mm"),
      legend.margin = margin(0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_blank(),
      strip.text = element_markdown(
        family = "Libre Franklin SemiBold", size = 10,
        margin = margin(t = 4, b = 1))
    ) 
)


selected_regions <- c("DR Congo", "Sierra Leone",
                      "Mali", "Gambia", "Burkina Faso", "Senegal",
                      "Zimbabwe", "Madagascar", "Angola", "Sudan", "Zambia",
                      "Malawi")
df |> 
  inner_join(change_per_country, by = "ref_area_label") |> 
  mutate(ref_area_label = fct_reorder(ref_area_label, change)) |> 
  ggplot(aes(time, obs_value, group = ref_area_label)) +
  geom_line(
    color = "grey80", linewidth = 0.1
  ) +
  geom_line(
    data = ~subset(., ref_area_label %in% c("Africa", "World")),
    aes(linetype = ref_area_label),
    color = "grey30", linewidth = 0.5
  ) +
  geom_line(
    data = ~subset(., ref_area_label %in% selected_regions) |> 
      mutate(highlighted_area = ref_area_label),
    aes(color = change >= 0),
    linewidth = 1
  ) +
  # Data labels at the start and end of the lines
  geom_text(
    data = ~subset(., ref_area_label %in% selected_regions &
                     (time == min(time) | time == max(time))) |>
      mutate(highlighted_area = ref_area_label),
    aes(label = paste0(round(obs_value), "%"), color = change >= 0),
    family = "Source Code Pro SemiBold", size = 3, nudge_y = 8, show.legend = FALSE
  ) +
  scale_color_manual(values = c("#00897B", "#C2185B")) +
  scale_linetype_manual(values = c("Africa" = "solid", "World" = "longdash")) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(
    vars(highlighted_area), ncol = 3, 
    labeller = as_labeller(function(x) {
      # change the color of the facet labels based on change over time
      color <- ifelse(
        x %in% change_per_country$ref_area_label[change_per_country$change < 0],
        "#00897B", "#C2185B"
      )
      sprintf("<span style='color:%s'>%s</span>", color, x)
    })) +
  guides(
    #color = guide_legend(order = 1),
    color = "none",
    linetype = guide_legend(order = 2)) +
  labs(
    title = "Working poverty rate in selected African countries",
    subtitle = "Percentage of employed living below US$2.15 PPP (%).
    Countries which the highest
    <span style='font-family:\"Libre Franklin SemiBold\"; color:#00897B'>decrease</span> and
    <span style='font-family:\"Libre Franklin SemiBold\"; color:#C2185B'>increase</span>
    from 2000 to 2023, compared to the continent and worldwide average, 
    the other African countries are shown in the background",
    caption = "Source: ILO, Statistics in Africa. Visualization: Ansgar Wolsing",
    x = NULL, y = NULL,
    color = NULL, linetype = NULL
  )
ggsave(here(base_path, "24-ilo-africa.png"), width = 6, height = 5, scale = 1.2)
