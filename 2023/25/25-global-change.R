library(tidyverse)
library(ggtext)
library(here)
library(readxl)
library(grid)

base_path <- here("2023", "25")

#' Source:
#' https://population.un.org/wpp/Download/
#' Download Demographic Indicators Medium zip. file

file <- here(base_path, "UN_PPP2022_Output_PopTot.xlsx")
# The quantiles reside in separate sheets
sheet_names <- excel_sheets(file)
sheet_names <- sheet_names[sheet_names != "NOTES"]

projections <- map_dfr(sheet_names, ~read_xlsx(file, sheet = .x,
               skip = 16, .name_repair = janitor::make_clean_names))

projections <- projections %>% 
  filter(type != "Label/Separator") %>% 
  rename(entity = region_subregion_country_or_area)

projections_long <- projections %>% 
  pivot_longer(cols = x2022:x2100, names_to = "year", values_to = "value",
               names_transform = function(x) as.numeric(str_remove(x, "x")),
               values_transform = as.numeric)

table(projections$variant)


indicators <- read_excel(here(base_path, "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_REV1.xlsx"),
                        skip = 16, .name_repair = janitor::make_clean_names)
colnames(indicators)
glimpse(indicators)

# convert columns to numeric
indicators <- indicators %>% 
  mutate(across(total_population_as_of_1_january_thousands:net_migration_rate_per_1_000_population, as.numeric)) %>% 
  rename(entity = region_subregion_country_or_area)


indicators_projection_median_combined <- indicators %>% 
  filter(entity %in% c("China", "India")) %>% 
  select(entity, year, value = total_population_as_of_1_january_thousands) %>% 
  # add the median projections to achieve a continuous line
  bind_rows(
    projections_long %>% 
      filter(entity %in% c("China", "India"), variant == "Median PI") %>% 
      select(entity, year, value)
  )

p <- projections_long %>% 
  pivot_wider(id_cols = everything(), names_from = "variant", values_from = "value") %>% 
  filter(entity %in% c("China", "India")) %>% 
  ggplot(aes(year)) +
  geom_ribbon(
    aes(ymin = `Lower 95 PI`, ymax = `Upper 95 PI`,
        fill = entity),
    alpha = 0.22
  ) +
  geom_ribbon(
    aes(ymin = `Lower 80 PI`, ymax = `Upper 80 PI`,
        fill = entity),
    alpha = 0.44
  ) +
  geom_line(
    data = indicators_projection_median_combined,
    aes(y = value, col = entity),
    linewidth = 1.25
  ) +
  annotate(
    "text",
    x = c(1950, 1965),
    y = c(7.2e5, 4.2e5),
    label = c("China", "India"),
    col = c("China" = "#EE1C25", "India" = "#FF9933"),
    hjust = 0, family = "Fira Sans Condensed Medium"
  ) +
  annotate(
    "richtext",
    x = c(2023, 2023),
    y = c(1.89e6, 2.08e6),
    label = c("80 % prediction interval", 
              "95 % prediction interval reflects the spread<br>in the distribution 
              of outcomes and provides<br>an assessment of the uncertainty"),
    hjust = 0, family = "Fira Sans", size = 2.5,
    label.size = 0, fill = NA
  ) +
  annotate(
    "curve",
    x = c(2052, 2059),
    xend = c(2063, 2070),
    y = c(1.87e6, 2.01e6),
    yend = c(1.75e6, 1.95e6),
    linewidth = 0.2, curvature = 0.2,
    arrow = arrow(angle = 20, length = unit(1.5, "mm"), type = "closed")
  ) +
  geom_vline(aes(xintercept = 2022), linetype = "dashed", linewidth = 0.25) + 
  annotate(
    "text",
    x = 2024, y = 3.5e5, label = "Projection \U2192",
    hjust = 0, family = "Fira Sans Condensed"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "m")) +
  scale_color_manual(values = c("China" = "#EE1C25", "India" = "#FF9933"),
                     aesthetics = c("fill", "color")) +
  guides(col = "none", fill = "none") +
  labs(
    title = "<span style='color:#FF9933'>India</span> has overtaken
    <span style='color:#EE1C25'>China</span> as the most populous nation",
    subtitle = "Probabilistic population projections of total population 
    based on the UN World Population Prospects 2022",
    caption = "Data: United Nations, World Population Prospects 2022.
    Visualisation: Ansgar Wolsing",
    x = NULL, y = "Population in million"
  ) + 
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.3, color = "#DAD9D9"),
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3), 
    axis.ticks.length.x = unit(2, "mm"), 
    axis.title = element_text(family = "Fira Sans Condensed Medium"),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_markdown(),
    plot.title = element_markdown(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(b = 8), lineheight = 1),
    plot.caption = element_markdown(
      hjust = 0, size = 7, family = "Fira Sans Condensed Light"),
    plot.caption.position = "plot"
  )
p
ggsave(here(base_path, "25-global-change-population-china-india.png"),
       width = 7, height = 6)
