library(tidyverse)
library(ggtext)
library(here)
library(treemapify)

base_path <- here("2023", "06")

#' Source: OWID
#' https://ourworldindata.org/grapher/political-regime
#' Download full data
political_regime <- read_csv(here(base_path, "political-regime.csv"))
political_regime_2022 <- subset(political_regime, Year == 2022)

#' POPULATION | Source: OWID
#' https://ourworldindata.org/explorers/population-and-demography?facet=none&hideControls=false&Metric=Population&Sex=Both+sexes&Age+group=Total&Projection+Scenario=None&country=CHN~IND~USA~IDN~PAK
#' Download full data
pop <- read_csv(here(base_path, "population-and-demography.csv"))
pop_2021 <- pop %>% 
  select(Entity = `Country name`, Year, Population) %>% 
  filter(Year == 2021)

political_regime_2022 %>% 
  anti_join(pop_2021, by = "Entity")

pop_2021 %>% 
  anti_join(political_regime_2022, by = "Entity") %>% 
  View()


remove_grouped_entities <- c(
  "Less developed regions",
  "Less developed regions, excluding least developed countries",
  "Less developed regions, excluding China",
  "Asia (UN)",
  "Lower-middle-income countries",
  "Upper-middle-income countries",
  "Africa (UN)",
  "More developed regions",
  "High-income countries",
  "Least developed countries",
  "Europe (UN)",
  "Low-income countries",
  "Latin America and the Caribbean (UN)",
  "Land-locked developing countries (LLDC)",
  "Northern America (UN)",
  "Small island developing states (SIDS)",
  "Oceania (UN)", 
  "World"
)

entity_state_mapping <- 
  c("Puerto Rico" = "United States",
    "Reunion" = "France",
    "Martinique" = "France",
    "Mayotte" = "France",
    "French Polynesia" = "France",
    "French Guiana" = "France",
    "Guam" = "United States",
    "Isle of Man" = "United Kingdom",
    "Guernsey" = "United Kingdom",
    "Macao" = "China")

pop_2021_prep <- pop_2021 %>% 
  filter(!Entity %in% remove_grouped_entities) %>% 
  mutate(Entity = ifelse(Entity %in% names(entity_state_mapping), entity_state_mapping[Entity], Entity)) %>% 
  count(Entity, Year, wt = Population, name = "Population")



political_regime_2022_pop <- pop_2021_prep %>% 
  select(-Year) %>% 
  left_join(political_regime_2022, by = "Entity") %>% 
  mutate(regime_row_owid = replace_na(regime_row_owid, 9))

# population per regime class
political_regime_2022_pop %>% 
  group_by(regime_row_owid) %>% 
  summarize(sum(Population))

# Custom function for string wrapping which uses <br> tags for linebreaks
str_wrap_html <- function (string, width = 80, indent = 0, exdent = 0) {
  if (width <= 0) 
    width <- 1
  out <- stringi::stri_wrap(string, width = width, indent = indent, 
                            exdent = exdent, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

# Create a dataset with treemap dimensions
show_label_threshold <- 1e7
treemap_df <- political_regime_2022_pop %>% 
  mutate(
    Entity_wrapped = str_wrap_html(Entity, width = 8),
    label = sprintf(
      "**%s**<br>%s",
      Entity_wrapped, 
      scales::number(Population, scale = 1e-6, suffix = "M", accuracy = 0.1)),
    label = ifelse(Population >= show_label_threshold, label, "")
  ) %>% 
  treemapify(
    .,
    area = "Population",
    subgroup = "regime_row_owid",
    subgroup2 = "Entity",
    layout = "squarified"
  ) %>% 
  group_by(regime_row_owid, Entity) %>% 
  mutate(
    subgroup2_xmin = min(xmin),
    subgroup2_xmax = max(xmax),
    subgroup2_ymin = min(ymin),
    subgroup2_ymax = max(ymax)
  ) %>% 
  ungroup() %>% 
  group_by(regime_row_owid) %>% 
  mutate(
    subgroup_xmin = min(xmin),
    subgroup_xmax = max(xmax),
    subgroup_ymin = min(ymin),
    subgroup_ymax = max(ymax)
  ) %>% 
  ungroup() %>% 
  inner_join(select(political_regime_2022_pop, Entity, Population), by = "Entity")



# Plot

regime_pal <- c("0" = "#D7191D", "1" = "#FDAE61", "2" = "#AAD9E9", "3" = "#2D7BB6", "9" = "grey70")

treemap_df %>% 
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(aes(fill = factor(regime_row_owid)), color = NA, size = 0) + 
  # regime classification borders
  geom_rect(
    aes(xmin = subgroup_xmin, xmax = subgroup_xmax,
      ymin = subgroup_ymin, ymax = subgroup_ymax),
    color = "white", fill = NA, size = 0.5) +
  # country borders
  geom_rect(
    aes(xmin = subgroup2_xmin, xmax = subgroup2_xmax,
        ymin = subgroup2_ymin, ymax = subgroup2_ymax),
    color = "white", fill = NA, 
    size = 0.2) +
  geom_richtext(
    data = . %>% group_by(regime_row_owid, label) %>% 
      summarize(xmin = min(xmin), ymax = max(ymax), Population = max(Population), .groups = "drop") %>% 
      filter(),
    aes(x = xmin, y = ymax, label = label, size = Population),
    color = "white",
    inherit.aes = FALSE, hjust = 0, vjust = 1, fill = NA, label.size = 0,
    family = "Roboto Condensed", lineheight = 1# , size = 2.25
    ) +
  scale_fill_manual(values = regime_pal) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = "grey24"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(t = 2, b = 8, l = 4, r = 4)
  )
ggsave(here(base_path, "06-owid-political-regimes.png"), width = 8, height = 6, scale = 1)
