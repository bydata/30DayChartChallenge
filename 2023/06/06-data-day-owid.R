library(tidyverse)
library(ggtext)
library(here)
library(treemapify)
library(colorspace)

base_path <- here("2023", "06")

#' Source: OWID
#' https://ourworldindata.org/grapher/political-regime
#' Download full data
political_regime <- read_csv(here(base_path, "political-regime.csv"))
political_regime_2021 <- subset(political_regime, Year == 2021) # latest data is 2022, but pop data is 2021

#' POPULATION | Source: OWID
#' https://ourworldindata.org/explorers/population-and-demography?facet=none&hideControls=false&Metric=Population&Sex=Both+sexes&Age+group=Total&Projection+Scenario=None&country=CHN~IND~USA~IDN~PAK
#' Download full data
pop <- read_csv(here(base_path, "population-and-demography.csv"))
pop_2021 <- pop %>% 
  select(Entity = `Country name`, Year, Population) %>% 
  filter(Year == 2021)

political_regime_2021 %>% 
  anti_join(pop_2021, by = "Entity")

pop_2021 %>% 
  anti_join(political_regime_2021, by = "Entity") %>% 
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
    "American Samoa" = "United States",
    "United States Virgin Islands"= "United States",
    "Reunion" = "France",
    "Martinique" = "France",
    "Mayotte" = "France",
    "French Polynesia" = "France",
    "French Guiana" = "France",
    "Saint Martin (French part)" = "France",
    "Niue" = "France",
    "Saint Pierre and Miquelon" = "France",
    "Wallis and Futuna" = "France",
    "Saint Barthelemy" = "France",
    "Guam" = "United States",
    "Isle of Man" = "United Kingdom",
    "Guernsey" = "United Kingdom",
    "Macao" = "China",
    "Sint Maarten (Dutch part)" = "Netherlands",
    "Curacao" = "Netherlands",
    "Bonaire Sint Eustatius and Saba" = "Netherlands",
    "Gibraltar" = "United Kingdom",
    "Falkland Islands" = "United Kingdom",
    "British Virgin Islands" = "United Kingdom",
    "Montserrat" = "United Kingdom",
    "Faeroe Islands" = "Denmark"
    )

pop_2021_prep <- pop_2021 %>% 
  filter(!Entity %in% remove_grouped_entities) %>% 
  mutate(Entity = ifelse(Entity %in% names(entity_state_mapping),
                         entity_state_mapping[Entity], Entity)) %>% 
  count(Entity, Year, wt = Population, name = "Population")



political_regime_2022_pop <- pop_2021_prep %>% 
  select(-Year) %>% 
  left_join(political_regime_2021, by = "Entity") %>% 
  mutate(regime_row_owid = replace_na(regime_row_owid, 9),
         regime_type = case_match(
           regime_row_owid,
           0 ~ "Closed autocracy",
           1 ~ "Electoral autocracy",
           2 ~ "Electoral democracy",
           3 ~ "Liberal democracy",
           9 ~ "No data"
         ))

# population per regime class
political_regime_2022_pop %>% 
  group_by(regime_type) %>% 
  summarize(total_pop = sum(Population)) %>% 
  mutate(share = total_pop / sum(total_pop))

# Custom function for string wrapping which uses <br> tags for linebreaks
str_wrap_html <- function (string, width = 80, indent = 0, exdent = 0) {
  if (width <= 0) 
    width <- 1
  out <- stringi::stri_wrap(string, width = width, indent = indent, 
                            exdent = exdent, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

# Create a dataset with treemap dimensions
show_label_threshold <- 1.5e7
treemap_df <- political_regime_2022_pop %>% 
  mutate(
    Entity_wrapped = str_wrap_html(Entity, width = 12),
    label = sprintf(
      "**%s**<br>%s",
      Entity_wrapped, 
      scales::number(Population, scale = 1e-6, suffix = "M", accuracy = 0.1,
                     big.mark = ",")),
    label = ifelse(Population >= show_label_threshold, label, "")
  ) %>% 
  treemapify(
    area = "Population",
    subgroup = "regime_type",
    subgroup2 = "Entity",
    layout = "squarified"
  ) %>% 
  group_by(regime_type, Entity) %>% 
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

regime_pal <- c("Closed autocracy" = "#D7191D", "Electoral autocracy" = "#FDAE61",
                "Electoral democracy" = "#AAD9E9", "Liberal democracy" = "#2D7BB6", 
                "No data" = "grey70")

treemap_df %>% 
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(aes(fill = factor(regime_type)), color = NA, size = 0) + 
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
    data = . %>% group_by(regime_type, label) %>% 
      summarize(
        xmin = min(xmin), ymax = max(ymax), Population = max(Population), 
        .groups = "drop") %>% 
      filter(),
    aes(x = xmin, y = ymax, label = label, size = Population, 
        color = ifelse(regime_type %in% c("Electoral autocracy", "Electoral democracy"), 
                       "black", "white")),
    # color = "white",
    inherit.aes = FALSE, hjust = 0, vjust = 1, fill = NA, label.size = 0,
    family = "Roboto Condensed", lineheight = 0.7, label.padding = unit(1, "mm")
    ) +
  scale_fill_manual(values = regime_pal) +
  scale_size_continuous(range = c(1.25, 5)) +
  scale_color_identity() +
  guides(size = "none") +
  labs(
    title = sprintf(
    "Share of people living in  <span style='color:%s'>autocracies</span> and 
    <span style='color:%s'>democracies</span>", 
    regime_pal["Closed autocracy"], regime_pal["Liberal democracy"]),
    subtitle = "Sizes of the tiles proportional to the countries' population. 
    Classification of political regimes based on the criteria by Lührmann et al. (2018) 
    and the assessment by V-Dem’s experts. 
    Classification and population data from 2021.",
    caption = "Source: OWID based on Lührmann et al. (2018); V-Dem (v13). 
    Visualisation: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = "grey24", lineheight = 1),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 2, b = 8, l = 4, r = 4),
    legend.position = "top",
    plot.title = element_markdown(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 0.9, hjust = 0.5, halign = 0.5, margin = margin(t = 6, b = 14)),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 4))
  )
ggsave(here(base_path, "06-owid-political-regimes.png"), width = 8, height = 6.4, scale = 1)

# Which political systems does the ‘Regimes of the World’ classification distinguish?
# In closed autocracies, citizens do not have the right to choose either the chief executive of the government or the legislature through multi-party elections.
# In electoral autocracies, citizens have the right to choose the chief executive and the legislature through multi-party elections; but they lack some freedoms, such as the freedoms of association or expression, that make the elections meaningful, free, and fair.
# In electoral democracies, citizens have the right to participate in meaningful, free and fair, and multi-party elections.
# In liberal democracies, citizens have further individual and minority rights, are equal before the law, and the actions of the executive are constrained by the legislative and the courts.