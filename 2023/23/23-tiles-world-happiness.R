library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "23")

#' Source: UN World Happiness Report 2023
#' https://worldhappiness.report/ed/2023/#appendices-and-data
#' Download "Data for Table 2"

table_url <- "https://happiness-report.s3.amazonaws.com/2023/DataForTable2.1WHR2023.xls"
table_file <- here(base_path, "DataForTable2.1WHR2023.xls")
download.file(table_url, destfile = table_file)

df <- readxl::read_xls(table_file, .name_repair = janitor::make_clean_names)

# the published life ladder score is calculated as the average of the last 3 years/waves
df_prep <- df %>% 
  group_by(country_name) %>% 
  mutate(life_ladder_rolling = zoo::rollmean(life_ladder, k = 3, align = "right", fill = NA)) %>% 
  ungroup() 


df_prep %>% 
  filter(!is.na(life_ladder_rolling)) %>% 
  count(country_name) %>% 
  arrange(n)

show_country_threshold <- 10

df_plot <- df_prep %>% 
  filter(!is.na(life_ladder_rolling)) %>% 
  add_count(country_name, name = "years_available") %>% 
  filter(years_available >= show_country_threshold) %>% 
  group_by(country_name) %>% 
  complete(year = min(year):max(year)) %>% 
  fill(life_ladder_rolling, .direction = "down") %>% 
  ungroup() %>% 
  filter(!is.na(life_ladder_rolling)) %>% 
  # add continent information
  mutate(
    country_name = ifelse(country_name == "Turkiye", "Turkey", country_name),
    continent = countrycode::countrycode(
      country_name, origin = "country.name", destination = "continent"),
    continent = ifelse(country_name == "Kosovo", "Europe", continent)) %>% 
  arrange(continent, desc(country_name), year) %>% 
  mutate(country_name = fct_inorder(country_name))

df_plot %>% 
  ggplot(aes(year, country_name)) +
  geom_tile(aes(fill = life_ladder_rolling),
            height = 1, col = "white") +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "right") +
  scale_fill_viridis_c(option = "D") +
  # coord_equal() +
  coord_cartesian(expand = FALSE) +
  facet_grid(rows = vars(continent), scales = "free_y", space = "free_y", switch = "y") +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "WORLD HAPPINESS",
    caption = "Source: Visualisation: Ansgar Wolsing",
    x = NULL, y = NULL,
    fill = "Life happiness (Cantril ladder),<br>rolling average"
  ) +
  theme_minimal(base_family = "Roboto", base_size = 9) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "bottom",
    # legend.position = c(0.5, 0.1),
    # legend.direction = "horizontal",
    legend.key.height = unit(4, "mm"),
    legend.box.just = "center",
    legend.title = element_markdown(),
    text = element_text(color = "grey30"),
    axis.text = element_text(size = 4.5),
    axis.text.x = element_text(color = "grey30"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24, color = "grey2"),
    plot.caption = element_markdown(hjust = 0.5),
    panel.grid = element_blank(),
    panel.spacing.y = unit(6, "mm"),
    strip.text.y.left = element_text(
      family = "Roboto Condensed",
      size = 12, face = "bold", color = "grey60", angle = 0, hjust = 0)
  )
ggsave(here(base_path, "23-tiles-un-world-happiness.png"), width = 5, height = 10, dpi = 600)
