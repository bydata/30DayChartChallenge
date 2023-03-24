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
  # geom_text(aes(label = scales::number(life_ladder_rolling, accuracy = 0.1)),
  #           col = "white", size = 2) +
  scale_x_continuous(position = "top") +
  scale_y_discrete(position = "right") +
  scale_fill_viridis_c(option = "D") +
  # coord_equal() +
  coord_cartesian(expand = FALSE) +
  facet_grid(rows = vars(continent), scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title = "",
    x = NULL, y = NULL,
    fill = "Ladder"
  ) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = "right",
    axis.text = element_text(size = 5),
    axis.text.x = element_text(color = "grey30"),
    panel.grid = element_blank(),
    strip.text.y.left = element_text(face = "bold", color = "grey60", angle = 0, hjust = 0)
  )
ggsave(here(base_path, "23-tiles-un-world-happiness.png"), width = 4, height = 10, dpi = 600)
