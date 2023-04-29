library(tidyverse)
library(ggtext)
library(here)
library(rvest)

base_path <- here("2023", "26")

#' Source: Destatis
# Scrape table from page
url <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsvorausberechnung/Tabellen/bevoelkerungsvorausberechnung-lebenserwartung.html"
page <- read_html(url)
raw_table <- page %>% 
  html_node("table") %>% 
  html_table()

# Prepare dataset
scenario_translation <- c(
  "Niedriger Anstieg der Lebenserwartung L1" = "Low increase",
  "Moderater Anstieg der Lebenserwartung L2" = "Moderate increase",
  "Hoher Anstieg der Lebenserwartung L3" = "High increase"
)

df <- 
  raw_table %>% 
  rename(year = Jahr, male = `Männer`, female = `Frauen`) %>% 
  # filter(!year %in% c("20201", "1: Nach der Sterbetafel 2019/2021.")) %>% 
  filter(year != "1: Nach der Sterbetafel 2019/2021.") %>% 
  mutate(
    scenario = ifelse(str_detect(year, "L\\d"), year, NA_character_),
    year = str_sub(year, 1, 4)) %>% 
  fill(scenario, .direction = "down") %>% 
  filter(str_detect(year, "^\\d{4}")) %>% 
  mutate(
    across(c(male, female), ~str_replace(.x, ",", ".")),
    across(c(year, male, female), as.numeric),
    scenario = scenario_translation[scenario]
  )

df_2020 <- df %>% 
  filter(year == 2020)

df %>% 
  add_row(
    year = 2020, male = df_2020$male, female = df_2020$female,
    scenario = "Moderate increase"
  ) %>% 
  add_row(
    year = 2020, male = df_2020$male, female = df_2020$female,
    scenario = "High increase"
  ) %>% 
  fill(scenario, .direction = "up")

# exclude 2020 from dataset
df <- df %>% filter(year != 2020)

df_long <- df %>% 
  pivot_longer(cols = c(male, female), names_to = "gender", values_to = "lifeexp")

df_scenario_wide <- df_long %>% 
  pivot_wider(id_cols = c(year, gender), names_from = "scenario", 
              values_from = "lifeexp", names_prefix = "scenario_", 
              names_repair = janitor::make_clean_names)

df_scenario_wide %>% 
  ggplot(aes(year, group = gender)) +
  geom_ribbon(
    aes(ymin = scenario_low_increase, ymax = scenario_high_increase,
        fill = gender),
    alpha = 0.22
  ) +
  geom_line(
    data = . %>% 
      add_row(year = 2020, gender = "male", scenario_moderate_increase = df_2020$male)%>% 
      add_row(year = 2020, gender = "female", scenario_moderate_increase = df_2020$female),
    aes(y = scenario_moderate_increase, col = gender),
    linewidth = 1.25
  ) +
  annotate(
    "label",
    x = 2065,
    y = c(88.7, 83),
    label = c("female", "male"),
    fill = c( "#725AC1", "#849483"),
    color = "white", family = "Fira Sans Condensed", label.r = unit(1.5, "mm")
  ) +
  annotate(
    "richtext",
    x = c(2045, 2025, 2020),
    y = c(79, 78, 85.5),
    label = c("The ribbon represents a **low**<br>and **high increase** scenario", 
              "The solid line indicates a<br>**moderate increase** scenario",
              "Drop in life expectancy due to<br>Coronavirus pandemic"),
    hjust = 0, family = "Fira Sans", size = 2.5,
    label.size = 0, fill = NA
  ) +
  annotate(
    "curve",
    x = c(2053, 2030, 2021.5),
    xend = c(2053, 2030, 2021.5),
    y = c(79.5, 78.5, 85),
    yend = c(82, 79.8, 83.5),
    linewidth = 0.2, curvature = 0.2,
    arrow = arrow(angle = 20, length = unit(1.5, "mm"), type = "closed")
  ) +
  scale_y_continuous(breaks = seq(0, 100, 5), minor_breaks = seq(0, 100, 2.5),
                     sec.axis = dup_axis()) +
  scale_fill_manual(values = c("male" = "#849483", "female" = "#725AC1"),
                    aesthetics = c("fill", "color")) +
  coord_cartesian(ylim = c(75, NA)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Projected <span style='color:#725AC1'>female</span> and
    <span style='color:#849483'>male</span> life expectation in Germany",
    subtitle = "Life expectancy at birth, 3 scenarios until 2070",
    caption = "Source: Destatis. Visualisation: Ansgar Wolsing",
    x = NULL, y = "Life expectancy at birth"
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "#DAD9D9"),
    panel.grid.minor.y = element_line(linewidth = 0.15, color = "#DAD9D9"),
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3), 
    axis.ticks.length.x = unit(2, "mm"), 
    axis.title = element_text(family = "Fira Sans Condensed Medium"),
    axis.title.y.right = element_blank(),
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
ggsave(here(base_path, "26-local-change-lifeexp-de.png"),
       width = 7, height = 6)
