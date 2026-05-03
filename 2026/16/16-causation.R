library(tidyverse)
library(ggtext)
library(readxl)
library(rvest)
library(here)

#' Source: 
#' Measle cases & deaths: 
#' https://www.gov.uk/government/publications/measles-historic-confirmed-cases-notifications-and-deaths/measles-historic-confirmed-cases-notifications-and-deaths#measles-notifications-and-deaths-in-england-and-wales-1940-to-2025
#' 
#' Timeline:
#' https://assets.publishing.service.gov.uk/media/69dcf3806b695d635c34dccc/UKHSA_Green_Book_on_Measles_2_4_26.pdf
#' 
#' Population figures England & Wales: 
#' https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland

url_pop <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/ukpopulationestimates1838to2024/ukpopulationestimates18382024.xlsx"
filepath_pop <- here("2026", "16", "ukpopulationestimates.xlsx")
download.file(url_pop, filepath_pop)

df_pop <- read_xlsx(filepath_pop, sheet = "Table 7", skip = 1, .name_repair = janitor::make_clean_names)
df_pop <- df_pop |> 
  mutate(year = as.integer(str_extract(year, "\\d{4}")))

# Measles data scraping
url_measles <- "https://www.gov.uk/government/publications/measles-historic-confirmed-cases-notifications-and-deaths/measles-historic-confirmed-cases-notifications-and-deaths#measles-notifications-and-deaths-in-england-and-wales-1940-to-2025"
page_measles <- read_html(url_measles)

df_measles <- page_measles |> 
  html_node(css = "#contents table:nth-child(28)") |> 
  html_table(header = TRUE) |> 
  janitor::clean_names() |> 
  select(-c(4:7)) |> 
  mutate(
    across(c(notifications, total_deaths),
      function(x) {
        no_comma <- str_remove(x, ",")
        only_digits <- str_extract(no_comma, "\\d+")
        as.integer(only_digits)
      }
    )
  )
glimpse(df_measles)


# Calculate cases/deaths relative to population
df_prep <- df_measles |> 
  inner_join(df_pop, by = join_by(year)) |> 
  select(-c(males, females)) |> 
  rename(pop = persons) |> 
  mutate(
    across(
      c(notifications, total_deaths),
      function(x) x  / pop * 1e5,
      .names = "{col}_per_100k"
    )
  )

df_prep_long <- df_prep |> 
  select(year, ends_with("_per_100k")) |> 
  pivot_longer(cols = -year, names_to = "metric") |> 
  mutate(
    metric = str_replace_all(metric, "_", " "),
    metric = str_to_sentence(metric)
  )


df_annotations_timeline <- tibble(
  year = c(1968, 1988, 1996),
  y = c(rep(max(df_prep$notifications_per_100k), 2), 
        max(df_prep$notifications_per_100k) - 150),
  label = c(
    "**1968**<br>Measles vaccine approved",
    "**1988**<br>MMR introduced",
    "**1996**<br>2nd MMR vaccination introduced"
  )
)

df_prep |> 
  ggplot(aes(year, notifications_per_100k)) +
  geom_col(fill = "grey60", alpha = 0.5, width = 1) +
  geom_smooth(
    span = 0.3, se = FALSE, col = "#021146",
    linewidth = 1.75
  ) +
  geom_vline(
    data = df_annotations_timeline,
    aes(xintercept = year),
    linetype = "dotted"
  ) +
  geom_textbox(
    data = df_annotations_timeline,
    aes(y = y, label = label),
    width = 0.2, fill = "white", box.size = 0,
    family = "Instrument Sans", size = 3.25,
    lineheight = 0.9, hjust = 0, vjust = 1, nudge_x = 0.1,
    box.padding = unit(1, "mm")
  ) +
  annotate(
    "label",
    x = c(1940, 2010),
    y = c(1300, 50),
    label = c("Annual cases", "Smoothed trendline"),
    fill  = c("grey50", "#021146"),
    family = "Instrument Sans Medium", col = "white", size = 2.5,
    hjust = 0
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Introduction of measles vaccination decreased infections significantly",
    subtitle = "*Number of notified cases per 100,000 inhabitants in England and Wales*",
    caption = "**Source:** UK Health Security Agency, UK Office for National Statistics.
      **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(
    base_family = "Instrument Sans", paper = "white",
    ink = "grey30") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = rel(1.4)),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, margin = margin(t = 4, b = 8),
      lineheight = 1.4, size = rel(0.9)),
    plot.caption =  element_markdown(
      hjust = 0, lineheight = 1.1, size = rel(0.8),
      margin = margin(t = 10)),
    strip.text = element_text(
      family = "Instrument Sans", size = rel(1.2), hjust = 0),
    axis.title = element_blank(),
    axis.text.x = element_text(vjust = 2),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.length.x = unit(1.5, "mm")
  )
ggsave(here("2026", "16", "16-causation.png"), width = 8, height = 6)
