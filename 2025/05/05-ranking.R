library(tidyverse)
library(ggtext)
library(ggbump)
library(here)

base_path <- here("2025", "05")

#' Manually download the data from
#' https://www-genesis.destatis.de/datenbank/online/url/32dce506
#' Format: CSV (Flat)


df <- read_csv2(here(base_path, "51000-0003_de_flat.csv"), na = "-")

df_prep <- df |> 
  filter(value_variable_label == "Einfuhr: Wert", !is.na(value)) |> 
  select(year = time, entity = `2_variable_attribute_label`, value) |> 
  arrange(entity, year) |> 
  mutate(value = as.numeric(value) * 1000) |> 
  mutate(
    annual_rank = rank(-value),
    .by = year
  ) |> 
  # Adjust country names
  mutate(
    entity = case_match(
      entity,
      "Belgien und Luxemburg (bis 1998)" ~ "Belgien",
      "Belgien (ab 1999)" ~ "Belgien",
      "Russische Föderation (ab 05/1992)" ~ "Russland",
      "Sowjetunion (bis 04/1992)" ~ "Russland",
      "Vereinigte Staaten von Amerika" ~ "USA",
      "Tschechien (ab 1993)" ~ "Tschechien",
      .default = entity
    ),
    entity_en = countrycode::countrycode(entity, "country.name.de", "country.name")
  )


geom_label_highlight <- function(hjust, nudge_x, ...) {
  geom_label(
    ...,
    color = "white", hjust = hjust, nudge_x = nudge_x, label.r = unit(0.5, "mm"),
    show.legend = FALSE, size = 3, family = "Roboto Condensed Medium"
  )
}

geom_label_light <- function(hjust, nudge_x, ...) {
  geom_label(
    ...,
    color = "grey40", fill = NA, label.size = 0,
    hjust = hjust, nudge_x = nudge_x, label.r = unit(0.5, "mm"),
    show.legend = FALSE, size = 3, family = "Roboto Condensed Medium"
  )
}


highlight_countries = c("China", "United Kingdom", "Poland", "France", "Japan",
                        "Czechia")

# Steps (years)
steps <- 5

df_prep |>
  filter(annual_rank <= 20) |> 
  filter(year %% steps == 0 | year == max(year)) |>
  mutate(
    highlighted_country = ifelse(entity_en %in% highlight_countries, entity_en, "Other"),
    entity_en = fct_reorder(entity_en, annual_rank)
    ) |> 
  ggplot(aes(year, annual_rank, col = highlighted_country, 
             group = entity_en, fill = entity_en)) +
  geom_bump(
    aes(linewidth = ifelse(highlighted_country == "Other", 0.5, 1))
  ) +
  geom_point(
    aes(size = ifelse(highlighted_country == "Other", 1.5, 3)),
    shape = 21, col = "white",
    show.legend = FALSE
  ) +
  geom_label_highlight(
    data = ~filter(., year == min(year), highlighted_country != "Other"),
    aes(label = entity_en, fill = highlighted_country),
    hjust = 1, nudge_x = -0.5
  ) +
  geom_label_highlight(
    data = ~filter(., year == max(year), highlighted_country != "Other"),
    aes(label = entity_en, fill = highlighted_country),
    hjust = 0, nudge_x = 0.5
  ) +
  geom_label_light(
    data = ~filter(., year == min(year), highlighted_country == "Other"),
    aes(label = entity_en),
    hjust = 1, nudge_x = -0.5
  ) +
  geom_label_light(
    data = ~filter(., year == max(year), highlighted_country == "Other"),
    aes(label = entity_en, fill = highlighted_country),
    hjust = 0, nudge_x = 0.5
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, 2020, steps), max(df_prep$year)),
    position = "top") +
  scale_y_reverse(breaks = seq(10, 1, -1)) +
  scale_fill_manual(
    values = c("Other" = "grey60", "China" = "#e76f51", "France" = "#264653",
               "United Kingdom" = "#264653", "Poland" = "#e76f51", 
               "Czechia" = "#e76f51",
               "Japan" = "#264653"),
    aesthetics = c("fill", "color")) +
  scale_linewidth_identity() +
  scale_size_identity() +
  coord_cartesian(xlim = c(1985, 2030), ylim = c(10, 1)) +
  guides(color = "none") +
  labs(
    title = "Where Germany sources its goods: 
    <span style='color:#e76f51'>China</span>'s rise to the top,
    and <span style='color:#264653'>UK</span>'s drop after Brexit.",
    subtitle = "Ranking of countries based on the value of the imports in EUR,
    in 5-year steps",
    caption = "**Source:** Destatis. **Visualization:** Ansgar Wolsing",
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", lineheight = 1.05),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.2, margin = margin(t = 2, b = 16)),
    plot.caption = element_markdown(
      hjust = 0, margin = margin(t = 12, b = 0)),
    plot.margin = margin(t = 4, r = 8, b = 4, l = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = "Roboto Condensed", size = 9),
    axis.text.y = element_text(
      family = "Roboto Condensed Bold", size = 11, color = "grey50")
  )
ggsave(here(base_path, "05-ranking.png"), width = 6.5, height = 5, scale = 1)
