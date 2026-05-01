library(tidyverse)
library(ggtext)
library(here)

#' Source: Reporters Without Borders
#' https://rsf.org/en/index?year=2025

url <- "https://rsf.org/sites/default/files/import_classement/2025.csv"
df <- read_csv2(url, locale = locale(encoding = "ISO-8859-15"))

oecd_members <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile",
  "Colombia", "Costa Rica", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Iceland", "Ireland", "Israel", "Italy",
  "Japan", "Latvia", "Lithuania", "Luxembourg", "Mexico",
  "Netherlands", "New Zealand", "Norway", "Poland", 
  "Portugal", "Slovakia", "Slovenia", "South Korea",
  "Spain", "Sweden", "Switzerland", "Türkiye",
  "United Kingdom", "United States"
)
length(oecd_members)

#' https://rsf.org/en/methodology-used-compiling-world-press-freedom-index-2025?year=2025&data_type=general
# [85 - 100 points]  good (green)
# [70 - 85 points[  satisfactory (yellow)
# [55 - 70 points[  problematic (light orange)
# [40 - 55 points[  difficult (dark orange)
# [0 - 40 points[ very serious (dark red)

df_oecd <- df |> 
  transmute(
    country_name = Country_EN,
    country_code = ISO,
    country_flag = countrycode::countrycode(
      country_code, origin = "iso3c", destination = "unicode.symbol"),
    score = `Score 2025`,
    classification = case_when(
      score >= 85 ~ "Good",
      score >= 70 ~ "Satisfactory",
      score >= 55 ~ "Problematic",
      score >= 40 ~ "Difficult",
      TRUE ~ "Very serious"
    ),
    classification = factor(classification, 
    levels = c("Good", "Satisfactory", "Problematic", "Difficult", "Very serious")),
    classification_num = as.integer(classification)
  ) |> 
  arrange(country_name) |> 
  filter(country_name %in% oecd_members)

# Check if number of countries match
nrow(df_oecd) == length(oecd_members)


df_oecd |> 
  mutate(country_name = fct_reorder(country_name, score)) |> 
  ggplot(aes(score, country_name, fill = classification)) +
  geom_point(
    aes(),
    size = 3, shape = 21, col = "grey20"
  ) +
  geom_text(
    aes(label = country_name),
    hjust = 0, family = "Instrument Sans", size = 2.25,
    nudge_x = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 10),
    position = "top") +
  scale_fill_brewer(palette = "PiYG", direction = -1) +
  coord_cartesian(clip = "off", xlim = c(NA, 100)) +
  labs(
    title = "Conditions for Journalism in OECD countries",
    subtitle = "World Press Freedom Index 2025 (range: 0 to 100)",
    caption = "**Source:** Reporters Without Borders.
    **Visualization:** Ansgar Wolsing",
    x = "Press Freedom Index",
    fill = "Press freedom\nclassification"
  ) +
  theme_minimal(base_family = "Instrument Sans") +
  theme(
    plot.title = element_text(family = "Instrument Sans SemiBold"),
    plot.caption = element_markdown(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.8),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(family = "Instrument Sans Medium")
  )
ggsave(here("2026", "06", "06-rwb.png"), width = 6, height = 6)
