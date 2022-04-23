library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(geofacet)

base_path <- here("2022", "23")

#' Source: Eurostat
#' House price index (2015 = 100) - quarterly data
#' online data code: PRC_HPI_Q 
#' Download TSV full dataset
df_raw <- read_tsv(here(base_path, "prc_hpi_q_tabular.tsv"),
                   na = c(":", ": c"))

#' purchases:
#' DW_EXST ... of existing dwellings
#' DW_NEW ... of new dwellings
#' TOTAL
#' 
#' unit of measure:
#' I10_Q      Index 2010 = 100
#' I15_Q      Index 2015 = 100
#' RCH_A_AVG  Annual average rate of change


df <- df_raw %>% 
  separate(1, into = c("freq", "purchase", "unit", "region"), sep = ",") %>% 
  select(-freq) %>% 
  pivot_longer(cols = -c("purchase", "unit", "region"), names_to = "quarter") %>% 
  mutate(year_quarter_date = zoo::as.Date(zoo::as.yearqtr(quarter, format = "%Y-Q%q")),
         country = countrycode::countrycode(region, "iso2c", "country.name"),
         value = as.numeric(value)) %>% 
  # replace missing values with the value from the previous quarter
  group_by(purchase, unit, region) %>% 
  fill(value, .direction = "down") %>% 
  ungroup() %>% 
  filter(year(year_quarter_date) >= 2010) %>% 
  filter(!is.na(country))



europe_grid <- europe_countries_grid1 %>% 
  filter(name %in% unique(df$country), !name %in% c("Turkey", "Switzerland"))




df %>% 
  filter(purchase == "DW_NEW", unit == "I10_Q") %>% 
  ggplot(aes(year_quarter_date, y = 1)) +
  geom_tile(aes(fill = value), col = NA, size = 0) +
  scale_fill_continuous() +
  facet_wrap(vars(country))

p <- df %>% 
  filter(purchase == "TOTAL", unit == "I10_Q") %>% 
  ggplot(aes(year_quarter_date, y = 1)) +
  geom_tile(aes(fill = value), col = NA, size = 0) +
  geom_label(
    data = . %>% filter(quarter == "2016-Q1"),
    aes(label = country), stat = "unique", color = "black",
    size = 3, fill = "#FFFFFFBB", family = "Fira Sans Condensed SemiBold",
    label.size = 0, label.r = unit(1, "mm")) +
  scale_fill_viridis_c() +
  facet_geo(vars(country), grid = europe_grid, label = "name") +
  guides(
    fill = guide_colorbar(title.position = "top")
  ) +
  labs(
    title = "Housing Prices rising in many European countries",
    subtitle = "The House Price Index (HPI) measures inflation in the residential 
    property market. 
    The HPI captures price changes of all kinds of residential property purchased 
    by households (flats, detached houses, terraced houses, etc.), both new and 
    existing. Only market prices are considered, self-build dwellings are therefore 
    excluded.
    The land component of the residential property is included.
    ",
    caption = "**Source:** Eurostat. **Visualization:** Ansgar Wolsing",
    fill = "Annual average index<br>*(2010=100)*"
  ) +
  theme_void(base_family = "Fira Sans Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey87"),
    panel.background = element_rect(color = "white"),
    text = element_text(color = "grey10"),
    plot.title = element_text(
      family = "Fira Sans Condensed SemiBold", color = "black",
      hjust = 0, size = 24, margin = margin(b = 8)),
    plot.subtitle = element_textbox(
      width = 0.8, hjust = 0, halign = 0, lineheight = 1.2, 
      margin = margin(b = 24)),
    plot.caption = element_markdown(margin = margin(t = 18)),
    # strip.text = element_text(family = "Fira Sans Condensed SemiBold",
    #                           color = "grey16", size = 7)
    strip.text = element_blank(),
    plot.margin = margin(12, 12, 12, 12),
    legend.direction = "horizontal",
    legend.position = c(0.9, 0.95),
    legend.title = element_markdown(hjust = 0.5, lineheight = 1),
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(3, "mm"),
    panel.spacing.y = unit(4, "mm")
  )
ggsave(here(base_path, "23-titles-europe-housing-prices.png"), width = 9, height = 8)

