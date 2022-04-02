library(tidyverse)
library(ggtext)
library(here)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)

base_path <- here("2022", "01")

theme_set(theme_minimal(base_family = "Helvetica Neue"))
theme_update(
  plot.background = element_rect(color = NA, fill = "white"),
  axis.text = element_blank(),
  panel.grid = element_blank(),
  text = element_text(color = "grey38"),
  plot.title = element_markdown(color = "grey4", family = "Oswald", size = 14),
  plot.subtitle = element_textbox_simple(
    lineheight = 1.1, size = 9,
    margin = margin(t = 8, b = 6)
  ),
  plot.caption = element_markdown(hjust = 0, color = "grey46", size = 8),
  strip.text = element_markdown(hjust = 0, lineheight = 1.2, size = 8,
                                margin = margin(t = 8, b = 2, l = 2))
)


#' Source: https://ec.europa.eu/eurostat/databrowser/view/T2020_31__custom_425303/default/table?lang=de
#' https://ec.europa.eu/eurostat/databrowser/view/sdg_07_40/default/table?lang=en

df_raw <- read_tsv(here(base_path, "t2020_31_tabular.tsv"), na = ":")
df_2020 <- df_raw %>%
  rename(X1 = 1) %>% 
  mutate(countrycode = str_match(X1, "(?:.+?,){3}(.{2})")[, 2]) %>% 
  select(countrycode, share_renewable = `2020`) %>% 
  mutate(country = countrycode::countrycode(countrycode, origin = "eurostat", destination = "country.name")) %>% 
  na.omit()


# EU countries
eu_countries <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", 
                  "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", 
                  "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE")


plot_labs <- labs(
  title = glue::glue("Share of
    <span style='color:{colorspace::darken(\"#77C3C2\", 0.2)}'>renewable energy</span> 
                       in gross final energy consumption (2020)"),
  subtitle = "The indicator measures the share of renewable energy consumption 
    in gross final energy consumption. 
    The gross final energy consumption is the energy used by end-consumers 
    (final energy consumption) plus grid losses and self-consumption of power plants.",
  caption = "**Source:** European Environment Agency (EEA) | **Visualization:** Ansgar Wolsing")

df_2020_waffle <- df_2020 %>% 
  filter(countrycode %in% eu_countries) %>% 
  mutate(share_renewable_precise = share_renewable,
         share_renewable = round(share_renewable),
         share_other = 100 - share_renewable) %>% 
  pivot_longer(cols = c(share_renewable, share_other), names_pattern = "share_(.+)") %>% 
  mutate(name = factor(name, levels = c("renewable", "other")))


p <- df_2020_waffle %>% 
  mutate(
    share_fmt = sprintf("%.1f", share_renewable_precise),
    label = glue::glue("**{country}**<br>{share_fmt} %"),
    label = fct_reorder(label, -share_renewable_precise)) %>%
  ggplot(aes(fill = name, values = value)) +
  geom_waffle(n_rows = 10, cols = 100, size = 0.2, colour = "white", flip = TRUE,
              show.legend = FALSE) +
  scale_fill_manual(name = NULL,
                    values = c("#77C3C2", "grey87")) +
  coord_fixed() +
  facet_wrap(vars(label)) +
  plot_labs +
  theme(axis.title = element_blank())

ggsave(here(base_path, "01-waffle-renewable.png"), width = 6, height = 7.2)
  

## GEOFACETTED VERSION ---------------------------------------------------------

library(geofacet)

df_2020_waffle_geo <- df_2020 %>% 
  mutate(share_renewable_precise = share_renewable,
         value = round(share_renewable)) %>% 
  mutate(dummy_id = 1) %>% 
  full_join(expand.grid(dummy_id = 1, x = 1:10, y = 1:10)) %>% 
  group_by(countrycode) %>% 
  mutate(name = ifelse(row_number() <= value, "renewable", "other"),
         name = factor(name, levels = c("renewable", "other"))) %>% 
  ungroup() %>% 
  select(-dummy_id) %>% 
  mutate(
    share_fmt = sprintf("%.1f", share_renewable_precise),
    # shorten name of North Macedonia
    country2 = ifelse(country == "North Macedonia", "N. Macedonia", country),
    label = glue::glue("**{country2}**<br>{share_fmt} %"))

europe_grid_labels <- europe_countries_grid1 %>% 
  mutate(code = ifelse(code == "GR", "EL", code)) %>% 
  inner_join(distinct(df_2020_waffle_geo, countrycode, label), by = c("code" = "countrycode")) %>% 
  mutate(name = label) %>%
  select(-label)

p_geofacets <- df_2020_waffle_geo %>%
  ggplot(aes(x, y)) +
  geom_tile(aes(fill = name), col = "white", show.legend = FALSE) +
  scale_fill_manual(name = NULL,
                    values = c("#77C3C2", "grey87")) +
  coord_fixed() +
  facet_geo(vars(countrycode), grid = europe_grid_labels, label = "name") +
  plot_labs +
  theme(
    axis.title = element_blank(),
    plot.title = element_markdown(size = 24),
    plot.subtitle = element_textbox_simple(
      size = 12, margin = margin(t = 8, b = 10))
  )

ggsave(here(base_path, "01-waffle-renewable_geofacets.png"), width = 6.75, height = 9, scale = 1.3)

