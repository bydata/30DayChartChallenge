library(tidyverse)
library(geofacet)
library(ggtext)
library(here)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)

base_path <- here("2022", "01")

#' Source: https://ec.europa.eu/eurostat/databrowser/view/T2020_31__custom_425303/default/table?lang=de
df_raw <- read_tsv(here(base_path, "t2020_31_tabular.tsv"), na = ":")
df_2020 <- df_raw %>%
  rename(X1 = 1) %>% 
  mutate(countrycode = str_match(X1, "(?:.+?,){3}(.{2})")[, 2]) %>% 
  select(countrycode, share_renewable = `2020`) %>% 
  na.omit()



# Remove UK from EU28 grid
eu27_grid <- subset(eu_grid1, name != "United Kingdom")
grid_preview(eu27_grid)


df_2020 %>% 
  ggplot(aes(x = 1, y = share_renewable)) +
  geom_col() +
  facet_geo(~countrycode, grid = eu27_grid)


# EU countries
eu_countries <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", 
                  "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", 
                  "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE")


df_2020_waffle <- df_2020 %>% 
  filter(countrycode %in% eu_countries) %>% 
  mutate(share_renewable_precise = share_renewable,
         share_renewable = round(share_renewable),
         share_other = 100 - share_renewable) %>% 
  pivot_longer(cols = c(share_renewable, share_other), names_pattern = "share_(.+)") %>% 
  mutate(name = factor(name, levels = c("renewable", "other")),
         country = countrycode::countrycode(countrycode, origin = "eurostat", destination = "country.name"))


df_2020_waffle %>% 
  filter(countrycode == "DE") %>% 
  ggplot(aes(fill = name, values = value)) +
  geom_waffle(flip = TRUE) +
  coord_fixed()

p <- df_2020_waffle %>% 
  mutate(
    share_fmt = sprintf("%.1f", share_renewable_precise),
    label = glue::glue("**{country}**<br>{share_fmt} %")) %>%
  ggplot(aes(fill = name, values = value)) +
  geom_waffle(n_rows = 10, cols = 100, size = 0.2, colour = "white", flip = TRUE,
              show.legend = FALSE) +
  scale_fill_manual(name = NULL,
                    values = c("#77C3C2", "grey87")) +
  coord_fixed() +
  facet_wrap(vars(label)) +
  labs(
    title = glue::glue("Share of
    <span style='color:{colorspace::darken(\"#77C3C2\", 0.2)}'>renewable energy</span> 
                       in gross final energy consumption (2020)"),
    caption = "**Source:** European Environment Agency (EEA) | **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "grey38"),
    plot.title = element_markdown(color = "grey4", family = "Oswald"),
    plot.caption = element_markdown(hjust = 0, color = "grey46"),
    strip.text = element_markdown(hjust = 0, lineheight = 1.2),
    # panel.spacing.y = unit(4.5, "mm")
  )
ggsave(here(base_path, "01-waffle-renewable.png"), width = 6, height = 7)
  
  
## TODO: A legend facet
