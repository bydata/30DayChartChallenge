library(tidyverse)
library(ggtext)
library(here)
# remotes::install_github("ropengov/eurostat")
library(eurostat)

base_path <- here("2023", "22")

#' Source: Eurostat
df <- get_eurostat("nrg_ind_ren", 
                   time_format = "num", 
                   type = "code"  # get variable codes and labels
)
df <- label_eurostat(df, code = "geo", fix_duplicated = TRUE)

# which countries?
unique(df$geo)
unique(df$geo_code)

eu_geo_codes <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL",
                       "ES", "EU27_2020", "FI", "FR", "HR", "HU", "IE", "IT", "LT",
                       "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK" )

heating_cooling <- df %>% 
  filter(nrg_bal == "Renewable energy sources in heating and cooling",
         geo_code %in% eu_geo_codes) %>% 
  mutate(geo = case_match(
    geo,
    "Germany (until 1990 former territory of the FRG)" ~ "Germany",
    "European Union - 27 countries (from 2020)" ~ "EU 27",
    .default = geo
  )) 
length(unique(df$geo))
length(unique(heating_cooling$geo))

# highest increase over time
heating_cooling %>% 
  arrange(geo, time) %>% 
  group_by(geo) %>% 
  filter(time == min(time) | time == max(time)) %>% 
  mutate(change = values / lag(values)) %>% 
  ungroup() %>%
  filter(!is.na(change)) %>% 
  arrange(-change)

heating_cooling %>% 
  select(-c(nrg_bal, unit)) %>% 
  arrange(geo, time) %>% 
  group_by(geo) %>% 
  filter(time == min(time) | time == max(time)) %>% 
  mutate(time = ifelse(time == min(time), "first", "last")) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(geo_code, geo), names_from = "time", values_from = "values") %>%
  mutate(change = last - first) %>% 
  View()
  

heating_cooling %>% 
  ggplot(aes(time, values)) + 
  geom_line() + 
  facet_wrap(vars(geo))


# selected_countries <- c("European Union", "Ireland", "Netherlands", "Belgium", 
#                         "Germany", "Hungary", "Cyprus", "Greece", 
#                         "Malta", "Latvia", "Estonia", "Sweden")
selected_countries <- unique(heating_cooling$geo)
selected_countries <- c("EU 27", selected_countries[selected_countries != "EU 27"])

heating_cooling %>% 
  ggplot(aes(time, values, group = geo)) +
  geom_line(linewidth = 0.1, col = "grey60") +
  geom_line(
    data = ~subset(., geo %in% selected_countries) %>% 
      mutate(geo2 = factor(geo, levels = c(selected_countries, "Other"))),
    # aes(col = geo2),
    linewidth = 0.5, col = "#33953A") +
  geom_point(
    data = ~subset(., geo %in% selected_countries & time == max(time)) %>% 
      mutate(geo2 = factor(geo, levels = c(selected_countries, "Other"))),
    # aes(col = geo2),
    col = "#33953A") +
  scale_x_continuous(expand = c(add = 0, mult = 0.2),
                     labels = function(x) str_replace(x, "20", "'")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     labels = scales::label_number(suffix = "%")) +
  # colorspace::scale_color_discrete_sequential(palette = "Greens") +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(geo2), ncol = 7) +
  guides(col = "none") +
  labs(
    title = "Renewable energy sources in heating and cooling:<br>
    Significant differences between EU countries",
    subtitle = "Share of renewable energy sources in heating and cooling 2004-2021 (in %)",
    caption = "Data: Eurostat *(nrg_ind_ren)*, EU27 countries. Visualisation: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = "grey19", lineheight = 1.1),
    axis.text.x = element_text(color = "grey30", size = 8),
    axis.text.y = element_text(color = "grey60", size = 6),
    axis.line.x = element_line(size = 0.42),
    axis.line.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    plot.title = element_markdown(face = "bold", color = "black"),
    plot.title.position = "plot",
    plot.caption = element_markdown(),
    strip.text = element_text(family = "Roboto Medium")
  )
ggsave(here(base_path, "22-green-energy.png"), width = 7, height = 5)
