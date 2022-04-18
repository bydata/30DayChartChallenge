library(tidyverse)
library(ggtext)
library(here)
library(grid)

base_path <- here("2022", "18")

oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia","Costa Rica",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan","South Korea",
  "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand",
  "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", 
  "Switzerland", "Turkey", "United Kingdom",  "United States"
)

#' Sources:
#' https://data.oecd.org/air/air-pollution-exposure.htm#indicator-chart
###' Exposure to PM2.5, Micrograms per cubic metre, 1990 – 2019
###' Air quality and health: Exposure to PM2.5 fine particles - countries and regions
#' https://data.oecd.org/air/air-pollution-effects.htm#indicator-chart


exposure <- read_csv(here(base_path, "oecd-pollutionexposure.csv"))
colnames(exposure) <- tolower(colnames(exposure))
exposure_1990 <- exposure %>%   
  filter(time == 1990, measure == "MICGRCUBM") %>% 
  select(location, micgrcubm = value) 
exposure_2019 <- exposure %>%   
  filter(time == 2019, measure == "MICGRCUBM") %>% 
  select(location, micgrcubm = value) 
  
effect <- read_csv(here(base_path, "oecd-pollutioneffect.csv"))
colnames(effect) <- tolower(colnames(effect))
effect_2019 <- filter(effect, time == 2019) %>% 
  select(location, pollution_mortality = value) 

exposure_2019 %>% 
  inner_join(effect_2019, by = "location") %>% 
  ggplot(aes(micgrcubm, pollution_mortality)) +
  geom_point() +
  geom_text(data = . %>% filter(location == "DEU"),
            aes(label = location)) +
  scale_x_log10()+
  scale_y_log10()


exposure_1990 %>% 
  inner_join(effect_2019, by = "location") %>% 
  ggplot(aes(micgrcubm, pollution_mortality)) +
  geom_point() +
  geom_text(data = . %>% filter(location == "DEU"),
            aes(label = location)) +
  scale_x_log10()+
  scale_y_log10()


with(exposure_1990 %>% 
  inner_join(effect_2019, by = "location"), cor(micgrcubm, pollution_mortality))

with(exposure_2019 %>% 
       inner_join(effect_2019, by = "location"), cor(micgrcubm, pollution_mortality))



# change_df <- exposure_1990 %>% 
#   inner_join(exposure_2019, by = "location", suffix = c("_1990", "_2019")) %>% 
#   mutate(change_abs = micgrcubm_2019 - micgrcubm_1990,
#          change_rel = change_abs / micgrcubm_1990,
#          country = countrycode::countrycode(location, origin = "iso3c", destination = "country.name"),
#          continent = countrycode::countrycode(location, origin = "iso3c", destination = "continent")) %>% 
#   na.omit()

selected_years <- c(1990, 2000, 2019)
change_oecd <- exposure %>% 
  filter(time %in% selected_years, measure == "MICGRCUBM") %>%
  select(location, time, value) %>%  
  pivot_wider(id_cols = location, names_from = "time", values_from = "value",
              names_prefix = "micgrcubm_") %>% 
  mutate(change_1990_2019_abs = micgrcubm_2019 - micgrcubm_1990,
         change_1990_2019_rel = change_1990_2019_abs / micgrcubm_1990,
         country = countrycode::countrycode(location, origin = "iso3c", destination = "country.name"),
         continent = countrycode::countrycode(location, origin = "iso3c", destination = "continent")) %>% 
  filter(country %in% oecd_countries) %>% 
  na.omit()


change_oecd %>% 
  ggplot(aes(micgrcubm_1990, micgrcubm_2019)) +
  geom_abline(slope = 1, intercept = 10, color = "red") +
  geom_abline(slope = 1, intercept = 0, color = "grey40") +
  geom_abline(slope = 1, intercept = -10, color = "blue") +
  geom_point() +
  geom_text(data = . %>% filter(location == "DEU", ),
            aes(label = location)) +
  coord_equal()


colors <- c("2019" = "#8E3C82", "1990" = "#DCB73D", "2000" = "grey55")
bg_color <- "#E0EAF0"
point_size <- 2

change_oecd %>% 
  filter(country %in% oecd_countries) %>%
  mutate(country = fct_reorder(country, micgrcubm_2019)) %>% 
  ggplot(aes(country, group = country)) +
  geom_point(aes(y = micgrcubm_2019, col = "2019", fill = stage(after_scale = alpha("white", 0.4))), 
             shape = 21, size = point_size) +
  geom_segment(aes(xend = country, y = micgrcubm_1990, yend = micgrcubm_2019),
               col = "grey61", size = 0.3,
               arrow = arrow(type = "closed", angle = 20, length = unit(1.5, "mm"))) +
  geom_point(aes(y = micgrcubm_1990), 
             shape = 21, size = point_size, fill = bg_color) +
  geom_point(aes(y = micgrcubm_1990, col = "1990", fill = stage(after_scale = alpha(color, 0.4))), 
             shape = 21, size = point_size) +
  geom_point(aes(y = micgrcubm_2000), 
             shape = 21, size = point_size, fill = bg_color) +
  geom_point(aes(y = micgrcubm_2000, col = "2000", fill = stage(after_scale = alpha(color, 0.4))), 
             shape = 21, size = point_size) +
  # Annotate years
  ggrepel::geom_text_repel(
    data = subset(exposure, 
                  location == change_oecd$location[which.max(x = change_oecd$micgrcubm_2019)] &
                    time %in% selected_years & measure == "MICGRCUBM"), 
    aes(x = nrow(change_oecd) + 1.5, y = value, label = time, color = factor(time)),
    inherit.aes = FALSE, family = "Raleway SemiBold", size = 2.5
  ) +
  scale_color_manual(values = colors) +
  coord_flip(clip = "off") +
  guides(color = "none") +
  labs(
    title = glue::glue("Development of air pollution exposure 
                       <span style='color:{colors['1990']}'>1990</span>,
                       <span style='color:{colors['2000']}'>2000</span>,
                       <span style='color:{colors['2019']}'>2019</span>"),
    subtitle = " Fine particulate matter (PM2.5) is the air pollutant that poses the greatest 
      risk to health globally, affecting more people than any other pollutant.
      Chronic exposure to PM2.5 considerably increases the risk of respiratory and
      cardiovascular diseases in particular. OECD member countries.",
    caption = "**Source:** OECD. Air quality and health: Exposure to PM2.5 fine particles -
    countries and regions.<br>**Visualization:** Ansgar Wolsing",
    x = NULL,
    y = "Exposure to PM2.5, mg per m<sup>3</sup>") + 
  theme_minimal(base_family = "Raleway") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    axis.title.x.bottom = element_markdown(),
    axis.line.x = element_line(color = "grey12"),
    axis.ticks.x = element_line(color = "grey12"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.major.x = element_line(size = 0.1),
    panel.grid.major.y = element_line(size = 0.4),
    panel.grid.minor = element_blank(),
    text = element_text(lineheight = 1.2),
    plot.title = element_markdown(face = "bold", color = "black"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 0.98, size = 9),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot")
ggsave(here(base_path, "18-oecd-air-pollution.png"), width = 6, height = 7)  


#' Fine particulate matter (PM2.5) is the air pollutant that poses the greatest 
#' risk to health globally, affecting more people than any other pollutant. 
#' Chronic exposure to PM2.5 considerably increases the risk of respiratory and 
#' cardiovascular diseases in particular. Data refer to population exposure to
#' more than 10 micrograms/m3 and are expressed as annual averages.
