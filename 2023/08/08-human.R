library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "08")

#' Source: OWID, NCD RisC
#' https://ourworldindata.org/grapher/average-height-by-year-of-birth?tab=table
#' Download the full data

height <- read_csv(here(base_path, "average-height-by-year-of-birth.csv"))



height %>% 
  filter(Year == 1996) %>% 
  ggplot(aes(`Mean male height (cm)`, `Mean female height (cm)`, group = Entity)) +
  geom_point()

# Animated version
# library(gganimate)
# height %>% 
#   pivot_longer(cols = starts_with("Mean"), values_to = "avg_height") %>% 
#   ggplot(aes(name, avg_height, group = Entity)) +
#   ggbeeswarm::geom_quasirandom() +
#   transition_time(Year)


height %>% 
  filter(Year %in% c(1896, 1996)) %>% 
  pivot_longer(cols = starts_with("Mean"), names_to = "gender", values_to = "avg_height") %>% 
  mutate(gender = str_extract(gender, "\\b(fe)?male")) %>% 
  ggplot(aes(gender, avg_height, group = Entity)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = gender, color = Entity == "United States"),
    shape = 21, stroke = 0.3 #, size = 1.5, color = "grey90"
  ) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("female" = "#64113F", "male" = "#F18F01")) +
  scale_color_manual(values = c("white", "grey20")) +
  facet_wrap(vars(Year)) +
  guides(fill = "none", color = "none") +
  labs(
    x = NULL 
  ) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = "grey96", fill = "grey96"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.placement = "outside"
  )
ggsave(here(base_path, "08-human-avg-height-1896-1996.png"), width = 5, height = 4)
