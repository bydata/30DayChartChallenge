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


# Are there countries in which women are taller then men 100 years ago?
height %>% 
  filter(Year %in% c(1896, 1996)) %>% 
  rename(male_height = 4, female_height = 5) %>% 
  pivot_wider(names_from = "Year", values_from = c("male_height", "female_height")) %>% 
  mutate(height_diff = female_height_1996 - male_height_1896) %>% 
  arrange(-height_diff) %>% 
  filter(height_diff > 0) %>% 
  View()
  

height_prep <- height %>% 
  filter(Year %in% c(1896, 1996)) %>% 
  pivot_longer(cols = starts_with("Mean"), names_to = "gender", values_to = "avg_height") %>% 
  mutate(gender = str_extract(gender, "\\b(fe)?male"),
         gender = factor(gender)) 

height_prep %>% 
  ggplot(aes(gender, avg_height, group = Entity)) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = gender, 
        color = Entity == "Portugal" & (gender == "male" & Year == 1896 | gender == "female" & Year == 1996), 
        stroke = ifelse(Entity == "Portugal", 0.8, 0.3)),
    shape = 21
  ) +
  # highlight Portugal
  geom_richtext(
    data = data.frame(
      Entity = "Portugal", Year = c(1896, 1996), gender = factor(c("male", "female")),
      avg_height = c(159, 163)),
    aes(
      x = as.numeric(gender) - 0.1, y = 174,
      label = sprintf("**%s**,<br> %s born %d<br>**%d cm**", Entity, gender, Year, avg_height)),
    stat = "unique",
    family = "Outfit", size = 2,
    label.size = 0, fill = NA, vjust = 0, hjust = 0, label.padding = unit(0, "mm")
  ) +
  geom_curve(
    data = ~subset(., Entity == "Portugal" & (Year == 1896 & gender == "male")),
    aes(x = as.numeric(gender) + 0.2, xend = as.numeric(gender) + 0.1, 
        y = 174.5, yend = avg_height),
    inherit.aes = FALSE, linewidth = 0.2, curvature = -0.2
  ) + 
  geom_curve(
    data = ~subset(., Entity == "Portugal" & (gender == "female" & Year == 1996)),
    aes(x = as.numeric(gender) - 0.12, xend = as.numeric(gender) - 0.2, 
        y = 174.5, yend = avg_height),
    inherit.aes = FALSE, linewidth = 0.2, curvature = 0.2
  ) + 
  scale_x_discrete(position = "top", labels = toupper) +
  scale_fill_manual(values = c("female" = "#64113F", "male" = "#F18F01")) +
  scale_color_manual(values = c("white", "grey20")) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(Year), labeller = labeller(Year = function(x) paste0("*", x))) +
  guides(fill = "none", color = "none") +
  labs(
    title = "People are taller today than 100 years ago",
    subtitle = "Average male and female height by birth year per country (1896 vs. 1996)",
    caption = "Source: OurWorldInData, NCD RisC. Visualisation: Ansgar Wolsing",
    x = NULL, y = "Average height in cm" 
  ) +
  theme_minimal(base_family = "Outfit", base_size = 10) +
  theme(
    plot.background = element_rect(color = "grey96", fill = "grey96"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 7),
    strip.text = element_text(family = "Chivo", size = 12),
    strip.placement = "outside"
  )
ggsave(here(base_path, "08-human-avg-height-1896-1996.png"), width = 5, height = 4)
