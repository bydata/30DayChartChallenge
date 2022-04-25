library(tidyverse)
library(lubridate)
library(here)

polls <- read_rds("data/wahlrecht_umfragen.rds")
colnames(polls) <- c("id", "institute", "date", "respondents", "party", "share")
polls <- polls %>% 
  mutate(
    party = case_when(
      party == "CDUCSU" ~ "CDU/CSU",
      party == "GRÜNE" ~ "GREENS",
      party == "LINKE" ~ "LEFTIST",
      TRUE ~party),
    party = fct_inorder(party))


# Custom theme
theme_set(
  theme_minimal(base_family = "Lato", base_size = 16) +
    theme(legend.position = "bottom", 
          legend.justification = "left", 
          legend.key.height = unit(2.5, "mm"),
          plot.background = element_rect(fill = "grey98"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = 0.3, color = "grey85"),
          panel.grid.minor.y = element_line(size = 0.15, color = "grey85"),
          text = element_text(color = "grey40", lineheight = 1.2),
          plot.title = element_markdown(
            color = "grey2", face = "bold", margin = margin(t = 4, b = 12)),
          # plot.subtitle = element_textbox_simple(
          #   size = 8),
          plot.caption = element_markdown(hjust = 1),
          axis.title = element_text(color = "grey45"),
          axis.title.y = element_text(hjust = 1),
          axis.title.y.right = element_blank(),
          axis.text = element_text(color = "grey45"),
          plot.margin = margin(8, 8, 8, 8))
)


# Party colors
unique(polls$party)
party_colors <- c(
  "CDU/CSU" = "grey9",
  "SPD" = "#ca0002",
  "GREENS" = rgb(100, 161, 45, maxColorValue = 255),
  "FDP" = colorspace::darken("#ffed00", 0.1),
  "LEFTIST" = "purple",
  "AfD" = rgb(0, 158, 224, maxColorValue = 255))


