library(tidyverse)
library(ggtext)
library(here)
library(grid)
library(lubridate)
# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

base_path <- here("2022", "17")

# Find Messi
fb_league_urls("FRA", gender = "M", season_end_year = 2022) %>% 
  fb_teams_urls()
fb_player_urls("https://fbref.com/en/squads/e2d8892c/Paris-Saint-Germain-Stats")

# Find Cristiano Ronaldo
fb_league_urls("ENG", gender = "M", season_end_year = 2022) %>% 
  fb_teams_urls()
fb_player_urls("https://fbref.com/en/squads/19538871/Manchester-United-Stats")


messi <- fb_player_season_stats("https://fbref.com/en/players/d70ce98e/Lionel-Messi", 
                     stat_type = "standard")
ronaldo <- fb_player_season_stats("https://fbref.com/en/players/dea698d9/Cristiano-Ronaldo", 
                                  stat_type = "standard")


# List of leagues to filter league appearances
leagues <- c("1. La Liga", "1. Premier League", "1. Ligue 1", "1. Serie A", "1. Primeira Liga")


## Career annotations
career_annotations <- tribble(
  ~player_name,       ~Season,      ~x, ~xend, ~y, ~yend, ~hjust, ~label,
  "Lionel Messi",      "2004-2005",  4,     0,  1,     1,      0,  "**04-05** | Professional debut (FC Barcelona)",
  "Lionel Messi",      "2006-2007",  3,     3, 18,    14,      0,  "**06-07** | 14 goals at 19 yrs",
  "Lionel Messi",      "2008-2009",  8,    11, 23,    23,      1,  "**08-09** | 23 goals, 11 assists",
  "Lionel Messi",      "2019-2020", 21,    21, 22,    25,      1,  "**19-20** | 21 assists",
  "Lionel Messi",      "2019-2020", 13,    16, 50,    50,      1,  "**11-12** | 50 goals",
  "Lionel Messi",      "2009-2010", 10,    10, 42,    42,      1,  "**Prime at Barcelona**",
  "Lionel Messi",      "2020-2021",  6,     9, 30,    30,      1,  "**21-22** | Last season<br>in Barcelona",
  "Lionel Messi",      "2021-2022", 16,    13,  3,     3,      0,  "**21-22** | Transfer to PSG*",
  
  "Cristiano Ronaldo", "2002-2003",  6,     3,  3,     3,      0, "**02-03** | Professional debut (Sporting CP)",
  "Cristiano Ronaldo", "2003-2004",  8,     4,  4,     4,      0, "**03-04** | Breakout at Manchester Utd.",
  "Cristiano Ronaldo", "2014-2015", 16,    16, 52,    48,      0, "**14-15** | Peak season in<br>terms of goals and assists",
  "Cristiano Ronaldo", "2012-2013", 15,    15, 40,    40,      0, "**Prime at Real Madrid**",
  "Cristiano Ronaldo", "2018-2019", 12,     8, 21,    21,      0, "**18-19** | 1st season for Juventus",
  "Cristiano Ronaldo", "2021-2022", 10,     3, 12,    12,      0, "**21-22** | Return to Manchester*"
)


p <- bind_rows(messi, ronaldo) %>%
  tibble() %>%
  filter(Comp %in% leagues) %>% 
  group_by(Season, player_name) %>% 
  summarize(across(c(Gls, Ast, MP_Time), sum),
            Squad = last(Squad),
            Comp = last(Comp)
            ) %>% 
  # generate a season id to transition along in the animation
  mutate(season_id = cur_group_id()) %>% 
  ungroup() %>% 
  # generate a variable with the leading season for geom_segment
  group_by(player_name) %>% 
  mutate(across(c(Gls, Ast), ~lead(.x, 1), .names = "{.col}_lead")) %>% 
  ungroup %>% 
  mutate(season_short = str_remove_all(Season, "20(?!(-|$))")) %>% 
  left_join(career_annotations, by = c("player_name", "Season")) %>% 
  ggplot(aes(Ast, Gls, col = player_name, group = season_id)) +
  # geom_segment(
  geom_curve(
    aes(xend = Ast_lead, yend = Gls_lead),
    curvature = 0.3,
               size = 0.8 #, arrow = arrow(length = unit(3, "mm"), type = "open", angle = 20)
               ) +
  geom_point(aes(size = MP_Time), shape = 21, fill = "grey18") +
  ## ggrepel::geom_label_repel(
  # geom_label(
  #   aes(label = season_short),
  #   size = 2, fill = alpha("grey2", 0.6),
  #   family = "Fira Sans Condensed",
  #   # set seed so that the position is constant throughout the transition frames
  #   # seed = 1
  # ) +
  geom_richtext(
    aes(x = x, y = y, label = label, hjust = hjust, group = season_id),
    inherit.aes = FALSE,
    label.size = 0, fill = NA, col = "grey90", 
    family = "Fira Sans Condensed", size = 3
  ) +
  geom_segment(
    aes(x = x, xend = xend, y = y, yend = yend, group = season_id),
    inherit.aes = FALSE, col = "grey90", size = 0.2
  ) +
  # Ronaldo prime
  geom_rect(
    data = data.frame(player_name = "Cristiano Ronaldo", season_id = 10L),
    aes(xmin = 5, xmax = 17, ymin = 29, ymax = 49, group = season_id),
    inherit.aes = FALSE, 
    stat = "unique", lty = "dashed", fill = NA, col = "grey70", size = 0.2
  ) +
  # Messi prime
  geom_rect(
    data = data.frame(player_name = "Lionel Messi", season_id = 8L),
    aes(xmin = 8, xmax = 20, ymin = 29, ymax = 52, group = season_id),
    inherit.aes = FALSE,
    stat = "unique", lty = "dashed", fill = NA, col = "grey70", size = 0.2
  ) +
  
  scale_y_continuous() +
  scale_color_manual(values = c("#e8fc35", "#c73bff")) +
  coord_cartesian(clip = "off") +
  guides(
    color = "none"
  ) +
  facet_wrap(vars(player_name)) +
  labs(
    x = "Assists",
    y = "Goals",
    size = "Matches played"
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey4"),
    legend.position = "bottom",
    text = element_text(color = "grey90"),
    axis.text = element_text(color = "grey80"),
    panel.grid.major = element_line(color = "grey30", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(color = "grey90", family = "Bangers", size = 18)
    )
ggsave(here(base_path, "17-connected-messi-ronaldo.png"), width = 10, height = 7, dpi = 500)



## ANIMATION -------------------------------------------------------------------

library(gganimate)
p_anim <- p +
  transition_reveal(season_id)

animate(p_anim, res = 200, width = 10, height = 7, units = "in", duration = 25,
        end_pause = 40)
anim_save(here(base_path, "17.gif"))
