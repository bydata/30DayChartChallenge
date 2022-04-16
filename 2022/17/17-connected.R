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


library(gganimate)

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
  ggplot(aes(Ast, Gls, col = player_name, group = season_id)) +
  geom_segment(aes(xend = Ast_lead, yend = Gls_lead),
               size = 0.8 #, arrow = arrow(length = unit(3, "mm"), type = "open", angle = 20)
               ) +
  geom_point(aes(size = MP_Time), shape = 21, fill = "grey18") +
  ggrepel::geom_label_repel(aes(label = season_short), 
                            size = 2, fill = alpha("grey2", 0.6),
                            family = "Fira Sans Condensed",
                            # set seed so that the position is constant throughout the transition frames
                            seed = 1
                            ) +
  scale_y_continuous() +
  scale_color_manual(values = c("#e8fc35", "#c73bff")) +
  guides(
    color = "none"
  ) +
  facet_wrap(vars(player_name)) +
  labs(
    x = "Assists",
    y = "Goals"
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey4"),
    legend.position = "bottom",
    text = element_text(color = "grey90"),
    axis.text = element_text(color = "grey80"),
    panel.grid.major = element_line(color = "grey30", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(color = "grey90", family = "Bangers", size = 12)
    )
p
ggsave(here(base_path, "17-connected-messi-ronaldo.png"), width = 10, height = 7, dpi = 500)


p_anim <- p +
  transition_reveal(season_id)

animate(p_anim, res = 200, width = 10, height = 7, units = "in")
anim_save(here(base_path, "17.gif"))
