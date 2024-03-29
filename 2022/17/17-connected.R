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

shiny_colors <- c("#e8fc35", "#34e9fa", "#347afa", "#c73bff", "#fa34e6")
bg_color <- "grey4"

## Career annotations
career_annotations <- tribble(
  ~player_name,       ~Season,      ~x, ~xend, ~y, ~yend, ~hjust, ~label,
  "Lionel Messi",      "2004-2005",  4,     0,  1,     1,      0,  "**04-05** | Professional debut (FC Barcelona)",
  "Lionel Messi",      "2006-2007",  3,     3, 18,    14,      0,  "**06-07** | 14 goals at 19 yrs",
  "Lionel Messi",      "2008-2009",  8,    11, 23,    23,      1,  "**08-09** | 23 goals, 11 assists",
  "Lionel Messi",      "2019-2020", 21,    21, 22,    25,      1,  "**19-20** | 21 assists",
  "Lionel Messi",      "2011-2012", 11,    16, 50,    50,      1,  "**11-12** | 50 goals",
  "Lionel Messi",      "2009-2010", 10,    10, 42,    42,      1,  "**Prime at Barcelona**",
  "Lionel Messi",      "2020-2021",  6,     9, 30,    30,      1,  "**20-21** | Last season<br>in Barcelona",
  "Lionel Messi",      "2021-2022", 16,    13,  3,     3,      0,  "**21-22** | Transfer to PSG*",
  
  "Cristiano Ronaldo", "2002-2003",  6,     3,  3,     3,      0, "**02-03** | Professional debut (Sporting CP)",
  "Cristiano Ronaldo", "2003-2004",  9,     4,  4.5,   4.5,    0, "**03-04** | Breakout at Manchester Utd.",
  "Cristiano Ronaldo", "2009-2010",  9,     7, 26,    26,      0, "**09-10** | Transfer to Real Madrid<br>(record fee at the time)",
  "Cristiano Ronaldo", "2014-2015", 16,    16, 52,    48,      0, "**14-15** | Peak season in<br>terms of goals and assists",
  "Cristiano Ronaldo", "2012-2013", 15,    15, 40,    40,      0, "**Prime at Real Madrid**",
  "Cristiano Ronaldo", "2018-2019", 12,     8, 21,    21,      0, "**18-19** | 1st season for Juventus",
  "Cristiano Ronaldo", "2021-2022", 10,     3, 15,    15,      0, "**21-22** | Return to Manchester*"
)


df_plot <- bind_rows(messi, ronaldo) %>%
  tibble() %>%
  filter(Comp %in% leagues) %>% 
  group_by(Season, player_name) %>% 
  summarize(across(c(Gls, Ast, MP_Time, Min_Time), sum),
            Squad = last(Squad),
            Comp = last(Comp)
            ) %>% 
  # generate a season id to transition along in the animation
  mutate(season_id = cur_group_id()) %>% 
  ungroup() %>% 
  # generate a variable with the leading season for geom_segment
  group_by(player_name) %>% 
  mutate(across(c(Gls, Ast), ~lead(.x, 1), .names = "{.col}_lead"),
         across(c(Gls, Ast), ~lag(.x, 1), .names = "{.col}_lag"),
         ) %>% 
  ungroup %>% 
  mutate(season_short = str_remove_all(Season, "20(?!(-|$))")) %>% 
  # add career annotations
  left_join(career_annotations, by = c("player_name", "Season")) %>% 
  mutate(player_name_label = 
           sprintf("<span style='color: %s'>%s</span>",
                   ifelse(player_name == "Cristiano Ronaldo", shiny_colors[1], shiny_colors[2]),
                   player_name
           ))

p <- df_plot %>%
  ggplot(aes(Ast, Gls, col = player_name, group = season_id)) +
  # geom_segment(
  geom_curve(
    aes(
      # xend = Ast_lead, yend = Gls_lead
      x = Ast_lag, xend = Ast, y = Gls_lag, yend = Gls),
    curvature = 0.3,
               size = 0.8 #, arrow = arrow(length = unit(3, "mm"), type = "open", angle = 20)
               ) +
  geom_point(aes(size = Min_Time), shape = 21, fill = "grey18") +
  # rectangle Ronaldo prime
  geom_rect(
    data = data.frame(
      player_name_label = unique(df_plot$player_name_label[df_plot$player_name == "Cristiano Ronaldo"]),
      season_id = 10L),
    aes(xmin = 7, xmax = 17, ymin = 29, ymax = 50, group = season_id),
    inherit.aes = FALSE, 
    stat = "unique", lty = "dashed", fill = NA, col = "grey70", size = 0.2
  ) +
  # rectangle Messi prime
  geom_rect(
    data = data.frame(
      player_name_label = unique(df_plot$player_name_label[df_plot$player_name == "Lionel Messi"]), 
      season_id = 8L),
    aes(xmin = 8, xmax = 22, ymin = 29, ymax = 52, group = season_id),
    inherit.aes = FALSE,
    stat = "unique", lty = "dashed", fill = NA, col = "grey70", size = 0.2
  ) +
  # Career annotations
  geom_richtext(
    aes(x = x, y = y, label = label, hjust = hjust, group = season_id),
    inherit.aes = FALSE,
    label.size = 0, fill = alpha("grey4", 0.6), col = "grey90", 
    family = "Fira Sans Condensed", size = 3, label.padding = unit("0", "mm")
  ) +
  geom_segment(
    aes(x = x, xend = xend, y = y, yend = yend, group = season_id),
    inherit.aes = FALSE, col = "grey90", size = 0.2
  ) +
  
  scale_y_continuous() +
  scale_color_manual(values = shiny_colors) +
  coord_cartesian(clip = "off") +
  guides(
    color = "none"
  ) +
  facet_wrap(vars(player_name_label)) +
  labs(
    title = "Lionel Messi & Cristiano Ronaldo - Head to head",
    subtitle = "Lionel Messi and Cristiano Ronaldo are considered two of the most
    exceptional football players of all time. 
    This chart shows their goals and assists scored in their *domestic leagues* in 
    each season of their professional career. Both players have achieved 40+ goals each 
    in 3 league seasons.",
    caption = "<i>\\* 21-22 incomplete (stats as of Apr 17, 2022)</i><br><br>
    **Data:** FBRef, worldfootballR R package. **Visualization:** Ansgar Wolsing",
    x = "Assists",
    y = "Goals",
    size = "Minutes played"
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    legend.position = "bottom",
    text = element_text(color = "grey90"),
    axis.title = element_text(family = "Fira Sans Condensed SemiBold"),
    axis.text = element_text(color = "grey80"),
    plot.title = element_text(
      hjust = 0.5, color = "grey99", family = "Bangers", size = 24),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.8, hjust = 0.5, margin = margin(t = 8, b = 8), lineheight = 1.2),
    plot.caption = element_markdown(hjust = 0.5, color = "grey74"),
    panel.grid.major = element_line(color = "grey30", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_markdown(
      # color = "grey90", 
      family = "Bangers", size = 16),
    plot.margin = margin(t = 4, b = 4, l = 16, r = 16)
    )
ggsave(here(base_path, "17-connected-messi-ronaldo.png"), width = 10, height = 7.5, dpi = 500)


## ANIMATION -------------------------------------------------------------------

library(gganimate)

# Version 1: Building the whole plot -------------------------------------------
p_anim <- p +
  # remove the subtitle
  labs(subtitle = "") +
  transition_reveal(season_id)

animate(p_anim, res = 200, width = 8, height = 6, units = "in", duration = 25,
        fps = 15, end_pause = 60)
anim_save(here(base_path, "17-connected-2.gif"))


# Version 2: Showing only the latest 3 seasons  --------------------------------

# p_anim <- p +
#   transition_states(season_id) +
#   shadow_trail(max_frames = 3) +
#   exit_recolor(color = "white", fill = NA)

p_anim <-  p +
  # remove the subtitle
  labs(subtitle = "") +
  transition_states(season_id) +
  shadow_mark(alpha = 0.8, colour = colorspace::desaturate(colour, 0.6), 
              size = size / 1.5, past = TRUE, future = FALSE)

animate(p_anim, res = 200, width = 8, height = 6, units = "in", duration = 25,
        fps = 10, end_pause = 60)
anim_save(here(base_path, "17-connected_part-only.gif"))


## Accessibility:
#' Table with goals and assists by Messi and Ronaldo: 
#' https://en.wikipedia.org/wiki/Messi%E2%80%93Ronaldo_rivalry#Club_statistics
