library(tidyverse)
library(ggtext)
library(here)
library(grid)
library(lubridate)
# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

base_path <- here("2022", "17")

# Find Haalnd
# fb_league_urls("GER", gender = "M", season_end_year = 2022) %>% 
#   fb_teams_urls()
# fb_player_urls("https://fbref.com/en/squads/add600ae/Dortmund-Stats")
# # "https://fbref.com/en/players/1f44ac21/Erling-Haaland"

haaland <- fb_player_season_stats("https://fbref.com/en/players/1f44ac21/Erling-Haaland", 
                     stat_type = "standard")

# List of leagues to filter league appearances
leagues <- c("1. Eliteserien", "1. Bundesliga")

shiny_colors <- c("#e8fc35", "#34e9fa", "#347afa", "#c73bff", "#fa34e6")
bg_color <- "grey4"

## Career annotations
career_annotations <- tribble(
  ~player_name,     ~Squad,              ~Season,      ~x, ~xend, ~y, ~yend, ~hjust, ~label,
  "Erling Haaland",  "Bryne",          "2016",       0,    0,  -1,     -1,      0, "**2016** | Professional debut at Bryne FK (Norway, 2nd tier)",
  "Erling Haaland",  "Molde",             "2017",       3,    1,  2,     2,      0,  "**2017** | Debut in 1st tier<br>at Molde BK (Norway)",
  "Erling Haaland",  "Molde",             "2018",       4.1,    4.1, 12,    12,      0,  "**2018** | 2nd year at Molde BK (Norway)",
  "Erling Haaland",  "RB Salzburg",       "2018-2019",  0,    0,  5,     1,      0,  "**18-19** | Transfer to RB Salzburg,<br>played only 2 games",
  "Erling Haaland",  "RB Salzburg",       "2019-2020",  4,    4,  18,    16,      1,  "**19-20** | Salzburg | 16 league goals",
  "Erling Haaland",  "Dortmund", "2019-2020",  1.9,    1.9,  13,    13,      1,  "**January 2020** | Transfer to BVB",
  "Erling Haaland",  "Dortmund", "2020-2021",  6,    6,  27,    27,      1,  "**20-21** | BVB | 27 league goals",
  "Erling Haaland",  "Dortmund", "2021-2022",  8,    8,   20,    20,      1,  "**21-22** | BVB | Injuries, yet still 21 goals",
)


df_plot <- tibble(haaland) %>%
  filter(Comp %in% leagues) %>% 
  select(Season, player_name, Squad, Gls, Ast, MP_Time, Min_Time, Age) %>% 
  # add row for first season in 2nd tier
  # source: https://www.transfermarkt.com/erling-haaland/leistungsdatendetails/spieler/418560
  add_row(Season = "2016", player_name = "Erling Haaland", Squad = "Bryne", 
          Gls = 0, Ast = 0, MP_Time = 16, Min_Time = 422, Age = 15,
          .before = 1
  ) %>% 
  # generate a season id to transition along in the animation
  mutate(season_id = row_number()) %>% 
  # generate a variable with the leading season for geom_segment
  group_by(player_name) %>% 
  mutate(across(c(Gls, Ast), ~lead(.x, 1), .names = "{.col}_lead"),
         across(c(Gls, Ast), ~lag(.x, 1), .names = "{.col}_lag"),
         ) %>% 
  ungroup %>% 
  mutate(season_short = str_remove_all(Season, "20(?!(-|$))")) %>% 
  # add career annotations
  left_join(career_annotations, by = c("player_name", "Season", "Squad")) 

p <- df_plot %>%
  ggplot(aes(Ast, Gls, col = player_name, group = season_id)) +
  geom_curve(
    aes( x = Ast_lag, xend = Ast, y = Gls_lag, yend = Gls),
    curvature = 0.3, size = 0.8) +
  geom_point(aes(size = Min_Time), shape = 21, fill = "grey18") +
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
  labs(
    title = sprintf("<span style='color:%s; font-size:40pt'>Erling Haaland</span><br>Goals & Assists", shiny_colors[1]),
    subtitle = "Goals and assists by Erling Haaland in *domestic league* games.",
    caption = "<i>\\* 21-22 incomplete (stats as of May 9, 2022)</i><br><br>
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
    plot.title = element_markdown(
      hjust = 0.5, color = "grey99", family = "Bangers", size = 24),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.8, hjust = 0.5, margin = margin(t = 8, b = 8), lineheight = 1.2),
    plot.caption = element_markdown(hjust = 0.5, color = "grey74"),
    panel.grid.major = element_line(color = "grey30", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_markdown(
      family = "Bangers", size = 16),
    plot.margin = margin(t = 4, b = 4, l = 16, r = 16)
    )
ggsave(here(base_path, "17-connected-haaland.png"), width = 10, height = 7.5, dpi = 500)
#' 
#' 
#' ## ANIMATION -------------------------------------------------------------------
#' 
#' library(gganimate)
#' 
#' # Version 1: Building the whole plot -------------------------------------------
#' p_anim <- p +
#'   # remove the subtitle
#'   labs(subtitle = "") +
#'   transition_reveal(season_id)
#' 
#' animate(p_anim, res = 200, width = 8, height = 6, units = "in", duration = 25,
#'         fps = 15, end_pause = 60)
#' anim_save(here(base_path, "17-connected-2.gif"))
#' 
#' 
#' # Version 2: Showing only the latest 3 seasons  --------------------------------
#' 
#' # p_anim <- p +
#' #   transition_states(season_id) +
#' #   shadow_trail(max_frames = 3) +
#' #   exit_recolor(color = "white", fill = NA)
#' 
#' p_anim <-  p +
#'   # remove the subtitle
#'   labs(subtitle = "") +
#'   transition_states(season_id) +
#'   shadow_mark(alpha = 0.8, colour = colorspace::desaturate(colour, 0.6), 
#'               size = size / 1.5, past = TRUE, future = FALSE)
#' 
#' animate(p_anim, res = 200, width = 8, height = 6, units = "in", duration = 25,
#'         fps = 10, end_pause = 60)
#' anim_save(here(base_path, "17-connected_part-only.gif"))
#' 
#' 
#' ## Accessibility:
#' #' Table with goals and assists by Messi and Ronaldo: 
#' #' https://en.wikipedia.org/wiki/Messi%E2%80%93Ronaldo_rivalry#Club_statistics
