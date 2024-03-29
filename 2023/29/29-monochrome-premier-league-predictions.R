library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "29")


#' Source: FiveThirtyEight Club Soccer Predictions
#' Download latest SPI match ratings from: https://github.com/fivethirtyeight/data/tree/master/soccer-spi
url <- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv"
spi_matches <- read_csv(url)

# Select remaining Premier League predictions (2023 matchday 30 starting April 28th, 2023)
spi_matches_epl_remaining <- spi_matches %>% 
  filter(league == "Barclays Premier League", season == "2022",
         date >= as.Date("2023-04-28")) %>% 
  select(season, date, league, team1, team2, prob1, prob2, probtie)
spi_matches_epl_remaining


spi_matches_epl_remaining <- spi_matches_epl_remaining %>% 
  pivot_longer(cols = c(team1, team2), names_to = "home_away", values_to = "team") %>% 
  mutate(home_away = ifelse(home_away == "team1", "home", "away")) %>% 
  # recode probabilities from the teams' perspective
  mutate(
    prob_win = ifelse(home_away == "home", prob1, prob2),
    prob_loss = ifelse(home_away == "home", prob2, prob1),
    prob_draw = probtie
  ) %>% 
  select(-c(prob1, prob2, probtie)) 

# # Select relevant teams
relevant_teams <- c("Arsenal", "Manchester City")
# spi_matches_buli_remaining_relevant_teams <- spi_matches_buli_remaining %>% 
#   filter(team %in% relevant_teams)


current_team_points <- tibble(
  team = c("Arsenal", "Manchester City", "Newcastle", "Manchester United"),
  current_pts = c(75, 73, 62, 60)
)


# Match simulations for n seasons
outcomes_classes <- c("w", "l", "d")
outcomes_points <- c(3, 0, 1)
n_samples <- 10000
set.seed(123)
outcomes_simulations <- spi_matches_epl_remaining %>% 
  rowwise() %>% 
  mutate(outcomes = list(sample(outcomes_points, n_samples, replace = TRUE, 
                           prob = c(prob_win, prob_loss, prob_draw)))) %>%
  ungroup()

# Calculate points for simulated match outcomes
outcomes_simulations_season <- outcomes_simulations %>% 
  group_by(team, date) %>% 
  unnest(c(outcomes)) %>%
  mutate(simulation_id = row_number()) %>% 
  ungroup() %>% 
  rename(outcome_pts = outcomes) %>% 
  group_by(simulation_id, team) %>% 
  summarize(outcome_total_pts = sum(outcome_pts), .groups = "drop")

outcomes_simulations_season_ranks <- outcomes_simulations_season %>% 
  inner_join(current_team_points, by = "team") %>% 
  mutate(predicted_season_pts = current_pts + outcome_total_pts) %>% 
  group_by(simulation_id) %>% 
  mutate(predicted_season_rank = rank(-predicted_season_pts)) %>% 
  ungroup()
outcomes_simulations_season_ranks


outcomes_simulations_season_ranks %>% 
  count(team, predicted_season_rank) %>% 
  filter(predicted_season_rank <= 1.5)

# Sample 100 outcomes from the total set of outcomes
set.seed(111)
sampled_season_outcomes <- outcomes_simulations_season_ranks %>% 
  filter(team %in% relevant_teams) %>% 
  group_by(simulation_id) %>% 
  mutate(pts_diff = predicted_season_pts - lag(predicted_season_pts, 1)) %>% 
  na.omit() %>% 
  select(-team) %>% 
  ungroup() %>% 
  slice_sample(n = 100, replace = FALSE)


discrete_outcomes_count <- sampled_season_outcomes %>% 
  mutate(predicted_champion = case_when(
    pts_diff > 0 ~ "Manchester City",
    pts_diff == 0 ~ "Manchster City (goal diff.)",
    pts_diff < 0 ~ "Arsenal")) %>% 
  count(predicted_champion) 
discrete_outcomes_count


# Style elements for the plot
font_color <- "#041729"
bg_color <- "#CAE5FF"
y_label_pos <- seq(-14, 10, 2)

sampled_season_outcomes %>% 
  mutate(pts_diff = -pts_diff) %>% 
  ggplot(aes(x = 0, pts_diff, fill = pts_diff > 0 )) +
  # custom grid lines
  annotate(
    "segment",
    y = y_label_pos, yend = y_label_pos,
    col = c(rep("#3478ba", 7), NA, rep("#052e57", 5)),
    x = -0.1, xend = 0.1, linewidth = 0.25
  ) +
  # custom axis labels
  annotate(
    "text",
    x = -0.105,
    y = y_label_pos,
    label = abs(y_label_pos),
    col = c(rep("#3478ba", 7), NA, rep("#052e57", 5)),
    size = 4, family = "PT Mono", hjust = 0.5
  ) +
  geom_hline(
    aes(yintercept = 0),
    linewidth = 1.75, col = font_color) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, col = "grey99", size = 6, width = 0.1, varwidth = TRUE,
    method = "quasirandom"
  ) +
  annotate(
    "richtext",
    x = -0.11, y = c(-16, 10),
    label = paste("Point difference in<br>favour of", c("Manchester City", "Arsenal")),
    col = c("#3478ba", "#052e57"),
    hjust = c(0, 1),
    fill = NA, label.size = 0, family = "PT Mono", size = 3, vjust = 1
  ) +
  annotate(
    "label",
    x = -0.12, y = 0,
    label = "TIE",
    family = "Outfit", size = 4, color = font_color,
    fill = "#CAE5FF", label.size = 0, label.r = unit(0, "mm")
  ) + 
  # discrete outcome labels
  annotate(
    "richtext",
    x = 0.11,
    y = c(-12, 9),
    label = sprintf("<span style='font-size: 10pt'>%s wins<br></span>%d in 100", 
                    c("Manchester City", "Arsenal"), c(89, 11)),
    col = c("#3478ba", "#052e57"),
    fill = NA, family = "Outfit Medium", label.size = 0, vjust = 0, size = 6
  ) +
  annotate(
    "richtext",
    x = 0.115,
    y = 0.5,
    label = "Ties counted for Man City<br>given their much better<br>goal difference",
    color = font_color, family = "Outfit", size = 3, fill = NA, hjust = 0,
    label.size = 0, label.padding = unit(0, "mm")
  ) +
  annotate(
    "curve",
    x = 0.10, xend = 0.06,
    y = 1.5, yend = 0.3,
    curvature = -0.2, col = font_color, linewidth = 0.25,
    arrow = arrow(angle = 20, type = "closed", length = unit(1, "mm"))
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#3478ba", "#052e57")) +
  coord_flip(xlim = c(-0.14, 0.14), clip = "off") +
  guides(fill = "none") +
  labs(
    title = "<span style='color:#3478ba'>City</span> is strongly 
    favoured to win the Premier League title",
    subtitle = "I simulated the remaining Premier League matches 10,000 times 
    to see who wins most often. The sample of 100 outcomes below provides an 
    indication of the range of scenarios.",
    caption = "**Source:** Own calculations based on 
    FiveThirtyEight Club Soccer Ratings. **Visualisation:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    text = element_text(color = font_color),
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_textbox(
      width = 0.95, hjust = 0.5, halign = 0.5, lineheight = 1.1, 
      margin = margin(b = 16)),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 1.5)
  )
ggsave(here(base_path, "29-monochrome-premier-league-champions-predictions.png"),
       width = 7, height = 5.5)
