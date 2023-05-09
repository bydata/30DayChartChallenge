library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "29")


## Prepare FiveThirtyEight SPI match ratings ===================================

#' Source: FiveThirtyEight Club Soccer Predictions
#' Download latest SPI match ratings from: 
#' https://github.com/fivethirtyeight/data/tree/master/soccer-spi
url <- "https://projects.fivethirtyeight.com/soccer-api/club/spi_matches_latest.csv"
spi_matches <- read_csv(url)

# Select remaining Bundesliga games
spi_matches_buli_remaining <- spi_matches %>% 
  filter(league == "German Bundesliga", season == "2022",
         date >= as.Date("2023-05-04")) %>% 
  select(season, date, league, team1, team2, prob1, prob2, probtie)
spi_matches_buli_remaining

# Current standings
current_team_points <- tibble(
  team = c("Bayern Munich", "Borussia Dortmund", "1. FC Union Berlin",
           "SC Freiburg", "RB Leipzig"),
  current_pts = c(62, 61, 56, 56, 54)
)


## Simulate season outcomes ====================================================

# Recode probabilities from the teams' perspective
spi_matches_buli_remaining <- spi_matches_buli_remaining %>% 
  pivot_longer(cols = c(team1, team2), names_to = "home_away", values_to = "team") %>% 
  mutate(home_away = ifelse(home_away == "team1", "home", "away")) %>% 
  mutate(
    prob_win = ifelse(home_away == "home", prob1, prob2),
    prob_loss = ifelse(home_away == "home", prob2, prob1),
    prob_draw = probtie
  ) %>% 
  select(-c(prob1, prob2, probtie)) 


# Match simulations for n_samples seasons, create a dataframe with a 
# outcomes list column
n_samples <- 10000
outcomes_points <- c(3, 0, 1)
set.seed(123)
outcomes_simulations <- spi_matches_buli_remaining %>% 
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

# Determine final club ranks for simulated outcomes
outcomes_simulations_season_ranks <- outcomes_simulations_season %>% 
  inner_join(current_team_points, by = "team") %>% 
  mutate(predicted_season_pts = current_pts + outcome_total_pts) %>% 
  group_by(simulation_id) %>% 
  mutate(predicted_season_rank = rank(-predicted_season_pts)) %>% 
  ungroup()

# View simulated season results
outcomes_simulations_season_ranks %>% 
  count(team, predicted_season_rank) %>% 
  filter(predicted_season_rank <= 1.5)

# Sample 100 outcomes from the total set of simulated outcomes
relevant_teams <- c("Borussia Dortmund", "Bayern Munich")
set.seed(111)
sampled_season_outcomes <- outcomes_simulations_season_ranks %>% 
  filter(team %in% relevant_teams) %>% 
  group_by(simulation_id) %>% 
  # arrange(predicted_season_rank, .by_group = TRUE) %>% 
  mutate(pts_diff = predicted_season_pts - lag(predicted_season_pts, 1)) %>% 
  na.omit() %>% 
  select(-team) %>% 
  ungroup() %>% 
  slice_sample(n = 100, replace = FALSE)

discrete_outcomes_count <- sampled_season_outcomes %>% 
  mutate(predicted_champion = case_when(
    pts_diff > 0 ~ "Dortmund",
    pts_diff == 0 ~ "Bayern (goal diff.)",
    pts_diff < 0 ~ "Bayern")) %>% 
  count(predicted_champion) 
discrete_outcomes_count


## Plots =======================================================================

# Style elements for the plot
font_color <- "#041729"
bg_color <- "#CAE5FF"
y_label_pos <- seq(-10, 10, 2)
    
sampled_season_outcomes %>% 
  ggplot(aes(x = 0, pts_diff, fill = pts_diff > 0 )) +
  # custom grid lines
  annotate(
    "segment",
    y = y_label_pos, yend = y_label_pos,
    col = c(rep("#3478ba", 5), NA, rep("#052e57", 5)),
    x = -0.1, xend = 0.1, linewidth = 0.25
  ) +
  # custom axis labels
  annotate(
    "text",
    x = -0.105,
    y = y_label_pos,
    label = abs(y_label_pos),
    col = c(rep("#3478ba", 5), NA, rep("#052e57", 5)),
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
    x = -0.11, y = c(-10, 10),
    label = paste("Point difference in<br>favour of", c("Bayern", "Dortmund")),
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
    y = c(-8, 8),
    label = sprintf("<span style='font-size: 10pt'>%s wins<br></span>%d in 100", 
                    c("Bayern", "Dortmund"), c(78, 22)),
    col = c("#3478ba", "#052e57"),
    fill = NA, family = "Outfit Medium", label.size = 0, vjust = 0, size = 6
  ) +
  annotate(
    "richtext",
    x = 0.115,
    y = 0.5,
    label = "Ties counted for Bayern<br>given their much better<br>goal difference",
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
    title = "<span style='color:#3478ba'>Bayern</span> is favoured to win the 
Bundesliga title",
    subtitle = "I simulated the remaining 5 Bundesliga match weeks 10,000 times 
to see who wins most often. The sample of 100 outcomes below provides an 
indication of the range of scenarios.",
    caption = "There is a negligible chance (< 0.03 %) that neither Dortmund or 
Bayern win the title. This is ignored in this chart.<br>
**Source:** Own calculations based on FiveThirtyEight Club Soccer Ratings. 
**Visualisation:** Ansgar Wolsing"
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
ggsave(here(base_path, "29-monochrome-bundesliga-champions-predictions-beforemd31.png"),
           width = 7, height = 5.5)
    
    
## Version with club colours ============================================

# Style elements for the plot
font_color <- "grey8"
bg_color <- "white"
bvb_color <- "#FDE100"
fcb_color <- "#DC052D"
y_label_pos <- seq(-10, 10, 2)

sampled_season_outcomes %>% 
  ggplot(aes(x = 0, pts_diff, fill = pts_diff > 0 )) +
  # custom grid lines
  annotate(
    "segment",
    y = y_label_pos, yend = y_label_pos,
    col = c(rep(fcb_color, 5), NA, rep("grey8", 5)),
    x = -0.1, xend = 0.1, linewidth = 0.25
  ) +
  # custom axis labels
  annotate(
    "text",
    x = -0.105,
    y = y_label_pos,
    label = abs(y_label_pos),
    col = c(rep(fcb_color, 5), NA, rep("grey8", 5)),
    size = 4, family = "PT Mono", hjust = 0.5
  ) +
  geom_hline(
    aes(yintercept = 0),
    linewidth = 1.75, col = font_color) +
  ggbeeswarm::geom_quasirandom(
    aes(col = pts_diff > 0),
    shape = 21, size = 6, width = 0.1, varwidth = TRUE,
    method = "quasirandom"
  ) +
  annotate(
    "richtext",
    x = -0.11, y = c(-10, 10),
    label = paste("Point difference in<br>favour of", c("Bayern", "Dortmund")),
    col = c(fcb_color, "grey8"),
    hjust = c(0, 1),
    fill = NA, label.size = 0, family = "PT Mono", size = 3, vjust = 1
  ) +
  annotate(
    "label",
    x = -0.12, y = 0,
    label = "TIE",
    family = "Outfit", size = 4, color = font_color,
    fill = bg_color, label.size = 0, label.r = unit(0, "mm")
  ) + 
  # discrete outcome labels
  annotate(
    "richtext",
    x = 0.11,
    y = c(-8, 8),
    label = sprintf("<span style='font-size: 10pt'>%s wins<br></span>%d in 100", 
                    c("Bayern", "Dortmund"), c(78, 22)),
    col = c(fcb_color, "grey8"),
    fill = NA, family = "Outfit Medium", label.size = 0, vjust = 0, size = 6
  ) +
  annotate(
    "richtext",
    x = 0.115,
    y = 0.5,
    label = "Ties counted for Bayern<br>given their much better<br>goal difference",
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
  scale_fill_manual(values = c(fcb_color, bvb_color)) +
  scale_color_manual(values = c("white", "grey8")) +
  coord_flip(xlim = c(-0.14, 0.14), clip = "off") +
  guides(fill = "none", color = "none") +
  labs(
    title = "<span style='color:#DC052D'>Bayern</span> is favoured to win the 
     Bundesliga title",
    subtitle = "I simulated the remaining 5 Bundesliga match weeks 10,000 times 
      to see who wins most often. The sample of 100 outcomes below provides an 
      indication of the range of scenarios.",
    caption = "There is a negligible chance (< 0.03 %) that neither Dortmund or 
      Bayern win the title. This is ignored in this chart.<br>
      **Source:** Own calculations based on FiveThirtyEight Club Soccer Ratings. 
    **Visualisation:** Ansgar Wolsing"
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
ggsave(here(base_path, "bundesliga-champions-predictions-beforemd31.png"),
       width = 7, height = 5.5)
