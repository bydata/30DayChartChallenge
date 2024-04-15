library(tidyverse)
library(ggtext)
library(here)
library(ggraph)
library(tidygraph)

base_path <- here("2024", "13")

#' Source: Kaggle
#' The Office (US) - Complete Dialogue/Transcript
#' https://www.kaggle.com/datasets/nasirkhalid24/the-office-us-complete-dialoguetranscript

# There are a couple of lines in which the line column does split into two columns
# Avoid this behaviour by guessing the columns only from the first row
df_file <- here(base_path, "The-Office-Lines-V4.csv")
theoffice <- read_csv(df_file, guess_max = 1) %>% 
  select(-7)

# speaker clean up
theoffice <- theoffice %>% 
  mutate(speaker = str_remove_all(speaker, "[\":]"),
         speaker = case_match(
           speaker,
           "(Pam's mom) Heleen" ~ "Pam's mom",
           "AJ" ~ "A.J.",
           "abe" ~ "Gabe",
           .default = speaker
         )) 

# Count the scenes in which characters appear together
characters_scenes <- theoffice %>% 
  distinct(season, episode, scene, speaker)
characters_scenes

characters_scene_count <- characters_scenes %>% 
  count(speaker, name = "speech_acts_n", sort = TRUE)

characters_to_display <- 20
selected_characters <- characters_scene_count |> 
  slice_max(order_by = speech_acts_n, n = characters_to_display) |> 
  pull(speaker)
selected_characters

correlations_by_seasons <- map(unique(characters_scenes$season),
  function(x) {
    characters_scenes |> 
      filter(season == x) |> 
      widyr::pairwise_cor(speaker, scene, method = "spearman")
  }
)

jim_and_pam <- correlations_by_seasons |> 
  bind_rows(.id = "season") |> 
  mutate(season = as.numeric(season)) |> 
  filter(item1 == "Pam" & item2 ==  "Jim") 


# Custom theme
theme_set(
  theme_minimal(base_family = "Outfit Light", base_size = 11) +
  theme(
    plot.background = element_rect(color = "#2B394D", fill = "#2B394D"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.title = element_text(
      family = "American Typewriter", size = 18, hjust = 0, color = "#FBA93A"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.2, size = 8.5, margin = margin(b = -4)),
    plot.caption = element_markdown(hjust = 0, size = 7),
    plot.margin = margin(t = 6, b = 4, l = 6, r = 6),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "white", linewidth = 0.2, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(14, "mm"),
    legend.key.height = unit(4, "mm")
  )
)

# Annotations
seasons_with_annotations <- c(2, 3, 4, 6)
annotations <- tibble(
  season = seasons_with_annotations,
  label_y = c(0.45, -0.08, 0.45, 0.45),
  segment_x = c(2, 3, 4, 6),
  segment_xend = seasons_with_annotations,
  segment_y = c(0.395, -0.07, 0.372, 0.395),
  segment_yend = c(0.24, 0, 0.295, 0.36),
  vjust = c(1, 1, 1, 1),
  description = c(
    "First kiss<br>after the \"Dundies\"", 
    "Jim works in Stamford,<br>a separate Dunder Mifflin branch",
    "Pam and Jim<br>start dating",
    "Pam and Jim get married")
)

titles <- labs(
  title = "When Pam and Jim met in The Office",
  subtitle = "Spearman's rho tells us how closely the appearances of the characters
    Pam Beesly/Halpert and Jim Halpert in the scenes of The Office are related.
    Positive values indicate they often appear together. The higher the value, 
    the more likely they appear in the same scenes.
    A value of 0 signifies there is no relationship between their appearances.
    <br><br><br>
    <span style='font-size:8pt'>\U2193 Correlation (Spearman's rho)</span>",
  caption = "Source: kaggle.com, The Office (US) - Complete Dialogue/Transcript.
    Visualization: Ansgar Wolsing",
  x = "Season",
  y = NULL
)


jim_and_pam |> 
  ggplot(aes(season, correlation, group = 1)) +
  geom_hline(
    aes(yintercept = 0), color = "white", linewidth = 0.2
  ) +
  geom_line(col = "white", linewidth = 1) +
  geom_point(
    data = subset(jim_and_pam, season %in% seasons_with_annotations),
    col = "white", fill = "#2B394D", shape = 21, size = 3, stroke = 0.7) +
  # Annotations & line
  geom_richtext(
    data = annotations,
    aes(
      x = season - 0.1, y = label_y, vjust = vjust,
      label = sprintf("<span style='font-family:\"Outfit SemiBold\";color:#FBA93A'>Season %s</span><br>%s", 
                      season, description)),
    hjust = 0, family = "Outfit Light", color = "white", size = 2.75, 
    fill = colorspace::lighten("#2B394D", 0.2), label.size = 0, lineheight = 1.1, 
    label.padding = unit(1, "mm"), label.r = unit(0.2, "mm")
  ) +
  geom_segment(
    data = annotations,
    aes(x = segment_x, xend = segment_xend, y = segment_y, yend = segment_yend),
    color = "white", linewidth = 0.2
  ) +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.2, 0.5)) +
  titles
  
ggsave(here(base_path, "13-family-the-office-by-season.png"), width = 5, height = 5, scale = 1.1)



## Correlation by episode ------------------------------------------------------

season_episode_keys <- characters_scenes |> 
  distinct(season, episode) |> 
  transmute(season_episode = paste(season, episode, sep = "_")) |> 
  pull()
  
correlations_by_episode <- map(
  season_episode_keys,
  function(x) {
    s <- as.numeric(str_extract(x, "^(\\d)_", 1))
    e <- as.numeric(str_extract(x, "_(\\d+)$", 1))
    characters_scenes |> 
      filter(season == s, episode == e) |> 
      widyr::pairwise_cor(speaker, scene, method = "spearman")                         }
)


jim_and_pam_by_episode <- correlations_by_episode |> 
  bind_rows(.id = "episode") |> 
  mutate(episode = as.numeric(episode)) |> 
  filter(item1 == "Pam" & item2 ==  "Jim") 

episode_ids <- characters_scenes |> 
  distinct(season, episode) |> 
  mutate(sequential_episode_id = row_number()) |> 
  mutate(
    season_center_episode_id = min(sequential_episode_id) + 
      (max(sequential_episode_id) - min(sequential_episode_id)) / 2,
    .by = season
  )


# Titles
titles <- labs(
title = "When Pam and Jim met in The Office",
subtitle = "Spearman's rho tells us how closely the appearances of the characters
  Pam Beesly/Halpert and Jim Halpert in the scenes of The Office are related.
  Positive values indicate they often appear together. The higher the value,
  the more likely they are to appear in the same scenes. Negative values indicate that
  when one character appears, the other is more likely not to be in that scene.
  A value of 0 signifies there is no relationship between their appearances.
  <br><br>
  <span style='font-size:8pt'>\U2193 Correlation (Spearman's rho)</span><br>",
caption = "Source: kaggle.com, The Office (US) - Complete Dialogue/Transcript.
    Visualization: Ansgar Wolsing",
x = "Season",
y = NULL
)


# Annotations
episodes_with_annotations <- c(7, 30, 52, 85, 107, 153)
annotations <- tibble(
  sequential_episode_id = episodes_with_annotations,
  season = c(2:6, 8),
  label_y = c(0.55, -0.2, 0.55, -0.08, 0.77, -0.02),
  segment_x = episodes_with_annotations,
  segment_xend = episodes_with_annotations,
  segment_y = c(0.419, -0.2, 0.457, -0.08, 0.64, -0.02),
  segment_yend = c(0.23, -0.05, 0.23, 0, 0.45, 0.08),
  vjust = 1,
  description = c(
    "First kiss<br>after the \"Dundies\"", 
    "Jim works in Stamford,<br>a separate Dunder Mifflin branch",
    "Pam and Jim start dating",
    "Pam studies graphic design<br>in NYC",
    "Pam and Jim get married in episode 4,<br>their daughter is born
    in episode 18",
    "Jim is in Tallahassee<br>for a \"Special Project\"")
)
season_description_template <- "<span style='font-family:\"Outfit SemiBold\";color:#FBA93A'>Season %s</span><br>%s"

jim_and_pam_by_episode |> 
  ggplot(aes(episode, correlation)) +
  geom_rect(
    data = episode_ids |> 
      group_by(season) |> 
      summarize(min = min(sequential_episode_id), max = max(sequential_episode_id)) |> 
      mutate(min = min - 0.5, max = max + 0.5),
    aes(
      xmin = min, xmax = max, ymin = -Inf, ymax = Inf,
      alpha = factor(season %% 2)
    ), 
    inherit.aes = FALSE, color = "white", fill = "white", linewidth = 0.025,
    show.legend = FALSE
  ) +
  # Season labels
  geom_label(
    data = distinct(episode_ids, season, season_center_episode_id),
    aes(x = season_center_episode_id, y = -0.35, label = season),
    family = "American Typewriter", size = 4, hjust = 0.5, vjust = 1,
    color = "white", fill = NA, label.size = 0.1
  ) +
  geom_hline(
    aes(yintercept = 0), color = "white", linewidth = 0.2
  ) +
  geom_smooth(span = 0.1, method = "loess", color = "white", se = FALSE) +
  # Annotations
  geom_richtext(
    data = annotations,
    aes(
      x = sequential_episode_id - 1, y = label_y, vjust = vjust,
      label = sprintf(season_description_template, season, description)),
    hjust = 0, family = "Outfit Light", color = "white", size = 2.75, 
    fill = colorspace::lighten("#2B394D", 0.2), label.size = 0, lineheight = 1.1, 
    label.padding = unit(1, "mm"), label.r = unit(0.2, "mm")
  ) +
  annotate(
    GeomTextBox,
    x = max(episode_ids$sequential_episode_id), y = 0.4,
    label = "Smoothed line<br>(LOESS, bw = 0.10)",
    color = "white", size = 2.5, family = "Outfit Medium", hjust = 1, halign = 0,
    vjust = 0, fill = NA, box.size = 0, width = 0.15, box.padding = unit(0, "mm")
  ) +
  geom_segment(
    data = annotations,
    aes(x = segment_x, xend = segment_xend, y = segment_y, yend = segment_yend),
    color = "white", linewidth = 0.2
  ) +
  scale_x_continuous(expand = expansion(add = c(1, 0)), breaks = seq(0, 200, 10)) +
  scale_alpha_manual(values = c(0.075, 0)) + 
  coord_cartesian(ylim = c(-0.35, 0.7), clip = "off") +
  titles +
  theme(
    axis.text.x = element_blank(), 
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.2, size = 8.5, margin = margin(b = 0))
  )
ggsave(here(base_path, "13-family-the-office-by-episodes.png"), width = 6, height = 5, scale = 1.1)
