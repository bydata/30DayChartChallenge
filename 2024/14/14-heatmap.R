library(tidyverse)
library(ggtext)
library(here)
library(ggraph)
library(tidygraph)

base_path <- here("2024", "14")

#' Source: Kaggle
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

correlations <- characters_scenes |> 
  widyr::pairwise_cor(speaker, scene, method = "spearman") |> 
  filter(item1 %in% selected_characters & item2 %in% selected_characters) |> 
  mutate(item1 = factor(item1), item2 = fct_rev(factor(item2))) |> 
  arrange(correlation) 

min_correlation <- min(correlations$correlation)
max_correlation <- max(correlations$correlation)

# Determine the order of characters by the office setting, e.g. Accouting sitting together
# First, set the levels by the frequency they appear (that already does some ordering)
(character_levels_by_setting <- factor(selected_characters, levels = selected_characters))
character_levels_by_setting <- fct_relevel(character_levels_by_setting, c("Erin", "Angela"), after = 5)
character_levels_by_setting <- fct_relevel(character_levels_by_setting, "Stanley", after = 10)

correlations |> 
  mutate(
    across(c(item1, item2),
    function(x) factor(x, levels = levels(character_levels_by_setting))),
    item2 = fct_rev(item2)) |> 
  ggplot(aes(item1, item2, fill = correlation)) +
  # geom_tile(color = "#435774", height = 0.9, width = 0.9) +
  geom_point(
    aes(size = abs(correlation)),
    shape = 21, stroke = 0
  ) +
  geom_hline(
    data = data.frame(yintercept = seq(1.5, characters_to_display - 0.5, 1)),
    aes(yintercept = yintercept),
    color = "white", linewidth = 0.1
  ) +
  geom_vline(
    data = data.frame(xintercept = seq(1.5, characters_to_display - 0.5, 1)),
    aes(xintercept = xintercept),
    color = "white", linewidth = 0.1
  ) +
  scale_x_discrete(position = "top", labels = toupper, expand = c(0, 0.4)) +
  scale_y_discrete(labels = toupper, expand = c(0, 0.4)) +
  scale_fill_gradient2(
    low = "#46604F", mid = "white", high = "#BD4047", midpoint = 0,
    breaks = seq(floor(min_correlation * 10) / 10,
                 ceiling(max_correlation * 10) / 10, 0.1),
    limits = c(floor(min_correlation * 10) / 10,
                 ceiling(max_correlation * 10) / 10)) +
  scale_size_area(max_size = 5) +
  coord_equal(expand = TRUE) +
  guides(
    size = "none", 
    fill = guide_colorsteps(title.position = "top")) +
  labs(
    title = "Who Appears With Whom in The Office",
    subtitle = "",
    x = NULL, y = NULL, fill = "Correlation (Spearman's Rho)"
  ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    plot.background = element_rect(color = "#435774", fill = "#435774"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.text.x.top = element_text(angle = 50, hjust = 0),
    plot.title = element_text(family = "American Typewriter", size = 18, hjust = 0.5),
    plot.margin = margin(t = 4, b = 4, l = 4, r = 4),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(14, "mm"),
    legend.key.height = unit(4, "mm")
  )
ggsave(here(base_path, "14-heatmap.png"), width = 5, height = 5, scale = 1.2)
