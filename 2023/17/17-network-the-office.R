library(tidyverse)
library(ggtext)
library(here)
library(ggraph)
library(tidygraph)

base_path <- here("2023", "17")

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

connections <- theoffice %>% 
  count(season, episode, scene, speaker, name = "speech_acts_n") %>% 
  group_by(season, episode) %>% 
  widyr::pairwise_count(speaker, scene, wt = speech_acts_n, upper = FALSE) %>% 
  # widyr::pairwise_cor(speaker, scene, upper = FALSE) %>% 
  ungroup() %>% 
  group_by(season, item1, item2) %>% 
  summarize(n = sum(n), .groups = "drop")
connections

characters_season_count <- theoffice %>% 
  count(season, speaker, name = "speech_acts_n")

graph_by_season <- connections %>% 
  group_split(season, .keep = FALSE) %>% 
  map(function(x) as_tbl_graph(x, directed = FALSE)) 
  
show_season <- 7
p <- graph_by_season[[show_season]] %>% 
  activate(nodes) %>%
  left_join(subset(characters_season_count, season == show_season), by = c("name" = "speaker")) %>%
  ggraph(layout = "fr") +
  geom_edge_fan(aes(edge_width = n), alpha = 0.2) +
  geom_node_point(aes(size = speech_acts_n)) +
  geom_node_label(
    aes(filter = speech_acts_n > 5, label = name),
    family = "Helvetica", size = 3, label.size = 0, fill = alpha("white", 0.2)) +
  scale_edge_width(range = c(0.1, 2)) +
  theme_void()
ü

layer_data(p, 2)
