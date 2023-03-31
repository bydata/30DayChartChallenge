library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "11")

# Sources: 
# FIFA 23: https://www.kaggle.com/datasets/sanjeetsinghnaik/fifa-23-players-dataset
# alternative: https://www.kaggle.com/datasets/bryanb/fifa-player-stats-database
# FIFA 22: https://www.kaggle.com/datasets/stefanoleone992/fifa-22-complete-player-dataset
# FIFA 15-21: https://www.kaggle.com/datasets/stefanoleone992/fifa-21-complete-player-dataset
# FIFA 10-14: https://github.com/kafagy/fifa-FUT-Data

# Load data
fifa_files <- list.files(here(base_path, "data"), pattern = ".csv")
players <- map(here(base_path, "data", fifa_files), read_csv)
players <- set_names(players, fifa_files)
# align list item names
names(players) <- paste("FIFA", str_extract(names(players), "\\d{2}"))
# order list items by edition
players <- players[order(names(players))]

# check the column names per edition
map(players, colnames)
map(players, function(x) length(colnames(x)))

# 15 to 22 have the same structure: short_name, overall
# 10 to 14 have the same structure: NAME, RATING
# 23: `Known As`, Overall 
columns_10_14 <- c("NAME", "RATING")
columns_15_22 <- c("short_name", "overall", "potential", "age")
columns_23 <- c("Known As", "Overall", "Potential", "Age")

players_only_ratings <- players
players_only_ratings[paste("FIFA", 10:14)] <- map(
  paste("FIFA", 10:14), function(x) players[[x]] %>% 
    select(all_of(columns_10_14)) %>% 
    rename(player_name = 1, overall = 2)) 
players_only_ratings[paste("FIFA", 15:22)] <- map(
  paste("FIFA", 15:22), function(x) players[[x]] %>% 
    select(all_of(columns_15_22)) %>% 
    rename(player_name = 1, overall = 2))
players_only_ratings[["FIFA 23"]] <- 
  players_only_ratings[["FIFA 23"]] %>% 
  select(all_of(columns_23)) %>% 
  rename(player_name = 1, overall = 2, potential = 3, age = 4)

players_only_ratings_df <- bind_rows(players_only_ratings, .id = "edition") 

players_only_ratings_df %>% 
  count(edition)

players_only_ratings_df %>% 
  ggplot(aes(overall)) +
  geom_histogram() +
  scale_x_continuous(limits = c(40, NA)) +
  facet_wrap(vars(edition))

players_only_ratings_df %>% 
  ggplot(aes(overall)) +
  geom_histogram(aes(fill = edition), alpha = 0.2) +
  theme_minimal()

players_only_ratings_df %>% 
  ggplot(aes(overall)) +
  geom_density(aes(fill = edition), alpha = 0.2) +
  theme_minimal()


players_only_ratings_df %>% 
  filter(str_detect(player_name, "Mbapp|Messi$")) %>% 
  mutate(year = as.numeric(paste0("20", str_extract(edition, "\\d{2}")))) %>% 
  ggplot(aes(year, overall, col = player_name)) +
  geom_line()


players_only_ratings_df %>% 
  filter(str_detect(player_name, "Mbapp")) %>% 
  mutate(year = as.numeric(paste0("20", str_extract(edition, "\\d{2}")))) %>% 
  ggplot(aes(year, overall)) +
  geom_line()


players_only_ratings_df %>% 
  filter(str_detect(player_name, "E. (Haaland|Håland)")) %>% 
  mutate(year = as.numeric(paste0("20", str_extract(edition, "\\d{2}")))) %>% 
  pivot_longer(cols = c(overall, potential), names_to = "rating_type") %>% 
  ggplot(aes(year, value, col = rating_type)) +
  geom_line() +
  geom_point()

# High potentials from FIFA 15
high_potentials_class15 <- players_only_ratings_df %>% 
  filter(edition == "FIFA 15", age <= 20) %>% 
  filter(player_name != "Junior Malanda") %>% 
  slice_max(potential, n = 20, with_ties = TRUE) %>% 
  # adjust names
  mutate(player_name2 = case_match(
    player_name,
    "Lucas Piazon" ~ "Lucas Piazón",
    .default = player_name
  )) %>% 
  left_join(subset(players_only_ratings_df, edition == "FIFA 23"), 
            by = c("player_name2" = "player_name")) %>% 
  mutate(potential_vs_rating = overall.y - potential.x) %>% 
  # keep only the best player in case of duplicate names
  group_by(player_name) %>% 
  slice_max(overall.y, n = 1, with_ties = FALSE) %>% 
  ungroup() 

high_potentials_class15 %>% 
  ggplot(aes(player_name, potential_vs_rating)) +
  geom_segment(aes(xend = player_name, y = 0, yend = potential_vs_rating)) +
  geom_point() +
  coord_flip()

high_potentials_class15 %>% 
  mutate(player_name = fct_reorder(player_name, overall.y)) %>% 
  ggplot(aes(player_name)) +
  geom_segment(aes(xend = player_name, y = potential.x, yend = overall.y),
               arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  geom_point(aes(y = potential.x, col = "Potential 2015")) +
  geom_point(aes(y = overall.y, col = "Rating 2015")) +
  coord_flip()




## Calculate percentiles to supplement raw values (ratings calculation might change from edition to another)
percentiles <- c(seq(0, 0.95, 0.01), seq(0.955, 1, 0.005))
players_only_ratings_df %>% 
  group_split(edition) %>% 
  map_dfr(~quantile(.x$overall, probs = percentiles), .id = "edition") %>% 
  pivot_longer(cols = -edition, names_to = "percentile") %>% 
  View()


