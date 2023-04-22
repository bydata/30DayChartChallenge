library(tidyverse)
library(ggtext)
library(tidytext)
library(here)
# install.packages("schrute")
library(schrute)

base_path <- here("2023", "23")

data(theoffice)
head(theoffice)
count(theoffice, season, episode)

# Speaker names clean up
theoffice <- theoffice %>% 
  mutate(
    character = str_remove_all(character, "[\":]"),
    character = case_match(
      character,
      "(Pam's mom) Heleen" ~ "Pam's mom",
      "AJ" ~ "A.J.",
      "abe" ~ "Gabe",
      "Todd Packer" ~ "Todd",
      "Deangelo" ~ "DeAngelo",
      .default = character
    ),
    character = case_when(
      # Pete Halpert vs. Pete Miller
      character == "Pete" & season < 9 ~ "Pete H.",
      TRUE ~ character
    )
    )

theoffice_wordcount_episode <- theoffice %>% 
  unnest_tokens(word, text, token = "words") %>% 
  mutate(character = str_remove_all(character, "\\\"")) %>% 
  count(season, episode, character, name = "words_n", sort = TRUE) %>% 
  group_by(season, episode, character) %>% 
  summarize(words_n = sum(words_n)) %>% 
  mutate(words_share = words_n / sum(words_n),
         words_episode_rank = rank(-words_n)) %>% 
  ungroup() %>% 
  group_by(season, episode) %>% 
  mutate(season_episode_id = cur_group_id()) %>% 
  ungroup() %>% 
  arrange(season, episode, words_episode_rank)

theoffice_wordcount_season <- theoffice %>% 
  unnest_tokens(word, text, token = "words") %>% 
  mutate(character = str_remove_all(character, "\\\"")) %>% 
  count(season, episode, character, name = "words_n", sort = TRUE) %>% 
  group_by(season, character) %>% 
  summarize(words_n = sum(words_n)) %>% 
  mutate(words_share = words_n / sum(words_n)) %>% 
  ungroup()

theoffice_wordcount_total <- theoffice_wordcount_season %>% 
  group_by(character) %>% 
  summarize(words_n = sum(words_n)) %>% 
  mutate(words_share = words_n / sum(words_n))


# Find the characters with the most words within each season
top_characters_season <- theoffice_wordcount_season %>% 
  group_by(season) %>% 
  slice_max(words_share, n = 15, with_ties = TRUE) %>% 
  ungroup() %>% 
  distinct(character) %>% 
  pull(character)

# Find the characters with the most words within each episode
top_characters_episode <- theoffice_wordcount_episode %>% 
  group_by(season, episode) %>% 
  slice_max(words_share, n = 3, with_ties = TRUE) %>% 
  ungroup() %>% 
  distinct(character) %>% 
  pull(character)


pal_office <- 
  c("#FBA93A", "#93BFE5", "#BD4047", "#D8ACD8", "#9C6A4B", "#2AA3A6", "#F0F4EC", 
             "#4BD9EF", "#5D77AA", "#46604F", "grey50")

theoffice_wordcount_episode %>% 
  filter(character %in% top_characters_season) %>% 
  mutate(
    character = fct_reorder(character, words_n, .fun = sum),
    words_episode_rank = floor(words_episode_rank),
    words_episode_rank_grp = ifelse(
      words_episode_rank <= 3, as.character(words_episode_rank), "4+")
         ) %>% 
  complete(season, episode, character) %>% 
  ggplot(aes(character, season_episode_id, fill = words_episode_rank_grp)) +
  # background fill per line
  geom_rect(
    aes(xmin = as.numeric(character) - 0.4, xmax = as.numeric(character) + 0.4,
        ymin = -Inf, ymax = Inf),
    fill = "#1F2041" # "#5D77AA"
  ) +
  geom_tile(height = 0.8, width = 0.8) +
  scale_x_discrete() +
  scale_y_discrete(position = "right", expand = c(0, 0)) +
  scale_fill_manual(values = c("#FBA93A", "#D14081", "#EF798A", "#5D77AA", "#1F2041"),
                    na.value = NA) +
  coord_flip() +
  facet_grid(cols = vars(season), scales = "free_x", space = "free_x", shrink = TRUE) +
  guides(fill = guide_legend(title.position = "left")) +
  labs(
    title = "Who speaks most in the episodes of The Office?",
    caption = "Source: {schrute} R package. Visualisation: Ansgar Wolsing",
    x = NULL, y = NULL,
    fill = "Episode rank (by # of words)"
  ) +
  theme_minimal(base_family = "Outfit", base_size = 10) +
  theme(
    plot.background = element_rect(color = "#435774", fill = "#435774"),
    legend.position = "top",
    legend.key.height = unit(3, "mm"),
    legend.key.width = unit(1.5, "mm"),
    legend.box.just = "center",
    legend.title = element_markdown(size = 9),
    legend.text = element_markdown(size = 7),
    text = element_text(color = "grey92"),
    axis.text = element_text(color = "grey92", size = 6),
    axis.text.x = element_blank(),
    plot.title = element_text(
      hjust = 0.5, family = "American Typewriter", size = 18, 
      color = "white"),
    plot.caption = element_markdown(hjust = 0.5),
    panel.grid = element_blank(),
    strip.text = element_text(color = "grey92", size = 10, family = "Outfit SemiBold")
  )
ggsave(here(base_path, "23-tiles-theoffice.png"), width = 7.5, height = 5, dpi = 600)
