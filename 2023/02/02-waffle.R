library(tidyverse)
library(ggtext)
library(tidytext)
library(here)
# install.packages("schrute")
library(schrute)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)

base_path <- here("2023", "02")

theme_set(theme_minimal(base_family = "Helvetica Neue"))
theme_update(
  plot.background = element_rect(color = "#435774", fill = "#435774"),
  axis.text = element_blank(),
  panel.grid = element_blank(),
  text = element_text(color = "grey92"),
  plot.title = element_markdown(color = "grey98", family = "American Typewriter", 
                                hjust = 0.5, size = 14),
  plot.subtitle = element_textbox(
    lineheight = 1.1, size = 8, width = 1.33, margin = margin(t = 2, b = 10),
    hjust = 0.5, halign = 0.5
  ),
  plot.caption = element_markdown(hjust = 0.5, size = 7),
  strip.text = element_markdown(
    hjust = 0.5, lineheight = 1.2, size = 12, family = "American Typewriter", 
    color = "grey98", margin = margin(t = 8, b = 2, l = 2)),
  legend.position = "top",
  legend.key.size = unit(4, "mm"),
  legend.text = element_text(size = 8, margin = margin(b = 0))
)

data(theoffice)
head(theoffice)
count(theoffice, season, episode)


theoffice %>% 
  distinct(season, episode, writer) %>% 
  separate_rows(writer, sep = ";") %>% 
  count(writer, sort = TRUE)

theoffice_wordcount <- theoffice %>% 
  unnest_tokens(word, text, token = "words") %>% 
  mutate(character = str_remove_all(character, "\\\"")) %>% 
  count(season, episode, character, name = "words_n", sort = TRUE) %>% 
  arrange(season, episode, character) %>% 
  mutate(season_episode = fct_inorder(paste(season, episode, sep = "#")),
         season_episode_id = as.numeric(season_episode))

theoffice_wordcount_season <- theoffice_wordcount %>% 
  group_by(season, character) %>% 
  summarize(words_n = sum(words_n)) %>% 
  mutate(words_share = words_n / sum(words_n)) %>% 
  ungroup()

theoffice_wordcount_season %>% 
  group_by(season) %>% 
  slice_max(order_by = words_share, n = 5, with_ties = TRUE) %>% 
  mutate(season_rank = rank(-words_share)) %>% 
  ungroup() 

pal_office <- 
  c("#FBA93A", "#93BFE5", "#BD4047", "#D8ACD8", "#9C6A4B", "#2AA3A6", "#F0F4EC", 
             "#4BD9EF", "#5D77AA", "#46604F")

theoffice_wordcount_season %>% 
  group_by(season) %>% 
  mutate(
    character2 = fct_lump_n(character, n = 5, w = words_n),
    # add Mr. Brown to Other
    character2 = if_else(character2 == "Mr. Brown", "Other", character2),
    season_rank = rank(-words_share)) %>% 
  # arrange(season_rank, .by_group = TRUE) %>% 
  ungroup() %>% 
  group_by(season, character2) %>% 
  summarize(
    words_share = sum(words_share),
    words_n = sum(words_n),
    .groups = "drop"
  ) %>% 
  mutate(words_share_rnd = round(100 * words_share, 0)) %>%
  group_by(season) %>% 
  mutate(
    character2 = fct_reorder(character2, -words_share_rnd),
    character2 = fct_relevel(character2, "Other", after = Inf),
    # Adjust for round errors (99 or 101 blocks)
    words_share_rnd = case_when(
      character2 == "Other" & sum(words_share_rnd) > 100 ~ words_share_rnd - 1, 
      character2 == "Other" & sum(words_share_rnd) < 100  ~ words_share_rnd + 1, 
      TRUE ~ words_share_rnd)
  ) %>% 
  ungroup() %>% 
  mutate(other_order = ifelse(character2 == "Other", 99, 1)) %>% 
  arrange(season, other_order, -words_share_rnd) %>% 
  # filter(season == 1) %>% 
  ggplot(aes(fill = character2, values = words_share_rnd)) +
  geom_waffle(
    radius = unit(0.2, "npc"), col = "#435774", size = 1
  ) +
  scale_fill_manual(values = pal_office) +
  coord_equal() +
  facet_wrap(vars(season)) +
  labs(
    title = "Who speaks in The Office?",
    subtitle = "The proportion of words spoken by characters in The Office by 
    season (word count). Characters with the highest proportion of words per season are shown.",
    caption = "Data: {schrute} R package. Visualisation: Ansgar Wolsing",
    fill = NULL
  )
ggsave(here(base_path, "02-waffle-the-office-characters.png"), width = 4, height = 5, scale = 1.1)
