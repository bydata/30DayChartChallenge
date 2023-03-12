library(tidyverse)
library(ggtext)
library(tidytext)
library(here)
# install.packages("schrute")
library(schrute)

base_path <- here("2023", "01")

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
    .default = character
  )) 

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


pal_office <- 
  c("#FBA93A", "#93BFE5", "#BD4047", "#D8ACD8", "#9C6A4B", "#2AA3A6", "#F0F4EC", 
             "#4BD9EF", "#5D77AA", "#46604F", "grey50")
             
inner_ring <- theoffice_wordcount_total %>% 
  mutate(character2 = fct_lump_n(character, 10, w = words_share)) %>% 
  group_by(character2) %>% 
  summarize(words_share = sum(words_share)) %>% 
  mutate(character2 = fct_reorder(character2, words_share),
         character2 = fct_relevel(character2, "Other", after = 0)) %>% 
  # generate the coordinates for create a stack bar chart from rectangles
  arrange(desc(character2)) %>% 
  mutate(
    words_share_cumul = cumsum(words_share),
    ymin = lag(words_share_cumul, 1, default = 0),
    ymax = words_share_cumul
  ) 

outer_ring <- theoffice_wordcount_season %>% 
  mutate(character2 = fct_lump_n(character, 10, w = words_share)) %>% 
  group_by(character2, season) %>% 
  summarize(words_n = sum(words_n), .groups = "drop") %>% 
   mutate(
    words_share_total = words_n / sum(words_n)) %>% 
  # mutate(
  #   character2 = fct_reorder(character2, words_share_total),
  #   character2 = fct_relevel(character2, "Other", after = 0)
  # ) %>% 
  # copy the character2 levels from the inner ring
  mutate(character2 = factor(character2, levels = levels(inner_ring$character2))) %>% 
  # generate the coordinates for create a stack bar chart from rectangles
  arrange(desc(character2), season) %>% 
  mutate(
    words_share = words_n / sum(words_n),
    words_share_cumul = cumsum(words_share),
    ymin = lag(words_share_cumul, 1, default = 0),
    ymax = words_share_cumul
  ) 

ggplot() +
  geom_rect(
    data = outer_ring,
    aes(xmin = 0.25, xmax = 2, ymin = ymin, ymax = ymax, fill = character2,
        alpha = season), color = "white", linewidth = 0.1) +
  geom_rect(
    data = inner_ring,
    aes(xmin = 0.5, xmax = 1.5, ymin = ymin, ymax = ymax, fill = character2),
    alpha = 1, color = "white", linewidth = 0.5) +
  geom_vline(aes(xintercept = 1.5), color = "white") +
  # geom_col(aes(fill = character2)) +
  annotate(
    "rect", xmin = 0, xmax = 0.75, ymin = -Inf, ymax = Inf,
    fill = "white"
           ) +
  annotate(
    "text",
    x = 0, y = 0, label = "The Office",
    family = "American Typewriter", fontface = "plain", color = "grey10", size = 7
  ) +
  scale_fill_manual(values = rev(pal_office)) +
  coord_polar(theta = "y") +
  theme_void()


