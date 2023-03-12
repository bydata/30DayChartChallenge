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

# outer_ring <- theoffice_wordcount_season %>% 
#   mutate(character2 = fct_lump_n(character, 10, w = words_share)) %>% 
#   group_by(character2, season) %>% 
#   summarize(words_n = sum(words_n), .groups = "drop") %>% 
#   # mutate(words_share_total = words_n / sum(words_n)) %>% 
#   # copy the character2 levels from the inner ring
#   mutate(character2 = factor(character2, levels = levels(inner_ring$character2))) %>%
#   # generate the coordinates for create a stack bar chart from rectangles
#   arrange(desc(character2), season) %>% 
#   mutate(
#     words_share = words_n / sum(words_n),
#     words_share_cumul = cumsum(words_share),
#     ymin = lag(words_share_cumul, 1, default = 0),
#     ymax = words_share_cumul
#   ) %>% 
#   # add season-level word shares
#   left_join(theoffice_wordcount_season, by = c("character2" = "character", "season"), 
#             suffix = c("", ".season")) %>% 
#   select(-words_n.season) %>% 
#   # group_by(season) %>% 
#   # mutate(
#   #   words_share.season = ifelse(character2 == "Other", 
#   #                               1 - sum(words_share.season, na.rm = TRUE), 
#   #                               words_share.season)
#   # ) %>% 
#   # ungroup() %>% 
#   # copy the character2 levels from the inner ring
#   mutate(character2 = factor(character2, levels = levels(inner_ring$character2))) %>% 
#   select(character2, season, words_share, words_share.season, ymin, ymax) %>% 
#   # add seasons in which a character did not appear
#   complete(character2, season = 1:9, fill = list(words_share = 0, words_share.season = 0))


# even spaced rectangles in the outer ring
outer_ring <- theoffice_wordcount_season %>% 
  mutate(character2 = fct_lump_n(character, 10, w = words_share)) %>% 
  group_by(character2, season) %>% 
  summarize(words_n = sum(words_n), .groups = "drop") %>% 
  # add seasons in which a character did not appear
  complete(character2, season = 1:9, fill = list(words_n = 0)) %>% 
  # copy the character2 levels from the inner ring
  mutate(character2 = factor(character2, levels = levels(inner_ring$character2))) %>%
  # generate the coordinates for create a stack bar chart from rectangles
  arrange(desc(character2), season) %>% 
  mutate(
    words_share = words_n / sum(words_n),
    words_share_cumul = cumsum(words_share),
    ymin = lag(words_share_cumul, 1, default = 0),
    ymax = words_share_cumul
  ) %>% 
  # add season-level word shares
  left_join(theoffice_wordcount_season, by = c("character2" = "character", "season"), 
            suffix = c("", ".season")) %>% 
  # copy the character2 levels from the inner ring (again)
  mutate(character2 = factor(character2, levels = levels(inner_ring$character2)),
         words_share.season = replace_na(words_share.season, 0)) %>% 
  select(character2, season, words_share, words_share.season, ymin, ymax)


inner_ring_xmax <- 1.5
outer_ring_factor <- 3

ggplot() +
  # annotate(
  #   "rect",
  #   xmin = 0, xmax = 2, ymin = 0, ymax = 1, fill = "white"
  # ) +
  geom_rect(
    data = outer_ring,
    aes(xmin = 0.25, xmax = inner_ring_xmax + outer_ring_factor * words_share.season, 
        ymin = ymin, ymax = ymax), fill = "white", color = "white", linewidth = 0.1) +
  geom_rect(
    data = outer_ring,
    aes(xmin = 0.25, xmax = inner_ring_xmax + outer_ring_factor * words_share.season, 
        ymin = ymin, ymax = ymax, fill = character2,
        alpha = season), color = "white", linewidth = 0.1) +
  geom_rect(
    data = inner_ring,
    aes(xmin = 0.5, xmax = inner_ring_xmax, ymin = ymin, ymax = ymax, fill = character2),
    alpha = 1, color = "white", linewidth = 0.5) +
  geom_vline(aes(xintercept = 1.5), color = "white") +
  geom_text(
    data = inner_ring,
    aes(x = ifelse(words_share > 0.05, 1.1, 2.25), 
        y = ymin + (ymax - ymin) / 2,
        label = character2,
        # color = ifelse(words_share > 0.05, "grey90", "white")
        ),
    family = "Helvetica Neue", size = 4, hjust = 0.5, color = "grey92"
  ) +
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
  scale_alpha_continuous(range = c(0.3, 1)) +
  coord_polar(theta = "y") +
  guides(alpha = "none", fill = "none") +
  theme_void() +
  theme(
    # plot.background = element_rect(color = "white", fill = "white")
    plot.background = element_rect(color = "#435774", fill = "#435774")
  )
ggsave(here(base_path, "01-part-to-whole-the-office-sunburst.png"), width = 5, 
       height = 4, scale = 2)

