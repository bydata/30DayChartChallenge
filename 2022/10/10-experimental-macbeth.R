# remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(tidyverse)
library(tidytext)
library(ggtext)
library(here)

base_path <- here("2022", "10")

macbeth_url <- "https://www.gutenberg.org/cache/epub/1129/pg1129.txt"
macbeth_lines <- read_lines(macbeth_url)

start_line <- 282
end_line <- 2899
exclude_lines <- 2358:2365

text_df <- tibble(line = macbeth_lines[start_line:end_line]) %>% 
  mutate(line = str_squish(line),
         line_id = row_number()) %>% 
  # remove empty or irrelevant lines
  filter(line != "" | line_id %in% exclude_lines) %>% 
  filter(!str_detect(line, "SERVICE THAT CHARGES|WITH PERMISSION|COMMERCIALLY")) %>% 
  # extract the act and scene
  mutate(act = str_extract(line, "^ACT\\b.+?\\."),
         scene = str_extract(line, "SCENE\\b.+?\\.")) %>% 
  fill(act, scene, .direction = "down") %>% 
  # extract the character name who speaks
  mutate(speaker = str_extract(line, "^[A-Z\\s]+\\."),
         speaker = str_remove(speaker, "\\.$"),
         speaker = ifelse(str_detect(speaker, "^ACT|SCENE\\b"), 
                            NA_character_, speaker),
         speaker = str_to_title(speaker)) %>% 
  # manage the switch of speakers from scene to scene
  group_by(act, scene) %>% 
  fill(speaker, .direction = "down") %>% 
  ungroup() %>% 
  # remove lines without a speaker
  filter(!is.na(speaker)) %>%
  # remove speaker names from the lines
  mutate(line = str_remove(line, paste0(speaker, ". "))) 


# count the number of words per speaker in each act and scene
word_count_speakers <- text_df %>% 
  # recode the witches and murderers into one category
  mutate(speaker_grp = ifelse(str_detect(speaker, "Witch$"), "Three Witches", speaker),
         speaker_grp = ifelse(str_detect(speaker_grp, "Mutherers?$"), "Mutherers", speaker_grp)
         ) %>% 
  unnest_tokens(word, line, token = "words", drop = TRUE) %>% 
  count(act, scene, speaker_grp, speaker, name = "word_count")

# how many speakers?
unique(word_count_speakers$speaker)
unique(word_count_speakers$speaker_grp)

# lump together speakers with rare appearances
word_count_speakers %>% 
  count(speaker, wt = word_count, name = "total_word_count", sort = TRUE)

word_count_speakers %>% 
  count(speaker, name = "appearance_count", sort = TRUE)

# identify character with only a few appearances
few_appearances_speakers <- word_count_speakers %>% 
  group_by(speaker_grp) %>% 
  summarize(scenes_count = n_distinct(act, scene),
            word_count_total = sum(word_count)
            ) %>% 
  filter(scenes_count <= 3, word_count_total < 500) %>% 
  pull(speaker_grp)


## Custom color palette by character affiliation
speaker_grp_levels = c(
  "Macbeth", "Lady Macbeth", 
  "Duncan", "Malcolm", "Macduff", "Ross",
  "Banquo", "Lennox",
  "Three Witches",
  "Other")
color_palette <- paletteer::paletteer_d(
  "palettetown::pidgey")[c(9, 8,
                           2, 5, 4, 6,
                           12, 6,
                           3,
                           11 )]

# Annotations
plot_titles <- list(
  title = "Who speaks when in Shakespeare's MACBETH?",
  subtitle = "Distribution of speech share (number of words) per character in 
  each scene. Acts are separated with vertical lines.",
  caption = "Visualization: Ansgar Wolsing"
)

story_annotations <- tibble(
  x = c()
  
)

word_count_speakers %>% 
  mutate(speaker_grp = ifelse(speaker_grp %in% c("All", few_appearances_speakers), "Other", speaker_grp),
         speaker_grp = factor(speaker_grp, levels = speaker_grp_levels)) %>% 
  count(act, scene, speaker_grp, wt = word_count, name = "word_count") %>% 
  # increment counter across act and scene
  group_by(act, scene) %>% 
  mutate(act_scene_id = cur_group_id()) %>% 
  ungroup() %>% 
  ggplot(aes(act_scene_id, word_count, fill = speaker_grp)) +
  # vertical lines for the acts
  geom_vline(
    data = . %>% filter(scene == "SCENE I."),
    aes(xintercept = act_scene_id), 
    color = "grey50", size = 0.2, lty = "dotted") +
  geom_stream(type = "mirror", bw = 0.5,  extra_span = 0.1) +
  # text labels for the acts
  geom_text(
    data = . %>% group_by(act) %>% summarize(x = mean(act_scene_id)),
    aes(x, y = -Inf, label = act), inherit.aes = FALSE, 
    vjust = -2, hjust = 0.4, color = "grey60", family = "Forum") +
  # paletteer::scale_fill_paletteer_d("palettetown::pidgey") +
  scale_fill_manual(values = color_palette) +
  # guides(fill = guide_legend(override.aes = list(size = 1))) +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption,
    fill = NULL
  ) +
  theme_void(base_family = "Forum", base_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = "grey8"),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.key.height = unit(3, "mm"),
    legend.spacing.y = unit(4, "cm"),
    legend.text = element_text(size = 9.5),
    text = element_text(color = "white"),
    plot.title = element_text(size = 24, family = "Forum"),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 1)
  )
ggsave(here(base_path, "10-experimental-macbeth.png"), width = 8, height = 7)




word_count_speakers %>% 
  mutate(speaker_grp = ifelse(speaker_grp %in% c("All", few_appearances_speakers), "Other", speaker_grp)) %>% 
  count(act, scene, speaker_grp, wt = word_count, name = "word_count") %>% 
  # increment counter across act and scene
  group_by(act, scene) %>% 
  mutate(act_scene_id = cur_group_id()) %>% 
  ungroup() %>% 
  ggplot(aes(act_scene_id, word_count, fill = speaker_grp)) +
  geom_col()


# Word count by act and scene
word_count_speakers %>% 
  count(act, scene, wt = word_count, sort = TRUE)
