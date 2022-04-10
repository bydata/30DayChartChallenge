# remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(tidyverse)
library(tidytext)
library(ggtext)
library(here)

base_path <- here("2022", "10")

# download text from Project Gutenberg
macbeth_url <- "https://www.gutenberg.org/cache/epub/1129/pg1129.txt"
macbeth_lines <- read_lines(macbeth_url)

# Lines where the manuscript starts and ends
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

text_df %>%
  distinct(act, scene) %>% 
  mutate(row_number()) %>% head(20)

# Annotations
plot_titles <- list(
  title = "Who speaks when in Shakespeare's MACBETH?",
  subtitle = "Distribution of speech share (number of words) per character in 
  each scene. Acts are separated with vertical lines.",
  caption = "Project Gutenberg. Visualization: Ansgar Wolsing"
)

# highlight key events - used for text and lines
story_annotations <- tibble(
  x    = c(13.5, 1.2, 7, 5, 14, 22),
  xend = c(19,   1.2, 7, 5, 14, 22),
  y    = c(-5000, -4200, -4500, 5000, 5000, 3000),
  yend = c(-5000,  -200,   400,  800, 1000,  500),
  vjust = c(   0,  0.25,   0.3,  0.85,  0.9,  0.8),
  label = c(
    "Macduff & Malcolm decide to go to war against Macbeth",
    "Three Witches<br>appear",
    "Macbeth kills King Duncan",
    "Lady Macbeth & Macbeth<br>plan the murder of King Duncan",
    "Murder of Banquo reported to Macbeth,<br>Ghost of Banquo appears",
    "Macduff<br>kills<br>Macbeth"
    )
)


p <-word_count_speakers %>% 
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
  # annotations for key events (text + segment)
  geom_textbox(
    data = story_annotations,
    aes(x - 0.08, y, label = label, vjust = vjust),
    inherit.aes = FALSE,
    color = "grey90", family = "Forum", hjust = 0, fill = NA,
    box.size = 0,
    width = unit(3.5, "cm")) +
  geom_segment(
    data = story_annotations,
    aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE,
    color = "grey90", size = 0.3
  ) +
  # text labels for the acts
  geom_text(
    data = . %>% group_by(act) %>% summarize(x = min(act_scene_id) + n_distinct(scene) / 2),
    aes(x, y = -Inf, label = act), inherit.aes = FALSE, 
    vjust = -1, hjust = 0.5, color = "grey60", family = "Forum") +
  scale_fill_manual(values = color_palette) +
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
