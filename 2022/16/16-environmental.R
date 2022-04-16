library(tidyverse)
library(ggtext)
library(here)
library(grid)

base_path <- here("2022", "16")

aquaculture <- read_csv(here(base_path, "aquaculture-farmed-fish-production.csv"))
capture_fishery <- read_csv(here(base_path, "capture-fishery-production.csv"))

# `Aquaculture production (metric tons)`
# `Capture fisheries production (metric tons)`
df <- aquaculture %>% 
  full_join(capture_fishery, by = c("Entity", "Code", "Year")) %>% 
  mutate(continent = countrycode::countrycode(Code, origin = "iso3c", destination = "continent")) %>% 
  rename(aquaculture_production = `Aquaculture production (metric tons)`,
         capture_fishery =  `Capture fisheries production (metric tons)`) %>% 
  filter(!is.na(Code) & !is.na(continent))

summary(df)



highlight_countries <- c("China", "Iceland", "Morocco", "Peru", "United States",
                         "Seychelles", "Philippines", "Norway", "Egypt", "India",
                         "Indonesia", "Vietnam", "Bangladesh")

df %>% 
  group_by(Entity) %>% 
  filter(Year == max(Year)) %>% 
  ungroup() %>% 
  # a small number to avoid log 0
  mutate(across(c(aquaculture_production, capture_fishery), ~.x + 0.1)) %>% 
  na.omit() %>% #View()
  ggplot(aes(aquaculture_production, capture_fishery)) +
  ggforce::geom_shape(
    data = data.frame(
      x = c(0.1, 10^8, 10^8),
      y = c(0.1, 0.1,  10^8)
    ),
    aes(x, y),
    fill = "grey78", alpha = 0.6
  ) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", size = 0.5) +
  annotate("text",
           x = c(10, 10^8),
           y = c(10^7, 1),
           hjust = c(0.5, 1.1),
           vjust = c(0.5, -0.2),
           label = c("More capture fishery", "More aquaculture production"),
           family = "Noto Serif", fontface = "bold", color = "grey38"
           ) +
  geom_point(aes(fill = continent, size = log(aquaculture_production + capture_fishery)),
             shape = 21, color = "white", alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = . %>% filter(Entity %in% highlight_countries),
    aes(label = Entity), size = 2, segment.size = 0.2, family = "Noto Serif",
    color = "grey20") +
  scale_x_log10(labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(labels = scales::number_format(accuracy = 1) ) +
  scale_fill_manual(values = MetBrewer::met.brewer("Wissing")) +
  scale_size_continuous(range = c(1, 5)) +
  coord_fixed(xlim = c(0.1, 10^8), ylim = c(0.1, 10^8), expand = FALSE, clip = "off") +
  guides(
    size = "none"
  ) +
  labs(
    title = "Here is the Title",
    subtitle = "",
    caption = "Source: World Development Indicators, Our World in Data.
    Visualization: Ansgar Wolsing",
    x = "Aquaculture production (tons, log)",
    y = "Capture fisheries production (tons, log)",
    fill = NULL
  ) +
  theme_minimal(base_family = "Noto Serif") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey98"),
    legend.position = "bottom",
    text = element_text(color = "grey20"),
    plot.title = element_text(hjust = 0.5, color = "grey2"),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0.5),
    plot.caption.position = "plot"
  )
ggsave(here(base_path, "16-environmental.png"), width = 7, height = 7)


## Top 10 producing countries 
df %>% 
  group_by(Entity) %>% 
  filter(Year == max(Year)) %>% 
  ungroup() %>% 
  mutate(total = aquaculture_production + capture_fishery,
         Entity = fct_reorder(Entity, total)) %>% 
  na.omit() %>% 
  slice_max(order_by = total, n = 20, with_ties = FALSE) %>% 
  ggplot(aes(Entity)) +
  geom_col(aes(y = aquaculture_production), fill = "steelblue") +
  geom_col(aes(y = -capture_fishery), fill = "darkblue") +
  annotate("text",
           x = c(10, 10^8),
           y = c(10^7, 1),
           hjust = c(0.5, 1.1),
           vjust = c(0.5, -0.2),
           label = c("More capture fishery", "More aquaculture production"),
           family = "Noto Serif", fontface = "bold", color = "grey38"
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::number_format()) +
  coord_flip(ylim = c(-3 * 10^7, 7 * 10^7)) +
  labs(
    title = "Top 15 Fish producing countries",
    subtitle = "",
    caption = "",
    x = NULL,
    y = "Metric tons"
  ) +
  theme_minimal(base_family = "Noto Serif") +
  theme(
   panel.grid = element_blank(),
   panel.grid.major.x = element_line(size = 0.2, color = "grey81"),
   plot.title = element_markdown(face = "bold"),
   plot.title.position = "plot",
   plot.caption.position = "plot"
  )
  
