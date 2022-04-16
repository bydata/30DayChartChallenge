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

p1 <- df %>% 
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
           label = c("More wild catch", "More fish farming"),
           family = "Noto Serif", fontface = "bold", color = "grey38"
           ) +
  geom_point(aes(fill = ifelse(aquaculture_production > capture_fishery, "steelblue", "darkblue"), 
                 size = log(aquaculture_production + capture_fishery)),
             shape = 21, color = "white", alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = . %>% filter(Entity %in% highlight_countries),
    aes(label = Entity), size = 2, segment.size = 0.2, family = "Noto Serif",
    color = "grey20", max.overlaps = 20) +
  scale_x_log10(labels = scales::number_format(accuracy = 1)) +
  scale_y_log10(labels = scales::number_format(accuracy = 1) ) +
  scale_fill_identity() +
  scale_size_continuous(range = c(1, 5)) +
  coord_fixed(xlim = c(0.1, 10^8), ylim = c(0.1, 10^8), expand = FALSE, clip = "off") +
  guides(
    size = "none"
  ) +
  labs(
    subtitle = "<b style='font-size:12pt;color:grey30'>Wild catch vs. fish farming</b>",
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


## Top 10 producing countries 
p2 <- df %>% 
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
  annotate("richtext",
           x = c(2, 2),
           y = c(-1.5 * 10^7, 1.5 * 10^7),
           label = c("Wild catch", "Fish farming"), 
           hjust = c(1, 0),
           family = "Noto Serif", fontface = "bold", color = "grey38",
           label.size = 0, fill = NA
  ) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = function(x) scales::number(abs(x), scale = 10^-6, suffix = "M")) +
  coord_flip(ylim = c(-5 * 10^7, 7 * 10^7)) +
  labs(
    subtitle = "<b style='font-size:12pt;color:grey30'>Top 15 Seafood producing countries</b>",
    x = NULL,
    y = "Million metric tons"
  ) +
  theme_minimal(base_family = "Noto Serif") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey98"),
    panel.grid = element_blank(),
   panel.grid.major.x = element_line(size = 0.2, color = "grey81"),
   plot.title = element_markdown(face = "bold"),
   plot.title.position = "plot",
   plot.caption.position = "plot"
  )

library(patchwork)

# total worldwide 2018
total_worldwide <- df %>% 
  group_by(Entity) %>% 
  filter(Year == max(Year)) %>% 
  ungroup() %>% 
  mutate(total = replace_na(aquaculture_production, 0) + replace_na(capture_fishery, 0)) %>% 
  summarize(total_worldwide = sum(total)) %>% 
  pull(total_worldwide)

total_worldwide_fmt <- scales::number(total_worldwide, scale = 10^-6, suffix = " million")

df %>% 
  group_by(Entity) %>% 
  filter(Year == max(Year)) %>% 
  ungroup() %>% 
  mutate(total = replace_na(aquaculture_production, 0) + replace_na(capture_fishery, 0)) %>% 
  slice_max(order_by = total, n = 10)

# Combine the plots
p1 + p2 + 
  plot_annotation(
    title = "FISH & SEAFOOD PRODUCTION",
    subtitle = glue::glue("
      {total_worldwide_fmt} tons of fish and seafood were produced worldwide in 2018.
      China is the largest producing country by far, followed by 
      Indonesia, India, Vietnam, and Peru.
      Many countries generate the majority of their fish and seafood production 
      from capture fishery. On the other hand, China produces the majority from 
      aquaculture.<br><br>"),
    caption = "Source: World Development Indicators, Our World in Data.
    Visualization: Ansgar Wolsing") + 
  plot_layout(
  design = c(
    "12"
  )) & theme(
  plot.background = element_rect(color = NA, fill = "grey98"),
  text = element_text(family = "Noto Serif", color = "grey20"),
  plot.title = element_markdown(hjust = 0.5, color = "grey2", face = "bold", size = 24),
  plot.title.position = "plot",
  plot.subtitle = element_textbox(hjust = 0.5, width = 0.8, lineheight = 1.3),
  plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 24)),
  plot.caption.position = "plot",
  plot.margin = margin(l = 20, r = 20)
)
ggsave(here(base_path, "16-environmental.png"), width = 12, height = 7.5, scale = 1)
