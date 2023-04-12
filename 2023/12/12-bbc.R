library(tidyverse)
library(ggtext)
library(here)
# devtools::install_github("bbc/bbplot")
library(bbplot)


#' Tweet data retrieve from the Twitter Search API on March 15 and March 16, 2023
#' Mentions of BBC and (Gary) Lineker extracted from the tweet texts. 
#' Data aggregated by day and content
lineker_bbc_tweet_agg <- read_csv("2023/12/lineker-bbc-tweet-agg.csv")

font <- "Helvetica"

lineker_bbc_tweet_agg <- lineker_bbc_tweet_agg %>% 
  mutate(content = factor(content, levels = c("Only Lineker", "Both", "Only BBC")))

lineker_bbc_tweet_agg %>% 
  filter(!with_retweets) %>% 
  ggplot(aes(interval, n_tweets, fill = content)) +
  geom_hline(aes(yintercept = -1000), linewidth = 1, color = "grey10") +
  geom_col(width = 0.6) +
  scale_x_date(date_breaks = "1 day", date_labels = "%m/%d") +
  scale_y_continuous(
    minor_breaks = seq(1, 1e6, 5e3), 
    labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  scale_fill_manual(values = c("#106AA6", "#C970BB", "#CD171B", "#F3DB4C")) +
  labs(
    title = "Gary Lineker vs. BBC",
    subtitle = "Number of tweets (excluding retweets) per day mentioning Lineker, BBC, 
    or both"
  ) +
  theme_minimal(base_family = font) +
  theme(
    plot.title = element_text(family = font, size = 28, face = "bold", color = "#222222"), 
    plot.subtitle = element_textbox(
      width = 1, family = font, size = 20, margin = margin(9, 0, 9, 0)), 
    plot.caption = element_blank(), 
    legend.position = "top", 
    legend.text.align = 0, 
    legend.background = element_blank(), 
    legend.title = element_blank(), 
    legend.key = element_blank(), 
    legend.text = element_text(family = font, size = 18, color = "#222222"), 
    axis.title = element_blank(), 
    axis.text = element_text(family = font, size = 18, color = "#222222"), 
    axis.text.x = element_text(margin = margin(5, b = 10)), 
    axis.ticks = element_blank(), 
    axis.line = element_blank(), 
    strip.background = element_rect(fill = "white"), 
    strip.text = element_text(size = 22, hjust = 0)) +
  theme(
    panel.ontop = TRUE,
    panel.grid = element_line(color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "#cbcbcb"),
    panel.grid.minor.x = element_blank()
  )

# Use the finalize_plot function from the {bbplot} package
finalise_plot(plot_name = last_plot(),
              source = "Source: Twitter Search API, March 2023",
              save_filepath = here("2023", "12", "12-bbc-lineker.png"),
              logo_image_path = here("2023", "12", "logo-aw.png"),
              width_pixels = 640,
              height_pixels = 550)
