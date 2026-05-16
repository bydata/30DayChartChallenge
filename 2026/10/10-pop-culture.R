library(tidyverse)
library(ggtext)
library(patchwork)
library(here)

df_episode_avg_ratings <- read_csv(here("2026", "10", "stranger_things_imdb_ratings-20260103.csv"))


main_color <- colorspace::lighten("#B1281E", 0.1)
bg_color <- "grey2"

theme_st <- function() {
   theme_minimal(paper = bg_color, ink = "grey82", base_family = "Montserrat") +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "grey62"),
        panel.background = element_rect(color = NA, fill = NA),
        text = element_text(color = "grey82"),
        plot.title = element_markdown(
          family = "Benguiat", size = 18, color = "white"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(lineheight = 1.25),
        plot.caption = element_markdown(hjust = 0, size = 8),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey20", linewidth = 0.2),
        panel.grid.minor.y = element_line(color = "grey20", linewidth = 0.1),
        strip.text = element_markdown(color = "grey82")
    )
}

titles <- list(
  "title" = "STRANGER\nTHINGS",
  "subtitle" = "
  Stranger Things is one of the most successful series on Netflix. It has an overall rating of 8.6 on IMDB.
  There is variation between the ratings of the episodes. 
  Each **dot** represents the average IMDB rating of an episode.",
  "caption" = "Source: IMDb.com (as of January 3rd, 2026). 
      Visualization: Ansgar Wolsing")
title_pos <- 12.5

df_episode_annotations <- tibble(
  x = c(5.7, 6.1, 9.05),
  y = c(1.1, 0.95, 1.33),
  label = c(
    "S5 E7 \"The Bridge\"",
    "S2 E7 \"The Lost Sister\"",
    "10 episodes have \na rating \U2265 9.0"
  )
)

p_titles <- ggplot() +
    # Custom title with shadowtext
    shadowtext::geom_shadowtext(
      data = NULL,
      aes(x = nrow(df_episode_avg_ratings) / 2, y = title_pos, label = titles$title),
      stat = "unique", 
      family = "Benguiat", color = bg_color, bg.color = main_color, size = 9,
      hjust = 0.5, vjust = 1, inherit.aes = FALSE, lineheight = 0.8) +
    # Custom subtitle
    annotate(GeomTextBox, x = nrow(df_episode_avg_ratings) / 2, y = title_pos - 0.75, 
            label = titles$subtitle, color = "grey82", 
            width = 1, hjust = 0.5, halign = 0, vjust = 0, size = 3.25,
            lineheight = 1.25, family = "Montserrat", fill = NA, box.size = 0) + 
    theme_void(paper = bg_color)


p_chart <- df_episode_avg_ratings |> 
  ggplot(aes(rating, y = 1)) +
  ggfx::with_outer_glow(
    ggbeeswarm::geom_quasirandom(
      aes(size = rating_count),
      method = "quasirandom", width = 0.42,
      col = "grey80"
    ),
    expand = 12, colour = main_color, sigma = 21
  ) +
  # Episode annotations
  geom_text(
    data = df_episode_annotations,
    aes(x, y, label = label),
    family = "Montserrat SemiBold", size = 2.5, col = "grey82",
    hjust = 0
  ) +
  scale_x_continuous() +
  scale_size_area(labels = scales::label_number(big.mark = ",")) +
  coord_cartesian(ylim = c(0.5, 1.5), clip = "off") +
  guides(size = guide_legend(override.aes = list(shape = 21))) +
  labs(
    caption = titles$caption,
    size = "Number of votes" 
  ) +
  theme_st() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey20", linewidth = 0.2),
    panel.grid.minor.x = element_line(color = "grey20", linewidth = 0.1),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.title = element_text(family = "Montserrat SemiBold", size = 8),
    legend.text = element_text(size = 7)
  )
# dev.off()
  

p <- p_titles / p_chart + 
  plot_layout(heights = c(0.33, 0.67)) & 
  theme(plot.background = element_rect(fill = bg_color))
ggsave(here("2026", "10", "beeswarm-title.png"), width = 6, height = 6)

df_episode_rating_histograms |> 
    mutate(share = count / sum(count), .by = c(season, episode)) |> 
    ggplot(aes(factor(rating), share)) +
    geom_col() +
    scale_y_continuous(labels = scales::label_percent()) +
    facet_grid(rows = vars(season), cols = vars(episode))



### All episodes histograms

rating_histograms <- read_rds(here("2026", "10", "rating_histogram_counts-20260103.rds"))
df_episode_rating_histograms <- bind_rows(rating_histograms, .id = "episode_id")

df_episode_rating_histograms <- df_episode_rating_histograms |> 
    mutate(across(c(episode_id, rating, count), as.integer))

rm(rating_histograms)

# Add the season/episodes
df_seasons_episodes <- df_episode_avg_ratings |> 
    arrange(season, episode) |> 
    mutate(episode_id = row_number()) |> 
    select(episode_id, season, episode)

df_episode_rating_histograms <- df_episode_rating_histograms |> 
    inner_join(df_seasons_episodes, by = join_by(episode_id)) |> 
    select(episode_id, season, episode, everything())


library(ggh4x)

df_episode_rating_histograms |> 
  mutate(share = count / sum(count), .by = c(season, episode)) |> 
  ggplot(aes(factor(rating), share)) +
  geom_col(
    aes(fill = season == 5 & episode == 7 | season == 2 & episode == 7)
  ) +
  geom_label(
    aes(x = 5.5, y = 1,
      label = sprintf("S%d E%d", season, episode)),
    hjust = 0.5, family = "Montserrat SemiBold", size = 2.5,
    col = "white", fill = bg_color, label.size = 0.05
  ) +
  scale_y_continuous(
    labels = scales::label_percent(), position = "left",
    expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("FALSE" = "grey50", "TRUE" = main_color)) +
  ggh4x::facet_grid2(
    rows = vars(season), cols = vars(episode), switch = c("y"),
    scales = "free_x", independent = "x") +
  guides(fill = "none") +
  labs(
    title = "Stranger Things Episode Ratings",
    subtitle = "The share of ratings given by IMDb users on a scale from 1-10 per episode.<br>
      **Episode 7 of season 2** and **episode 7 of season 5** stand out",
    caption = titles$caption
  ) +
  theme_st() +
  theme(
    plot.subtitle = element_markdown(margin = margin(t = 4, b = 18)),
    plot.caption = element_markdown(margin = margin(t = 12, b = 2)),
    panel.spacing.y = unit(0.5, "cm"),
    panel.spacing.x = unit(0.33, "cm"),
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
ggsave(here("2026", "10", "multiple-histograms.png"), width = 7.5, height = 6, scale = 1.2)


sum(df_episode_rating_histograms$count[df_episode_rating_histograms$season == 5 & df_episode_rating_histograms$episode == 7])

df_episode_rating_histograms |> 
  filter(season == 5 & episode == 7 | season == 2 & episode == 7) |> 
  mutate(share = count / sum(count), .by = c(season, episode)) |> 
  ggplot(aes(factor(rating), share)) +
  geom_col(
    aes(fill = season == 5 & episode == 7 | season == 2 & episode == 7)
  ) +
  geom_label(
    aes(x = 5.5, y = 0.4,
      label = sprintf("S%d E%d", season, episode)),
    hjust = 0.5, family = "Montserrat SemiBold", size = 4,
    col = "white", fill = bg_color, label.size = 0.05
  ) +
  scale_y_continuous(
    labels = scales::label_percent(), position = "left",
    expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("FALSE" = "grey50", "TRUE" = main_color)) +
  facet_grid(
    rows = vars(episode), cols = vars(season)) +
  guides(fill = "none") +
  labs(
    title = "Stranger Things Episode Ratings",
    subtitle = "The share of ratings given by IMDb users on a scale from 1-10 per episode.
      For **episode 7 of season 5**, a remarkable share of 70 percent of users choose an 
      extreme value (1 or 10).",
    caption = titles$caption
  ) +
  theme_st() +
  theme(
    plot.subtitle = element_textbox(
      width = 1, margin = margin(t = 4, b = 10)),
    plot.caption = element_markdown(margin = margin(t = 12, b = 2)),
    panel.spacing.y = unit(0.5, "cm"),
    panel.spacing.x = unit(0.33, "cm"),
    strip.text = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 9)
  )
ggsave(here("2026", "10", "multiple-histograms-s2e7+s5e7.png"), width = 6, height = 6)


## Reviewer profiles ------------------------

df_reviewers_s5e7 <- read_csv(here("2026", "10", "df_reviewers_s5e7.csv"))
df_reviews_s5e7 <- read_csv(here("2026", "10", "df_reviews_s5e7.csv"))



df_reviewers_s5e7 |> 
  mutate(one_review = review_count == 1) |> 
  count(rating, one_review) |> 
  mutate(share = n / sum(n), .by = rating) |> 
  filter(one_review) |> 
  select(-one_review) |> 
  ggplot(aes(factor(rating), share)) +
  geom_col(
    aes(fill = rating %in% c(1, 10)),
    col = main_color) +
  geom_text(
    aes(label = scales::percent(share, accuracy = 1)),
    family = "Montserrat SemiBold", vjust = 1, nudge_y = -0.02,
    col = "white"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::label_percent()) +
  scale_color_identity() +
  scale_fill_manual(values = c("FALSE" = alpha(main_color, 0.42), "TRUE" = main_color)) +
  guides(fill = "none") +
   labs(
    title = "Stranger Ratings",
    subtitle = sprintf("IMDb users who gave Season 5 Episode 7 an
    <b style='color:%s'>extreme rating (1 or 10)</b>
    were more likely to be **first-time reviewers** than those who gave a moderate rating.
    <br><br>
      <i>Share of users with only one review in total (in %%)<i>",
      main_color
    ),
    caption = titles$caption,
    x = "Rating"
  ) +
  theme_st() +
  theme(
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_blank(),
    axis.title.x = element_text(
      size = 9, vjust = 0, margin = margin(t = 4)),
    axis.title.y = element_blank(),
    plot.subtitle = element_textbox(
      width = 1, margin = margin(t = 4, b = 10)),
    plot.caption = element_markdown(margin = margin(t = 12, b = 2))
  )
ggsave(here("2026", "10", "s5e7-one-review-reviewers.png"), width = 6, height = 6)


df_reviewers_s5e7 |>
  inner_join(df_reviews_s5e7, by = join_by(rating, profile_id == reviewer_profile_id)) |> 
  mutate(account_created_on_review_day = join_date == review_date) |> 
  count(rating, account_created_on_review_day) |>
  mutate(share = n / sum(n), .by = rating) |> 
  filter(account_created_on_review_day) |> 
  select(-account_created_on_review_day) |> #View()
  ggplot(aes(factor(rating), share)) +
  geom_col(
    aes(fill = rating %in% c(1, 10)),
    col = main_color) +
  geom_text(
    aes(label = scales::percent(share, accuracy = 1)),
    family = "Montserrat SemiBold", vjust = 1, nudge_y = -0.02,
    col = "white"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::label_percent()) +
  scale_color_identity() +
  scale_fill_manual(values = c("FALSE" = alpha(main_color, 0.42), "TRUE" = main_color)) +
  guides(fill = "none") +
   labs(
    title = "Stranger Ratings",
    subtitle = sprintf("A significant share of IMDb users who gave Season 5, Episode 7  an
      <b style='color:%s'>extreme rating (1 or 10)</b>
      created their accounts on the same day they posted the review.
      <br><br>
      <i>Share of reviewers who created their account on the same day as their review (in %%)<i>",
      main_color
    ),
    caption = titles$caption,
    x = "Rating"
  ) +
  theme_st() +
  theme(
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(
      size = 9, vjust = 0, margin = margin(t = 4)),
    plot.subtitle = element_textbox(
      width = 0.95, margin = margin(t = 4, b = 10)),
    plot.caption = element_markdown(margin = margin(t = 12, b = 2))
  )
  ggsave(here("2026", "10", "s5e7-new-reviewers.png"), width = 6, height = 6)

