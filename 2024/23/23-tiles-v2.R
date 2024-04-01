library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "23")

#' Source: kaggle.com (Billboard charts) - updated weekly 
#' https://www.kaggle.com/datasets/ludmin/billboard?select=hot100.csv
hot100 <- read_csv(here("2024", "23", "kaggle-billboard-charts", "hot100.csv"))

hot100 |> 
  filter(str_detect(Artist, "Taylor Swift")) |> 
  count(Artist)


colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin Light") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.text = element_text(family = "Source Code Pro"),
      axis.line.x = element_line(linewidth = 0.33),
      axis.line.x.top = element_blank(),
      plot.title = element_markdown(
        color = "grey8",
        family = "Libre Franklin SemiBold", hjust = 0.5, size = 16, 
        lineheight = 1.3, margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0.5, halign = 0.5, width = 1, color = "grey35", lineheight = 1.2,
        margin = margin(t = 4, b = 20)),
      plot.caption = element_markdown(),
      plot.margin = margin(rep(4, 4)),
      panel.grid = element_blank(),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 10, color = "grey35",
        margin = margin(t = 4, b = 1)),
      legend.position = "bottom",
      legend.key.height = unit(3, "mm")
      
    )
)

df_prep <- hot100 |> 
  filter(str_detect(Artist, "Taylor Swift")) |> 
  select(Date, Rank, Artist, Song) |> 
  mutate(
    year = year(Date),
    month = month(Date),
    month_abb = month.abb[month],
    month_abb = fct_reorder(month_abb, month)) |> 
  arrange(Date, Rank) |>
  # count each song only once per month
  distinct(year, month_abb, Song) |> 
  filter(year < 2024) |> 
  count(year, month_abb) |>
  complete(year, month_abb, fill = list(n = 0)) |> 
  mutate(n_cat = case_when(
    n == 0 ~ "0",
    n <= 5 ~ as.character(n),
    n <= 10 ~ "6-10",
    TRUE ~ "11-25"
  ),
  n_cat = fct_reorder(n_cat, n)) 

hot100 |> 
  filter(str_detect(Artist, "Taylor Swift")) |> 
  select(Date, Rank, Artist, Song) |> 
  mutate(
    year = year(Date),
    month = month(Date),
    month_abb = month.abb[month],
    month_abb = fct_reorder(month_abb, month)) 


df_prep |> 
  ggplot(aes(month_abb, year)) +
  geom_tile(fill = NA, col = "grey30", linewidth = 0.2) +
  geom_point(
    data = ~subset(., n > 0),
    aes(fill = n),
    shape = 22, col = "grey40", stroke = 0.1, size = 6
  ) +
  geom_text(
    data = ~subset(., n > 0),
    aes(label = n, color = n >= 10),
    size = 2, show.legend = FALSE
  ) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = seq(1960, 2024)) +
  scale_size_area() +
  scale_fill_gradient(low = colorspace::lighten("#eee6ff", 0.2), high = "#5c00d2") +
  scale_color_manual(values = c("grey10", "white")) +
  # colorspace::scale_fill_discrete_sequential() +
  coord_fixed(expand = FALSE) +
  # guides(fill = guide_legend()) + 
  labs(
    title = "26 songs in the Hot 100 in the same month<br>(Taylor's Version)",
    subtitle = "Unique number of songs by Taylor Swift in the<br>
    Billboard Hot 100 per month",
    caption = "Source: kaggle.com, Billboard. Visualization: Ansgar Wolsing",
    x = NULL, y = NULL,
    fill = "# of songs"
  ) +
  theme(
    plot.margin = margin(t = 4, b = 4, l = 100, r = 100),
    legend.title.position = "top"
  )
ggsave(here(base_path, "23-tiles-monthly.png"), width = 5, height = 5, scale = 1.5)


## Weekly version --------------------------------------------------------------

df_prep_week <- hot100 |> 
  filter(str_detect(Artist, "Taylor Swift")) |> 
  select(Date, Rank, Artist, Song) |> 
  mutate(
    year = year(Date),
    week = isoweek(Date)) |> 
  arrange(Date, Rank) |>
  # count each song only once per month
  distinct(year, week, Song) |> 
  count(year, week) |>
  complete(year, week, fill = list(n = 0)) |> 
  mutate(n_cat = case_when(
    n == 0 ~ "0",
    n <= 5 ~ as.character(n),
    n <= 10 ~ "6-10",
    TRUE ~ "11-25"
  ),
  n_cat = fct_reorder(n_cat, n)) 


df_prep_week |> 
  ggplot(aes(week, year)) +
  geom_tile(
    fill = "grey95", col = "white", linewidth = 0.4, width = 0.95, height = 0.9) +
  geom_tile(
    data = ~subset(., n > 0),
    aes(fill = n),
    col = "#5c00d2", linewidth = 0.1, width = 0.95, height = 0.85
  ) +
  geom_text(
    data = ~subset(., n > 0),
    aes(label = n, color = n >= 10),
    size = 2, show.legend = FALSE
  ) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = seq(1960, 2024)) +
  scale_size_area() +
  scale_fill_gradient(low = colorspace::lighten("#eee6ff", 0.2), high = "#5c00d2") +
  scale_color_manual(values = c("grey10", "white")) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "26 songs in the Hot 100 in the same week (Taylor's Version)",
    subtitle = "Unique number of songs by Taylor Swift in the 
    Billboard Hot 100 per week",
    caption = "Source: kaggle.com, Billboard. Visualization: Ansgar Wolsing",
    x = NULL, y = NULL,
    fill = "# of songs"
  ) +
  theme(
    plot.margin = margin(t = 4, b = 4, l = 10, r = 10),
    legend.title.position = "left"
  )
ggsave(here(base_path, "23-tiles-weekly.png"), width = 6, height = 5, scale = 1.5)

