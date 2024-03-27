library(tidyverse)
library(ggtext)
# remotes::install_github("hrbrmstr/ggchicklet")
library(ggchicklet)
library(here)

base_path <- here("2024", "01")

df <- read_tsv(here(base_path, "europe-top10-leagues-champions.tsv"))

df <- df |> 
  mutate(country = fct_inorder(country)) |> 
  group_by(country) |> 
  mutate(uefa_rank = cur_group_id()) |> 
  ungroup() 

champions_country <- df |> 
  count(country, champion) |> 
  mutate(most_titles = rank(-n) == 1, .by = country) |> 
  arrange(country, desc(most_titles), -n)

colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin Light") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.text = element_text(family = "Source Code Pro"),
      axis.line.x = element_line(linewidth = 0.33),
      plot.title = element_markdown(
        color = "grey8",
        family = "Libre Franklin Medium", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", lineheight = 1.1, width = 1,
        margin = margin(b = 8)),
      plot.caption = element_markdown(),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      # panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05)
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 10, color = "grey35",
        margin = margin(t = 4, b = 1))
    )
)

champions_country |> 
  ggplot(aes(x = 1, y = n, group = champion, fill = most_titles)) +
  geom_col(color = "white") +
  coord_flip() +
  facet_wrap(vars(country), ncol = 1) +
  theme_void()

champions_country |> 
  mutate(
    champion = fct_reorder2(champion, most_titles, n),
    # country = fct_rev(country),
    country = fct_reorder2(country, most_titles, -n)
    ) |> 
  ggplot(aes(x = country, y = n, group = champion, fill = most_titles)) +
  ggchicklet::geom_chicklet(color = "white", radius = unit(3.5, "pt"), size = 1.2) +
  geom_text(
    data = ~subset(., most_titles),
    aes(y = 0, label = champion),
    hjust = 0, nudge_y = 0.2, family = "Libre Franklin SemiBold", color = "white"
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(breaks = seq(0, 10, 2), expand = expansion(add = c(0.1, 0.1))) +
  scale_fill_manual(values = c("#D5D5D5", "#1D00DB")) +
  # coord_flip(expand = FALSE,  clip = "off") +
  coord_flip() +
  guides(fill = "none") +
  labs(
    title = "Bundesliga is hoping for Scottish conditions",
    subtitle = "Number of titles by the most successful team in each league vs. 
    the rest in the top 10 football leagues in Europe in the last the seasons
    (2013/13 to 2022/23)",
    caption = "Source: kicker.de. Visualization: Ansgar Wolsing.",
    y = "# of titles"
  ) + 
  # facet_wrap(vars(country), ncol = 1) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(family = "Libre Franklin"),
    panel.grid.major.y = element_blank(), 
    strip.text = element_text(hjust = 0, family = "Libre Franklin SemiBold")
  )
ggsave(here(base_path, "01-part-to-whole-football-champions.png"), 
       width = 5, height = 5, scale = 1.2)

