library(tidyverse)
library(ggtext)
library(here)

#' Source: 
#' Data sources: Michalis Moatsos (2021) – with major processing by Our World in Data
#' https://ourworldindata.org/extreme-poverty-in-brief

url <- "https://ourworldindata.org/grapher/share-of-population-living-in-extreme-poverty-cost-of-basic-needs.csv?v=1&csvType=full&useColumnShortNames=true"
df <- read_csv(url)
colnames(df)

unique(df$entity)

df_prep <- df |> 
  select(-c(code, headcount_above_cbn__annotations, headcount_cbn__annotations)) |> 
  mutate(
    share_below_cbn = headcount_cbn / (headcount_cbn + headcount_above_cbn),
    entity = str_remove(entity, " \\(Moatsos\\)"),
    entity = case_match(
      entity,
      "Western offshoots" ~ "USA, Canada, Australia, New Zealand", 
      "Sub-Saharan Africa" ~ "Africa, South of the Sahara",
      .default = entity),
    entity = factor(entity, levels = c("East Asia", "South and South-East Asia", 
      "Middle East and North Africa", "Africa, South of the Sahara", "Latin America and Caribbean",
      "USA, Canada, Australia, New Zealand", "Western Europe", "Eastern Europe and former USSR",
      "World"
    ))
  )


loess_bw <- 0.08
df_prep |> 
  ggplot(aes(year, share_below_cbn, group = entity)) +
  geom_smooth(
    col = "grey60", linewidth = 0.2, se = FALSE, span = loess_bw,
    layout = "fixed") +
  geom_smooth(
    aes(col = entity),
    linewidth = 1, se = FALSE, span = loess_bw) +
  geom_point(
    data = ~filter(., year == max(year)),
    aes(col = entity)
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  paletteer::scale_color_paletteer_d("ggsci::lanonc_lancet") +
  facet_wrap(vars(entity), axes = "all_x") +
  guides(color = "none") +
  labs(
    title = "Two centuries of significant progress, yet 1 in 10 still live in extreme poverty -
      and 4 in 10 in Africa South of the Sahara",
    subtitle = "Estimated proportional of the population (in %) **unable to meet basic needs**
    (such as minimal nutrition and adequately heated shelter)
    according to prices of locally available goods and services, 1820-2018",
    caption = sprintf("*Note: Lines smoothed with LOWESS (bandwidth=%s)*
      <br>
      **Source:** Michalis Moatsos (2021) - with major processing by Our World in Data.
    **Visualization:** Ansgar Wolsing",
    loess_bw)
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    plot.title = element_textbox(
      width = 1, family = "Instrument Sans SemiBold", lineheight = 1.4,
      size = 14, margin = margin(b = 12)
    ),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1, lineheight = 1.4),
    plot.caption = element_textbox(
      width = 1, lineheight = 1.75, margin = margin(t = 12)),
    plot.caption.position = "plot",
    strip.text = element_text(
      family = "Instrument Sans Bold", color = "grey50"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "grey50")
  )
ggsave(here("2026", "20", "20-global-change.png"), width = 7.5, height = 6)


# Absolute figures

y_labels <- c(0, 0.5, 1, 1.5, "2.0B people")

df_prep |> 
  filter(entity == "World") |> 
  ggplot(aes(year, headcount_cbn)) +
  geom_area(
    col = "black", fill = "#000000DD") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = y_labels) +
  labs(
    title = "People living in extreme poverty, 1820-2018",
    subtitle = "Estimated number of people **unable to meet basic needs**
    (such as minimal nutrition and adequately heated shelter)
    according to prices of locally available goods and services.",
    caption = sprintf("*Note: Lines smoothed with LOWESS (bandwidth=%s)*
      <br>
      **Source:** Michalis Moatsos (2021) - with major processing by Our World in Data.
    **Visualization:** Ansgar Wolsing", 
    loess_bw)
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "Instrument Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1, lineheight = 1.4),
    plot.caption = element_textbox(
      width = 1, lineheight = 1.75, margin = margin(t = 12)),
    plot.caption.position = "plot",
    strip.text = element_text(family = "Instrument Sans Bold", color = "grey50"),
    axis.title = element_blank(),
    axis.text = element_text(size = 8, color = "grey50"),
    axis.text.y = element_text(hjust = 0)
  )
ggsave(here("2026", "20", "20-global-change-absolute.png"), width = 7.5, height = 6)
