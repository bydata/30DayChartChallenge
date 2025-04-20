library(tidyverse)
library(rvest)
library(here)

base_path <- here("2025", "20")

df_birds <- read_csv(here(base_path, "NABU-Stunde-der-Gartenvögel.csv"))

# What are the biggest shifts from 2006 to 2024?

df_birds |> 
  filter(year == min(year) | year == max(year)) |> 
  mutate(
    vogel_pro_garten_prev = lag(vogel_pro_garten, 1),
    vogel_pro_garten_change_abs = vogel_pro_garten - vogel_pro_garten_prev,
    vogel_pro_garten_change_rel = vogel_pro_garten_change_abs / vogel_pro_garten_prev,
    .by = vogelart) |> 
  filter(!is.na(vogel_pro_garten_change_rel), 
         vogel_pro_garten_change_rel != Inf, 
         vogel_pro_garten_change_rel != -1) |> 
  select(vogelart, vogel_pro_garten, vogel_pro_garten_prev, 
         vogel_pro_garten_change_abs, vogel_pro_garten_change_rel) |> 
  arrange(-abs(vogel_pro_garten_change_rel - 1)) |> 
  head(10)


#  Buntspecht, Eichelhäher und Ringeltaube

## Image credits:
#' Buntspecht: Marek Szczepanek (CC BY-SA 3.0)
#' Grünspecht: Remyymer (CC BY-SA 4.0)
bird_images <- c(
  "Buntspecht" = "https://upload.wikimedia.org/wikipedia/commons/e/ea/Dendrocopos_major_2_%28Marek_Szczepanek%29.jpg?download",
  "Grünspecht" = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/65/European_green_woodpecker_eating.jpg/500px-European_green_woodpecker_eating.jpg?download"
)

# Crop the images
df_bird_images <- tibble(image = bird_images) |>
  mutate(image_cropped = circle_crop(image)) |> 
  bind_cols(name = names(bird_images))

# Add the images to the birds dataframe
df_birds_prep <- df_birds |> 
  arrange(vogelart, year) |> 
  filter(vogelart %in% c("Buntspecht", "Grünspecht")) |> 
  mutate(
    vogel_pro_garten_base = first(vogel_pro_garten, order_by = year),
    vogel_pro_garten_indexed = 100 * vogel_pro_garten / vogel_pro_garten_base,
    .by = vogelart
  ) |> 
  left_join(df_bird_images, by = join_by(vogelart == name)) |> 
  select(-image) |> 
  mutate(name_en = case_when(
    vogelart == "Buntspecht" ~ "Great spotted woodpecker",
    vogelart == "Grünspecht" ~ "European green woodpecker"
  ))


p <- df_birds_prep |> 
  ggplot(aes(year, vogel_pro_garten)) +
  geom_smooth(
    aes(col = vogelart),
    method = "loess", span = 0.5, alpha = 0.2) +
  geom_point(
    aes(col = vogelart),
    shape = 21, stroke = 0.3, size = 1.5) +
  ggimage::geom_image(
    data = ~filter(., !is.na(image_cropped) & year == max(year)),
    aes(x = year + 1.75, image = image_cropped),
    size = 0.15) +
  geom_label(
    data = ~filter(., year == max(year)),
    aes(x = year + 1.75, y = vogel_pro_garten, label = str_wrap(name_en, 15), col = vogelart),
    family = "Roboto Condensed Medium", size = 2.5, fill = "#F8F8F8", hjust = 0.5,
    label.size = 0, lineheight = 0.8, label.padding = unit(0, "mm"),
    nudge_y = -0.05
  ) +
  annotate(
    "label",
    x = 2006,
    y = 0.3,
    label = str_wrap("Urban sightings have more than doubled between 2006 and 2017", 25),
    family = "Roboto Condensed", size = 2.5, fill = "#F8F8F8", hjust = 0,
    label.size = 0, lineheight = 0.8, label.padding = unit(0, "mm")
  ) +
  # y-axis label
  annotate(
    "label",
    x = 2006, y = 0.5, label = "\U2191 Number of birds per garden", hjust = 0,
    family = "Roboto Condensed", size = 3.25, fill = "#F8F8F8", label.size = 0,
    label.padding = unit(0, "mm")
  ) +
  scale_x_continuous(breaks = seq(2006, 2024, 2)) +
  scale_color_manual(values = c("#242038", "#968B44")) +
  coord_cartesian(
    xlim = c(NA, 2024),
    ylim = c(0, NA), clip = "off") +
  labs(
    title = "The woodpeckers are coming to town",
    subtitle = "Number of birds per garden over the years 
    in the NABU Garden Bird Survey in Germany. Smoothed trendline (Loess, bw = 0.5)",
    x = NULL, y = NULL,
    caption = "***Note:** Birds have been counted by volunteers from the public for 
    one hour during the survey period, which runs over 3 days in May.*
    <br><br>
    **Source:** NABU, Stunde der Gartenvögel 2006-2024.
    **Image credits:** Marek Szczepanek (CC BY-SA 3.0),  Remyymer (CC BY-SA 4.0).
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.text = element_text(color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.1),
    panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.05),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95,
      lineheight = 1.2),
    plot.subtitle = element_textbox(
      margin = margin(t = 2, b = 16), lineheight = 1.2, width = 1, size = 10
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      lineheight = 1.2, margin = margin(t = 10, b = 6), hjust = 0, size = 7, 
      width = 0.95),
    plot.caption.position = "plot",
    legend.position = "none",
    plot.margin = margin(t = 4, r = 40, b = 4, l = 8),
    axis.title.x = element_text(size = 9, hjust = 0),
    axis.title.y = element_blank()
  )
ggsave(here(base_path, "20-urbanization.png"), width = 5, height = 5)
