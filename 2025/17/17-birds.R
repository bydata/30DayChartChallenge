library(tidyverse)
library(ggtext)
library(rvest)
library(here)
# devtools::install_github("doehm/cropcircles")
library(cropcircles)


base_path <- here("2025", "17")

#' Source: NABU "Stunde der Gartenvögel" 2024
#' https://www.nabu.de/tiere-und-pflanzen/aktionen-und-projekte/stunde-der-gartenvoegel/ergebnisse/15767.html

url <- "https://www.nabu.de/tiere-und-pflanzen/aktionen-und-projekte/stunde-der-gartenvoegel/ergebnisse/15767.html"
page <- read_html(url)
df_birds <- page |> 
  html_node(css = "table.datatable") |> 
  html_table() |> 
  janitor::clean_names() |> 
  mutate(
    across(c(anzahl, percent_der_garten, vogel_pro_garten, 
             vergleich_zum_vorjahr_vogel_pro_garten, vergleich_zum_vorjahr_trend),
           function(x) {
             x <- str_replace(x, ",", ".")
             x <- str_remove_all(x, "\\s")
             if (str_detect(x[1], "%")) {
               x <- str_remove(x, "%")
               x <- as.numeric(x) / 100
             } else {
               x <- as.numeric(x)
             }
             x
            }
           ),
    rang = as.integer(rang),
    anteil_voegel = round(anzahl / sum(anzahl, na.rm = TRUE), 3)) |> 
  filter(!is.na(rang))

# Add families
df_bird_families <- read_tsv(here(base_path, "bird-families.tsv")) |> 
  distinct()
df_birds <- df_birds |> 
  left_join(df_bird_families, by = join_by(vogelart == name_de))

# Number of bird occurrences
sum(df_birds$anzahl)

## Image credits:
#' Haussperling: Arnold Paul, CC BY-SA 2.5
#' Amsel: Bengt Nyman, CC BY-SA 2.0
#' Kohlmeise: Luc Viatour, CC BY-SA 2.0
#' Blaumeise: Sławek Staszczuk, CC BY-SA 3.0
#' Star: Marek Szczepanek, CC BY-SA 3.0
bird_images <- c(
  "Haussperling" = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8b/Passer_domesticus_03.jpg/500px-Passer_domesticus_03.jpg?download",
  "Amsel" = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/NZ7_6931_%2851844641169%29.jpg/960px-NZ7_6931_%2851844641169%29.jpg?download",
  "Kohlmeise" = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/Parus_major_Luc_Viatour.jpg/960px-Parus_major_Luc_Viatour.jpg?download",
  "Blaumeise" = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Parus_caeruleus1.jpg/640px-Parus_caeruleus1.jpg?download",
  "Star" = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Sturnus_vulgaris_2_%28Marek_Szczepanek%29.jpg/640px-Sturnus_vulgaris_2_%28Marek_Szczepanek%29.jpg?download"
)

# Crop the images
df_bird_images <- tibble(image = bird_images) |>
  mutate(image_cropped = circle_crop(image)) |> 
  bind_cols(name = names(bird_images))

# Add the images to the birds dataframe
df_birds <- df_birds |> 
  left_join(df_bird_images, by = join_by(vogelart == name)) |> 
  select(-image)


p <- df_birds |> 
  filter(anzahl > 5000) |> 
  mutate(family_en_grp = fct_lump_n(family_en, n = 8, w = anzahl)) |> 
  ggplot(aes(anteil_voegel, percent_der_garten)) +
  geom_point(
    aes(fill = family_en_grp),
    shape = 21, color = "white", size = 3) +
  # Bird images & arrows
  geom_segment(
    data = ~filter(., !is.na(image_cropped)),
    aes(xend = ifelse(vogelart == "Amsel", anteil_voegel, anteil_voegel + 0.035),
        yend = ifelse(vogelart == "Amsel", percent_der_garten + 0.1, percent_der_garten)
    ),
    linewidth = 0.5, col = "white",
    arrow = arrow(angle = 20, length = unit(1.5, "mm"), ends = "first", type = "closed")
  ) +
  geom_segment(
    data = ~filter(., !is.na(image_cropped)),
    aes(xend = ifelse(vogelart == "Amsel", anteil_voegel, anteil_voegel + 0.035),
        yend = ifelse(vogelart == "Amsel", percent_der_garten + 0.1, percent_der_garten)
    ),
    linewidth = 0.2, col = "grey40",
    arrow = arrow(angle = 20, length = unit(1.5, "mm"), ends = "first", type = "closed")
  ) +
  geom_label(
    data = ~filter(., !is.na(image_cropped)),
    aes(
      y = ifelse(vogelart == "Amsel", percent_der_garten + 0.18, percent_der_garten),
      label = name_en),
    size = 2, nudge_x = 0.35, nudge_y = -0.07, family = "Roboto Condensed",
    fill = "#F8F8F8", label.size = 0
  ) +
  ggimage::geom_image(
    data = ~filter(., !is.na(image_cropped)),
    aes(
      x = anteil_voegel - ifelse(vogelart == "Amsel", 0.027, 0.0),
      y = percent_der_garten + ifelse(vogelart == "Amsel", 0.13, 0),
      image = image_cropped),
    nudge_x = 0.35, size = 0.1) +
  annotate(
    "label",
    x = 0.0059, y = 1, label = "\U2191 Share of gardens (%)",
    family = "Roboto Condensed", size = 3.25, fill = "#F8F8F8", label.size = 0
  ) +
  scale_x_continuous(
    breaks = c(0.00625, 0.0125, 0.025, 0.05, 0.10, 0.20),
    labels = scales::label_percent(accuracy = 0.1),
    transform = "log") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(
    values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
               "#CC79A7", "#000000", "grey70")
  ) +
  coord_cartesian(xlim = c(NA, 0.2), ylim = c(0, 1), clip = "off") +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  labs(
    title = "<span style='color:#0072B2'>House sparrows</span> are observed most often, 
    but <span style='color:#CC79A7'>blackbirds</span> are found in
    almost every garden",
    subtitle = "Share of observations of bird species and share of gardens where 
    a species was observed in the NABU Garden Bird Survey in Germany 2024",
    x = "Proportion of total bird observations (log scale %) \U2192",
    y = "Share of gardens (%)",
    fill = "Family",
    caption = "***Note:** Birds were counted by volunteers from the public during 
    3 days in May 2024. Chart limited to species with more than 5,000 observations.
    1.21 millions birds were counted in total.*
    <br><br>
    **Source:** NABU, Stunde der Gartenvögel 2024.
    **Image credit:** Arnold Paul (CC BY-SA 2.5), Bengt Nyman (CC BY-SA 2.0),
    Luc Viatour (CC BY-SA 2.0), Sławek Staszczuk (CC BY-SA 3.0), 
    Marek Szczepanek (CC BY-SA 3.0).
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.text = element_text(color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.1),
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
    legend.position = "right",
    legend.justification = "center",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(4, "mm"),
    plot.margin = margin(t = 4, r = 20, b = 4, l = 8),
    axis.title.x = element_text(size = 9, hjust = 0),
    axis.title.y = element_blank()
  )
ggsave(here(base_path, "17-birds.png"), width = 6, height = 5)
