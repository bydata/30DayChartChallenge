library(tidyverse)
library(here)
library(ggimage)
library(ggtext)

base_path <- here("2022", "02")

federal_states <- rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
saarland <- federal_states %>% filter(name == "Saarland")

saarland_img_path <- here(base_path, "saarland.png")
ggplot(saarland) +
  geom_sf(fill = "grey8", col = NA) +
  theme_void()
ggsave(saarland_img_path, width = 300, height = 300, units = "px")

saarland_highlight_img_path <- here(base_path, "saarland-highlight.png")
ggplot(saarland) +
  geom_sf(fill = "#347CE9", col = NA) +
  theme_void()
ggsave(saarland_highlight_img_path, width = 300, height = 300, units = "px")


saarland_area <- sf::st_area(saarland)

states_area <- tibble(
  state = federal_states$name,
  area = sf::st_area(federal_states)
)


max_cols <- 5

df <- states_area %>% 
  filter(!state %in% c("Saarland", "Bremen", "Hamburg", "Berlin")) %>% 
  mutate(factor = unclass(area / saarland_area),
         factor_id = round(factor)) %>% 
  group_by(state) %>% 
  complete(factor_id = seq_len(factor_id)) %>% 
  ungroup() %>% 
  select(state, factor_id) %>% 
  mutate(x = (factor_id - 1) %% max_cols + 1,
         y = (factor_id + (max_cols - 1)) %/% max_cols)  %>% 
  arrange(state, factor_id, x, y)


# Create the pictograms facetted by federal state, pass plot titles as an argument
plot_picto <- function(plot_titles, highlight_color = "#347CE9") {
  saarland_img_tag <- glue::glue("<img src='{saarland_img_path}' width='15', height='15'>")
  saarland_highlight_img_tag <- glue::glue("<img src='{saarland_highlight_img_path}' width='15', height='15'>")
  df %>% 
    mutate(state = fct_reorder(state, -factor_id),
           # create a grid for facetting the subplots 
           # so that we can use the `space` parameter from facet_grid 
           facet_x = (as.numeric(state) - 1) %/% 4,
           facet_y = (as.numeric(state) - 1) %% 4,
           # add the Saarland image as HTML tag
           image = saarland_img_tag) %>% 
    # flag a picto to highlight
    mutate(image = ifelse(state == "Nordrhein-Westfalen" & factor_id == 13,
                          saarland_highlight_img_tag,
                          image)) %>% 
    ggplot(aes(x, y)) +
    # add the Saarland images
    geom_richtext(aes(label = image),
                  label.colour = NA, fill = NA) +
    # facet title
    geom_text(aes(x = 0.5, y = 0, label = state),
              stat = "unique", family = "Oswald", size = 3, angle = 2, hjust = 0) +
    facet_grid(facet_x ~ facet_y, scales = "free_y", space = "free_y") +
    scale_x_continuous(expand = c(0, 1)) +
    scale_y_reverse(expand = c(0, 1)) +
    scale_color_manual(values = c("FALSE" = "grey8", "TRUE" = highlight_color)) +
    guides(color = "none") +
    labs(
      title = plot_titles$title,
      subtitle = plot_titles$subtitle,
      caption = plot_titles$caption
    ) +
    theme_void(base_family = "Helvetica", base_size = 8) +
    theme(
      plot.background = element_rect(color = NA, fill = "#edd958"),
      strip.text = element_blank(),
      panel.spacing.y = unit(0, "mm"),
      plot.title = element_markdown(family = "Oswald", size = 14),
      plot.subtitle = element_textbox_simple(
        margin = margin(t = 12, b = 8),
        padding = margin(2, 2, 2, 2)
      ),
      plot.caption = element_textbox_simple(
        hjust = 0, lineheight = 1.2, margin = margin(t = 6)),
      plot.margin = margin(t = 2, l = 6, r  = 6, b = 4)
    )  
}


## English titles

plot_titles_en <- list(
  title = "How many times does <span style='color: #347CE9'>Saarland</span> fit into other German federal states?",
  subtitle = "Saarland is the smallest of German federal states (excluding city-states). 
  In German news media, the size of the Saarland often serves as a reference
  for comparing larger areas. Common examples are areas affected by wildfires, 
  ocean plastic pollution, or oil spill in the sea.
  The search query \"so groß wie das Saarland\" (engl.: \"as big as Saarland\")
  returns over 13 million results on Google web search.",
  caption = "Germany city-states Berlin, Bremen, and Hamburg excluded. 
    Geometry of Saarland and state areas: NaturalEarth data. Google Web Search results retrieved April 2, 2022.
  <b style='font-family: Helvetica Bold'>Visualization:</b> Ansgar Wolsing"
)
plot_picto(plot_titles_en)
ggsave(here(base_path, "plot-picto-saarland-en.png"), dpi = 300, width = 5.75, height = 5)


# German titles


plot_titles_de <- list(
  title = "Wie oft passt das <span style='color: #347CE9'>Saarland</span> in die anderen Bundesländer?",
  subtitle = "Wo immer ein Wald brennt oder ein Ölteppich das Meer verschmutzt - 
  größere Flächen werden in den deutschen Medien häufig mit der Größe des Saarlands verglichen.
  Der Suchbegriff \"so groß wie das Saarland\" ergibt über 13 Mio. Suchergebnisse auf Google.",
  caption = "Die Stadtstaaten Berlin, Bremen und Hamburg ausgeschlossen. 
    Umriss des Saarlands sowie Flächenangaben der Bundesländer: NaturalEarth. 
    Google Web-Suche abgerufen am 02.04.2022.
  <b style='font-family: Helvetica Bold'>Visualisierung:</b> Ansgar Wolsing"
)
plot_picto(plot_titles_en)
ggsave(here(base_path, "plot-picto-saarland-de.png"), dpi = 300, width = 5.75, height = 5)






