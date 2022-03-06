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


# saarland_area <- 2570
# states_area <- tribble(
#   ~state, ~area,
#   # "Saarland", saarland_area,
#   "Baden-Württemberg", 35751,
#   "Bayern", 70550,
#   "Brandenburg", 29479,
#   "Hessen", 21115,
#   "Mecklenburg-Vorpommern", 23174,
#   "Niedersachsen", 47614,
#   "Nordrhein-Westfalen", 34098,
#   "Rheinland-Pfalz", 19858,
#   "Sachsen", 18416,
#   "Sachsen-Anhalt", 20452,
#   "Schleswig-Holstein", 15763,
#   "Thüringen", 16171,
# )

max_rows <- 4

df <- states_area %>% 
  filter(!state %in% c("Saarland", "Bremen", "Hamburg", "Berlin")) %>% 
  mutate(factor = unclass(area / saarland_area),
         factor_id = round(factor)) %>% 
  group_by(state) %>% 
  complete(factor_id = seq_len(factor_id)) %>% 
  ungroup() %>% 
  select(state, factor_id) %>% 
  mutate(x = (factor_id - 1) %% max_rows + 1,
         y = (factor_id + (max_rows - 1)) %/% max_rows)  %>% 
  arrange(state, factor_id, x, y)



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
    # mutate(image = ifelse(state == "Thüringen" & factor_id == 6,
    #                       saarland_highlight_img_tag,
    #                       image)) %>% 
    mutate(image = ifelse(state == "Nordrhein-Westfalen" & factor_id == 13,
                          saarland_highlight_img_tag,
                          image)) %>% 
    ggplot(aes(x, y)) +
    # add the Saarland images
    geom_richtext(aes(label = image),
                  label.colour = NA, fill = NA) +
    # facet title
    geom_text(aes(x = 2.5, y = 0, label = state),
              stat = "unique", family = "Oswald", size = 3, angle = 2, hjust = 0.5) +
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
    theme_void(base_family = "Helvetica Neue Light", base_size = 8) +
    theme(
      plot.background = element_rect(color = NA, fill = "#edd958"),
      # strip.text = element_text(family = "Oswald", angle = 2),
      strip.text = element_blank(),
      panel.spacing.y = unit(0, "mm"),
      plot.title = element_markdown(family = "Oswald", size = 14),
      plot.subtitle = element_textbox_simple(
        margin = margin(t = 8, b = 8),
        padding = margin(2, 2, 2, 2)
      ),
      plot.caption = element_textbox_simple(
        hjust = 0, lineheight = 1.2, margin = margin(t = 6)),
      plot.margin = margin(t = 2, l = 6, r  = 6, b = 4)
    )  
}

plot_titles_en <- list(
  title = "How many times does <span style='color: #347CE9'>Saarland</span> fit into other German federal states?",
  subtitle = "In German news media, the size of the Saarland is often used as a reference
  for comparing larger areas. 
  For instance, ",
  caption = "Germany city-states Berlin, Bremen, and Hamburg excluded.<br>
    Shape Saarland and state areas: NaturalEarth data | 
  <b style='font-family: Helvetica Neue Bold'>Visualization:</b> Ansgar Wolsing"
)


plot_picto(plot_titles_en)
ggsave(here(base_path, "plot-picto-saarland-en.png"), dpi = 300, width = 5, height = 5)






