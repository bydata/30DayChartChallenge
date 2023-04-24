library(tidyverse)
library(ggtext)
library(gganimate)
library(here)

base_path <- here("2023", "24")

#' UN Women Brand Identity: 
#' http://unwomen.org.pk/pakistan/uploads/UNWomen_Branding_Guidelines_and_Policy_on_Logo_Use.pdf

unw_pal_blue <- c("#0397d6", "#8bc0e8", "#6d6e70")
unw_pal_black <- c("#231f20", "#77787b", "#6d6e70")
unw_pal_secondary <- c("#A48771", "#DBD0C7")
unw_topic_pal <- c("#6cb33e", "#f15c22", "#67c7c5", "#ddb307", "#77278b",
                   "#d31145")
main_color <- unw_pal_blue[1]
font_family <- "Montserrat"


#' Source:
#' 
#' https://www.catalyst.org/wp-content/uploads/2019/05/Catalyst_Women_Fortune_CEOs_1972-2019_Historical_List_5.16.2019.pdf
#' https://en.wikipedia.org/wiki/List_of_women_CEOs_of_Fortune_500_companies

fortune500_female_ceos <- tribble(
  ~year, ~n_female_ceos,
  1995, 0,
  1996, 1,
  1997, 2,
  1998, 2,
  1999, 2,
  2000, 2,
  2001, 4,
  2002, 6,
  2003, 7,
  2004, 8,
  2005, 9,
  2006, 10,
  2007, 12,
  2008, 12,
  2009, 15,
  2010, 15,
  2011, 12,
  2012, 18, # Margaret “Meg” Whitman, Hewlett-Packard (No. 15)
  2013, 20,
  2014, 24, # Mary Barra, General Motors (No. 7)
  2015, 24, # Mary Barra, General Motors (No. 6)
  2016, 21,
  2017, 32,
  2018, 24,
  2019, 33, 
  2020, 37,
  2021, 41,
  2022, 47
)

fortune500_female_ceos %>% 
  mutate(share_female_ceos = n_female_ceos / 500) %>% 
  ggplot(aes(year, share_female_ceos)) +
  geom_area(col = main_color, fill = alpha(main_color, 0.2), size = 1) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "THIS IS THE TITLE",
    x = NULL
  ) +
  theme_minimal(base_family = font_family) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_markdown(face = "bold")
  )

fortune500_female_ceos <- fortune500_female_ceos %>% 
  mutate(share_female_ceos = n_female_ceos / 500,
         share_male_ceos = 1 - share_female_ceos) 

p <- fortune500_female_ceos %>% 
  ggplot(aes(year)) +
  geom_area(
    aes(y = share_female_ceos), 
    col = main_color, fill = alpha(main_color, 0.2), size = 1) +
  # geom_line(
  #   aes(y = share_male_ceos),
  #   col = unw_topic_pal[3], size = 0.7) +
  geom_label(
    aes(x = max(year), y = 1, label = "Male CEOs"),
    stat = "unique", family = "Montserrat Medium", hjust = 1, fill = unw_topic_pal[3],
    size = 3.5, color = "white", label.r = unit(1.5, "mm")
  ) + 
  scale_x_continuous(breaks = seq(1995, 2030, 5)) +
  scale_y_continuous(labels = scales::label_percent()
                     #, breaks = seq(0, 1, 0.1)
                     ) +
  labs(
    title = "<span style='color:#0397d6'>WOMEN CEOS</span> OF THE FORTUNE 500",
    subtitle = "Share of CEOs from 1995 to 2022",
    caption = "Source: Fortune 500. Visualisation: Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = font_family, base_size = 9) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_markdown(face = "bold"),
    plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
p

p_anim <- p + 
  transition_layers(layer_length = 1, transition_length = 2, from_blank = FALSE) + 
  view_follow(fixed_y = FALSE, exclude_layer = 3) +
  enter_drift(y_mod = -min(fortune500_female_ceos$share_male_ceos))

animate(p_anim,
        nframes = 5 * 12, start_pause = 10, end_pause = 10,
        res = 200, width = 5, height = 4, unit = "in")
anim_save(here(base_path, "24-un-women-ceos-animated.gif"))
