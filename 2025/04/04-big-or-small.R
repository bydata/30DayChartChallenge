library(tidyverse)
library(ggtext)
library(here)
library(ggraph)
library(tidygraph)

base_path <- here("2025", "04")

#' Download the PDF report from
#' https://www.dfl.de/de/hintergrund/lizenzierungsverfahren/finanzkennzahlen-der-proficlubs/

df <- read_tsv(here(base_path, "Bundesliga_Kennzahlen_2023.tsv"))

club_colors <- c(
  "FC Augsburg" = "#C8102E",
  "1. FC Union Berlin" = "#DA291C",
  "VfL Bochum" = "#1D3C92",
  "Werder Bremen" = "#1E8F3F",
  "Borussia Dortmund" = "#FDE100",
  "Eintracht Frankfurt" = "#000000",
  "SC Freiburg" = "#D00027",
  "1. FC Heidenheim" = "#005BAC",
  "TSG Hoffenheim" = "#1746A2",
  "Holstein Kiel" = "#005CA9",
  "RB Leipzig" = "#C8102E",
  "Bayer 04 Leverkusen" = "#E32219",
  "1. FSV Mainz 05" = "#ED1C24",
  "Borussia Mönchengladbach" = "#00815A",
  "FC Bayern München" = "#DC052D",
  "FC St. Pauli" = "#422518",
  "VfB Stuttgart" = "#FFFFFF",
  "VfL Wolfsburg" = "#65B32E"
)

font_colors <- c(
  "FC Augsburg" = "white",
  "1. FC Union Berlin" = "white",
  "VfL Bochum" = "white",
  "Werder Bremen" = "white",
  "Borussia Dortmund" = "black",
  "Eintracht Frankfurt" = "white",
  "SC Freiburg" = "white",
  "1. FC Heidenheim" = "white",
  "TSG Hoffenheim" = "white",
  "Holstein Kiel" = "white",
  "RB Leipzig" = "white",
  "Bayer 04 Leverkusen" = "white",
  "1. FSV Mainz 05" = "white",
  "Borussia Mönchengladbach" = "white",
  "FC Bayern München" = "white",
  "FC St. Pauli" = "white",
  "VfB Stuttgart" = "red",
  "VfL Wolfsburg" = "white"
)


df <- df |> 
  mutate(Personalaufwand = -Personalaufwand) |> 
  bind_cols(club_color = club_colors) |> 
  bind_cols(font_color = font_colors) |> 
  mutate(Verein = case_match(
    Verein,
    "1. FC Union Berlin" ~ "Union Berlin",
    "1. FSV Mainz 05" ~ "Mainz 05",
    "1. FC Heidenheim" ~ "FC Heidenheim",
    .default = Verein
  ))

graph <- as_tbl_graph(df) |> 
  activate(nodes) |> 
  inner_join(df, by = join_by("name" == "Verein")) |> 
  mutate(name = ifelse(
    name == "Borussia Mönchengladbach", "Borussia M'gladbach", name)) 

set.seed(1)
graph |> 
  ggraph(layout = "circlepack", weight = Personalaufwand) + 
  geom_node_circle(
    aes(fill = club_color, color = font_color),
    linewidth = 0.5
  ) +
  geom_node_text(
    aes(
      label = str_wrap(name, 10),
      size = Personalaufwand,
      color = font_color),
    family = "Roboto Condensed Medium", lineheight = 0.9,
    show.legend = FALSE) +
  scale_size_continuous(range = c(2, 7)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_equal() +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8")
    )


#' For advanced labelling, create the plot only with points and use the points 
#' for the label positions in a separate plot

# Custom str_wrap function that replaces \n with <br>
str_wrap_custom <- function(string, width = 10, break_characters = "  \n") {
  s <- str_wrap(string, width = width)
  str_replace_all(s, "\n", break_characters)
}

# Use the same seed across plots to ensure a constant positioning of the clubs
seed <- 1

set.seed(seed)
p <- graph |> 
  ggraph(layout = "circlepack", weight = Personalaufwand) + 
  geom_node_point(
    aes(group = name),
  ) +
  scale_size_continuous(range = c(2, 7)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_equal()

head(p$data)


set.seed(seed)
graph |> 
  ggraph(layout = "circlepack", weight = Personalaufwand) + 
  geom_node_circle(
    aes(fill = club_color, color = font_color),
    linewidth = 0.5
  ) +
  geom_richtext(
    data = p$data,
    aes(
      x, y, label = sprintf(
        "%s<br><span style='font-size: %dpt;font-family:Roboto Condensed'>%s</span>",
        str_wrap_custom(name, 9, break_characters = "<br>"),
        # font size
        case_when(
          Personalaufwand > 1.5e5 ~ 11, 
          Personalaufwand > 1e5 ~ 9, 
          TRUE ~ 6),
        scales::number(Personalaufwand, scale = 1e-3, accuracy = 0.1, 
                       suffix = "M")),
      size = Personalaufwand,
      color = font_color),
    family = "Roboto Condensed Medium", lineheight = 0.9, hjust = 0.5, 
    fill = NA, label.size = 0,
    show.legend = FALSE) +
  scale_size_continuous(range = c(1.8, 7)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_equal() +
  labs(
    title = "Big and Small Clubs in the Bundesliga",
    subtitle = "The size of the bubbles indicates the total staff costs for each
    Bundesliga club (financial results from 2023)",
    caption = "**Source:** DFL. **Visualization:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", lineheight = 1.05),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1, lineheight = 1.2),
    plot.caption = element_markdown(hjust = 0),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
  )
ggsave(here(base_path, "04-big-or-small.png"), width = 5, height = 5, dpi = 500,
       scale = 1.2)
