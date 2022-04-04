# devtools::install_github("lchiffon/wordcloud2")
pacman::p_load("tidyverse", "here", "glue", "ggtext", "wordcloud2")

base_path <- here("2022", "04")

## GET DATA ====================================================================
#' Tree population in Cologne, 2020
#' Source: Offene Daten Köln,
#' https://www.offenedaten-koeln.de/dataset/baumkataster-koeln

url_trees_2020 <- "https://www.offenedaten-koeln.de/sites/default/files/20200610_Baumbestan_Koeln.zip"
data_dir <- here(base_path, "cologne_trees")
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
  download.file(url_trees_2020, destfile = here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020.zip"))
  unzip(here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020.zip"),
        exdir = here(data_dir, "Shapes_Bestand_Einzelbaeume_Koeln_2020"))
}

trees_2020_sf <- sf::st_read(here(data_dir,
                              "Shapes_Bestand_Einzelbaeume_Koeln_2020",
                              "Baumbestand.shp"))


# Replace NA with 1 for ANZAHL (number of trees)
df <- trees_2020_sf %>%
  sf::st_drop_geometry() %>% 
  mutate(ANZAHL2 = replace_na(ANZAHL, 1),
         ANZAHL2 = ifelse(ANZAHL2 == 0, 1, ANZAHL2)) %>% 
  transmute(Gattung = ifelse(Gattung == "Verschiedene", "Other", Gattung),
            gattung = ifelse(is.na(Gattung) | Gattung == "unbekannt", "Unknown", Gattung),
            art = ifelse(!is.na(Art), Art, ""),
            gattung_art = paste(Gattung, art, sep = " "),
            n = ANZAHL2) %>% 
  count(gattung, art, gattung_art, wt = n) 

# number of groups
unique(df$gattung)
unique(df$gattung_art)

df %>% 
  count(gattung, wt = n) %>% 
  arrange(-n)


chunk_size <- 200

df %>% 
  as_tibble() %>% 
  count(gattung, wt = n) %>% 
  arrange(-n) %>% 
  filter(gattung != "Unknown") %>% 
  mutate(rep = n %/% chunk_size + 1,
         new_n = ifelse(n < chunk_size, n, chunk_size)) %>% 
  uncount(rep) 
  

# Credit: https://www.pinterest.de/pin/498562621254655108/
flora_palette <- c(
  "#BDA679", "#162231", "#6F7C47", "#8B7356", "#A1B36F", "#6E5542", 
  "#4F5F6F", "#3B191E", "#576148", "#30302E", "#8799A8", "#434318"
)


# Source: https://de.wikipedia.org/wiki/Datei:Maple_leaf_--_Unknown.svg
maple_leaf_file <- here(base_path, "maple_leaf.png")

df_plot <- df %>% 
  as_tibble() %>% 
  count(gattung, wt = n) %>% 
  arrange(-n) %>% 
  filter(gattung != "Unknown") %>% 
  mutate(rep = n %/% chunk_size + 1,
         new_n = ifelse(n < chunk_size, n, 100)) %>% 
  uncount(rep) %>% 
  select(gattung, n = new_n)

# for the color palette, determine the number of rows for the top N species
length(flora_palette)
tree_rows <- df_plot %>% 
  count(gattung, sort = TRUE) %>% 
  pull(n)
tree_rows

# create a palette with values for each individual row
wordcloud_palette <- map2(flora_palette, tree_rows[seq_along(flora_palette)], rep) %>% 
  unlist()

# create the wordcloud
df_plot %>%
  wordcloud2(
    figPath = maple_leaf_file,
    size = 0.1,
    fontFamily = "Source Serif Pro",
    fontWeight = 500,
    maxRotation = pi / 2,
    color = wordcloud_palette,
    backgroundColor = "white",
    widgetsize = c(800, 750),
    shape = "circle"
  )


# Maybe a bar chart instead?
df %>%
  mutate(gattung2 = ifelse(n < 500, "Other", gattung)) %>% 
  count(gattung2, wt = n, sort = TRUE) %>%
  mutate(gattung2 = fct_reorder(gattung2, n)) %>% 
  ggplot(aes(gattung2, n)) +
  geom_col(aes(fill = gattung2), width = 0.75) +
  scale_y_continuous(expand = expansion(add = c(0, 100))) +
  scale_fill_manual(values = rev(c("grey40", "grey60", flora_palette, rep("grey60", 9)))) +
  coord_flip() +
  guides(fill = "none") +
  labs(
    title = "Tree Genera in Cologne",
    subtitle = "Number of trees",
    caption = "**Source:** Open Data Cologne, Tree Cadastre | **Visualization:** Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", size = 0.1),
    text = element_text(color = "grey41"),
    plot.title = element_text(color = "black", size = 16),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot",
    axis.line.x = element_line(color = "grey20", size = 0.2),
    axis.ticks.x = element_line(color = "grey20", size = 0.2)
  )
ggsave(here(base_path, "04-trees-bar-chart.png"), width = 5, height = 5)
