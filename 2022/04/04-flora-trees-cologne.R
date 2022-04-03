pacman::p_load("tidyverse", "here", "glue", "ggtext", "sf")

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

crs <- "EPSG:4326"
trees_2020_sf <- st_read(here(data_dir,
                              "Shapes_Bestand_Einzelbaeume_Koeln_2020",
                              "Baumbestand.shp"))
trees_2020_sf <- st_transform(trees_2020_sf, crs)
st_crs(trees_2020_sf)


## Basic EDA ===================================================================

trees_2020_sf %>%
  st_drop_geometry() %>%
  ggplot(aes(ANZAHL)) +
  geom_boxplot()


# Replace NA with 1 for ANZAHL (number of trees)
trees_2020_sf_prep <- trees_2020_sf %>%
  mutate(ANZAHL2 = replace_na(ANZAHL, 1),
         ANZAHL2 = ifelse(ANZAHL2 == 0, 1, ANZAHL2))


trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  count(Gattung, sort = TRUE) 

trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  filter(!is.na(Gattung), Gattung != "unbekannt") %>% 
  transmute(Gattung, gattung_art = paste(Gattung, ifelse(!is.na(Art), Art, "Other")),
            ANZAHL2) %>% 
  count(gattung_art, sort = TRUE) %>% 
  head(10)

trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  filter(!is.na(Gattung), Gattung != "unbekannt") %>% 
  count(Gattung, sort = TRUE) %>% 
  slice_max(n, n = 10) %>% 
  mutate(Gattung = fct_reorder(Gattung, n)) %>% 
  ggplot(aes(Gattung, n)) +
  geom_segment(aes(xend = Gattung, y = 0, yend = n, size = n),
               col = "#4C332C") +
  geom_point(aes(size = n, col = n)) +
  colorspace::scale_color_continuous_sequential(
    "Greens", trans = "pseudo_log", aesthetics = list("fill", "color")) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  )
  

library(treemapify)

treemap_df <- trees_2020_sf_prep %>% 
  st_drop_geometry() %>% 
  transmute(Gattung = ifelse(Gattung == "Verschiedene", "Other", Gattung),
            gattung = ifelse(is.na(Gattung) | Gattung == "unbekannt", "Unknown", Gattung),
            gattung = fct_lump_min(gattung, min = 2000, other_level = "Other"),
            art = ifelse(!is.na(Art), Art, ""),
            gattung_art = paste(Gattung, art, sep = " "),
            gattung_art = fct_lump_min(gattung_art, min = 50, other_level = "Other"),
            n = ANZAHL2) %>% 
  count(gattung, art, gattung_art, wt = n)

# number of groups
unique(treemap_df$gattung) 
unique(treemap_df$gattung_art)

# https://www.pinterest.de/pin/498562621254655108/
flora_palette <- c(
  "#BDA679", "#162231", "#6F7C47", "#8B7356", "#A1B36F", "#6E5542", 
  "#4F5F6F", "#3B191E", "#576148", "#30302E", "#8799A8", "#434318"
)


p <- treemap_df %>% 
  filter(gattung != "Unknown") %>% 
  mutate(color = case_when(
    gattung == "Other" ~ "grey32",
    as.character(gattung_art) == str_c(as.character(gattung), " ") ~ "#c2f0cf",
    TRUE ~ "grey89")) %>% 
  ggplot(aes(area = n, fill = gattung, subgroup = gattung)) +
  geom_treemap(color = "white") +
  geom_treemap_text(aes(label = gattung_art, color = color), 
                    place = "center",
                    reflow = TRUE, grow = FALSE, family = "Helvetica Neue") + # 
  geom_treemap_subgroup_border(color = "white") +
  scale_color_identity() +
  scale_fill_manual(values = rep(rev(flora_palette), 2)) +
  guides(
    fill = "none"
  ) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"), # "#ede5ca"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA, color = NA)
  )
ggsave(here(base_path, "04-treemap.png"), width = 8, height = 7)
