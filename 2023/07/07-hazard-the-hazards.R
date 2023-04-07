library(tidyverse)
library(ggtext)
library(here)
library(worldfootballR)
library(ggchicklet)
# devtools::install_github("doehm/cropcircles")
library(cropcircles)

base_path <- here("2023", "07")

fbref_urls <- c(
  "Eden" = "https://fbref.com/en/players/a39bb753/Eden-Hazard",
  "Thorgan" = "https://fbref.com/en/players/50871bcc/Thorgan-Hazard",
  "Kylian" = "https://fbref.com/en/players/f331375e/Kylian-Hazard"
)

stats <- fb_player_season_stats(fbref_urls, stat = "standard")
stats <- stats %>%
  mutate(player_name = factor(
    player_name, levels = c("Eden Hazard", "Thorgan Hazard", "Kylian Hazard")))

# In which competitions have they played?
unique(stats$Comp)

# which clubs have the Hazards played for so far?
unique(stats$Squad)

# maybe use ggpattern?
# Club colors
club_main_colors <- c(
  "Lille" = "#e01e13",
  "Chelsea" = "#034694",   
  "Real Madrid" = "white",
  "Lens" = "#ec1c24",  # #fff200
  "Zulte Waregem"  = "#d30240", # #00883b
  "M'Gladbach" = "black", # white
  "Dortmund"   = "#FDE100", #black
  "PSV Eindhoven" = "#f00000", ##a8862d
  "Újpest"  = "#6209B9",
  "Cercle Brugge" = "#009e45",
  "RWD Molenbeek"  = "#E62D23" ##212121
)


# Player images 

#' Image credits: 
#' Eden: Brian Minkoff-London, CC BY-SA 4.0
#' Kylian: Nadine Hazard, CC BY-SA 4.0
#' Thorgan: Steffen Prößdorf, CC BY-SA 4.0

# Download images from Wikipedia
player_photo_urls <- 
  c("Eden" = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/EdenHazardDecember_2016.jpg/634px-EdenHazardDecember_2016.jpg?download",
    "Kylian" = "https://upload.wikimedia.org/wikipedia/commons/2/2c/Kylian_Hazard_%28cropped%29.jpg?download",
    "Thorgan" = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/2021-11-06_Fu%C3%9Fball%2C_M%C3%A4nner%2C_1._Bundesliga%2C_RB_Leipzig_-_Borussia_Dortmund_1DX_1729_by_Stepro.jpg/640px-2021-11-06_Fu%C3%9Fball%2C_M%C3%A4nner%2C_1._Bundesliga%2C_RB_Leipzig_-_Borussia_Dortmund_1DX_1729_by_Stepro.jpg?download"
    )
player_photo_files <- here(base_path, paste(names(player_photo_urls), "jpg", sep = "."))
walk2(player_photo_urls, player_photo_files, 
      ~download.file(.x, destfile = .y))

# Crop images
player_photos <- tibble(
  player_name = paste(names(player_photo_urls), "Hazard"),
  image = player_photo_files
  ) %>% 
  mutate(image_cropped = circle_crop(image, border_size	= 8, border_colour = "white"))

# Use as a labeller function in facet_wrap
label_player_photo <- function(x) {
  file <- player_photos$image_cropped[player_photos$player_name == x]
  firstname <- str_remove(x, " Hazard")
  sprintf("%s<br><img src='%s' width=30 height=30>", toupper(firstname), file)
}

# When did they join there clubs?
joined_squad_age <- stats %>% 
  group_by(player_name, Squad) %>% 
  summarize(first_age = min(Age), .groups = "drop")

bg_color <- "grey90"

stats %>% 
  group_by(player_name, Season, Squad, Age) %>% 
  summarize(total_goals = sum(Gls, na.rm = TRUE), .groups = "drop") %>% 
  inner_join(player_photos, by = "player_name") %>% 
  ggplot(aes(Age, total_goals)) + 
  geom_segment(
    data = joined_squad_age,
    aes(x = first_age - 0.3, xend = first_age - 0.3, y = 0, yend = 20),
    color = "grey60", linetype = "dashed", linewidth = 0.2
  ) +
  geom_text(
    data = joined_squad_age,
    aes(x = first_age, y = 20, label = Squad),
    hjust = 1, vjust = 0.33, angle = 90, size = 2, family = "Roboto Condensed"
  ) +
  geom_chicklet(
    aes(fill = Squad), 
    position = "stack", size = 0.1, color = "white", radius = unit("1", "pt")) +
  scale_fill_manual(values = club_main_colors) +
  facet_wrap(vars(player_name), ncol = 1, strip.position	= "right",
             labeller = labeller(player_name = label_player_photo)) +
  guides(fill = "none") +
  labs(
    title = "Goals scored by the Hazard family",
    subtitle = "Total goals per season for their clubs in professional football",
    caption = "Data: FBRef. 
    Image credits:
    Eden: Brian Minkoff-London, CC BY-SA 4.0,
    Kylian: Nadine Hazard, CC BY-SA 4.0,
    Thorgan: Steffen Prößdorf, CC BY-SA 4.0. 
    Visualisation: Ansgar Wolsing",
    x = "Age",
    y = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.background = element_rect(color = bg_color, fill = bg_color),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(),
    plot.caption = element_textbox(width = 1.2, size = 6, lineheight = 0.95, hjust = 0),
    strip.text.y = element_markdown(angle = 0, face = "bold", color = "grey40"),
    axis.text = element_text(size = 8)
  )
ggsave(here(base_path, "07-hazard-the-hazards-goals.png"), width = 4, height = 5)
