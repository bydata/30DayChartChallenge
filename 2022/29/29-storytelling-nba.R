library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(ggdist)
library(patchwork)
library(gganimate)
library(glue)
library(magick)

base_path <- here("2022", "29")

library(DBI)
library(dbplyr)
# Use the SQLite DB from day 07
con <- dbConnect(RSQLite::SQLite(), here("2022", "07", "basketball.sqlite"))

dbListTables(con)

query <- "
SELECT 
  FIRST_NAME, LAST_NAME, POSITION, HEIGHT, WEIGHT, FROM_YEAR, TO_YEAR 
FROM
  Player_Attributes
"
res <- dbSendQuery(con, query)
players <- dbFetch(res) %>% as_tibble()
colnames(players) <- tolower(colnames(players))
dim(players)

players <- players %>% 
  filter(!is.na(height), height > 0, position != "",
         !is.na(weight), weight > 0) %>% 
  # recode positions
  mutate(position = fct_recode(position, "Guard-Forward" = "Forward-Guard",
                               "Center-Forward" = "Forward-Center"
  ))
dim(players)


## custom ggplot2 theme
text_color <- "grey83"
bg_color <- "grey12"
theme_set(theme_void(base_family = "Poppins", base_size = 8))
theme_update(
  plot.background = element_rect(color = NA, fill = bg_color),
  text = element_text(color = text_color),
  strip.text = element_text(face = "bold",
                            margin = margin(t = 4, b = 4, l = 4, r = 4)),
  strip.text.y = element_text(angle = 90),
  strip.background = element_rect(color = NA, fill = alpha("grey20", 0.8)),
  panel.background = element_rect(color = NA, fill = "grey12"),
  axis.text = element_text(color = text_color, size = 6),
  axis.title.x = element_text(color = text_color, margin = margin(t = 4),
                              size = 8, family = "Poppins SemiBold"),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  plot.margin = margin(6, 6, 6, 6),
  axis.line.x = element_line(color = text_color),
  axis.ticks.x = element_line(color = text_color),
  axis.ticks.length.x = unit(2, "mm"),
  plot.title = element_text(color = "white", size = 14, family = "Poppins SemiBold"),
  plot.subtitle = element_markdown(hjust = 0.5, size = 10, lineheight = 1.3),
  plot.caption = element_textbox_simple(
    hjust = 0, margin = margin(t = 8, b = 2)
  )
)


## SAMPLING FROM 4k NBA PLAYERS ------------------------------------------------

get_sample_means <- function(n, repetitions) {
  samples <- map(seq_len(repetitions),  ~sample_n(players, size = n, replace = FALSE))
  means <- map_dbl(samples, ~mean(.x$height))
  means
}

means <- get_sample_means(n = 10, repetitions = 100)
means[order(means)]
mean(means)
sd(means)


data.frame(
  mean = means
) %>% 
  ggplot(aes(mean, y = 0)) + 
  geom_jitter(shape = "|", size = 6, alpha = 0.5, height = 0) +
  coord_cartesian(xlim = c(75, 81)) +
  theme_minimal()

data.frame(
  mean = means
) %>% 
  ggplot(aes(mean, y = 0)) + 
  stat_interval() +
  theme_minimal()

means <- get_sample_means(n = 100, repetitions = 100)
mean(means)
sd(means)

data.frame(
  mean = means
) %>% 
  ggplot(aes(mean, y = 0)) + 
  geom_jitter(shape = "|", size = 6, alpha = 0.5, height = 0) +
  coord_cartesian(xlim = c(75, 81)) +
  theme_minimal()



means <- get_sample_means(n = 1000, repetitions = 100)
mean(means)
sd(means)

data.frame(
  mean = means
) %>% 
  ggplot(aes(mean, y = 0)) + 
  geom_jitter(shape = "|", size = 6, alpha = 0.5, height = 0) +
  coord_cartesian(xlim = c(75, 81)) +
  theme_minimal()





foo <- pnorm(seq(0, 1, 0.001), mean = 78)
dnorm(seq(0, 1, 0.01), mean = 78.136, sd = 0.5)
qnorm(seq(0, 1, 0.01), mean = 78.106, sd = 0.5)
pnorm(seq(0, 1, 0.01), mean = 78.068, sd = 0.5)#

pnorm(78, mean = 78, sd = 0.5)
qnorm(0.5, mean = 78, sd = 0.5)
dnorm(78, mean = 78, sd = 0.5)
dnorm(79, mean = 78, sd = 0.5)

foo <- data.frame(
  x = rep(seq(73, 83, 0.1), 3),
  density = c(
    dnorm(seq(73, 83, 0.1), mean = 78.136, sd = 1.031),
    dnorm(seq(73, 83, 0.1), mean = 78.106, sd = 0.344),
    dnorm(seq(73, 83, 0.1), mean = 78.068, sd = 0.105)),
  group = c(rep("A", 101), rep("B", 101), rep("C", 101))
) 

foo %>% 
  ggplot(aes(x, density, col = group, fill = group)) +
  geom_area(alpha = 0.1) +
  facet_wrap(vars(group)) +
  theme_minimal()



## DRAW CIRCLES -----------------

n_players <- nrow(players)
n_selected <- 100

# Returns a dataframe of x, y coordinates of points placed in a shape of points
make_circular_points <- function(n, n_selected, seed = 1) {
  # https://stackoverflow.com/a/68619289/18952284
  set.seed(seed)
  r <- runif(n_players)
  th <- runif(n_players, 0, 2 * pi)
  df <- 
    data.frame(x = sqrt(r) * cos(th), 
               y = sqrt(r) * sin(th),
               selected = FALSE
    ) 
  df$selected[seq_len(n_selected)] <- TRUE
  df
}

# Create a plot of circular points with subtitle
plot_circular_points <- function(df_circular_points) {
  n_players <- nrow(df_circular_points)
  n_selected <- sum(df_circular_points$selected)
  p <- ggplot(df_circular_points, aes(x, y)) +
    geom_point(
      fill = "grey80",
      alpha = 0.6, shape = 21, size = 1, color = "white", stroke = 0.1) +
    geom_point(data = ~filter(., selected),
               aes(fill = selected), 
               alpha = 1, size = 1, stroke = 0.2, shape = 21, color = "white") +
    scale_fill_manual(values = c("FALSE" = "grey80", "TRUE" = "#f403fc")) + ##f0fc03
    coord_fixed() +
    guides(fill = "none") +
    labs(subtitle = glue("From {n_players} basketball players in NBA history<br>
       we randomly draw <br>
       <b style='font-size:12pt'>{n_selected}</b> <br>
       players
       <br><br>
       ... 100 times.")) +
    theme(axis.line.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank())
  p
}

# Animate circular points plot
animate_circular_points <- function(p = last_plot()) {
  p_anim <- p +
    transition_layers(from_blank = FALSE, layer_length = 1.5)
  animate(p_anim, res = 300, width = 1200, height = 1000, 
          duration = 2, bg = "grey12")
}


df_circular_points_10 <- make_circular_points(n_players, 10)
df_circular_points_100 <- make_circular_points(n_players, 100)
df_circular_points_1000 <- make_circular_points(n_players, 1000)

p_10   <- plot_circular_points(df_circular_points_10)
p_100  <- plot_circular_points(df_circular_points_100)
p_1000 <- plot_circular_points(df_circular_points_1000)

animate_circular_points(p_10)
anim_save(here(base_path, "29-anim-circles-10.gif"))
animate_circular_points(p_100)
anim_save(here(base_path, "29-anim-circles-100.gif"))
animate_circular_points(p_1000)
anim_save(here(base_path, "29-anim-circles-1000.gif"))


gif1_10   <- image_read(here(base_path, "29-anim-circles-10.gif"))
gif1_100  <- image_read(here(base_path, "29-anim-circles-100.gif"))
gif1_1000 <- image_read(here(base_path, "29-anim-circles-1000.gif"))
image_animate(c(gif1_10, gif1_100, gif1_1000), fps = 4)


# Display all player heights
players %>% 
  # head(1000) %>% 
  ggplot(aes(height)) +
  stat_dots(size = 2, shape = 21, alpha = 0.6, position = position_jitter(height = 0, width = 0.25))



# 
# 
# p_height <- players %>% 
#   ggplot(aes(position, height)) +
#   stat_halfeye(aes(fill = position), col = "white") +
#   geom_text(aes(position, y = min(height), label = position, col = position), 
#             stat = "unique", size = 4, hjust = 0, nudge_x = 0.3, family = "Bangers") +
#   paletteer::scale_color_paletteer_d("miscpalettes::jojo") +
#   paletteer::scale_fill_paletteer_d("miscpalettes::jojo") +
#   coord_flip() +
#   guides(color = "none", fill = "none")+
#   labs(y = "Height (in inches)")
# 
# 
# p_weight <- players %>% 
#   ggplot(aes(position, weight, fill = position, col = position)) +
#   stat_halfeye(col = "white") +
#   geom_text(aes(position, y = max(weight), label = position), stat = "unique",
#             hjust = 1, size = 4, nudge_x = 0.3, family = "Bangers") +
#   scale_y_continuous(breaks = seq(150, 350, 50)) +
#   paletteer::scale_color_paletteer_d("miscpalettes::jojo") +
#   paletteer::scale_fill_paletteer_d("miscpalettes::jojo") +
#   coord_flip() +
#   guides(color = "none", fill = "none") +
#   labs(y = "Weight (in pounds)")
# 
# n_players <- nrow(players)
# 
# p_height + p_weight +
#   plot_annotation(
#     title = "NBA Player Heights and Weights",
#     subtitle = glue::glue("Height and weight attributes of
#                           {scales::number(n_players, big.mark = ',')}
#                           NBA professionals from 1951 to 2019"),
#     caption = "Positions Guard-Forward and Forward-Guard as well as Forward-Center and Center-Forward 
#     merged.<br>Source: NBA, Kaggle. Visualization: Ansgar Wolsing")
# ggsave(here(base_path, "07-physical-nba-height-weight.png"), width = 7, height = 4)
# 
