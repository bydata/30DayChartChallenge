library(tidyverse)
library(waffle)
library(treemapify)

#' Source: https://www.joyofbaking.com/breakfast/Waffles.html
#' 
#' Makes about 4 - 4 1/2 inch (10 cm) square Belgain waffles, 
#' or 4 - 6 1/2 inch (16.5 cm) round regular waffles.

Read more: https://www.joyofbaking.com/breakfast/Waffles.html#ixzz8VQKy77Bj
Follow us: @joyofbaking on Twitter | joyofbaking on Facebook
df <- data.frame(
  ingredient = c("Flour", "Baking Powder", "Salt", "Sugar", "Egg", "Milk", "Butter"),
  g = c(130, 5, 1.5, 25, 45, 240, 28) 
)

# Find the next greater square number
(total_g <- sum(df$g))
(next_gt_square <- ceiling(sqrt(total_g)) ^ 2)

df$g_weighted <- df$g * next_gt_square / total_g
df$g_weighted_rounded <- round(df$g_weighted)
head(df)
sum(df$g_weighted_rounded)

# Create waffle chart
df |> 
  ggplot(aes(fill = ingredient, values = g_weighted_rounded)) +
  geom_waffle(
    color = "white", size = 0.25, n_rows = sqrt(next_gt_square), na.rm = FALSE) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "bottom")

# Create waffle chart
df |> 
  ggplot(aes(group = ingredient, area = g_weighted_rounded)) +
  geom_treemap(color = "black", fill = "transparent", size = 1, alpha = 0.6) +
  geom_treemap_text(aes(label = ingredient)) +
  coord_equal() +
  theme_void() +
  theme(plot.background = element_rect(color = "transparent", fill = "transparent"))
ggsave(file.path("2024", "04", "04-waffle-recipe-grid-labels.png"), width = 4, height = 4)


df |> 
  ggplot(aes(group = ingredient, area = g_weighted_rounded)) +
  geom_treemap(color = "black", fill = NA, size = 1, alpha = 0.6) +
  geom_treemap_text(aes(label = ingredient)) +
  coord_equal() +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = NA))
ggsave(file.path("2024", "04", "04-waffle-recipe-grid.png"), width = 4, height = 4)


# Without labels, alpha levels to distinguish ingredients
df |> 
  mutate(ingredient = fct_reorder(ingredient, g)) |> 
  ggplot(aes(group = ingredient, area = g_weighted_rounded)) +
  geom_treemap(
    aes(alpha = ingredient),
    color = "black", fill = "white", size = 1.5, linetype = "dotted", show.legend = FALSE) +
  # geom_treemap_text(aes(label = ingredient)) +
  scale_alpha_manual(values = seq(0.2, 0.65, 0.075)) +
  coord_equal() +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = NA))
ggsave(file.path("2024", "04", "04-waffle-recipe-grid-alpha.png"), width = 4, height = 4)

# TODO: Auf eine Waffel runterrechnen oder Angabe, auf welche Menge sich das Rezept bezieht
