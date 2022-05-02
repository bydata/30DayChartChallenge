library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2022", "28")

set.seed(1)
mean <- 0
n <- 10^3
sd_values <- c(0.1, 0.5, 1.25, 2, 3)
mean_values <- runif(length(sd_values), -2, 2)
# df <- data.frame(sapply(sd_values, function(x) rnorm(n = n, mean = mean, x)))
df <- map2_dfc(mean_values, sd_values, ~rnorm(n = n, .x, .y))

names_m_sd <- sprintf(
  "x\U0304 = %s<br>\U03C3 = %s", 
  str_pad(scales::number(mean_values, accuracy = 0.1), 4, side = "left", pad = " "), 
  scales::number(sd_values, accuracy = 0.1))

p <- df %>% 
  set_names(names_m_sd) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value, fill = name, color = name)) +
  stat_density(geom = "area", alpha = 0.1, col = NA, bw = 2, n = 2^12, size = 0.8) +
  stat_density(geom = "line", alpha = 1, bw = 2, n = 2^12, size = 0.8) +
  paletteer::scale_color_paletteer_d("jcolors::pal3") +
  paletteer::scale_fill_paletteer_d("jcolors::pal3") +
  guides(fill = "none") +
  labs(
    title = "STANDARD DEVIATIONS",
    subtitle = "5 distributions with different means and standard deviations",
    caption = "**Visualization:** Ansgar Wolsing",
    col = NULL) +
  theme_void(base_family = "Noto Sans Math") +
  theme(
    plot.background = element_rect(color = NA, fill = "#210d33"),
    plot.margin = margin(t = 8, b = 8, l = 16, r = 16),
    text = element_text(color = "grey99"),
    plot.title = element_text(
      family = "Oswald", color = "white", size = 24, hjust = 0.5,
      margin = margin(b = 4)),
    plot.subtitle = element_markdown(hjust = 0.5, margin = margin(b = 12)),
    plot.caption = element_markdown(family = "Noto Sans", size = 6),
    legend.position = c(0.9, 0.8),
    legend.text = element_markdown(family = "Noto Sans Math", size = 6),
    legend.key.height = unit(6, "mm"))
ggsave(here(base_path, "28-deviation.png"), dpi = 400, width = 5, height = 4)
