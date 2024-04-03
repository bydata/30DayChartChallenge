library(tidyverse)
library(ggtext)
library(patchwork)
library(here)

base_path <- here("2024", "02")

#' Source: Box Office Mojo (IMDb Pro)
df <- read_tsv(here(base_path, "matrix-box-office-gross.tsv"))

df_long <- df |> 
  mutate(us_release_date = mdy(us_release_date)) |> 
  pivot_longer(
    cols = c(domestic_gross, international_gross),
    names_to = "region", names_transform = function(x) str_remove(x, "_gross"),
    values_to = "gross") |> 
  mutate(
    release = toupper(release),
    release = fct_reorder(release, us_release_date),
    region = factor(region, levels = c("international", "domestic"),
                    labels = c("International", "US"))) |>
  mutate(
    gross_label_pos = lag(gross, 1, default = 0) + gross / 2,
    .by = release) 

bar_width <- 0.45
matrix_green <- colorspace::lighten("#75C2A0", 0.5)
colorspace::lighten("#365989", 0.8)

p <- 
  df_long |>
  ggplot(aes(y = release, group = region)) +
  ggfx::with_inner_glow(
    geom_col(
    aes(x = gross, fill = region),
    width = bar_width),
    sigma = 2, expand = 1, colour = matrix_green) +
  # Data labels inside bars
  geom_text(
    aes(x = gross_label_pos, 
        label = scales::number(gross, 
                               accuracy = 1, scale_cut = scales::cut_long_scale())),
    hjust = 0.5, family = "Source Sans Pro SemiBold", size = 3, color = "grey20"
  ) +
  # Data label for worldwide numbers
  geom_text(
    aes(x = worldwide_gross,
        label = scales::number(worldwide_gross, accuracy = 1, scale_cut = scales::cut_long_scale())),
    hjust = 0, nudge_x = 7e6, family = "Source Sans Pro SemiBold", size = 3,
    color = matrix_green
  ) +
  # Annotation for the regions inside the first bar
  geom_text(
    data = ~subset(., us_release_date == min(us_release_date)),
    aes(x = gross_label_pos,
        label = paste(region, "earnings")),
    color = "grey40", hjust = 0.5, vjust = 2.3, size = 2.5, family = "Source Sans Pro"
  ) +
  scale_y_discrete(expand = c(0.33, 0)) +
  scale_fill_manual(values = c("#EFFEFC", matrix_green)) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(release), ncol = 1, scales = "free_y") +
  guides(fill = "none") +
  labs(
    title = "Box office gross earnings of The Matrix films",
    subtitle = "(in million USD)",
    caption = "Source: Box Office Mojo (IMDb Pro). Visualization: Ansgar Wolsing") +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "#101010", fill = "#101010"),
    plot.margin = margin(t = 4, b = 2, l = 2, r = 12),
    text = element_text(color = matrix_green),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(
      family = "Syne Mono",
      color = matrix_green, hjust = 0, size = 11,
      margin = margin(l = 6, b = -2)),
    panel.grid = element_blank(),
    panel.spacing.y = unit(4, "mm"),
    plot.title = element_text(
      family = "Source Sans Pro SemiBold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(
      size = 11, hjust = 0.5, margin = margin(t = 2, b = 8)),
    plot.caption = element_text(hjust = 0.5)
  )

# Add annotation for total
p_annotations <- ggplot() +
  annotate(
    "text", x = 4.1, y = 3.15, label = c("Total"),
    color = matrix_green, size = 3,
    family = "Source Sans Pro", hjust = 0.8
  ) +
  annotate(
    "curve",
    x = 4.1, xend = 4.1, y = 3.05, yend = 2.7, 
    curvature = -0.2, color = matrix_green, linewidth = 0.25,
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +
  theme_void()

p + inset_element(p_annotations, t = 1, b = 0, l = 0, r = 1) &
  theme(plot.background = element_rect(color = NA, fill = NA))

ggsave(here(base_path, "02-neo.png"), width = 4, height = 4, scale = 1.33, bg = "grey9")
