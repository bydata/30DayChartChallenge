library(tidyverse)
library(ggtext)
library(rvest)
library(patchwork)
library(here)

base_path <- here("2024", "02")

#' Source: Box Office Mojo (IMDb Pro)
#' https://www.boxofficemojo.com/franchise/fr3142029061/

# Scrape the date from the page
url <- "https://www.boxofficemojo.com/franchise/fr3142029061/"
page <- read_html(url)
df_raw <- page |> 
  html_node("table") |> 
  html_table()

# Prepare dataset
df <- df_raw |> 
  janitor::clean_names() |> 
  mutate(
    across(everything(), function(x) na_if(x, "-")),
    across(c(lifetime_gross, opening, max_theaters, open_th), function(x) as.numeric(str_remove_all(x, "[$,]"))),
    release_date = mdy(release_date),
    estimated = as.logical(estimated)
    ) |> 
  # select only the original movies, exclude re-releases
  filter(release %in% c("The Matrix Reloaded", "The Matrix", "The Matrix Revolutions", 
                        "The Matrix Resurrections"))

df |> 
  select(release, release_date, lifetime_gross, opening) |> 
  deparse() |> 
  writeLines()

# Instead of scraping the data
df <- structure(
  list(
    release = c("The Matrix Reloaded", "The Matrix", "The Matrix Revolutions", 
                "The Matrix Resurrections"),
    release_date = structure(c(12187,10681, 12361, 18983), class = "Date"), 
    lifetime_gross = c(281576461, 171479930, 139313948, 37686805), 
    opening = c(91774413, 27788331, 48475154, 10749011)), 
  row.names = c(NA, -4L), 
  class = c("tbl_df", "tbl", "data.frame"))
        
bar_width <- 0.5
matrix_green <- colorspace::lighten("#75C2A0", 0.5)
colorspace::lighten("#365989", 0.8)

p <- df |> 
  mutate(
    # release = sprintf("%s (%d)", toupper(release), year(release_date)),
    release = toupper(release),
    release = fct_reorder(release, release_date)) |> 
  arrange(release) |> 
  ggplot(aes(y = fct_rev(release))) +
  ggfx::with_inner_glow(
    geom_col(
    aes(x = lifetime_gross, fill = "Overall"),
    width = bar_width),
    sigma = 2, expand = 1, colour = matrix_green) +
  ggfx::with_inner_glow(
    geom_col(
    aes(x = opening, fill = "Opening week"),
    width = bar_width),
    sigma = 2, expand = 1, colour = matrix_green) +
  geom_text(
    aes(x = lifetime_gross, 
        label = scales::number(lifetime_gross, scale_cut = scales::cut_long_scale())),
    hjust = 0, nudge_x = 5e6, family = "Source Sans Pro SemiBold", color = matrix_green
  ) +
  # Annotation inside the longest bar
  geom_text(
    data = ~subset(., lifetime_gross == max(lifetime_gross)),
    aes(x = opening / 2, 
        label = "Opening week"), 
    color = "grey9", hjust = 0.5, family = "Source Sans Pro"
  ) +
  geom_text(
    data = ~subset(., lifetime_gross == max(lifetime_gross)),
    aes(x = opening + (lifetime_gross - opening) / 2, 
        label = "After opening week"), 
        color = "grey9", hjust = 0.5, family = "Source Sans Pro"
  ) +
  scale_y_discrete(expand = c(0.4, 0)) +
  scale_fill_manual(values = c("#EFFEFC", matrix_green)) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(release), ncol = 1, scales = "free_y") +
  guides(fill = "none") +
  labs(
    title = "Box office gross earnings of The Matrix films",
    subtitle = "(in USD)",
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
    plot.title = element_text(
      family = "Source Sans Pro SemiBold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )

# Add annotation for total
p_annotations <- ggplot() +
  annotate(
    "text", x = 4.1, y = 3.15, label = c("Total"),
    color = matrix_green,
    family = "Source Sans Pro", hjust = 0.8
  ) +
  annotate(
    "curve",
    x = 4.1, xend = 4.1, y = 3.05, yend = 2.7, 
    curvature = -0.2, color = matrix_green, linewidth = 0.3,
    arrow = arrow(angle = 20, length = unit(1.3, "mm"))
  ) +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 4)) +
  theme_void()

p + inset_element(p_annotations, t = 1, b = 0, l = 0, r = 1) &
  theme(plot.background = element_rect(color = NA, fill = NA))

ggsave(here(base_path, "02-neo.png"), width = 4, height = 4, scale = 1.33, bg = "grey9")
