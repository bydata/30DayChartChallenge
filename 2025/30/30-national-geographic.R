library(tidyverse)
library(ggtext)
library(ggdist)
library(grid)
library(here)

base_path <- here("2025", "30")

#' Download ESS survey data from 
#' https://ess.sikt.no/en/series/321b06ad-1b98-4b7d-93ad-ca8a24e8788a
#' ESS 9

df_ess09 <- read_csv(here(base_path, "ESS9e03_2.csv"))

# Select the relevant variables
relevant_vars <- c("essround", "anweight", "cntry", "pdempyr", "yrbrn")
df_ess09_prep <- df_ess09 |> 
  select(all_of(relevant_vars)) |> 
  filter(pdempyr < 6666, yrbrn < 7777) |> 
  mutate(
    cntry_name = countrycode::countrycode(
      cntry, origin = "iso2c", destination = "country.name"),
    age_started_work = pdempyr - yrbrn) |> 
  # Exclude very low / negative values
  filter(age_started_work >= 12)
nrow(df_ess09_prep)

#' National Geographic theme
#' Inspiration: https://www.nationalgeographic.com/what-the-world-eats/
#' https://www.nationalgeographic.com/travel/article/charts-show-coronavirus-impact-on-travel-industries
bg_color <- "#F0F0F0"
brand_color <- "#FFCE00"
main_color <- "#F9A619"
header_color <- "#2C2A29"
grey <- "#7D7D7D"
main_font <- "Gill Sans"
sec_font <- "Crimson Text" # Original NG font is Minion Pro

theme_ng <- function(...) {
  theme_minimal(base_family = main_font) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.grid.major = element_line(color = "grey50", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      paste(main_font, "SemiBold"), color = "black"),
    plot.margin = margin(rep(6, 4)),
    plot.caption = element_markdown(hjust = 0, lineheight = 1.3),
    legend.position = "top", legend.justification = "left"
  )
}

plot_title <- "Age people started their first paid job (or apprenticeship)"

# Annotations for the intervals
legend_annotations <- c("Average", "50 %", "80 %", "90 % of respondents")
n_rows <- length(unique(df_ess09_prep$cntry_name))
legend_pos_x <- n_rows + 1.7
legend_pos_xend <- n_rows + 0.2
legend_pos_y <- c(23, 26, 28, 32)
legend_pos_yend <- c(23, 24.5, 27, 30)


ragg::agg_png(
  here(base_path, "30-national-geographic.png"), width = 5, height = 6.5,
  units = "in", res = 300
)
df_ess09_prep |> 
  # Order countries by average age
  mutate(cntry_name = fct_reorder(cntry_name, age_started_work, .fun = mean)) |> 
  ggplot(aes(cntry_name, age_started_work)) +
  stat_interval(
    aes(weight = anweight),
    .width = c(0.5, 0.8, 0.9),
    width = 0.7) +
  stat_summary(
    geom = "point",
    fun = mean,
    shape = "|", size = 6, col = bg_color
  ) +
  annotate(
    "label",
    x = legend_pos_x,
    y = legend_pos_y,
    label = legend_annotations,
    family = "Roboto Condensed", size = 3, fill = bg_color, label.size = 0,
    hjust = 0.2
  ) +
  annotate(
    GeomCurve,
    x = legend_pos_x - 0.5, xend = legend_pos_xend,
    y = legend_pos_y, yend = legend_pos_yend,
    color = "grey40", linewidth = 0.2, curvature = 0.1,
    arrow = arrow(angle = 15, length = unit(2, "mm"), type = "closed")
  ) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(position = "right", expand = c(0, 0)) +
  scale_color_manual(values = c("grey70", "grey30", main_color)) +
  coord_flip(xlim = c(NA, n_rows + 2), clip = "off") +
  labs(
    title = plot_title,
    caption = "***Note**: Weighted with analytics weight.*<br><br>
    **Source:** European Social Survey Wave 8 (2018).<br>
    **Visualization:** Ansgar Wolsing | in the style of National Geographic",
    y = "Average age (years)"
  ) +
  theme_ng() +
  theme(
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      family = paste(main_font, "SemiBold"), color = "grey40", hjust = 1),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(
      margin = margin(t = 4, l = 18, b = 20)),
    legend.position = "none"
  )

grid.rect(
  x = 0, y = 1, width = 2, height = 0.115,
  gp = gpar(col = NA, fill = header_color, lwd = 0)
)
# NG icon
grid.rect(
  x = 0.03, y = 0.973, width = 0.03, height = 0.04,
  gp = gpar(col = brand_color, fill = NA, lwd = 2)
)
grid.text(
  label = plot_title,
  x = 0.07, y = 0.973, hjust = 0, just = "left", 
  gp = gpar(col = "white", fontfamily = paste(main_font, "SemiBold"))
)
dev.off()
