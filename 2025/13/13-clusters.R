library(tidyverse)
library(ggtext)
library(worldfootballR)
library(factoextra)
library(here)

base_path <- here("2025", "13")

#' Download the PDF report from
#' https://www.dfl.de/de/hintergrund/lizenzierungsverfahren/finanzkennzahlen-der-proficlubs/

df_kpi <- read_tsv(here(base_path, "Bundesliga_Kennzahlen_2023.tsv"))

df_kpi <- df_kpi |> 
  mutate(Personalaufwand = -Personalaufwand) |> 
  mutate(Verein = case_match(
    Verein,
    "1. FC Union Berlin" ~ "Union Berlin",
    "1. FSV Mainz 05" ~ "Mainz 05",
    "1. FC Heidenheim" ~ "FC Heidenheim",
    .default = Verein
  ))

## Bundesliga standings
df_standings <- fb_season_team_stats(
  country = "GER", gender = "M", season_end_year = 2025, tier = "1st", 
  stat_type = "league_table")

# Consolidate club names
club_mapping <- c(
  "Augsburg" = "FC Augsburg",
  "Union Berlin" = "Union Berlin",
  "Bochum" = "VfL Bochum",
  "Werder Bremen" = "Werder Bremen",
  "Dortmund" = "Borussia Dortmund",
  "Eint Frankfurt" = "Eintracht Frankfurt",
  "Freiburg" = "SC Freiburg",
  "Heidenheim" = "FC Heidenheim",
  "Hoffenheim" = "TSG Hoffenheim",
  "Holstein Kiel" = "Holstein Kiel",
  "RB Leipzig" = "RB Leipzig",
  "Leverkusen" = "Bayer 04 Leverkusen",
  "Mainz 05" = "Mainz 05",
  "Gladbach" = "Borussia Mönchengladbach",
  "Bayern Munich" = "FC Bayern München",
  "St. Pauli" = "FC St. Pauli",
  "Stuttgart" = "VfB Stuttgart",
  "Wolfsburg" = "VfL Wolfsburg"
)

df_standings <- df_standings |> 
  mutate(Club = club_mapping[Squad])

# Check club mapping
df_kpi |> 
  anti_join(df_standings, by = join_by(Verein == Club)) |> 
  nrow() == 0


df_kpi_standings <- df_kpi |> 
  inner_join(df_standings, by = join_by(Verein == Club)) |> 
  # Remove Kiel and St. Pauli (2nd division during the period covered) and 
  # Eintracht Frankfurt (only 6 months)
  filter(!Verein %in% c("Holstein Kiel", "FC St. Pauli", "Eintracht Frankfurt"))
 
# Regression model
mod <- lm(Pts ~ log(Personalaufwand), data = df_kpi_standings)
summary(mod)
str(mod)
adj_r2 <- summary(mod)$adj.r.squared

df_kpi_standings_fitted <- df_kpi_standings |> 
  bind_cols(pred = mod$fitted.values) |> 
  bind_cols(residual = mod$residuals)

df_kpi_standings_fitted_scaled <- df_kpi_standings_fitted |> 
  select(Pts, Personalaufwand, residual) |> 
  scale()
rownames(df_kpi_standings_fitted_scaled) <- df_kpi_standings_fitted$Verein

(res_opt <- fviz_nbclust(df_kpi_standings_fitted_scaled, kmeans, method = "silhouette"))
n_optimal_clusters <- which.max(res_opt$data$y)

set.seed(123)
km_result <- kmeans(df_kpi_standings_fitted_scaled, n_optimal_clusters, nstart = 25)

df_kpi_standings_clusters <- df_kpi_standings_fitted |> 
  bind_cols(cluster = km_result$cluster) 


color_pal <- c(MetBrewer::met.brewer("Egypt", direction = 1)[c(2, 4, 1, 3)], "grey50")
current_matchday <- max(df_standings$MP)

# Calculations for the background area
axis_xmin <- 25e6 # min(df_kpi_standings$Personalaufwand)
axis_xmax <- max(df_kpi_standings$Personalaufwand) * 1e3
axis_ymin <- predict(mod, newdata = data.frame(Personalaufwand = axis_xmin / 1e3))  
axis_ymax <- predict(mod, newdata = data.frame(Personalaufwand = axis_xmax / 1e3))

poly_axis_x <- c(axis_xmin, axis_xmin, axis_xmax, axis_xmax, axis_xmin)
poly_axis_y <- c(axis_ymin,        15,        15, axis_ymax, axis_ymin)
df_poly_axis <- data.frame(
  x = poly_axis_x,
  y = poly_axis_y
)


ragg::agg_png(here(base_path, "13-clusters.png"), width = 5, 
              height = 5, units = "in", res = 300)
df_kpi_standings_clusters |> 
  mutate(cluster2 = case_when(
    Verein %in% c("Borussia Mönchengladbach", "SC Freiburg", "VfB Stuttgart",
                  "RB Leipzig", "FC Bayern München", "FC Heidenheim") ~ 5, 
    Verein %in% c("Bayer 04 Leverkusen") ~ 4,
    TRUE ~ cluster)) |> 
  ggplot(aes(Personalaufwand * 1e3, Pts)) +
  # Background fill
  geom_polygon(
    data = df_poly_axis,
    aes(x, y),
    inherit.aes = FALSE, fill = "grey90", alpha = 0.2
  ) + 
  stat_smooth(method = "lm", fill = alpha("grey80", 0.1), linetype = "solid",
              col = "grey20", linewidth = 0.4, se = TRUE, fullrange = TRUE) +
  geom_point(
    aes(col = factor(cluster2)),
    size = 3
  ) + 
  ggrepel::geom_label_repel(
    aes(label = str_wrap(Verein, 14)), 
    size = 2, segment.size = 0.1, min.segment.length = 0.3,  hjust = 0,
    label.padding = unit(0, "mm"), family = "Roboto Condensed", lineheight = 0.8,
    fill = NA, 
    label.size = 0, seed = 1) +
  annotate(
    "label",
    x = c(80e6, 240e6, 160e6, 45e6),
    y = c(24, 35, 68, 61),
    label = str_wrap(
      c("Underperforming / below EUR 100M",
        "Underperforming / above EUR 100M",
        "Overperforming / above EUR 100M",
        "Overperforming teams with personnel costs below EUR 100M"), 
      24),
    fill = color_pal[c(2, 1, 4, 3)],
    col = c("black", "white", "white", "white"),
    size = 2.75, hjust = 0.5, family = "Roboto Condensed Medium", lineheight = 0.9,
    alpha = 0.9, label.size = 0
  ) +
  annotate(
    "label", 
    x = 24e6, y = 70, label = "\U2193 Points", fill = "#F8F8F8", label.size = 0,
    family = "Roboto Condensed", hjust = 0, size = 3
  ) +
  annotate(
    "richtext", 
    x = 210e6, y = 53, 
    label = sprintf("Regression line: clubs above<br>perform above average 
    (Adj. R<sup>2</sup>: %s)", scales::percent(adj_r2)),
    color = "grey20", angle = 26, lineheight = 0.9, fill = NA,
    family = "Roboto Condensed", hjust = 0, vjust = 1, size = 2, label.size = 0
  ) +
  scale_x_continuous(
    labels = scales::label_number(scale_cut = scales::cut_long_scale()),
    breaks = c(25, 50, 100, 200, 400) * 1e6,
    transform = "log") +
  scale_color_manual(values = color_pal) +
  coord_cartesian(xlim = c(25e6, NA), ylim = c(15, NA), expand = FALSE, clip = "off") +
  labs(
    title = "How far does a Euro go? Bundesliga spending efficiency",
    subtitle = sprintf("Budget spent on personnel vs. points gained in Bundesliga 2024-25 
    (matchday %d)", current_matchday),
    caption = sprintf("**Note:** Promoted Holstein Kiel and FC St.Pauli are excluded 
    because their financial KPIs reflect their status in Bundesliga 2.<br><br>
    **Source:** DFL, clubs' financial KPIs 2023; FBRef, Bundesliga standings
                      (matchday %d).<br>**Visualization:** Ansgar Wolsing", 
                      current_matchday),
    x = "Total personnel costs (EUR, log scale)",
    y = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.text = element_text(color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.1),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.1),
    panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.05),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95,
      lineheight = 1.2),
    plot.subtitle = element_textbox(
      margin = margin(t = 2, b = 16), lineheight = 1.2, width = 1, size = 10
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      lineheight = 1.1, margin = margin(t = 10, b = 6), hjust = 0, size = 7, width = 1),
    legend.position = "none",
    plot.margin = margin(t = 4, r = 20, b = 4, l = 8),
    axis.title.x = element_text(size = 9)
  )
dev.off()
