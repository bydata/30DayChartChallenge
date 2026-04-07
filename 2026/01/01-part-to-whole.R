library(tidyverse)
library(ggtext)
library(patchwork)
library(here)

# Coalitions in Bundestag
seats <- list(
  "2017-2021" = list("CDU" = 246, "CSU" = 0, "SPD" = 153, "Opposition" = 709 - 246 - 0 - 153),
  "2021-2024" = list("SPD" = 206, "Grüne" = 118, "FDP" = 92, "Opposition" = 736 - 206 - 118 - 92),
  "Since 2025" = list("CDU" = 164, "CSU" = 44, "SPD" = 120, "Opposition" = 630 - 164 - 44 - 120)
)

# Parties colors
party_pal <- c(
  "SPD" = "#E3000F",
  "CDU/CSU" = "#000000",
  "CDU" = "#000000",
  "CSU" = "#333333",
  "FDP" = "#FEE465",
  "Grüne" = "#1AA037",
  "Opposition" = "#DDD"
  )


df_seats <- seats |> 
  bind_rows(.id = "period") |> 
  pivot_longer(cols = -c(period),
        names_to = "party", values_to = "n_seats") |> 
  filter(!is.na(n_seats)) |> 
  mutate(party = factor(party, levels = c("CDU", "CSU", "SPD", "Grüne", "FDP", "Opposition")))


df_plot <- df_seats |> 
  filter(period == "Since 2025") |> 
  arrange(period, party) |> 
  mutate(
    share = n_seats / sum(n_seats),
    label_pos_y = lag(cumsum(share), n = 1, default = 0) + share / 2,
    .by = period
  ) 

p_arc <- df_plot |> 
  ggplot(aes(1, n_seats, fill = fct_rev(party))) +
  geom_col(
    position = "fill", col = "white", linewidth = 0.5) +
  geom_text(
    data = ~filter(. , party != "Opposition"),
    aes(x = 1.65, y = label_pos_y, label = party),
    col = "grey8", family = "Fira Sans Medium"
  ) +
  geom_hline(yintercept = 0.5, col = "white", linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = party_pal) +
  coord_radial(expand = FALSE, theta = "y", 
    start = -0.5 * pi, end = 0.5 * pi,
    inner.radius = 0.3) +
  guides(fill = "none") +
  labs(subtitle = "**Arc**") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


p_donut <- df_plot |> 
  ggplot(aes(1, n_seats, fill = fct_rev(party))) +
  geom_col(
    position = "fill", col = "white", linewidth = 0.5) +
  geom_text(
    data = ~filter(. , party != "Opposition"),
    aes(x = 1.75, y = label_pos_y, label = party),
    col = "grey8", family = "Fira Sans Medium"
  ) +
  geom_hline(yintercept = 0.5, col = "white", linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = party_pal) +
  coord_radial(expand = FALSE, theta = "y", 
    inner.radius = 0.3) +
  guides(fill = "none") +
  labs(subtitle = "**Donut chart**") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


p_pie <- df_plot |> 
  ggplot(aes(1, n_seats, fill = fct_rev(party))) +
  geom_col(
    position = "fill", col = "white", linewidth = 0.5) +
  geom_text(
    data = ~filter(. , party != "Opposition"),
    aes(x = 1.65, y = label_pos_y, label = party),
    col = "grey8", family = "Fira Sans Medium"
  ) +
  geom_hline(yintercept = 0.5, col = "white", linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(values = party_pal) +
  coord_radial(expand = FALSE, theta = "y", 
    inner.radius = 0) +
  guides(fill = "none") +
  labs(subtitle = "**Pie chart**") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

p_stacked <- df_plot |> 
  ggplot(aes(1, n_seats, fill = fct_rev(party))) +
  geom_col(
    position = "fill", col = "white", linewidth = 0.5) +
  geom_text(
    data = ~filter(. , party != "Opposition"),
    aes(x = 1.65, y = label_pos_y, label = party),
    col = "grey8", family = "Fira Sans Medium"
  ) +
  geom_hline(yintercept = 0.5, col = "white", linetype = "dashed", linewidth = 0.5) +
  # Annotate 50 %
  annotate(
    "text",
    x = 0.2, y = 0.35, 
    label = "Majority of seats",
    family = "Fira Sans", hjust = 0.5, col = "grey35", size = 3.5
  ) +
  annotate(
    GeomCurve,
    x = 0.28, xend = 0.5, y = 0.5, yend = 0.5,
    curvature = -0.1, col = "grey35",
    arrow = arrow(angle = 25, length = unit(0.15, "cm")),
    linewidth = 0.2
  ) +
  scale_fill_manual(values = party_pal) +
  coord_flip(xlim = c(-0.5, 2)) +
  guides(fill = "none") +
  labs(subtitle = "**Cubist pie chart**") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


# Combine charts
p_pie + p_arc + p_donut + p_stacked +
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "The (Not so) Grand Coalition Majority in the 21st Bundestag",
    subtitle = "Breakdown of the coalition majority (328 of 630 seats)
formed by the CDU/CSU and SPD following the 2025 federal election in four different chart types",
    caption = "Visualization: Ansgar Wolsing") & 
  theme(
    plot.title = element_text(family = "Fira Sans", hjust = 0.5, size = 18),
    plot.subtitle = element_textbox(
      family = "Fira Sans", hjust = 0, halign = 0.5, size = 12, width = 1, lineheight = 1.25),
    plot.caption = element_text(family = "Fira Sans"),
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5)
  )
ggsave(here("2026", "01", "01-part-to-whole.png"), width = 6, height = 6, scale = 1.2)
