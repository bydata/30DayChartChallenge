library(tidyverse)
library(ggtext)
library(ggalluvial)
library(cowplot)
library(here)

#' Source:
#' Deutsche Bundesbank (2025), 
#' Vermögen und Finanzen privater Haushalte in Deutschland:
#' Ergebnisse der Vermögensbefragung 2023, Monatsbericht, April 2025.
#' https://publikationen.bundesbank.de/publikationen-de/berichte-studien/monatsberichte/monatsbericht-april-2025-954594?article=vermoegen-und-finanzen-privater-haushalte-in-deutschland-ergebnisse-der-vermoegensbefragung-2023--954598

quintile_labels <- c("0-20", "20-40", "40-60", "60-80", "80-100")
quintile_labels_long <- c(
  "1st quintile</span><br><span style='font-family:\"Instrument Sans Medium\"'>(lowest)</span>", 
  "2nd quintile", "3rd quintile", "4th quintile", 
  "5th quintile</span><br><span style='font-family:\"Instrument Sans Medium\"'>(highest)</span>"
)
names(quintile_labels_long) <- quintile_labels_long

df <- data.frame(
  quintiles_2011 = factor(
    rep(quintile_labels, each = 5),
    levels = quintile_labels),
  quintiles_2023 = factor(
    rep(quintile_labels, 5),
    levels = quintile_labels),
  share = c(
    0.63, 0.17, 0.10, 0.08, 0.03,
    0.24, 0.43, 0.20, 0.10, 0.03,
    0.09, 0.28, 0.35, 0.23, 0.06,
    0.02, 0.10, 0.25, 0.32, 0.32,
    0.02, 0.03, 0.10, 0.28, 0.57
  )
)


## CHART TYPE 1: Transition matrix / Heatmap

df |> 
  ggplot(aes(quintiles_2023, fct_rev(quintiles_2011))) +
  geom_tile(
    aes(fill = share),
    height = 0.75, width = 0.9, col = "grey10"
  ) +
  geom_label(
    aes(
      col = ifelse(share > 0.4, "white", "grey4"),
      label = scales::percent(share)),
    fill = "#FFFFFF11", label.size = 0,
    family = "Roboto Mono Medium"
  ) +
  scale_x_discrete(position = "top", expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  scale_color_identity() +
  scale_fill_distiller(
    palette = "Greys", direction = 1,
    breaks = seq(0.1, 1, 0.1),
    labels = scales::label_percent()
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    x = "2023 \U2192", y = "2011 \U2193",
    fill = "Share of 2011 quintile"
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(angle = 0),
    legend.position = "top",
    legend.justification = "right",
    panel.grid = element_blank()
  )


# caption = "***Notes:** Household net income (in EUR).
#     Due to rounding, shares within two of the groups do not add up to 100 %.*
#     <br><br>"


## CHART TYPE 2: ALLUVIAL PLOT

p_alluvial <- df |> 
  mutate(
    across(c(quintiles_2011, quintiles_2023), function(x) quintile_labels_long[x]),
    across(c(quintiles_2011, quintiles_2023), function(x) fct_rev(fct_inorder(x)))
  ) |> 
  ggplot(
    aes(axis1 = quintiles_2011, axis2 = quintiles_2023, y = share)) +
  geom_alluvium(
    aes(fill = quintiles_2011),
    alpha = 0.75, width = 0.25, knot.pos = 0.33, discern = TRUE
  ) +
  geom_stratum(
    aes(fill = after_stat(stratum)),
    width = 0.25, color = "white", linewidth = 0.4
  ) +
  geom_richtext(
    aes(label = str_remove(after_stat(stratum), "\\.\\d")),
    stat = "stratum",
    size = 4, color = "white", family = "Instrument Sans Bold",
    fill = NA, label.size = 0,
    discern = TRUE
  ) +
  scale_x_discrete(
    limits = c("2011", "2023"), position = "top",
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  paletteer::scale_fill_paletteer_d("calecopal::lupinus", direction = -1) + 
  guides(fill = "none") +
  labs(
    title = "Most wealth mobility occurs in the middle of the wealth distribution",
    subtitle = "The chart tracks the movement of households
    across the **wealth distribution in Germany** over a **twelve-year period**.
    The wealth quintiles divide all surveyed households into
    5 equal-sized groups based on their net wealth - ranging from the 
    20% of households with the lowest wealth (1st quintile) to the 
    20% with the highest wealth (5th quintile).
    The **thickness of the streams** represents the proportion of households
    which shifted from one quintile in 2011 (left side) to another in 2023 (right-side).
    A straight horizontal flow indicates that a household remained in the same relative wealth bracket.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Instrument Sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_textbox(
      width = 1, color = "grey40", size = 12, lineheight = 1.3,
      margin = margin(b = 24)),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_markdown(family = "Instrument Sans SemiBold", size = 14, vjust = 0),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 8, r = 12, b = 4, l = 12)
  )

# Tabular
p_tab <- df |> 
  mutate(
    across(c(quintiles_2011, quintiles_2023), function(x) quintile_labels_long[x]),
    across(c(quintiles_2011, quintiles_2023), function(x) fct_rev(fct_inorder(x)))
  ) |> 
  ggplot(aes(quintiles_2023, fct_rev(quintiles_2011))) +
  geom_tile(
    aes(fill = quintiles_2011),
    alpha = 0.25
  ) +
  geom_label(
    aes(label = scales::percent(share)),
    fill = "#FFFFFF11", label.size = 0,
    family = "Roboto Mono Medium"
  ) +
  geom_hline(
    data = data.frame(
      yintercept = seq_along(levels(df$quintiles_2011)) - 0.5
    ),
    aes(yintercept = yintercept),
    linewidth = 0.1
  ) +
  scale_x_discrete(position = "top", expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  paletteer::scale_fill_paletteer_d("calecopal::lupinus", direction = -1) + 
  guides(fill = "none") +
  labs(
    caption = "**Source:** Bundesbank, Monthly Report - April 2025. 
    Households which were part of the Bundesbank's panel<br>on household
    finances in 2011 and 2023.
    **Visualization:** Ansgar Wolsing",
    x = "2023 \U2192", y = "2011 \U2193"
  ) +
  theme_minimal(base_family = "Instrument Sans", base_size = 12, 
paper = "white") +
  theme(
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(angle = 0),
    axis.text.x.top = element_markdown(),
    axis.text.y.left = element_markdown(),
    legend.position = "top",
    legend.justification = "right",
    panel.grid = element_blank(),
    plot.margin = margin(t = 4, r = 20, b = 4, l = 20),
    plot.caption =  element_markdown(
      hjust = 0, lineheight = 1.1, size = rel(0.9),
      margin = margin(t = 12, b = 2)
    ),
    plot.caption.position = "plot"
  )

# Combine the plots
plot_grid(
  ggplotGrob(p_alluvial), ggplotGrob(p_tab),
  ncol = 1,
  rel_heights = c(5, 2),
  align = "none" 
)
ggsave(here("2026", "17", "17-remake.png"), width = 8, height = 10)
