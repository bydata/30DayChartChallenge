library(tidyverse)
library(ggtext)
library(gganimate)
library(here)

#' Source: Politbarometer (Forschungsgruppe Wahlen), April 2026
#' https://www.forschungsgruppe.de/Umfragen/Politbarometer/Archiv/Politbarometer_2026/April_2026/
#' Hypothetical Outcome Plots: 
#' https://idl.uw.edu/papers/hops

# Party data: share in percent
df_poba <- tibble(
  party = c("AfD", "CDU/CSU", "Grüne", "SPD", "Linke", "FDP"),
  share = c(26, 25, 14, 12, 11, 3)
)

# Party main colors
party_pal <- c(
  "SPD" = "#E41E34",
  "CDU/CSU" = "#333333",
  "FDP" = "#F3D43B",
  "Grüne" = "#5CA700",
  "AfD" = "#0489DB",
  "Linke" = "#C13298")

#' Linear interpolation for margin of error based on Politbarometer methodology documentation:
#' Estimate of 40% --> +/-3pp, 10% --> +/-2pp
df_poba <- df_poba |>
  mutate(
    party = fct_inorder(party),
    margin_of_error = 1/300 * share + 5/3,
    sd_val = margin_of_error / 1.96
  )



# Generate n random points for each party
n_points <- 100
n_digits <- 3
set.seed(123)
df_simulated_points <- df_poba |>
  uncount(n_points) |>
  mutate(
    simulated_value = round(
      rnorm(n(), share, sd_val), digits = n_digits),
    # index to iterate through the animation
    frame_id = row_number(),
    .by = party
  )


p <- df_simulated_points |>
  ggplot(aes(simulated_value, fct_rev(party))) +
  geom_col(
    aes(fill = party),
    width = 0.5, alpha = 0.1
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(
    values = party_pal, aesthetics = c("color", "fill")) +
  guides(fill = "none") +
  labs(
    title = "What if there was a federal election in Germany next Sunday<br>
      *...and we included the uncertainty in the chart?*",
    subtitle = "The results of every poll come with a margin of error.
      Rather than simply presenting the estimates,
      this visualisation illustrates hypothetical outcomes by
      plotting 100 simulated results within the margin of error.
      <br><br>
      *Projected vote share (in %)*",
    caption = "***Note:** Based on the margin of error reported by the institute
    for a projected vote share of 40% (3%-p) and 10% (2%-p), the margins of error
    were interpolated linearly.*
    <br><br>
    **Source:** Politbarometer April 2026 (Forschungsgruppe Wahlen).
    <br>
    **Visualization:** Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white", ink = "grey30") + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_markdown(
      family = "Instrument Sans SemiBold", lineheight = 1.4),
    plot.subtitle = element_textbox(
      width = 0.95, size = 10, lineheight = 1.4),
    plot.caption = element_textbox(
      width = 0.95, hjust = 0, lineheight = 1.3,
      margin = margin(t = 6, b = 2)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.text.y = element_text(
      family = "Instrument Sans SemiBold", hjust = 0),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  transition_states(frame_id) +
  shadow_wake(wake_length = 1, alpha = 0.05)
animate(p, rewind = TRUE, width = 800, height = 800, units = "px", res = 150)
anim_save(here("2026", "27", "27-animation.gif"))
