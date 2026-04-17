library(tidyverse)
library(ggtext)
library(here)

#' Source: DLRG, Forsa 2022
#' https://www.dlrg.de/informieren/die-dlrg/presse/schwimmfaehigkeit/
#' Parents assessment of children's aged 6-10 swimming capabilities 

df <- tribble(
  ~hh_net_income,         ~skill_level,       ~share,
  "Less than 2,500 EUR", "Confident swimmer", 0.34,
  "Less than 2,500 EUR", "Inexperienced swimmer", 0.17,
  "Less than 2,500 EUR", "Non-swimmer", 0.49,
  "2,500-4,000 EUR", "Confident swimmer", 0.50,
  "2,500-4,000 EUR", "Inexperienced swimmer", 0.22,
  "2,500-4,000 EUR", "Non-swimmer", 0.27,
  "More than 4,000 EUR", "Confident swimmer", 0.62,
  "More than 4,000 EUR", "Inexperienced swimmer", 0.25,
  "More than 4,000 EUR", "Non-swimmer", 0.12 
)

df <- df |>
  mutate(
    hh_net_income = fct_inorder(hh_net_income),
    skill_level = fct_inorder(skill_level)
  )


# Tiles
df |> 
  ggplot(aes(hh_net_income, fct_rev(skill_level))) +
  geom_tile(
    aes(fill = share),
    height = 0.9, width = 0.9,
    col = "white", linewidth = 0.25
  ) +
  geom_label(
    aes(label = scales::percent(share)),
    fill = "#FFFFFF88", label.size = 0,
    family = "Roboto Mono SemiBold"
  ) +
  scale_x_discrete(position = "top") +
  coord_equal() +
  theme_minimal()


# Custom axis labels
skill_level_labels = c(
  levels(df$skill_level)[1:2],
  "<b style='color:#1D2D44;'>Non-swimmer</b>"
)
names(skill_level_labels) <- levels(df$skill_level)

df |> 
  ggplot(aes(skill_level, share)) +
  geom_hline(yintercept = 0, linewidth = 0.3, col = "grey30") +
  geom_col(
    aes(fill = skill_level == "Non-swimmer"),
    width = 0.55
  ) +
  geom_text(
    aes(
      label = scales::percent(share),
      hjust = ifelse(share > 0.20, 1.2, -0.2),
      col = ifelse(skill_level == "Non-swimmer" & share > 0.20, "white", "grey2")
    ),
    family = "Roboto Mono SemiBold"
  ) +
  scale_x_discrete(
    labels = skill_level_labels, expand = expansion(mult = c(0.2, 0.2))) +
  scale_color_identity() +
  scale_fill_manual(values = c("TRUE" = "#1D2D44", "FALSE" = "grey70")) +
  coord_flip() +
  facet_wrap(vars(hh_net_income), nrow = 1) +
  guides(fill = "none") +
  labs(
    title = "In Germany, the swimming ability of children is
    correlated with their parents' income",
    subtitle = "Half of the children aged 6 to 10 living in lower-income households
      <b style='color:#1D2D44'>cannot swim</b> (49%). In higher-income households, it is
      only about one in eight (12%).",
    caption = "***Notes:** Household net income (in EUR).
    Due to rounding, shares within two of the groups do not add up to 100 %.*
    <br><br>
      **Source:** DLRG/Forsa (2022), Telephone interviews with parents or carers
    of children aged between 6 and 10 in Germany; assessing their children's swimming 
    skills (N=500).
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y.left = element_markdown(),
    panel.grid = element_blank(),
    plot.title = element_markdown(family = "Instrument Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.4, margin = margin(t = 3, b = 15)),
    plot.caption = element_textbox(
      width = 1, lineheight = 1.2, margin = margin(t = 12)),
    plot.caption.position = "plot",
    strip.text = element_text(
      family = "Instrument Sans Semibold", color = "grey40", hjust = 0,
      margin = margin(t = 4, b = 4, l = 6)
    )
  )
ggsave(here("2026", "15", "15-correlation.png"), width = 6, height = 3.5, scale = 1.25)
