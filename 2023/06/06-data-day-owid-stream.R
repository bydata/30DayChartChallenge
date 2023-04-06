library(tidyverse)
library(ggtext)
library(here)
library(ggstream)
library(shadowtext)

base_path <- here("2023", "06")

#' Source: OWID
#' https://ourworldindata.org/grapher/political-regime
#' Download full data
political_regime <- read_csv(here(base_path, "people-living-in-democracies-autocracies.csv"))

# Prepare data
political_regime_prep <- political_regime %>% 
  select(entity = Entity, year = Year, pop_missreg_row_owid:pop_libdem_row_owid) %>% 
  pivot_longer(cols = -c(entity, year), values_to = "pop") %>% 
  mutate(regime_type = case_match(
    name,
    "pop_closedaut_row_owid" ~ "Closed autocracy",
    "pop_electaut_row_owid"  ~ "Electoral autocracy",
    "pop_electdem_row_owid"  ~ "Electoral democracy",
    "pop_libdem_row_owid"    ~ "Liberal democracy",
    "pop_missreg_row_owid"   ~ "No data"
  )) %>% 
  select(-name)


# Colours from the OWID map
regime_pal <- c("Closed autocracy" = "#D7191D", "Electoral autocracy" = "#FDAE61",
                "Electoral democracy" = "#AAD9E9", "Liberal democracy" = "#2D7BB6", 
                "No data" = "grey70")

max_year <- max(political_regime_prep$year)
scale_people <- sum(political_regime_prep$pop[political_regime_prep$entity == "World" & political_regime_prep$year == max_year])

regime_definitions <- "**Political systems in the 'Regimes of the World' classification**<br><br>
  \U2022 In **closed autocracies**, citizens do not have the right to choose either 
  the chief executive of the government or the legislature through multi-party elections.<br>
  \U2022 In **electoral autocracies**, citizens have the right to choose the chief 
  executive and the legislature through multi-party elections; but they lack some freedoms,
  such as the freedoms of association or expression, that make the elections meaningful, 
  free, and fair.<br>
  \U2022 In **electoral democracies**, citizens have the right to participate in 
  meaningful, free and fair, and multi-party elections.<br>
  \U2022 In **liberal democracies**, citizens have further individual and minority 
  rights, are equal before the law, and the actions of the executive are constrained 
  by the legislative and the courts.
  *(Source: OWID)*
  "

political_regime_prep %>% 
  filter(entity == "World") %>% 
  ggplot(aes(year, pop, fill = regime_type)) +
  geom_stream(bw = 0.42, extra_span = 0.0025, n_grid = 5000) +
  annotate(
    GeomShadowText,
    x = c(rep(2020, 4), 1850),
    y = c(3e9, 0.7e9, -1.7e9, -3e9, -0.55e9),
    label = names(regime_pal),
    color = regime_pal,
    hjust = 1, family = "Roboto Condensed",
    bg.colour = c("white", "grey20", "grey20", "white", "white")
  ) +
  # scale on the right-hand side
  annotate(
    "segment",
    x = max_year + 3, xend = max_year + 3,
    y = -scale_people / 2 , yend = scale_people / 2,
    col = "grey40", arrow = arrow(length = unit(1, "mm"), ends = "both", angle = 90)
  ) + 
  # scale steps
  annotate(
    "segment",
    x = max_year + 2.25, xend = max_year + 3.75,
    y = seq(-scale_people / 2, scale_people / 2, 2e9),
    yend = seq(-scale_people / 2, scale_people / 2, 2e9),
    col = "grey40"
  ) + 
  # scale label
  annotate(
    "text",
    x = max_year + 6, y = 0,
    label = scales::number(scale_people, scale = 1e-9, suffix = " billion people"),
    angle = 90, family = "Roboto Condensed", size = 3, color = "grey40"
  ) +
  # textbox for definitions
  annotate(
    GeomTextBox,
    x = 1803, y = -4e9,
    label = regime_definitions,
    family = "Roboto Condensed", width = 0.4, color = "grey50", size = 2.25,
    hjust = 0, vjust = 0
  ) +
  scale_x_continuous(position = "top") +
  scale_y_continuous(position = "right") +
  scale_fill_manual(values = regime_pal) +
  guides(fill = "none") +
  labs(
    title = sprintf(
    "Number of people living in  <span style='color:%s'>autocracies</span> and 
    <span style='color:%s'>democracies</span>", 
    regime_pal["Closed autocracy"], regime_pal["Liberal democracy"]),
    subtitle = "Classification of political regimes based on the criteria by Lührmann et al. (2018) 
    and the assessment by V-Dem’s experts.",
    caption = "Source: OWID based on Lührmann et al. (2018); V-Dem (v13). 
    Visualisation: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = "grey24", lineheight = 1),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x.top = element_text(size = 7),
    # axis.text.y.right = element_text(size = 7),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey72"),
    panel.grid.minor.x = element_line(linewidth = 0.05, color = "grey72"),
    plot.margin = margin(t = 2, b = 8, l = 4, r = 4),
    plot.title = element_markdown(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 0.95, hjust = 0.5, halign = 0.5, margin = margin(t = 6, b = 14)),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 4))
  )
ggsave(here(base_path, "06-owid-political-regimes-stream.png"), width = 8, height = 6.4, scale = 1)
