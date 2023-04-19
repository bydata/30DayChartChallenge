library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "19")

#' Source:
#' https://www.bpb.de/shop/zeitschriften/apuz/269298/das-anthropozaen-erzaehlen-fuenf-narrative/
#' https://en.wikipedia.org/wiki/Anthropocene#Temporal_limit
#' https://www.nature.com/articles/415023a

anthro <- list(
  list(
    year = 1610,
    reason = "European colonization of the Americas",
    source = "Maslin & Lewis"
  ),
  list(
    year = 1712,
    reason = "Industrial revolution, invention of<br>the Newcomen atmospheric engine",
    source = "Lovelock"
  ),
  list(
    year = 1748,
    reason = "Industrial revolution,<br>invention of the steam engine",
    source = "Crutzen"
  ),
  list(
    year = 1945,
    reason = "Great Acceleration, End of WW II",
    source = "Anthropocene Working Group"
  )
)

anthro_df <- bind_rows(anthro)
anthro_df

bg_color <- "grey90"

anthro_df %>% 
  ggplot(aes(year, y = 0)) +
  geom_hline(aes(yintercept = 0), linewidth = 2) +
  geom_segment(
    aes(x = year, xend = year, y = 0, 
        yend = c(0.1, 0.05, -0.1, 0.1)),
    linewidth = 0.33
  ) +
  geom_point(shape = 21, fill = "grey90", size = 4, stroke = 1) +
  geom_richtext(
    aes(y = c(0.1, 0.05, -0.1, 0.1), 
        label = sprintf("**%d**<br>%s<br>(*%s*)",
                        year, reason, source)),
    angle = 00, hjust = 0, family = "Source Sans Pro", size = 3, nudge_x = -10,
    fill = bg_color, label.size = 0
  ) +
  scale_x_continuous(limits = c(1600, 2023)) +
  scale_y_continuous(limits = c(-0.15, 0.15)) +
  coord_cartesian(clip = "off", expand = TRUE) +
  labs(
    title = "When did the ANTHROPOCENE begin?",
    subtitle = "The Anthropocene is an \"unofficial unit of geologic time, used to 
    describe the most recent period in Earth's history when human activity began 
    to have a significant impact on the planet's climate and ecosystems.\"<sup>1</sup><br><br>
    4 hypotheses as to when it began.",
    caption = "Sources: <sup>1</sup>National Geographic, Bundeszentrale für Politische Bildung, Wikipedia.
    Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    plot.margin = margin(rep(4, 4)),
    plot.title = element_text(size = 20, hjust = 0.5, family = "Source Serif Pro"),
    plot.subtitle = element_textbox(size = 12, hjust = 0.5, halign = 0.5, 
                                    lineheight = 1.15, width = 0.8),
    plot.caption = element_markdown()
  )
ggsave(here(base_path, "19-anthropocene-timeline.png"), width = 5, height = 4, scale = 1.5)


## CO2 EMISSIONS ===============================================================

library(ggstream)

emissions <- read_csv(here(base_path, "annual-co2-emissions-per-country.csv"),
                      name_repair = janitor::make_clean_names)

emissions %>% 
  filter(str_detect(entity, "income countries")) %>% 
  distinct(entity)

emissions %>% 
  filter(str_detect(entity, "income countries")) %>% 
  ggplot(aes(year, annual_co2_emissions)) +
  geom_line(aes(col = entity))

max_year <- max(emissions$year)
annual_world_emissions_max_year <- 
  emissions$annual_co2_emissions[emissions$entity == "World" & 
                                   emissions$year == max_year]
annual_world_emissions_1750 <- 
  emissions$annual_co2_emissions[emissions$entity == "World" & 
                                   emissions$year == 1750]
annual_world_emissions_1945 <- 
  emissions$annual_co2_emissions[emissions$entity == "World" & 
                                   emissions$year == 1945]



emissions %>% 
  filter(str_detect(entity, "income countries")) %>% 
  mutate(
    entity = str_remove(entity, "-income countries"),
    entity = factor(entity, levels = c("High", "Upper-middle", "Lower-middle", "Low"))
  ) %>% 
  ggplot(aes(year, annual_co2_emissions, fill = entity)) +
  # split background
  annotate(
    "rect", 
    xmin = -Inf, xmax = Inf, 
    ymin = c(-Inf, 0), ymax = c(0, Inf),
    fill = c("#d4f089", "#b7e8f7"),
    alpha = 0.5
  ) +
  geom_vline(
    data = anthro_df[c(3,4),],
    aes(xintercept = year),
    col = "grey50", size = 0.3
  ) +
  geom_richtext(
    data = anthro_df[c(3,4),],
    aes(
      # y = c(-1.35e10, -0.8e10, 6e9, 1.3e10), 
      x = year + ifelse(year == 1748, 2, -2),
      y = c(6e9, 1.3e10), 
      label = sprintf("**%d**<br>%s<br>*(Source: %s)*",
                        year, reason, source),
        hjust = c(0, 1)),
    angle = 0, family = "Source Sans Pro", size = 3, fill = NA, label.size = 0
  ) +
  geom_stream(bw = 0.5, extra_span = 0.001, col = "white", linewidth = 0.0) +
  # scale on the right-hand side
  annotate(
    "segment",
    x = max_year + 3, xend = max_year + 3,
    y = -annual_world_emissions_max_year / 2 , yend = annual_world_emissions_max_year / 2,
    col = "grey30", arrow = arrow(length = unit(1, "mm"), ends = "both", angle = 90),
    size = 0.3
  ) + 
  # scale steps
  annotate(
    "segment",
    x = max_year + 2.25, xend = max_year + 3.75,
    y = seq(-annual_world_emissions_max_year / 2, annual_world_emissions_max_year / 2, 2e9),
    yend = seq(-annual_world_emissions_max_year / 2, annual_world_emissions_max_year / 2, 2e9),
    col = "grey30",
    size = 0.2
  ) + 
  # scale label
  annotate(
    "richtext",
    x = max_year + 8, y = 0,
    label = scales::number(annual_world_emissions_max_year, scale = 1e-9, 
                           suffix = " billion tonnes CO<sub>2</sub>"),
    angle = 90, family = "Source Sans Pro SemiBold", size = 3, color = "grey40",
    fill = NA, label.size = 0, label.r = unit(0, "mm")
  ) +
  # annotation for 1750 emissions
  annotate(
    "richtext",
    x = 1757, y = -2.6e9,
    label = scales::number(annual_world_emissions_1750, scale = 1e-9, 
                           accuracy = 0.001,
                           suffix = " billion tonnes CO<sub>2</sub>"),
    angle = 0, family = "Source Sans Pro SemiBold", size = 3, color = "grey40",
    fill = NA, label.size = 0, label.r = unit(0, "mm"), hjust = 0
  ) +
  annotate(
    "curve",
    x = 1760, xend = 1750,
    y = -2e9, yend = -0.1e9,
    curvature = 0.2, linewidth = 0.2, color = "grey30",
    arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed")
  ) +
  # annotation for 1945 emissions
  annotate(
    "richtext",
    x = 1945, y = -5e9,
    label = scales::number(annual_world_emissions_1945, scale = 1e-9, 
                           suffix = " billion tonnes CO<sub>2</sub>"),
    angle = 0, family = "Source Sans Pro SemiBold", size = 3, color = "grey40",
    fill = NA, label.size = 0, label.r = unit(0, "mm"), hjust = 1
  ) +
  annotate(
    "curve",
    x = 1938, xend = 1945,
    y = -4.3e9, yend = -2.7e9,
    curvature = 0.2, linewidth = 0.2, color = "grey30",
    arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed")
  ) +
  scale_x_continuous(breaks = seq(1750, 2050, 50), expand = c(0, 4)) +
  scale_fill_grey(start = 0.8, end = 0.0) +
  guides(fill = guide_legend(title.position = "top")) +
  labs(
    title = "CO<sub>2</sub> emissions since the Industrial Revolution",
    subtitle = "Annual CO<sub>2</sub> emissions by country income groups",
    caption = "Source: Our World in Data. Visualisation: Ansgar Wolsing",
    fill = "Countries grouped by income level"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    legend.position = c(0.075, 0.20),
    legend.direction = "horizontal",
    # legend.background = element_rect(color = "white", fill = "white"),
    legend.justification = "left",
    axis.text.x = element_text(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(1, "mm"),
    axis.line.x = element_line(),
    plot.margin = margin(rep(6, 4)),
    plot.title = element_markdown(family = "Source Serif Pro SemiBold", size = 18),
    plot.subtitle = element_markdown(margin = margin(t = 4, b = 4))
  )
ggsave(here(base_path, "19-anthropocene-co2-emissions.png"), width = 8, height = 6.4)
