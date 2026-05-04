library(tidyverse)
library(ggtext)
library(ggdist)
library(here)

#' Source:
#' Mariya Stavnichuk & Tatsuya Corlett (2020)
#' https://data.mendeley.com/datasets/86tsnnbv2w/1
#' https://github.com/rfordatascience/tidytuesday/tree/2e9bd5a67e09b14d01f616b00f7f7e0931515d24/data/2020/2020-07-14


url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/2e9bd5a67e09b14d01f616b00f7f7e0931515d24/data/2020/2020-07-14/astronauts.csv"
df <- read_csv(url)

df_prep <- df |> 
  # Only keep the first mission
  filter(mission_number == 1) |> 
  select(
    id, name, year_of_selection, year_of_mission, year_of_birth
  ) |> 
  mutate(
    years_from_selection_to_mission = year_of_mission - year_of_selection,
    age_first_mission = year_of_mission - year_of_birth,
    decade_of_mission = year_of_mission %/% 10 * 10
  ) 


# Plausibility check
df_prep |> 
  filter(years_from_selection_to_mission < 0)

#' https://en.wikipedia.org/wiki/Franco_Malerba: selection in 1989, not 1998
#' https://en.wikipedia.org/wiki/Andy_Thomas (Andrew Sydney Withiel Thomas): mission in 1996, not 1983 

# Replace incorrect values
df_prep <- df_prep |> 
  mutate(
    year_of_selection = ifelse(id == 648, 1989, year_of_selection),
    year_of_mission = ifelse(id == 862, 1996, year_of_mission),
    # Recalculate decade of mission
    years_from_selection_to_mission = year_of_mission - year_of_selection,
    decade_of_mission = year_of_mission %/% 10 * 10
  )

df_prep |> 
  summarize(mean(years_from_selection_to_mission), .by = decade_of_mission) |>
  arrange(decade_of_mission)

df_prep |> 
  summarize(mean(age_first_mission), .by = decade_of_mission) |>
  arrange(decade_of_mission)

df_prep |> 
  ggplot(aes(years_from_selection_to_mission, factor(decade_of_mission))) +
  geom_jitter()

df_prep |> 
  ggplot(aes(years_from_selection_to_mission, factor(decade_of_mission))) +
  geom_jitter()



df_prep |> 
  ggplot(aes(factor(decade_of_mission), years_from_selection_to_mission)) +
  stat_halfeye()


 df |> 
   mutate(
    age = year_of_mission - year_of_birth,
    decade_of_mission = year_of_mission %/% 10 * 10
  ) |> 
  summarize(mean(age), .by = decade_of_mission) |>
  arrange(decade_of_mission)


df_plot <- df |> 
  mutate(
    age = year_of_mission - year_of_birth,
    decade_of_mission = year_of_mission %/% 10 * 10,
    decade_of_mission = paste0(decade_of_mission, "s")
  )

bg_color <- "grey4"
p <- df_plot |> 
  ggplot(aes(decade_of_mission, age)) +
  geom_point(
    position = position_jitter(width = 0.3, height = 0.2, seed = 10),
    shape = 21, col = "white", alpha = 0.3) +
  ggfx::with_outer_glow(
    stat_summary(
      fun = median, col = "yellow"
    ),
    col = "#FFFF00CC", sigma = 15, expand = 3
  ) +
  stat_summary(
    geom = "label",
    fun.data = function(x) {
      y_offset <- 3
      med <- median(x)
      data.frame(y = med + y_offset, label = paste(med, "yrs"))
    },
    col = "#FFFF00CC", fill = alpha(bg_color, 0.6),
    family = "Instrument Sans Bold", size = 4, linewidth = 0
  ) +
  # John Glenn 1998
  annotate(
    GeomTextBox,
    x = 4.2, y = 74,
    label = "<b>John Glenn</b> was 77 years old when he returned to space in 1998",
    width = 0.25, col = "grey72", family = "Instrument Sans",
    size = 2.5, lineheight = 0.85, fill = NA, box.size = 0,
    vjust = 1
  ) +
  annotate(
    GeomCurve,
    x = 4.2, xend = 4.18, y = 73.8, yend = 76.5,
    linewidth = 0.2, col = "grey72",
    arrow = arrow(angle = 25, length = unit(0.1, "cm"), type = "closed")
  ) +
  # Custom axis labels
  annotate(
    "text",
    x = c(min(df_plot$decade_of_mission), max(df_plot$decade_of_mission)),
    y = c(max(df_plot$age), min(df_plot$age) - 5),
    label = c(
      "Astronaut age\n in year of mission",
      "Decade\nof mission"),
    vjust = c(-0.5, 0.5),
    family = "Instrument Sans SemiBold", size = 2.5, col = "grey80",
    hjust = 0.5, lineheight = 0.9
  ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The <span style='color: #FFFF00CC; font-family:\"Instrument Sans Bold\"'>median age</span>
      of astronauts has increased by 11 years between the 1960s and 2010s",
    subtitle = "The yellow dots show the median age of astronauts in the year of their mission per decade.
    Each of the smaller dots represent one astronaut's age.",
    caption = "**Source:** Mariya Stavnichuk & Tatsuya Corlett (2020). 
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(paper = bg_color, ink = "grey80", base_family = "Instrument Sans") +
  theme(
    plot.title = element_textbox(
      width = 1, lineheight = 1.25,
      family = "Instrument Sans SemiBold", size = 16, color = "white"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.25,
      margin = margin(b = 24)),
    plot.caption = element_markdown(
      hjust = 0, margin = margin(t = 12)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 10, r = 6, b = 6, l = 6),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length = unit(0.1, "cm")
  )
ggsave(here("2026", "25", "25-space.png"), width = 6, height = 6)
