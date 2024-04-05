library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "05")

#' Source: United Nations Statistics Division
#' https://data.un.org/Data.aspx?d=POP&f=tableCode%3A22
df <- read_csv(here(base_path, "UNdata_Export.csv"), 
               name_repair = janitor::make_clean_names)

# Which age groups are available?
unique(df$age)

# Avg. age
df |>
  filter(area == "Total", sex %in% c("Male", "Female")) |> 
  mutate(age = ifelse(age == "100 +", "100", age)) |> 
  filter(str_detect(age, "^\\d+$")) |> 
  group_by(country_or_area) |> 
  filter(year == max(year)) |> 
  ungroup() |> 
  mutate(
    age = as.numeric(age),
    age_mult = age * value) |> 
  group_by(country_or_area) |> 
  summarize(avg_age = sum(age_mult) / sum(value)) |> 
  arrange(avg_age) 


colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.text = element_text(family = "Source Code Pro"),
      axis.line.x = element_line(linewidth = 0.33),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_markdown(
        hjust = 0, color = "grey35", # family = "Poppins",
        margin = margin(b = 8)),
      plot.caption = element_markdown(hjust = 0, lineheight = 1.1, size = 7),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 10, color = "grey35",
        margin = margin(t = 4, b = 1))
    )
)


df |>
  filter(area == "Total", sex %in% c("Male", "Female")) |> 
  mutate(age = ifelse(age == "100 +", "100", age)) |> 
  filter(str_detect(age, "^\\d+$")) |> 
  mutate(age = as.numeric(age)) |> 
  group_by(country_or_area, age) |> 
  filter(year == max(year)) |> 
  summarize(value = sum(value)) |> 
  arrange(age, .by_group = TRUE) |> 
  mutate(
    value_cumul = cumsum(value),
    share_cumul = value_cumul / sum(value)) |> 
  filter(share_cumul >= 0.5) |> 
  summarize(median_age = min(age))

df |>
  filter(area == "Total", sex %in% c("Male", "Female")) |> 
  mutate(age = ifelse(age == "100 +", "100", age),) |> 
  filter(str_detect(age, "^\\d+$")) |> 
  mutate(age = as.numeric(age)) |> 
  group_by(country_or_area) |> 
  filter(year == max(year)) |> 
  mutate(share = value / sum(value)) |> 
  ungroup() |> 
  mutate(
    value = ifelse(sex == "Female", -1, 1) * value,
    share = ifelse(sex == "Female", -1, 1) * share) |> 
  ggplot(aes(age, share, fill = sex)) +
  geom_col(
    aes(col = sex), width = 0.9) +
  geom_rect(
    data = data.frame(
      country_or_area = c("Japan", "Niger"),
      xmin = c(48.3, -0.7), xmax = c(101, 14.7),
      ymin = c(-0.011, -0.024), ymax = c(0.011, 0.0245)
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, col = "grey40", linewidth = 0.3, fill = "transparent"
  ) +
  geom_label(
    data = data.frame(
      country_or_area = c("Japan", "Niger"),
      x = c(48.5, 18), y = c(-0.021, -0.025),
      label = "50% of the\npopulation"
    ),
    aes(x, y, label = label),
    inherit.aes = FALSE, family = "Poppins", lineheight = 0.9, size = 3,
    hjust = 0, fill = "#FBFAFC", label.size = 0
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0.5, 2)),
    breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5)) +
  scale_y_continuous(labels = function(x) sprintf("%s%%", abs(x) * 100)) +
  scale_fill_manual(values = c("#6200EE", "#03DAC5"), aesthetics = c("fill", "color")) +
  coord_flip(ylim = c(-0.022, 0.022), clip = "off") +
  facet_wrap(vars(country_or_area), labeller = as_labeller(toupper)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Population pyramids for the countries with the oldest and<br>the youngest population",
    subtitle = "Share of
    <span style='color:#6200EE;font-family:\"Poppins Medium\"'>female</span> and
    <span style='color:#03DAC5;font-family:\"Poppins Medium\"'>male</span>
    population by age",
    caption = "Note: The age group 100 includes the population 100 years and older.
    <br>Source: United Nations Statistics Division.
    Visualization: Ansgar Wolsing",
    x = "Age (years)",
    y = "Share of population (%)"
  )
ggsave(here(base_path, "05-diverging.png"), width = 5, height = 5, scale = 1.3)
