library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2025", "06")

#' Download data from
#' https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Einkommen-Konsum-Lebensbedingungen/Zeitverwendung/Ergebnisse/_inhalt.html#805170

df <- read_csv2(here(base_path, "1-zeitverwendung-tagesverlauf.csv"),
                locale = locale(decimal_mark = ","))

df <- df |> 
  select(-Essen_Qualitaet) |> 
  mutate(
    Essen = as.numeric(str_replace(Essen, ",", ".")),
    Essen = replace_na(Essen, 0)) 

df_longer <- df |> 
  pivot_longer(cols = -c(Kategorie),
               names_to = "activity", values_to = "share") |> 
  mutate(
    activity = case_match(
      activity,
      "Schlafen/Körperpflege" ~ "Sleep / personal hygiene",
      "Arbeit/Bildung" ~ "Work / education",
      "Essen" ~ "Eating",
      "Freizeit" ~ "Leisure"
    ),
    activity = factor(
      activity, 
      levels = c("Sleep / personal hygiene",
                 "Work / education",
                 "Eating", "Leisure"))
  ) |> 
  rename(time = Kategorie)

df_longer |> 
  mutate(
    activity = fct_rev(activity),
    time = hour(time)) |> 
  group_by(time, activity) |> 
  summarize(share = mean(share),
            .groups = "drop") |>
  ggplot(aes(time, share, fill = activity)) +
  geom_col(
    position = "stack", alpha = 0.9, linewidth = 0.25, col = "#F8F8F8") +
  scale_x_continuous(
    breaks = seq(0, 24, 4),
    labels = as_labeller(function(x) paste0(x, ":00"))) +
  scale_color_manual(
    values = rev(c("#443536", "#EBBDBB", "#004346", "#91A5AA")),
    aesthetics = c("fill", "color")) +
  coord_radial(
    theta = "x", expand = FALSE, inner.radius = 0.02, r.axis.inside = TRUE) +
  labs(
    title = "How Hans Mustermann and Gabi Musterfrau<sup>1</sup><br>spend their day",
    subtitle = "Persons 10 years and older in Germany, activities in percent",
    caption = "<sup>1</sup> Mustermann/Musterfrau is the German equivalent to
    John/Jane Doe.<br><br>
    Source: Destatis, Time Utilisation Survey 2022.
    Visualization: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_minimal(base_family = "Faculty Glyphic") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    strip.text = element_text(family = "Roboto Condensed SemiBold", size = 10),
    plot.title = element_markdown(
      family = "Faculty Glyphic", face = "bold", hjust = 0.5, lineheight = 1.05),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      hjust = 0.5, face = "italic", margin = margin(t = 0, b = 12)),
    plot.caption = element_markdown(
      hjust = 0.5, margin = margin(t = 12, b = 4)),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "Faculty Glyphic", size = 9),
    axis.text.y = element_blank(),
    panel.spacing = unit(10, "mm"),
    legend.position = "top",
    legend.key.size = unit(4, "mm")
  )
ggsave(here(base_path, "06-nightingale-1-rose.png"), width = 5, height = 5)


df_longer |> 
  mutate(
    activity = fct_rev(activity),
    time = hour(time),
    part_of_day = ifelse(time < 12, "am", "pm"),
    part_of_day = factor(
      part_of_day, levels = c("am", "pm"), labels = c("morning", "afternoon")),
    time_am_pm = ifelse(part_of_day == "morning", time, time - 12)) |> 
  group_by(part_of_day, time_am_pm, activity) |> 
  summarize(share = mean(share),
            .groups = "drop") |>
  ggplot(aes(time_am_pm, share, fill = activity)) +
  geom_col(
    position = "stack", alpha = 0.9, linewidth = 0.25, col = "#F8F8F8") +
  scale_x_continuous(
    breaks = seq(0, 24, 3),
    labels = as_labeller(function(x) ifelse(x == "0", "12", x))) +
  scale_color_manual(
    values = rev(c("#443536", "#EBBDBB", "#004346", "#91A5AA")),
    aesthetics = c("fill", "color")) +
  coord_radial(
    theta = "x", expand = FALSE, inner.radius = 0.02, r.axis.inside = TRUE,
    clip = "off", start = -0.25) +
  facet_wrap(vars(part_of_day), scales = "free_x") +
  labs(
    title = "How Hans Mustermann and Gabi Musterfrau<sup>1</sup> spend their day",
    subtitle = "Persons 10 years and older in Germany, share of activities in a given hour",
    caption = "<sup>1</sup> Mustermann/Musterfrau is the German equivalent to
    John/Jane Doe.<br><br>
    Source: Destatis, Time Utilisation Survey 2022.
    Visualization: Ansgar Wolsing",
    fill = NULL
  ) +
  theme_minimal(base_family = "Faculty Glyphic") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    strip.text = element_text(family = "Faculty Glyphic", face = "bold", size = 10),
    plot.title = element_markdown(
      family = "Faculty Glyphic", face = "bold", hjust = 0.5, lineheight = 1.05,
      size = 11),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      hjust = 0.5, face = "italic", margin = margin(t = 0, b = 12), size = 10),
    plot.caption = element_markdown(
      hjust = 0.5, size = 8, margin = margin(t = 12, b = 4)),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "Faculty Glyphic", size = 9),
    axis.text.y = element_blank(),
    panel.spacing = unit(10, "mm"),
    legend.position = "top",
    legend.key.size = unit(4, "mm")
  )
ggsave(here(base_path, "06-nightingale-2-roses.png"), width = 5, height = 5)
