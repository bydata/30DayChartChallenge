library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2025", "03")

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
  ggplot(aes(time, share, col = activity, fill = activity)) +
  geom_area(
    position = "identity", alpha = 0.3, linewidth = 1,
    show.legend = FALSE) +
  scale_x_time(labels = scales::time_format("%H:%M")) +
  scale_color_manual(values = c("#662E9B", "#A7E8BD", "#FCBCB8", "#F9C80E"),
                     aesthetics = c("fill", "color")) +
  coord_radial(
    theta = "x", expand = FALSE, inner.radius = 0.1, r.axis.inside = TRUE) +
  facet_wrap(vars(fill = activity)) +
  labs(
    title = "How Hans Mustermann and Gabi Musterfrau<sup>1</sup> spend their day",
    subtitle = "Persons 10 years and older in Germany, activities in percent",
    caption = "<sup>1</sup> Mustermann/Musterfrau is the German equivalent to
    John/Jane Doe.<br><br>
    Source: Destatis, Time Utilisation Survey 2022.
    Visualization: Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    strip.text = element_text(family = "Roboto Condensed SemiBold", size = 10),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", hjust = 0.5, lineheight = 1.05),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(t = 0, b = 12)),
    plot.caption = element_markdown(
      hjust = 0.5, margin = margin(t = 12, b = 4)),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 6),
    panel.spacing = unit(10, "mm")
  )
ggsave(here(base_path, "03-circular.png"), width = 5, height = 5)
