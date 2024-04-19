library(tidyverse)
library(here)

base_path <- here("2024", "19")

period_timeframes <- tribble(
  ~period, ~min_ma, ~max_ma,
  "Cretaceous", 66.0, 145.0,
  "Jurassic", 145.1, 201.4,
  "Trias", 201.5, 252.5
)

period_timeframes |> 
  ggplot() +
  # Periods
  geom_rect(
    aes(
      xmin = min_ma - 0.1, xmax = max_ma,
      ymin = 0, ymax = 2,
      fill = period
    ), 
    show.legend = FALSE
  ) +
  # T-Rex
  annotate(
    "rect",
      xmin = 72.1, xmax = 66, ymin = 0, ymax = 2,
    fill = alpha("grey20", 0.3)
  ) +
  scale_x_reverse() +
  scale_fill_manual(values = c("Jurassic" = "#6002ee", "Cretaceous" = "#90ee02", 
                               "Trias" = "grey80")) +
  coord_cartesian(expand = FALSE) +
  theme_void()
ggsave(here(base_path, "19-dinosaurs-period-tiles.png"), width = 5, height = 3)
