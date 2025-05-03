library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2025", "11")

# https://www-genesis.destatis.de/datenbank/online/url/2164a474
# Code: 61111-0002


df_raw <- read_csv2(here(base_path, "61111-0002_de.csv"), 
                skip = 6, na = c(".", "..."),
                col_names = c("year", "month", "index_2020", "X1",
                              "change_previous_year", "X2", 
                              "change_previous_month", "X3"))

df <- df_raw |> 
  select(-starts_with("X")) |> 
  filter(!is.na(month), month != "") |> 
  mutate(
    year = as.integer(year),
    change_previous_year = ifelse(change_previous_year == "-", 0, 
                                  change_previous_year)) |> 
  fill(year, .direction = "down") |> 
  mutate(month = row_number(), .by = year) |> 
  mutate(
    across(c(index_2020, change_previous_year, change_previous_month),
                function(x) as.numeric(str_replace(x, ",", "."))),
    date = ymd(paste0(year, str_pad(month, 2, side = "left", pad = "0"), "01"))
    )

df |> 
  filter(!is.na(change_previous_year)) |> 
  ggplot(aes(date, 1)) +
  geom_tile(aes(fill = change_previous_year), width = 31) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red") +
  coord_cartesian(expand = FALSE) + 
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    # axis.text = element_text(color = "grey40", size = 8),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", size = 18),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "none"
  )
ggsave(here(base_path, "11-stripes-art.png"), width = 4, height = 3)


df |> 
  filter(!is.na(change_previous_year)) |> 
  mutate(change_previous_year = change_previous_year / 100) |> 
  ggplot(aes(date, change_previous_year)) +
  geom_line(col = "#900E3D") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_percent()) +
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red") +
  coord_cartesian(expand = FALSE) + 
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.1),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", size = 18),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "none",
    plot.margin = margin(t = 4, r = 20, b = 4, l = 8)
  )

df |> 
  filter(!is.na(change_previous_year)) |> 
  ggplot(aes(date, index_2020)) +
  geom_hline(yintercept = 100, linewidth = 0.3, linetype = "dashed") +
  geom_line(col = "#900E3D") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  colorspace::scale_fill_continuous_diverging(palette = "Blue-Red") +
  coord_cartesian(expand = FALSE) + 
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 8),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.1),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", size = 18),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "none",
    plot.margin = margin(t = 4, r = 20, b = 4, l = 8)
  )
