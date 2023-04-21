library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

base_path <- here("2023", "21")

#' Source: Destatis
#' 61111-0004
#' Download "Butter" (code CC13-01151), 2019-2023

df <- read_csv2(here(base_path, "61111-0004_$F_flat.csv"), na = "...",
                locale = locale(encoding = "ISO-8859-15"))

df_prep <- df %>% 
  select(year = Zeit, month = `2_Auspraegung_Code`,
           code = `3_Auspraegung_Code`, label = `3_Auspraegung_Label`,
         value = `PREIS1__Verbraucherpreisindex__2020=100`) %>% 
  filter(!is.na(value)) %>% 
  mutate(year_month = ymd(paste(year, str_sub(month, start = 6, end = 7), "01")),
         value = str_replace(value, ",", ".") %>% as.numeric(),
         value_rel_to_prev = value / lag(value, 1),
         value_rel_to_prev = replace_na(value_rel_to_prev, 1),
         change_to_prev_month = value_rel_to_prev - 1,
         value_rel_to_first = value / .$value[1] - 1)


bg_color <- "grey98"

df_prep %>% 
  ggplot(aes(year_month, change_to_prev_month)) +
  geom_area(
    aes(y = value_rel_to_first),
    col = "grey8", linewidth = 0.8, fill = alpha("grey50", 0.1)) +
  geom_point(
    data = ~subset(., year_month %in% c(min(year_month), max(year_month))),
    aes(y = value_rel_to_first)
  ) +
  geom_col(fill = "#FDC400") +
  geom_line(
    aes(y = value_rel_to_first),
    col = "grey8", linewidth = 0.8) +
  annotate(
    GeomTextBox,
    x = c(as_date("2020-05-01"), as_date("2022-03-01")),
    y = c(0.1, 0.25),
    label = c("Change in prices<br>compared to previous month",
              "Prices relative<br>to Jan. 2019"),
    fill = bg_color, box.size = 0, family = "Schibsted Grotesk",
    size = 2, hjust = 1, width = 0.18, col = "grey8"
  ) +
  annotate(
    "curve",
    x = c(as_date("2019-10-15"), as_date("2022-01-01")), 
    xend = c(as_date("2019-10-15"), as_date("2022-04-01")),
    y = c(0.07, 0.25), 
    yend = c(0.033, 0.24), 
    curvature = -0.2, linewidth = 0.2,
    arrow = arrow(angle = 20, length = unit(1.2, "mm"))
  ) +
  scale_x_date(position = "top") +
  scale_y_continuous(labels = scales::label_percent(), breaks = seq(-0.5, 0.5, 0.1)) +
  labs(
    title = toupper("The Price of Butter"),
    subtitle = "Between January 2019 and March 2023, the price of butter in 
    Germany has been a on rollercoaster ride. After significant ups and downs, it is
    now just about 1.9 % higher than it was 4 years ago.",
    caption = "Data: Destatis. Visualisation: Ansgar Wolsing",
    x = NULL,
    y = "Change (in %)"
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 9) +
  theme(
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(family = "Source Sans Pro SemiBold", size = 12),
    plot.subtitle = element_textbox(
      width = 0.95, lineheight = 1.1, margin = margin(t = 0,  b = 16)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, size = 6),
    axis.text = element_text(),
    axis.ticks.x.top = element_line(linewidth = 0.2)
  )
ggsave(here(base_path, "21-up-down-butter.png"), width = 5, height = 4)
