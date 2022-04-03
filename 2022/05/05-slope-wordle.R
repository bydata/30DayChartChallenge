library(tidyverse)
library(ggtext)
library(here)
library(gtrendsR)
library(glue)
library(lubridate)

base_path <- here("2022", "05")

time <- glue("2021-10-01 {format(Sys.Date(), '%Y-%m-%d')}")
countries <- c("", "DE", "US")

trends <- map(countries, ~gtrends("wordle", geo = .x, gprop = "web", time = time))
trends <- set_names(trends, c("world", countries[2:3]))

df <- map_df(seq_along(trends), ~pluck(trends, .x , "interest_over_time")) %>% 
  filter(date <= as_date("2022-03-31")) %>% 
  mutate(hits = ifelse(hits == "<1", 0.5, as.numeric(hits))) 

df %>%
  filter(geo == "world") %>% 
  ggplot(aes(date, hits)) +
  geom_line(size = 1.25, color = "grey12") +
  annotate("text",
           label = "The Rising Popularity of",
           x = as_datetime("2021-10-01"),  y = 95, 
           family = "Helvetica Neue", fontface = "bold", size = 7.5, hjust = 0) +
  annotate("tile",
           x = seq(as_datetime("2021-10-08"), as_datetime("2022-01-05"), "16 days"),
           y = 84.5, width = 14.5 * 86400, height = 10,
           fill = c("#797C7E", "#79A86B", "#797C7E", "#C6B566", "#79A86B", "#797C7E")
  ) +
  annotate("richtext",
           label = c("W", "O", "R", "D", "L", "E"),
           x = seq(as_datetime("2021-10-08"), as_datetime("2022-01-05"), "16 days"),
           y = 84,
           hjust = 0.5, vjust = 0.5, size = 7, family = "Helvetica Neue",
           fontface = "bold", color = "white",
           fill = NA, label.size = 0,
           label.padding = unit(0.5, "lines")) +
  geom_textbox(aes(x =  as_datetime("2021-10-01"), y = 65),
               stat = "unique", 
           label = "The plot shows normalized **Google Web Search** hits for \"Wordle\"
           in web searches worldwide from October 2021 to March 2022. A value of 100 denotes 
           maximum search interest.",
           width = unit(0.6, "npc"),
           box.size = 0,  box.r = unit(0, "mm"), box.padding = unit(0, "mm"),
           family = "Noto Serif", size = 3, hjust = 0, color = "grey30") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%b") +
  scale_y_continuous(position = "right") +
  labs(
    caption = "**Source:** Google Trends | **Visualization:** Ansgar Wolsing",
    x = NULL, y = "Normalized search interest") +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    plot.caption = element_textbox_simple(hjust = 0, width = 1, lineheight = 1.2,
                                          margin = margin(t = 8, b = 4, l = 10)),
    text = element_text(color = "grey30"),
    axis.title.y = element_text(hjust = 0.2),
    axis.text = element_text(color = "grey50"))
ggsave(here(base_path, "05-slope-wordle.png"), width = 6, height = 5)

