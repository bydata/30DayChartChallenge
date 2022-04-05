library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2022", "06")

#' Download working hours dataset from OWID:
#' https://ourworldindata.org/working-hours
df <- read_csv(here(base_path, "annual-working-hours-per-worker.csv"))
df <- rename(df, Hours = 4)

min(df$Year)
max(df$Year)

df %>% 
  filter(Year == min(Year))


df %>% 
  filter(Year == 1880) %>% 
  arrange(-Hours) %>% 
  head(20)

df %>% 
  filter(Year == 1900) %>% 
  arrange(-Hours) %>% 
  head(20)


df %>% 
  filter(Year == 1960) %>% 
  arrange(-Hours) 

df %>% 
  filter(Year == 1970) %>% 
  arrange(-Hours)


df %>% 
  filter(Year == 1970 | Year == max(Year)) %>% 
  pivot_wider(id_cols = c(Entity, Code), names_from = "Year", values_from = "Hours",
              names_prefix = "Hours_") %>% 
  na.omit() %>% 
  mutate(diff = Hours_2017 - Hours_1970) %>% 
  arrange(desc(diff))



# countries to label in the plot
highlight_countries <- c("Vietnam", "Norway", "Germany", "China", 
"Bangladesh", "Ireland", "South Korea", "Singapore", "Myanmar", "Greece")

df %>% 
  filter(Year == 1970 | Year == max(Year)) %>% 
  # keep countries with data for both years
  group_by(Entity) %>% 
  filter(n() == 2) %>% 
  mutate(max_value_year = which.max(Hours),
         trend = ifelse(max_value_year == 1, "decrease", "increase")) %>% 
  ungroup() %>% 
  mutate(highlight = case_when(
    !(Entity %in% highlight_countries) ~  "other",
    Entity %in% c("Myanmar", "Singapore") ~ "same",
    TRUE ~ trend)) %>% 
  ggplot(aes(factor(Year), Hours, group = Entity, color = highlight)) +
  geom_line(aes(size = ifelse(highlight == "other", 0.1, 0.7))) +
  # use 2 geoms to make sure highlighted countries' dots are placed on top
  geom_point(data = . %>% filter(highlight == "other"), size = 0.2) +
  geom_point(data = . %>% filter(highlight != "other")) +
  ggrepel::geom_text_repel(
    data = . %>% filter(highlight != "other"),
    aes(x = ifelse(Year == min(Year), 1 - 0.25, 2 + 0.25),
        label = glue::glue("{Entity} ({scales::number(Hours, accuracy = 1)})"),
        hjust = ifelse(Year == min(Year), 1, 0)), 
    size = 2.5, nudge_x = 0, direction = "y", family = "Fira Sans",
    segment.size = 0) +
  scale_x_discrete(position = "top") +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  scale_color_manual(
    values = c("other" = "grey60", "decrease" = "#092044", "increase" = "#C33C2E", 
               "same" = colorspace::darken("#F0C94C", 0.2))) +
  guides(col = "none") +
  labs(title = "Average Annual Working Hours 1970 and 2017",
       subtitle = "Working hours per worker have <b style='color:#092044'>decreased</b> 
       in most countries, which can be interpreted as a sign of progress. 
       Notable exceptions where working hours <b style='color:#C33C2E'>increased</b>, are
       Bangladesh and China. There are still significant inequalities in working times
       between countries.",
       caption = "**Source:** Huberman & Minns (2007); PWT 9.1 (2019), Our World in Data |
       **Visualization:** Ansgar Wolsing") +
  theme_minimal(base_family = "Fira Sans", base_size = 8) + 
  theme (
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "#ECEEF2", size = 5),
    text = element_text(color = "#555555"),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", color = "grey38"),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 6, l = 16, r = 16, b = 4),
    plot.title = element_text(family = "Playfair Display", size = 14, color = "grey12"),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 6, b = 12)
    ),
    plot.caption = element_markdown(
      hjust = 0, margin = margin(t = 8))
  )
ggsave(here(base_path, "06-owid-slope.png"), width = 5, height = 5)
