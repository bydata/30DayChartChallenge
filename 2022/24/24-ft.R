library(tidyverse)
library(lubridate)
library(ggtext)
library(here)
library(grid)

base_path <- here("2022", "24")


#' John Burn-Murdoch's chart comparing Covid-19 confirmed cases and deaths in 
#' Hong Kong and New Zealand
#' https://twitter.com/jburnmurdoch/status/1503420660869214213/photo/1

# Download OWID data
owid_new_deaths_per_million_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths_per_million.csv"
covid_deaths <- read_csv(owid_new_deaths_per_million_url)
owid_new_cases_per_million_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases_per_million.csv"
covid_cases <- read_csv(owid_new_cases_per_million_url)

# reporting period in plot
start_date <- as_date("2022-02-01")
# end_date <- as_date("2022-03-15")
end_date <- today()
lag_cases_deaths <- duration("14 days")

# prepare dataframes
prep_long_data <- function(df, 
                      start = start_date, end = end_date, 
                      metric = c("cases", "deaths")) {
  df %>% 
    pivot_longer(cols = -date, names_to = "region", values_to = "new_per_million") %>% 
    filter(region %in% c("New Zealand", "Hong Kong")) %>% 
    mutate(new_per_million = replace_na(new_per_million, 0),
           new_per_100k = new_per_million / 10) %>% 
    group_by(region) %>% 
    mutate(new_per_100k_7drollmean = zoo::rollmean(new_per_100k, 7, fill = NA, align = "right"),
           new_per_million_7drollmean = zoo::rollmean(new_per_million, 7, fill = NA, align = "right"),
           metric = metric) %>% 
    ungroup() %>% 
    arrange(region, date) 
}

covid_df_long <- prep_long_data(covid_cases, metric = "cases") %>% 
  bind_rows(prep_long_data(covid_deaths, metric = "deaths")) %>% 
  # shift cases by x days to account for lag between infections and deaths
  mutate(date2 = if_else(metric == "cases", date + lag_cases_deaths, date)) %>% 
  # filter(date2 >= start_date & date2 <= today() - duration("1 day"))
  filter(date2 >= start_date & date2 <= as_date("2022-03-11"))

color_text_cases <- "#488BC4"
color_text_deaths <- "#BE4045"

plot_titles <- list(
  title = "Cases are translating into deaths at much higher rates in
  Hong Kong than in New Zealand, where elderly vaccination rates are much higher  ",
  subtitle = glue::glue("Daily
  <span style='color:{color_text_cases}; font-family: \"Outfit SemiBold\"'>cases</span>
  per 100,000 people, and daily
  <span style='color:{color_text_deaths}; font-family: \"Outfit SemiBold\"'>deaths</span>
  per 2 million"),
  caption = "Cases shifted forward (14 days) to account for lag between infection and death.<br>
  Original plot by John Burn-Murdoch (Financial Times).
  Data: Johns Hopkins University, Our World in Data")

country_annotations <- data.frame(
  region = c("Hong Kong", "New Zealand"),
  label = c(
    "<b style='color: black; font-size: 14pt'>Hong Kong</b><br>
    66% of over-80s unvaccinated<br>when Omicron took off<br>
    <span style='color: #BE4045; font-family: \"Outfit SemiBold\"'>Case fatality<br>rate: 4.7%</span>",
    "<b style='color: black; font-size: 14pt'>New Zealand</b><br>
    2% unvaccinated<br>
    <span style='color: #BE4045; font-family: \"Outfit SemiBold\"'>CFR: 0.1%</span>"
    )
)


ragg::agg_png(
  here(base_path, "24-ft.png"), res = 300, width = 2800, height = 1616, units = "px"
)
covid_df_long %>% 
  ggplot(aes(date2)) +
  geom_area(data = . %>% filter(metric == "cases"),
            aes(y = new_per_100k_7drollmean),
            fill = "#85C6E1") +
  geom_area(data = . %>% filter(metric == "deaths"),
            aes(y = -2 * new_per_million_7drollmean),
            fill = "#BE4045") +
  # country annotation
  geom_richtext(data = country_annotations,
            aes(x = as_date("2022-02-01"), y = 107.5, label = label),
            size = 4, label.size = 0, fill = NA, family = "Outfit",
            color = "#68625D", hjust = 0, vjust = 1, lineheight = 1
            ) +
  scale_x_date(breaks = as_date(c("2022-02-01", "2022-03-01")), 
               date_labels = "%b") +
  scale_y_continuous(
    breaks = seq(-200, 200, 20),
    labels = function(x) {
    value <- ifelse(x > 0, x, -x) 
    color <- ifelse(x > 0, color_text_cases, color_text_deaths)
    glue::glue("<span style='color: {color}'>{value}</span>")
    }) +
  coord_cartesian(ylim = c(NA, 100), clip = "off") +
  facet_wrap(vars(region)) +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  theme_minimal(base_family = "Outfit Medium", base_size = 16) + 
  theme(
    plot.background = element_rect(color = NA, fill = "#FEF1E7"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#E3DACE", size = 0.3),
    text = element_text(color = "#68625D", lineheight = 1.3),
    plot.title = element_textbox(color = "black", family = "Outfit Medium", 
                                 face = "plain", size = 18, width = 1),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "Outfit"),
    plot.caption = element_markdown( family = "Outfit Medium",
      hjust = 0, size = 10, color = "#5E5751"),
    axis.title = element_blank(),
    axis.text.x = element_text(hjust = 0, color = "#68625D"),
    axis.text.y.left = element_markdown(family = "Outfit Medium"),
    axis.ticks.x = element_line(size = 0.3),
    axis.ticks.length.x = unit(1.8, "mm"),                     # set length of axis ticks
    plot.margin = margin(t = 12, b = 2, l = 1, r = 1),
    strip.text = element_blank()                               # remove default facet titles
  )

 # Add black thin rectangle in the top left corner
grid.rect(
  x = 0, y = 1, width = 0.165, height = 0.018,
  gp = gpar(fill = "black")
)
invisible(dev.off())

