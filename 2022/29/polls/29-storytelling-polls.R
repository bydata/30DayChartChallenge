library(tidyverse)
library(lubridate)
library(ggtext)
library(here)
library(glue)
library(gganimate)

base_path <- here("2022", "29", "polls")

#' GETTING THE DATA
#' Scrape the poll results on national level from Wahlrecht.de using the R script
#' created by German weekly DIE ZEIT:
#' https://github.com/ZeitOnline/wahltrend/blob/main/get_polls.R
#' The script might need some adaptations to run on your local machine

# Load the data scraped from Wahlrecht.de
polls <- read_rds(here(base_path, "wahlrecht_umfragen.rds"))
colnames(polls) <- c("id", "institute", "date", "respondents", "party", "share")
polls <- polls %>% 
  mutate(
    party = case_when(
      party == "CDUCSU" ~ "CDU/CSU",
      party == "GRÜNE" ~ "GREENS",
      party == "LINKE" ~ "LEFTIST",
      TRUE ~party),
    party = fct_inorder(party))


# Custom theme
theme_set(
  theme_minimal(base_family = "Lato", base_size = 9) +
    theme(legend.position = "bottom", 
          legend.justification = "left", 
          legend.key.height = unit(2.5, "mm"),
          plot.background = element_rect(color = NA, fill = "grey98"),
          panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = 0.3, color = "grey85"),
          panel.grid.minor.y = element_line(size = 0.15, color = "grey85"),
          text = element_text(color = "grey40", lineheight = 1.2),
          plot.title = element_markdown(
            color = "grey2", face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_textbox_simple(
            size = 8, hjust = 0, margin = margin(t = 2, b = 9), width = 0.95),
          plot.caption = element_textbox_simple(
            width = 1, size = 6, hjust = 0, 
            margin = margin(t = 10, b = 4)),
          axis.title = element_text(color = "grey45"),
          axis.title.y = element_text(hjust = 0.5),
          axis.title.y.right = element_blank(),
          axis.text = element_text(color = "grey45"))
)

# Party colors
unique(polls$party)
party_colors <- c(
  "CDU/CSU" = "grey9",
  "SPD" = "#ca0002",
  "GREENS" = rgb(100, 161, 45, maxColorValue = 255),
  "FDP" = colorspace::darken("#ffed00", 0.1),
  "LEFTIST" = "purple",
  "AfD" = rgb(0, 158, 224, maxColorValue = 255))


# Election date
election_date <- as_date("2021-09-26")

# Annotations
annotations <- data.frame(
  label = c(glue("<b style='color:{party_colors[\"CDU/CSU\"]}'>CDU/CSU</b>
                 surged at the start of the pandemic"),
                 glue("<b style='color:{party_colors[\"SPD\"]}'>SPD</b> numbers were
            flat going into the campaign"),
                 glue("Beginning of 2021, 
            <b style='color:{party_colors[\"CDU/CSU\"]}'>CDU/CSU</b> 
            lost support, while 
            <b style='color:{party_colors[\"GREENS\"]}'>Greens</b> and 
            <b style='color:{party_colors[\"FDP\"]}'>FDP</b> gained in the polls"),
                 glue("As quickly as the <b style='color:{party_colors[\"GREENS\"]}'>Greens'</b>
            share increase,  it dropped to previous level"),
                 glue("<b style='color:{party_colors[\"SPD\"]}'>SPD</b> 
            eventually passed <b style='color:{party_colors[\"CDU/CSU\"]}'>CDU/CSU</b> 
            to become the strongest party in the federal election")),
  x = c(as_date("2020-03-01"), as_date("2020-09-01"), as_date("2021-03-01"), 
        as_date("2021-06-01"), as_date("2021-09-10")),
  y = c(38, 15, 28, 22, 33))
annotations$date_id <- seq_along(annotations$label)
annotations$label <- glue("<span style='font-size:14pt'><b>{annotations$date_id}</b> |</span> {annotations$label}")

## All parties
p <- polls %>%
  filter(party %in% c("CDU/CSU", "SPD", "GREENS", "FDP")) %>% 
  filter(date >= election_date - period("2 years") & date < election_date) %>% 
  ggplot(aes(date, share)) +
  geom_point(aes(color = party), alpha = 0.2, size = 0.5, shape = 21) +
  geom_smooth(aes(color = party, fill = stage(party, after_scale = alpha(color, 0.2))), 
              size = 0.2, se = TRUE, method = "loess", span = 0.1, level = 0.95) +
  geom_text(data = ~filter(., date == max(date)),
            aes(y = share, label = party, color = party),
            stat = "summary", fun = mean, nudge_x = 1, hjust = 0, vjust = 0.5, family = "Lato", 
            fontface = "bold", size = 2.5
            ) +
  
  # Text on top of the chart area
  geom_textbox(
    data = annotations,
    aes(x = as_date("2020-11-01"), y = 51, label = label),
    inherit.aes = FALSE, hjust = 0.5, halign = 0, width = 1.1,
    family = "Lato", size = 3, color = "grey2", fill = "grey98", 
    box.padding = unit(5, "mm"),
    box.r = unit(0, "mm"), box.size = 0, box.colour = "grey60", vjust = 1) +
  # Notes inside the chart
  geom_richtext(
    data = annotations,
    aes(x, y, label = date_id),
    inherit.aes = FALSE, hjust = 0.5, fontface = "bold",
    family = "Lato", size = 5, color = "grey99", fill = alpha("grey2", 0.6), 
    label.r = unit(1.5, "mm"), label.size = 0.1, label.colour = "grey60", vjust = 1) +
  scale_x_date(expand = expansion(add = c(10, 45)), breaks = "3 months",
               date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 40, 10), 
                     labels = function(x) ifelse(x == 40, "40%", x),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = party_colors) +
  coord_cartesian(ylim = c(0, 50), clip = "off") +
  guides(color = "none", fill = "none") +
  labs(
    title = "Polling Trends in the Run-up to the German Federal Election 2021",
    subtitle = "Many polls from different institutes were published in the run-up to the election.
    Instead of looking at each individual published polling result, it is more valuable to
    observe general trends. The trend lines are fitted using LOWESS 
    (**Lo**cally **We**ighted **S**catterplot **S**moother).
    ",
    caption = "LOWESS bandwidth = 0.2. 
    For every single 
    data point a local regression is calculated, taking into account only a certain
    range of neighboring data points. A weighting function ensures that the 
    importance of these points decreases with increasing distance.
    The ribbons around the lines indicate the 95% confidence intervals of the 
    point estimates. Individual poll results are shown as points.<br>
    Source: Wahlrecht.de. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Projected vote share"
  )
ggsave(here(base_path, "29-all-parties.png"), width = 6, height = 5)  

p_anim <- p + 
  transition_states(date_id, transition_length = 0, state_length = 1)
animate(p_anim, res = 300, width = 1800, height = 1500, duration = 20,
        start_pause = 10, end_pause = 20, bg = "grey98")
anim_save(here(base_path, "29-polls-storytelling.gif"))
