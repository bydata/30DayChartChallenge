library(tidyverse)
library(lubridate)
library(ggtext)
library(here)
library(glue)

base_path <- here("2022", "25")

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
            size = 8, hjust = 0, margin = margin(t = 2, b = 8), width = 0.95),
          plot.caption = element_markdown(size = 7, hjust = 1),
          axis.title = element_text(color = "grey45"),
          axis.title.y = element_text(hjust = 1),
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




polls %>% 
  filter(party %in% c("CDU/CSU", "SPD")) %>% 
  # filter(year(date) >= 2020 & date < election_date) %>% 
  filter(date >= election_date - period("2 years") & date < election_date) %>% 
  ggplot(aes(date, share)) +
  geom_point(aes(color = party), alpha = 0.2, size = 0.5, shape = 21) +
  geom_smooth(aes(color = party, fill = stage(party, after_scale = alpha(color, 0.2))), 
              size = 0.2, se = TRUE, method = "loess", span = 0.1) +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  scale_color_manual(values = party_colors[c("CDU/CSU", "SPD")]) +
  coord_cartesian(ylim = c(0, NA)) 
ggsave(here(base_path, "25-union-spd.png"), width = 6, height = 5)  

polls %>% 
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
  annotate("richtext", 
           label = c("Each party polling result<br>is represented by a point.",
                     "The smoothed line indicates the trend<br>
                     of the polls, the ribbon the 95% confidence<br>interval."),
           x = c(as_date("2020-05-01"), as_date("2020-05-20")),
           y = c(24, 30),
           hjust = 0, family = "Lato", size = 2.5, color = "grey12", fill = NA, 
           label.size = 0
  ) +
  annotate("segment", 
           x = c(as_date("2020-05-01"), as_date("2020-06-01")), 
           xend = c(as_date("2020-03-20"), as_date("2020-06-01")),
           y = c(24, 32),
           yend = c(24, 38.5),
           color = "grey12", size = 0.2
  ) +
  annotate("point",
           x = as_date("2020-03-05"),
           y = 24,
           size = 6, fill = NA, color = "grey12", stroke = 0.3, shape = 21
           ) +
  scale_x_date(expand = expansion(add = c(10, 45)), breaks = "3 months",
               date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 50, 10), 
                     labels = function(x) ifelse(x == 40, "40%", x),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = party_colors) +
  coord_cartesian(ylim = c(0, NA), clip = "off") +
  guides(color = "none", fill = "none") +
  labs(
    title = "Polling Trends in the Run-up to the German Federal Election 2021",
    subtitle = "Many polls from different institutes were published in the run-up to the election.
    Instead of looking at each individual polling result, it is more valuable to
    observe general trends. The trend lines are fitted using LOWESS 
    (**Lo**cally **We**ighted **S**catterplot **S**moother): for every single 
    data point a local regression is calculated, taking into account only a certain
    range of neighboring data points. A weighting function ensures that the 
    importance of these points decreases with increasing distance.
    The ribbons around the lines indicate the 95% confidence intervals of the 
    point estimates. Individual poll results are shown as points.
    ",
    caption = "LOWESS bandwidth = 0.2. Source: Wahlrecht.de. Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Projected vote share"
  )
ggsave(here(base_path, "25-all-parties.png"), width = 6, height = 5)  

