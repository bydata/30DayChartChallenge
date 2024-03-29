library(tidyverse)
library(ggtext)
library(here)
library(gtrendsR)
library(glue)
library(lubridate)
library(grid)

base_path <- here("2022", "27")

# Get US trends for search term "what will happen"
trends <- gtrends("\"what will happen\"", geo = "US", gprop = "web", time = "all")

# Replace values "<1" with 0.5
df <- trends$interest_over_time %>% 
  mutate(hits = ifelse(hits == "<1", 0.5, as.numeric(hits))) 

# which are the months with highest search interest?
highest_interest <- df %>% 
  slice_max(hits, n = 10) %>% 
  select(date, hits) 

highest_interest

# add some interesting peaks manually
highest_interest <- highest_interest %>% 
  add_row(date = as_date("2008-09-01"), hits = NA) %>% 
  add_row(date = as_date("2008-10-01"), hits = NA) %>% 
  add_row(date = as_date("2008-11-01"), hits = NA) %>% 
  add_row(date = as_date("2011-03-01"), hits = NA) %>% 
  add_row(date = as_date("2011-04-01"), hits = NA) %>% 
  add_row(date = as_date("2011-05-01"), hits = NA) %>% 
  add_row(date = as_date("2013-09-01"), hits = NA)


# retrieve the Google Trends data for the months with the highest search interest
# to find related topics and related queries
highest_interest$end_date <- highest_interest$date + period("1 months") - period("1 day")

gtrends_safely <- safely(gtrends, otherwise = NULL, quiet = FALSE) 
trends_monthly <- map2(as.character(highest_interest$date), as.character(highest_interest$end_date),
           ~gtrends_safely("\"what will happen\"", geo = "US", gprop = "web", time = paste(.x, .y)))

trends_monthly_results <- transpose(trends_monthly) %>% 
  pluck("result") %>% 
  set_names(as.character(highest_interest$date))


# Background color for plot
bg_color <- "#090626"

# Custom text annotation function
annotate_text <- function(x, y, label, hjust = 0.5, nudge_y = 2, ...) {
  annotate("richtext", x = x, y = y + nudge_y,
           label = label,  hjust = hjust, vjust = 0, 
           size = 2.5, fill = NA, label.size = 0, 
           col = "grey90", family = "Noto Sans", ...)
}

# Custom annotation function, adds a circle to the data point
annotate_point <- function(x, y, fill = bg_color) {
  annotate("point", x = x, y = y, shape = 21, fill = fill, 
             color = "white", size = 2, stroke = 1)
}


# Events to highlight in the plot
highlight_events <- list(
  x = c(as_datetime("2008-09-01"), as_datetime("2011-05-01"), as_datetime("2012-12-01"), 
        as_datetime("2016-11-01"), as_datetime("2020-03-01"), as_datetime("2020-11-01"), 
        as_datetime("2022-02-01")),
  y = c(33, 57, 81, 91, 97, 100, 77),
  label = c(
    "**Mortgage loans**", 
    "**Judgement Day**<br>on 5/21/11?",
    "**Maya Calendar 12/21/12**<br>End of the World?",
    "**Donald Trump**<br>elected president",
    "",
    "**Trump**<br>doesn't concede",
    "Russia<br>invades<br>**Ukraine**"
  )
)

# Create the plot with a line added via grid -----------------------------------
ragg::agg_png(here(base_path, "27-what-will-happen.png"), res = 400, 
              width = 7, height = 5.25, units = "in")
df %>% 
  ggplot(aes(date, hits)) +
  
  # area to highlight Corona pandemic
  annotate("rect", xmin = as_datetime("2020-01-01"), xmax = as_datetime("2022-03-31"),
           ymin = 0, ymax = 102, fill = alpha("white", 0.2)
  ) +
  
  geom_area(col = "white", fill = alpha("white", 0.1)) +
  
  annotate_text(x = highlight_events$x, y = highlight_events$y,
                label = highlight_events$label) +

  annotate_point(x = highlight_events$x, y = highlight_events$y) + 
  
  # Coronavirus Pandemic
  annotate("richtext", x = as_datetime("2020-03-15"), y = 10,
           label = "**Coronavirus<br>pandemic**", angle = 90, size = 5,
           hjust = 0, vjust = 1, fill = NA, label.size = 0, col = "grey90", 
           family = "Noto Sans"
  ) +
  
  scale_x_datetime(expand = c(0, 0)) +
  scale_y_continuous(position = "right") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Google Search Queries for **\"What will happen\"** in the U.S.",
    caption = "The plot shows normalized Google Search hits for \"What will happen\"
           in web searches from the United States 2004-2022.
           A value of 100 denotes maximum search interest.
           The topics highlighted in the plot are based on the related topics and 
           related queries returned by the Google Trends API.
           <br><br>
           **Source:** Google Trends | **Visualization:** Ansgar Wolsing",
    y = "Normalized search interest"
  ) +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    plot.margin = margin(8, 8, 8, 8),
    text = element_text(color = "grey83"),
    axis.text = element_text(color = "grey75"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_line(color = "grey75", size = 0.2),
    plot.title = element_markdown(
      color = "white", margin = margin(t = 4, b = 4)),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(
      size = 7, hjust = 0, margin = margin(t = 8))
  )

# Add line below the title
grid.lines(
  x = c(0.0125, 0.725),
  y = 0.925,
  gp = gpar(col = "white", lwd = 1)
)
invisible(dev.off())
