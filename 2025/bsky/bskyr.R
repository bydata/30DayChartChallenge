library(tidyverse)
# devtools::install_github("christopherkenny/bskyr")
library(bskyr)
library(ggtext)
library(here)

#' Authentication 
#' --> see https://christophertkenny.com/bskyr/index.html#authentication


## Extracts the objects from the list columns into separate columns
prepare_posts_data <- function(x) {
  x |> 
    select(uri, cid, author, record, ends_with("_count")) |> 
    unnest_wider(author) |>
    # select(-c(createdAt, avatar, associated, viewer, labels)) |>
    select(-c(createdAt, avatar, viewer, labels)) |>
    unnest_wider(record) 
}

# Build the post URLs to repost
build_post_url <- function(uri, handle) {
  sprintf(
    "https://bsky.app/profile/%s/post/%s",
    handle,
    str_extract(uri, "/app.bsky.feed.post/(.+)$", group = 1)        
  )
}

# Pull all posts since the beginning of the challenge
posts <- bs_search_posts("30DayChartChallenge", 
                         since = "2025-03-31T12:00:00.000Z",
                         sort = "latest", limit = 10000)
posts_prep <- prepare_posts_data(posts)



## Posts per day

color_pal_challenge <- c("#4668E0", "#5AF5F1", "#8EE86A", "#F5C53B", "#E94E0D")
bg_gradient <- grid::linearGradient(c("#143352", "#596AA0"))

# posts_prep |> 
#   filter(createdAt >= as_date("2025-04-01")) |> 
#   transmute(
#     day = day(createdAt),
#     category = factor((day - 1) %/% 6 + 1)) |> 
#   count(category, day) |> 
#   ggplot(aes(day, n)) +
#   geom_col(
#     aes(fill = category), 
#     col = "white", linewidth = 0.2, width = 0.8) +
#   scale_x_continuous(breaks = seq(1, 30, 6)) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   scale_fill_manual(values = color_pal_challenge) +
#   coord_cartesian(ylim = c(0, NA)) +
#   theme_minimal(base_family = "Gill Sans") +
#   theme(
#     plot.background = element_rect(color = bg_gradient, fill = bg_gradient),
#     text = element_text(color = "white"),
#     axis.text = element_text(color = "white"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.major.y = element_line(color = "white", linewidth = 0.33, 
#                                       linetype = "dotted"),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_line(color = "white", linewidth = 0.15, 
#                                       linetype = "dotted")
#   )


# Identify topics / themes

regex_day <- "(?i)(?:\\b|#)(?:day|dia|día|tag)\\s?(\\d+)"
number_names <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty",
  "twenty-one", "twenty-two", "twenty-three", "twenty-four", "twenty-five", "twenty-six", "twenty-seven", "twenty-eight", "twenty-nine", "thirty"
)
numbers <- 1:30
names(numbers) <- number_names
regex_number_names <- paste0("(?i)day\\s(", paste(number_names, collapse = "|"), ")")
category_names <- c("Comparisons", "Distributions", "Relationships", 
                    "Time Series", "Uncertainty")

day_prompts <- posts_prep |> 
  select(createdAt, text) |> 
  mutate(
    day = as_date(createdAt),
    day_num = mday(day),
    day_prompt1 = str_extract(text, regex_day, group = 1),
    day_prompt2 = str_extract(text, regex_number_names, group = 1),
    day_prompt2 = ifelse(!is.na(day_prompt2), numbers[day_prompt2] , NA_integer_),
    day_prompt = case_when(
      !is.na(day_prompt1) ~ as.numeric(day_prompt1),
      !is.na(day_prompt2) ~ as.numeric(day_prompt2),
      # if neither regex catches the prompt day, use the date (UTC)
      day_num <= 30 ~ day_num
    ),
    category = (day_prompt - 1) %/% 6 + 1,
    category_name = category_names[category]
  ) 

day_prompts |> 
  count(category_name, day_prompt) |> View()
  filter(!is.na(category_name)) |> 
  ggplot(aes(day_prompt, n)) +
  geom_col(
    aes(fill = category_name), 
    col = "white", linewidth = 0.2, width = 0.8) +
  scale_x_continuous(breaks = seq(1, 30, 6)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = color_pal_challenge) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    title = "#30DayChallenge contributions on Bluesky 2025",
    subtitle = "Number of posts per day",
    x = "Prompt day", y = NULL,
    fill = "Category",
    caption = "***Note:** The prompt is determined by the tagged day in the post
    text in English, Spanish, French and German. If there is no match in the text,
    the number of the day in the month is used (UTC).*"
  ) +
  theme_minimal(base_family = "Gill Sans") +
  theme(
    plot.background = element_rect(color = bg_gradient, fill = bg_gradient),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.33, 
                                      linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(color = "white", linewidth = 0.15, 
                                      linetype = "dotted"),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    legend.direction = "vertical",
    legend.key.size = unit(5, "mm"),
    plot.title = element_text(size = 16),
    plot.title.position = "plot",
    plot.caption = element_textbox(width = 1, size = 8)
  )
ggsave(here("2025", "bsky", "posts-per-prompt.png"), width = 5, height = 5)
