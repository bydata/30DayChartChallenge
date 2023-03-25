library(tidyverse)
library(ggtext)
library(here)
library(gtrendsR)
library(lubridate)
library(geofacet)
library(grid)

base_path <- here("2023", "23")

us_states <- paste0("US-", 
   c(
    "AL",
    "AK",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",
    "DE",
    "DC",
    "FL",
    "GA",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MA",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY"
   )
)

gtrends_possibly <- possibly(gtrends, otherwise = NULL)

time_period <- "2020-03-01 2022-12-31"

# Compare each states search interest against the US
trends_states <- map(
  us_states, 
  ~gtrends_possibly(
    keyword = "covid vaccine", geo = c("US", .x), gprop = "web",
    time = time_period, onlyInterest = TRUE)
  )
trends_states <- set_names(trends_states, us_states)
write_rds(trends_states, here(base_path, "23-tiles-gtrends-vaccine-states.rds"))

trends_us <- gtrends_possibly(
    keyword = "covid vaccine", geo = "US", gprop = "web",
    time = time_period, onlyInterest = TRUE)
write_rds(trends_us, here(base_path, "23-tiles-gtrends-vaccine-us.rds"))


low_volume_value <- 0.5
trends_states_df <- flatten(trends_states) %>% 
  set_names(us_states) %>% 
  map_dfr(~mutate(.x, hits = as.character(hits)), .id = "state_data") %>% 
  mutate(hits = ifelse(hits == "<1", low_volume_value, as.numeric(hits)),
         date = as_date(date))

trends_us_df <- trends_us$interest_over_time %>% 
  mutate(hits = ifelse(hits == "<1", low_volume_value, as.numeric(hits)),
         date = as_date(date))

# Raw values by state
trends_states_df %>% 
  filter(geo != "US") %>% 
  ggplot(aes(date, hits, group = geo)) +
  # geom_smooth(span = 0.1, method = "loess", linewidth = 0.2) +
  geom_col() +
  facet_wrap(vars(geo))

trends_states_df %>% 
  group_by(geo) %>% 
  summarize(max_hits = max(hits)) %>% 
  arrange(max_hits)

trends_states_df %>% 
  filter(geo != "US") %>% 
  ggplot(aes(date, geo)) +
  geom_tile(aes(fill = hits)) +
  scale_fill_viridis_c(option = "C") +
  coord_cartesian(xlim = as_date(c("2020-09-01", "2022-06-30")), expand = FALSE)


## Check the how US and state-level search interest are correlated -------------

trends_states_df %>% 
  filter(state_data %in% c("US-CA", "US-AL", "US-TX", "US-HI", "US-SC")) %>% 
  ggplot(aes(date, hits, col = geo)) +
  geom_line() +
  facet_wrap(vars(state_data), ncol = 1)

trends_states_df %>% 
  filter(state_data %in% c("US-CA", "US-AL", "US-TX")) %>%
  filter(geo == "US") %>% 
  ggplot(aes(date, hits, col = state_data)) +
  geom_line()

# check correlation between the US data from separate state data pull
with(
  trends_states_df %>% 
    filter(state_data %in% c("US-CA", "US-TX")) %>%
    filter(geo == "US") %>% 
    select(date, state_data, hits) %>% 
    pivot_wider(id_cols = date, names_from = "state_data", values_from = "hits", values_fn = max), 
  cor(`US-CA`, `US-TX`)
  )
# result: r = .9998 (not exactly 1 due to the 0.5 imputation for "<1" values)


df_plot <- trends_states_df %>% 
  tibble() %>% 
  mutate(level = ifelse(geo == "US", "national", "state")) %>% 
  select(-c(time, gprop, category, geo)) %>%
  pivot_wider(id_cols = c(state_data, date, keyword), 
              names_from = "level", values_from = "hits", names_prefix = "hits_") %>% 
  inner_join(trends_us_df, by = "date", suffix = c("", ".us")) %>% 
  rename(hits_us = hits) %>% 
  select(-c(starts_with("category"), starts_with("gprop"), starts_with("time"), 
            keyword.us, geo)) %>% 
  # ....
  mutate(
    hits_norm_state = hits_state * hits_us / hits_national,
    hits_norm_state = replace_na(hits_norm_state, 0)) %>% 
  rename(state = state_data) %>% 
  group_by(state) %>% 
  mutate(hits_norm_state = zoo::rollmean(hits_norm_state, k = 5, align = "center", fill = NA)) %>% 
  ungroup() %>% 
  mutate(hits_norm_state2 = 100 * hits_norm_state / max(hits_norm_state, na.rm = TRUE)) %>% 
  mutate(state = str_sub(state, 4, 5))


## US States' PVI
# Source: https://www.cookpolitical.com/cook-pvi/2022-partisan-voting-index/state-map-and-list
pvi <- read_tsv(here(base_path, "us-states-pvi.tsv"))

# generate a numeric PVI value
pvi <- pvi %>% 
  mutate(
    pvi_party = str_sub(pvi, 1, 1),
    pvi_value = as.numeric(str_sub(pvi, 3, 5)),
    pvi_value_directed = ifelse(pvi_party == "D", -1, 1) * pvi_value
    )

df_plot %>% 
  inner_join(pvi, by = c("state" = "state_abbr")) %>% 
  mutate(state = fct_reorder(state, dem_2020)) %>% 
  ggplot(aes(date, state)) +
  geom_tile(aes(fill = hits_norm_state2),
            color = "white", linewidth = 0.05, height = 1) +
  scale_fill_viridis_c(option = "C") +
  coord_cartesian(xlim = as_date(c("2020-09-01", "2021-12-31")), expand = FALSE) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white")
  )
ggsave(here(base_path, "23-tiles-covid-vaccine.png"), width = 8, height = 8, dpi = 500)


# A tile map with area plots for each states
ragg::agg_png(here(base_path, "23-tiles-covid-vaccine-geofacets.png"), 
              res = 300, units = "in", width = 5, height = 4)
df_plot %>% 
  ggplot(aes(date, hits_norm_state2, group = state)) +
  geom_area() +
  scale_fill_viridis_c(option = "C", trans = "pseudo_log") +
  coord_cartesian(xlim = as_date(c("2020-09-01", "2022-03-31")), expand = FALSE) +
  # facet_wrap(vars(state))
  facet_geo(grid = "us_state_grid1", facets = "state") +
  labs(
    title = toupper("Google Search Trends for Covid Vaccine"),
    subtitle = "Google Search Trends by U.S. state from 09/2020 to 03/2022.<br>
    A value of 100 indicates the highest relative search interest.",
    caption = "Source: Google Trends. Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    panel.background = element_rect(color = "#fcba03", fill = alpha("#fcba03", 0.8)),
    plot.margin = margin(t = 2, l = 4, r = 4, b = 4),
    panel.grid.major.y = element_line(color = "white", linewidth = 0.1),
    text = element_text(color = "grey40"),
    strip.text = element_text(
      color = "grey40", face = "bold", size = 8, margin = margin(t = 2, b = 1)),
    strip.background = element_rect(color = "grey90", fill = "grey90"),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "grey4"),
    plot.subtitle = element_markdown(
      size = 8, hjust = 0.5, margin = margin(t = 2, b = 8)),
    plot.caption = element_text(size = 6, margin = margin(t = 14, b = 0))
  )

# Place y-axis label on Florida facet
yaxis_label_fontsize <- 4
yaxis_label_xpos <- 0.72
yaxis_label_ypos_min <- 0.095
yaxis_label_ypos_offset <- 0.0165
yaxis_label_gpar <- gpar(family = "Roboto Condensed", hjust = 0,
                         fontsize = yaxis_label_fontsize)
grid.text(label = "25", x = yaxis_label_xpos, y = yaxis_label_ypos_min, 
          gp = yaxis_label_gpar)
grid.text(label = "50", x = yaxis_label_xpos, 
          y = yaxis_label_ypos_min + yaxis_label_ypos_offset, 
          gp = yaxis_label_gpar)
grid.text(label = "75", x = yaxis_label_xpos, 
          y = yaxis_label_ypos_min + 2 * yaxis_label_ypos_offset, 
          gp = yaxis_label_gpar)
grid.text(label = "100", x = yaxis_label_xpos, 
          y = yaxis_label_ypos_min + 3 * yaxis_label_ypos_offset, 
          gp = yaxis_label_gpar)

# Place x-axis label on Florida facet
xaxis_label_fontsize <- 3.5
xaxis_label_ypos <- 0.07
xaxis_label_gpar <- gpar(family = "Roboto Condensed", hjust = 0,
                         fontsize = xaxis_label_fontsize)
grid.text(label = "09/20", x = 0.73, y = xaxis_label_ypos, 
          gp = xaxis_label_gpar)
grid.text(label = "07/21", x = 0.77, y = xaxis_label_ypos, 
          gp = xaxis_label_gpar)
grid.text(label = "03/22", x = 0.81, y = xaxis_label_ypos, 
          gp = xaxis_label_gpar)
invisible(dev.off())
