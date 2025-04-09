library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2025", "02")

#' Download ESS survey data from 
#' https://ess.sikt.no/en/series/321b06ad-1b98-4b7d-93ad-ca8a24e8788a
#' ESS 9
#' ESS 11

df_ess09 <- read_csv(here(base_path, "ESS9e03_2.csv"))
df_ess11 <- read_csv(here(base_path, "ESS11.csv"))

# Select the relevant variables
relevant_vars <- c("essround", "anweight", "cntry", "netustm")
df_ess09_prep <- df_ess09 |> select(all_of(relevant_vars))
df_ess11_prep <- df_ess11 |> select(all_of(relevant_vars))

# Combine the datasets
df_ess_comb <- bind_rows(df_ess09_prep, df_ess11_prep)

# Which countries are in the data only in one wave?
countries_exclude <- df_ess_comb |> 
  group_by(cntry) |> 
  summarize(n_rounds = n_distinct(essround)) |> 
  arrange(n_rounds) |> 
  filter(n_rounds == 1) |> 
  pull(cntry)

# Weighted aggregated values
df_netustm <- df_ess_comb |> 
  filter(!cntry %in% countries_exclude) |> 
  # exclude missing values
  filter(netustm <= 24 * 60) |> 
  # calculate weighted mean per ESS round
  arrange(cntry, essround) |> 
  group_by(cntry, essround) |> 
  mutate(netustm_wgt = netustm * anweight / sum(anweight)) |> 
  summarize(netustm_wgt = sum(netustm_wgt), .groups = "drop") |> 
  mutate(
    netustm_change_abs = netustm_wgt - lag(netustm_wgt, 1),
    netustm_change_rel = netustm_change_abs / lag(netustm_wgt, 1),
    .by = cntry) |> 
  mutate(
    cntry_name = countrycode::countrycode(cntry, origin = "iso2c", destination = "country.name"),
    year = case_when(
      essround == 9 ~ 2018,
      essround == 11 ~ 2023
    ))
  

countries_ordered <- df_netustm |> 
  filter(year == max(year)) |> 
  arrange(-netustm_wgt) |> 
  pull(cntry_name)

df_netustm |>
  mutate(cntry_name = factor(cntry_name, levels = countries_ordered)) |> 
  ggplot(aes(factor(year), netustm_wgt, group = cntry)) +
  geom_area(
    color = "#703D57",
    fill = alpha("#703D57", 0.5),
    linewidth = 0.5) +
  geom_point(size = 0.7) +
  geom_label(
    aes(x = 1.5, y = 70, 
        label = scales::percent(netustm_change_rel, 
                                accuracy = 0.1, style_positive = "plus")),
    family = "Roboto Condensed SemiBold", color = "white",
    fill = "#222222DD", size = 3, label.size = 0, label.r = unit(0, "mm")
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(vars(cntry_name)) +
  labs(
    title = "Europeans spent more time online",
    subtitle = "Internet use on a typical day (in minutes) 2018 vs. 2023, personal use or work",
    caption = "**Source:** ESS 9 & 11, 2018/2023, weighted by analytics weight. 
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 8),
    axis.line.y = element_line(linewidth = 0.3),
    axis.ticks.y = element_line(linewidth = 0.3),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey40", linewidth = 0.15, linetype = "dashed"),
    panel.spacing.x = unit(0, "mm"),
    panel.spacing.y = unit(3, "mm"),
    strip.text = element_text(family = "Roboto Condensed SemiBold", size = 10),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", lineheight = 1.05),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = 0)
  )
ggsave(here(base_path, "02-slope.png"), width = 6, height = 6)
