library(tidyverse)
library(ggtext)
library(factoextra)
library(here)

base_path <- here("2025", "15")

#' Download ESS survey data from 
#' https://ess.sikt.no/en/series/321b06ad-1b98-4b7d-93ad-ca8a24e8788a
#' ESS 11

df_ess <- read_csv(here(base_path, "ESS11.csv"))

# Select the relevant variables
relevant_vars <- c("essround", "anweight", "cntry", "gndr", "ccrdprs", "wrclmch")
df_ess_prep <- df_ess |> 
  select(all_of(relevant_vars)) |> 
  mutate(cntry_name = countrycode::countrycode(cntry, "iso2c", "country.name"))

table(df_ess_prep$ccrdprs, useNA = "ifany")
table(df_ess_prep$wrclmch, useNA = "ifany")

# Remove missing values
df_ess_prep <- df_ess_prep |> 
  filter(ccrdprs <= 10, wrclmch <= 5)

# Correlation
with(df_ess_prep, cor(ccrdprs, wrclmch))
# by country
df_ess_prep |> 
  group_by(cntry) |> 
  group_split() |> 
  map_dbl(function(x) with(x, cor(ccrdprs, wrclmch)))

length(unique(df_ess_prep$cntry))

p <- df_ess_prep |> 
  ggplot(aes(wrclmch, ccrdprs, weights = anweight)) + 
  geom_jitter(col = "#073b4c", alpha = 0.2, size = 0.1) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_equal() +
  facet_wrap(vars(str_wrap(cntry_name, 6)), ncol = 8) +
  labs(
    title = "How worried are Europeans about climate change - <br>
    and do they feel it's their job to help fix it?",
    subtitle = "Each dot represents a respondent's level of worries about 
    climate change and their feeling of personal responsibility to address it.",
    caption = "**Source:** European Social Survey wave 11 (2023). 
    **Visualization:** Ansgar Wolsing",
    x = "**\U2190 Not at all** \U2022 Worried about climate change
    \U2022 **Extremely \U2192**",
    y = "**\U2190 Low** \U2022 Personal responsibility to try to reduce climate 
    change \U2022 **High \U2192**"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_markdown(size = 9),
    axis.text = element_text(color = "grey40", size = 8),
    panel.grid.major = element_line(color = "grey50", linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(color = "grey50", linewidth = 0.1),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", size = 14, lineheight = 1.25),
    plot.subtitle = element_textbox(
      margin = margin(t = 2, b = 2), lineheight = 1.2, width = 1, size = 10
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      lineheight = 1.1, margin = margin(t = 10, b = 6), hjust = 0, size = 7, width = 1),
    legend.position = "bottom",
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
    strip.text = element_text(
      family = "Roboto Condensed Medium", size = 7.5, vjust = 0)
  )
ggsave(here(base_path, "15-complicated.png"), width = 6, height = 6)
