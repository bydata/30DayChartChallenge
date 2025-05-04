library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2025", "10")

#' Download ESS survey data from 
#' https://ess.sikt.no/en/series/321b06ad-1b98-4b7d-93ad-ca8a24e8788a
#' ESS 11

df_ess11 <- read_csv(here(base_path, "ESS11.csv"))

# Select the relevant variables
relevant_vars <- c("essround", "anweight", "gndr", "cntry", "height")
df_ess11_prep <- df_ess11 |> 
  select(all_of(relevant_vars)) |> 
  mutate(
    gndr_label = case_when(gndr == 1 ~ "male", gndr == 2 ~ "female"),
    cntry_name = countrycode::countrycode(
      cntry, origin = "iso2c", destination = "country.name")
  ) |> 
  # exclude missing values
  filter(!height %in% c(777, 888, 999))



df_ess11_prep |> 
  ggplot(aes(height, color = factor(gndr), fill = factor(gndr))) +
  stat_density(
    alpha = 0.6,
    geom = "area", bw = "nrd0"
  ) +
  facet_wrap(vars(gndr), ncol = 1)

df_ess11_prep |> 
  ggplot(aes(height, color = factor(gndr), fill = factor(gndr))) +
  stat_density(
    alpha = 0.6,
    geom = "area", bw = "nrd0"
  ) +
  facet_grid(rows = vars(cntry), cols = vars(gndr))


countries_height_by_gender <- df_ess11_prep |> 
  mutate(cntry_name = factor(cntry_name, levels = countries_ordered)) |> 
  group_by(cntry_name, gndr) |> 
  summarize(
    mean_height = weighted.mean(height, w = anweight),
    .groups = "drop") |> 
  mutate(gndr = case_when(gndr == 1 ~ "male", gndr == 2 ~ "female"))

countries_ordered <- countries_height_by_gender |> 
  filter(gndr == "female") |> 
  arrange(-mean_height) |> 
  pull(cntry_name)

countries_ordered <- countries_height_by_gender |> 
  mutate(mean_height = round(mean_height)) |> 
  pivot_wider(id_cols = cntry_name, names_from = "gndr", values_from = "mean_height") |> 
  arrange(-female, -male) |> 
  pull(cntry_name)

countries_height_by_gender <- countries_height_by_gender |> 
  mutate(cntry_name = factor(cntry_name, levels = countries_ordered))

p <- df_ess11_prep |> 
  filter(height >= 110) |> 
  mutate(
    cntry_name = factor(cntry_name, levels = countries_ordered),
    gndr = case_when(gndr == 1 ~ "male", gndr == 2 ~ "female")) |> 
  ggplot(
    aes(height, color = factor(gndr), fill = factor(gndr),
        weight = anweight)) +
  stat_density(
    alpha = 0.2,
    geom = "area", bw = "nrd", n = 256, linewidth = 0.6,
    position = "dodge", adjust = 1.25
  ) +
  geom_vline(
    data = countries_height_by_gender,
    aes(xintercept = mean_height, col = gndr),
    linetype = "solid", linewidth = 0.4
  ) +
  geom_text(
    data = countries_height_by_gender,
    aes(mean_height, y = 0, 
        label = paste0(round(mean_height, 0), ifelse(gndr == "male", " cm", "")),
        hjust = ifelse(gndr == "male", 0.3, 0.7),
        col = gndr),
    family = "Roboto Condensed SemiBold", size = 3, vjust = 1.5,
    inherit.aes = FALSE
  ) +
  geom_hline(aes(yintercept = 0), color = "grey40", linewidth = 0.5) +
  # Custom y-axis title in the first facet
  geom_text(
    data = data.frame(
      cntry_name = factor(countries_ordered[1], levels = countries_ordered), 
                          x = 120, y = 0.04),
    aes(x, y, label = "\U2191 Proportion\nof respondents"),
    family = "Roboto Condensed", size = 2.5, hjust = 0, lineheight = 0.8,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("male" = "#18A0AA", "female" = "#730C6D"),
    aesthetics = c("color", "fill")) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE, clip = "off") +
  facet_wrap(vars(cntry_name), scales = "free_y") +
  labs(
    title = "Average body height by gender in European countries",
    subtitle = "Distributions for self-reported height of
    <b style='color:#730C6D'>female</b> and
    <b style='color:#18A0AA'>male</b> respondents in the European Social Survey 
    (2023).<br>Vertical lines mark average height by gender.
    Countries are sorted by average female height.",
    caption = "**Source:** ESS 11 (2023). 
    **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_markdown(
      family = "Roboto Condensed SemiBold", size = 18),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    strip.text = element_text(
      hjust = 0, family = "Roboto Condensed Medium",
      margin = margin(b = -3)),
    panel.spacing.x = unit(3, "mm"),
    panel.spacing.y = unit(9, "mm"),
    legend.position = "none",
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
  )
ggsave(here(base_path, "10-multimodal.png"), width = 7, height = 6, scale = 1.1)
  