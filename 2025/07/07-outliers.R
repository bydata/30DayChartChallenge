library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(here)

base_path <- here("2025", "07")

#' Source: World Bank
#' https://data.worldbank.org/indicator/EN.POP.DNST

data_url <- "https://api.worldbank.org/v2/en/indicator/EN.POP.DNST?downloadformat=csv"
file_path_zip <- here(base_path, "worldbank-pop-density.zip")
filename <- "API_EN.POP.DNST_DS2_en_csv_v2_13897.csv"
download.file(data_url, destfile = file_path_zip)
unzip(file_path_zip, files = filename, exdir = base_path)

df <- read_csv(here(base_path, filename), skip = 4)

df_long <- df |> 
  rename(country_name = `Country Name`, country_code = `Country Code`) |> 
  select(-c("Indicator Name", "Indicator Code")) |> 
  pivot_longer(cols = -c(country_name, country_code), 
               names_to = "year", values_to = "pop_density") |> 
  filter(!is.na(pop_density)) |> 
  # Exclude entities that are no countries (but keep World for the average)
  mutate(
    country_name_from_code = countrycode::countrycode(country_code, "iso3c", "country.name"),
    un_code = countrycode::countrycode(country_code, "iso3c", "un.name.en")) |> 
  filter(!is.na(country_name_from_code) & !is.na(un_code) | country_name == "World") |> 
  select(-c(country_name_from_code, un_code))


# Latest data per country
df_long_latest <- df_long |> 
  filter(year == max(year), .by = country_name) |> 
  filter(country_name != "World")


# Average value for the world
pop_density_world <- df_long |> 
  filter(year == max(year), .by = country_name) |> 
  filter(country_name == "World") |> 
  pull(pop_density)
pop_density_world


df_long_latest |> 
  ggplot(aes(x = 1, y = pop_density)) +
  geom_beeswarm(
    # aes(fill = pop_density_in_sd_3x),
    fill = "#264653",
    shape = 21, color = "white", size = 2, show.legend = FALSE
  ) +
  geom_hline(
    data = data.frame(pop_density = pop_density_world),
    aes(yintercept = pop_density),
    linetype = "dashed"
  ) +
  # geom_text(
  #   data = ~filter(., pop_density_in_sd_3x),
  #   aes(label = country_name),
  #   size = 1.75, angle = 45, hjust = 0, nudge_x = 0.005
  # ) +
  # scale_fill_manual(values = c("FALSE" = "#264653", "TRUE" = "#e76f51")) +
  coord_flip() +
  labs(
    title = "Title",
    caption = "**Source:** Our World in Data, Global Carbon Budget (2024);
    Population based on various sources (2024).
    **Visualization:** Ansgar Wolsing",
    y = "Inhabitants per square kilometer"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    strip.text = element_text(family = "Roboto Condensed SemiBold", size = 10),
    plot.title = element_markdown(
      family = "Roboto Condensed", face = "bold", hjust = 0, lineheight = 1.05,
      size = 11),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      hjust = 0, face = "italic", margin = margin(t = 0, b = 12), size = 10),
    plot.caption = element_markdown(
      hjust = 0, size = 8, margin = margin(t = 12, b = 4)),
    axis.title.x = element_markdown(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(t = 4, r = 16, b = 4, l = 4)
  )


df_long_latest |> 
  ggplot(aes(log(pop_density))) +
  geom_density(
    # data = ~filter(., pop_density < pop_density_world * 15),
    fill = alpha("red", 0.2)
  ) +
  theme_minimal(base_family = "Roboto Condensed")


df_long_latest |> 
  ggplot(aes(pop_density)) +
  geom_histogram(
    data = ~filter(., pop_density < pop_density_world * 15),
    fill = alpha("red", 0.2), binwidth = 80
  ) +
  geom_point(
    data = ~filter(., pop_density >= pop_density_world * 15),
    aes(y = 0.0),
    shape = 21, color = "white", fill = alpha("red", 0.7), stroke = 0.2
  ) +
  ggrepel::geom_label_repel(
    data = ~filter(., pop_density >= pop_density_world * 15),
    aes(y = 0.0, label = country_name),
    size = 3, direction = "both",
    min.segment.length = 0, segment.size = 0.2, max.overlaps = 12,
    fill = alpha("#F8F8F8", 0.33), label.size = 0, label.padding = 0.1,
    family = "Roboto Condensed", seed = 1
  ) +
  annotate(
    "text",
    x = 600, y = 43, label = "Rest of the world",
    size = 3, family = "Roboto Condensed", hjust = 0
  ) +
  annotate(
    "segment",
    x = 550, xend = 200, y = 43, yend = 42,
    linewidth = 0.2
  ) + 
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Population Density Outliers",
    subtitle = sprintf("The average population density of countries is %0.0f
    inhabitants per square kilometer.<br>There are significant outliers.", 
                       pop_density_world),
    caption = "**Source:** World Bank;
    **Visualization:** Ansgar Wolsing",
    x = "Inhabitants per square kilometer",
    y = "# countries"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    plot.title = element_markdown(
      family = "Roboto Condensed", face = "bold", lineheight = 1.05),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      margin = margin(t = 0, b = 12), lineheight = 1.2),
    plot.caption = element_markdown(
      hjust = 0, size = 8, margin = margin(t = 12, b = 4)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 9)
  ) 
ggsave(here(base_path, "07-outliers.png"), width = 6, height = 5)
