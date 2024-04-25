library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "25")

#' Source: UN, World Population Prospects
#' Download file from https://population.un.org/wpp/Download/Standard/CSV/
#' Descriptions of the projection variants: 
#' https://population.un.org/wpp/DefinitionOfProjectionScenarios/
url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Demographic_Indicators_OtherVariants.zip"
local_zipfile <- here(base_path, "WPP2022_Demographic_Indicators_OtherVariants.zip")
local_file <- gsub("\\.zip", ".csv", local_zipfile)
download.file(url, local_zipfile, mode = "wb")
unzip(local_zipfile, exdir = base_path)
list.files(base_path)

df <- read_csv(local_file)

table(df$LocTypeName)
unique(df$Variant)

df_prep <- df |> 
  filter(LocTypeName == "Geographic region") |> 
  # Select first and latest year
  filter(Time == min(Time) | Time == max(Time)) |> 
  select(Time, Location, Variant, population = TPopulation1Jan)


# Example for variants for Africa
df_prep |> 
  filter(Location == "Africa", Time == max(Time)) |> 
  arrange(population)
  

selected_variants <- c("Median PI", "Upper 95 PI", "Lower 95 PI")

df_prep |> 
  filter(Variant %in% selected_variants) |> 
  # filter(Variant == "Median PI") |> 
  ggplot(aes(factor(Time), population, group = paste(Variant, Location))) +
  geom_line(aes(color = Location))



# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title = element_text(size = 8),
      axis.text = element_markdown(family = "Source Code Pro", size = 7),
      # axis.text.y = element_blank(),
      axis.line.x = element_line(linewidth = 0.15),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 8.5, width = 0.9, lineheight = 1.2,
        margin = margin(b = 8)),
      plot.caption = element_markdown(
        hjust = 0, lineheight = 1.1, size = 7),
      plot.margin = margin(rep(4, 4)),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.height = unit(6, "mm"),
      legend.key.width = unit(10, "mm"),
      legend.box.spacing = unit(0, "mm"),
      legend.margin = margin(0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.08),
      strip.text = element_markdown(
        family = "Libre Franklin SemiBold", size = 10,
        margin = margin(t = 4, b = 1))
    ) 
)


# Prepare dataset for polygons / triangles
df_polygons <- df_prep |> 
  filter(Variant %in% selected_variants) |> 
  filter(
    Time == min(Time) & Variant == "Median PI" | 
    Time == max(Time) & Variant %in% c("Lower 95 PI", "Upper 95 PI"),
    .by = Location
  ) |> 
  select(Location, Time, population)

df_prep |> 
  filter(Variant %in% selected_variants) |> 
  ggplot(aes(factor(Time), population, group = paste(Variant, Location))) +
  geom_polygon(
    data = df_polygons,
    aes(factor(Time), population, fill = Location, group = Location),
    inherit.aes = FALSE, alpha = 0.3
  ) +
  # Line for the median
  geom_line(
    data = ~subset(., Variant == "Median PI"),
    aes(color = Location),
    linewidth = 0.7) +
  # Line for the 95 % interval
  geom_line(
    aes(color = Location),
    linewidth = 0.1, linetype = "dashed") +
  scale_x_discrete(expand = expansion(add = c(0.1, 0.1))) +
  scale_y_continuous(
    # labels = scales::number_format(scale_cut = scales::cut_short_scale(), scale = 1000),
    labels = function(x) {
      x <- x / 1e6
      ifelse(x == max(x), paste(x, "<i>billion<br>people</i>", sep = "<br>"), x)
      },
    sec.axis = dup_axis()) +
  scale_color_manual(
    values = c("#EC407A", "#26C6DA", "#AB47BC", "#5C6BC0", "#26A69A", "#FFA726"),
    aesthetics = c("color", "fill")
  ) +
  labs(
    title = ""
  )


# Line chart with ribbons for Africa and Asia only
df |> 
  filter(LocTypeName == "Geographic region", 
         Location %in% c("Asia", "Africa"),
         Variant %in% selected_variants) |> 
  select(Time, Location, Variant, population = TPopulation1Jan) |> 
  pivot_wider(id_cols = c(Time, Location), 
              names_from = "Variant", values_from = population,
              names_repair = janitor::make_clean_names) |>
  ggplot(aes(time)) +
  geom_ribbon(
    aes(ymin = lower_95_pi, ymax = upper_95_pi, fill = location),
    alpha = 0.3
  ) +
  # Line for the median
  geom_line(
    aes(y = median_pi, color = location),
    linewidth = 0.7) +
  # Lines for the 95 % interval
  geom_line(
    aes(y = lower_95_pi, color = location),
    linewidth = 0.1, linetype = "dashed") +
  geom_line(
    aes(y = upper_95_pi, color = location),
    linewidth = 0.1, linetype = "dashed") +
  geom_point(
    data = ~subset(., time == min(time)),
    aes(y = median_pi, color = location),
    size = 1
  ) +
  # scale_x_continuous() +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(1, 6, 1) * 1e6,
    expand = expansion(mult = c(0, 0.1)),
    labels = function(x) {
      x <- x / 1e6
      ifelse(x == max(x), paste(x, "<i>billion<br>people</i>", sep = "<br>"), x)
    },
    sec.axis = dup_axis()) +
  scale_color_manual(
    values = c("#EC407A", "#26C6DA"),
    aesthetics = c("color", "fill")
  ) +
  guides(color = "none", fill = "none") +
  labs(
    title = "The most populous continent in 2100?",
    subtitle = "Population projections for
    <span style='font-family:\"Libre Franklin SemiBold\";color:#EC407A'>Africa</span>
    and <span style='font-family:\"Libre Franklin SemiBold\";color:#26C6DA'>Asia</span>.
    The <span style='font-family:\"Libre Franklin SemiBold\"'>lines</span> 
    show the median prediction for each continent, the
    <span style='font-family:\"Libre Franklin SemiBold\"'>ribbons</span> indicate the 
    95 % prediction interval.",
    caption = "Data: UN World Population Prospects 2022. 
    Visualization: Ansgar Wolsing",
    x = NULL, y = NULL
  ) 
ggsave(here(base_path, "25-global-change.png"), width = 5, height = 5)
  