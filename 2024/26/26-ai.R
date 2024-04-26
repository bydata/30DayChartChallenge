library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "26")

#' Source: UN, World Population Prospects
#' Download file from https://population.un.org/wpp/Download/Standard/CSV/
#' ChatGPT, Gemini:
#'   Prompt: 
#'   Predict the world population from 2025 to 2100 in 5 year intervals. 
#'   Provide a dataset with 2 columns: year, prediction. (Do not use web search.)

# UN World Population Prospects
url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Demographic_Indicators_OtherVariants.zip"
local_zipfile <- here(base_path, "WPP2022_Demographic_Indicators_OtherVariants.zip")
local_file <- gsub("\\.zip", ".csv", local_zipfile)
download.file(url, local_zipfile, mode = "wb")
unzip(local_zipfile, exdir = base_path)
list.files(base_path)

un_pop <- read_csv(local_file)
chatgpt <- read_tsv(here(base_path, "world-population-predictions-chatgpt-3.5.tsv"))
gemini <- read_tsv(here(base_path, "world-population-predictions-gemini.tsv"))

chatgpt_long <- chatgpt |> 
  pivot_longer(cols = starts_with("run"), names_to = "variant", values_to = "population") |> 
  transmute(
    time = year, 
    source = "ChatGPT-3.5", 
    variant, 
    population = 1e9 * population)

gemini_long <- gemini |> 
  pivot_longer(cols = starts_with("run"), names_to = "variant", values_to = "population") |> 
  transmute(
    time = year, 
    source = "Gemini", 
    variant, 
    population = 1e9 * population)

selected_variants <- c("Median PI", "Upper 95 PI", "Lower 95 PI")

df_prep <- un_pop |> 
  filter(LocTypeName == "World", Variant %in% selected_variants) |> 
  select(time = Time, variant = Variant, population = TPopulation1Jan) |> 
  mutate(
    population = population * 1e3,
    source = "UN World Population Prospects") |> 
  bind_rows(chatgpt_long) |> 
  bind_rows(gemini_long)


df_un_pop_ribbon <- un_pop |> 
  filter(LocTypeName == "World", Variant %in% selected_variants) |> 
  select(Time, Variant, population = TPopulation1Jan) |> 
  mutate(population = population * 1e3) |> 
  pivot_wider(id_cols = Time, 
              names_from = "Variant", values_from = population,
              names_repair = janitor::make_clean_names) 

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
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 8.5, width = 0.9, lineheight = 1.2,
        margin = margin(b = 8)),
      plot.caption = element_textbox(
        width = 1, hjust = 0, lineheight = 1.1, size = 7),
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


df_prep |> 
  filter(!variant %in% c("Lower 95 PI", "Upper 95 PI")) |> 
  ggplot(aes(time, population, group = paste(source, variant), color = source)) +
  geom_ribbon(
    data = df_un_pop_ribbon,
    aes(time, ymin = lower_95_pi, ymax = upper_95_pi, fill = "UN World Population Prospects"),
    alpha = 0.2, inherit.aes = FALSE
  ) +
  # Lines for the 95 % intervals (UN)
  geom_line(
    data = df_un_pop_ribbon,
    aes(time, lower_95_pi, color = "UN World Population Prospects"),
    inherit.aes = FALSE,
    linewidth = 0.1, linetype = "dashed") +
  geom_line(
    data = df_un_pop_ribbon,
    aes(time, upper_95_pi, color = "UN World Population Prospects"),
    inherit.aes = FALSE,
    linewidth = 0.2, linetype = "dashed") +
  geom_line(
    aes(linewidth = ifelse(source == "UN World Population Prospects", 0.8, 0.3))) +
  # Direct labels
  annotate(
    "text",
    x = c(2065, 2090, 2070),
    y = c(12.2, 11.2, 10.0) * 1e9,
    label = c("ChatGPT 3.5", "Gemini", "UN"),
    color = c("#26A69A", "#5E35B1", "grey50"),
    family = "Libre Franklin SemiBold"
  ) +
  # Nonsense warning
  annotate(
    "richtext",
    x = 2040, y = 14e9,
    label = "JUST FOR FUN",
    family = "Libre Franklin", fontface = "bold", size = 7,
    fill = "#FF0266", color = "white", angle = 10, vjust = 0.5,
    label.padding = unit(8, "mm")
  ) +
  scale_x_continuous(
    breaks = seq(2020, 2100, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(1, 15, 1) * 1e9,
    expand = expansion(mult = c(0, 0.1)),
    labels = function(x) {
      x <- x / 1e9
      ifelse(x == max(x), paste(x, "<i>billion<br>people</i>", sep = "<br>"), x)
    },
    sec.axis = dup_axis()) +
  scale_color_manual(
    values = c("#26A69A", "#5E35B1", "grey50"),
    aesthetics = c("color", "fill")) +
  scale_linewidth_identity() +
  coord_cartesian(clip = "off") +
  guides(color = "none", fill = "none") +
  labs(
    title = "Gen AI predicts the world population",
    subtitle = "Predictions from 
    <span style='font-family:\"Libre Franklin SemiBold\";color:#26A69A'>ChatGPT-3.5</span> (OpenAI)
    and <span style='font-family:\"Libre Franklin SemiBold\";color:#5E35B1'>Gemini</span> (Google).
    The <span style='font-family:\"Libre Franklin SemiBold\"'>grey ribbon</span> indicates the 
    95 % prediction interval and the line the median from the UN World Population Prospects.",
    caption = "<i>Prompt used: \"Predict the world population from 2025 to 2100 in 5 year intervals. 
    Provide a dataset with 2 columns: year, prediction.\"
    \"[NO_WEB_SEARCH]\" was added for Gemini.</i><br><br>
    Data: UN World Population Prospects 2022, ChatGPT-3.5, Gemini. 
    Visualization: Ansgar Wolsing",
    x = NULL, y = NULL
  ) 
ggsave(here(base_path, "26-ai.png"), width = 5, height = 5)
