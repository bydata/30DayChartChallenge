library(tidyverse)
library(here)
library(ggtext)
library(ggdist)
library(glue)
library(readxl)

base_path <- here("2022", "30")

#' Source: UN Population Data
#' 
#'  In projecting future levels of fertility and mortality, probabilistic methods 
#'  were used to reflect the uncertainty of the projections based on the historical 
#'  variability of changes in each variable. The method takes into account the past 
#'  experience of each country, while also reflecting uncertainty about future 
#'  changes based on the past experience of other countries under similar conditions. 
#'  The medium-variant projection corresponds to the median of several thousand 
#'  distinct trajectories of each demographic component derived using the 
#'  probabilistic model of the variability in changes over time. Prediction 
#'  intervals reflect the spread in the distribution of outcomes across the 
#'  projected trajectories and thus provide an assessment of the uncertainty 
#'  inherent in the medium-variant projection. See also Figures for probabilistic 
#'  results. For further details, see the report World Population Prospects 2019: 
#'  Methodology of the United Nations Population Estimates and Projections.
#' 
#' https://population.un.org/wpp/Download/Probabilistic/Fertility/

data_url <- "https://population.un.org/wpp/Download/Files/2_Indicators%20(Probabilistic%20Projections)/UN_PPP2019_Output_ASFR.xlsx"
file_path <- here(base_path, "un-fertility-projects-by-age-groups.xlsx")
download.file(data_url, file_path)

sheet_names <- excel_sheets(file_path)
sheet_names <- sheet_names[which(sheet_names != "NOTES")]

sheets_raw <- map(sheet_names, ~read_xlsx(file_path, sheet = .x, skip = 16, na = "..."))
sheets_raw <- set_names(sheets_raw, sheet_names)
columns <- c(
  "index", "variant", "region", "notes", "country_code", "type", "parent_code", 
  "reference_date", "age_15-19", "age_20-24", "age_25-29", "age_30-34", "age_35-39",
  "age_40-44", "age_45-49"                       
)
for(name in names(sheets_raw)) {
  colnames(sheets_raw[[name]]) <- columns
}

df <- 
  bind_rows(sheets_raw) %>% 
  mutate(ci_level = str_remove(variant, " PI"),
           ci_level = fct_inorder(ci_level)) %>% 
  select(-c(notes, variant))

df_long <- df %>% 
  pivot_longer(cols = starts_with("age_"), names_to = "age_group", 
               values_to = "births_per_1k", names_pattern = "age_(.+)")
  

df %>% 
  filter(region == "WORLD") %>% 
  ggplot(aes(reference_date, `age_15-19`, col = ci_level, group = ci_level)) +
  geom_line()



df_long %>% 
  select(region, reference_date, ci_level, age_group, births_per_1k) %>% 
  pivot_wider(id_cols = c(region, reference_date, age_group),
              names_from = "ci_level", values_from = "births_per_1k") %>% 
  filter(region == "WORLD") %>% 
  ggplot(aes(reference_date, group = age_group)) +
  geom_ribbon(aes(ymin = `Lower 95`, ymax = `Upper 95`), fill = "grey70") + 
  geom_ribbon(aes(ymin = `Lower 80`, ymax = `Upper 80`), fill = "grey50") + 
  geom_line(aes(y = Median), size = 1.5) +
  facet_wrap(vars(age_group))



df_long %>% 
  select(region, type, reference_date, ci_level, age_group, births_per_1k) %>% 
  pivot_wider(id_cols = c(region, type, reference_date, age_group),
              names_from = "ci_level", values_from = "births_per_1k") %>% 
  filter(type == "Income Group", 
         region %in% c("High-income countries", 
                       "Middle-income countries",
                       "Low-income countries")) %>% 
  mutate(region = factor(
    region, levels = c("High-income countries",
                       "Middle-income countries",
                       "Low-income countries")),
    year = as.numeric(str_match(reference_date, "-(2\\d{3})")[, 2]),
    age_group = paste0(age_group, "y")) %>% 
  ggplot(aes(year, group = age_group)) +
  geom_ribbon(aes(ymin = `Lower 95`, ymax = `Upper 95`, fill = "95% CI")) + 
  geom_ribbon(aes(ymin = `Lower 80`, ymax = `Upper 80`, fill = "80% CI")) + 
  geom_line(aes(y = Median, fill = "Median"), col = "grey8") + ##1DC9A4
  scale_y_continuous(limits = c(0, NA), position = "right") +
  scale_fill_manual(
    values = c("Median" = "grey8", "80% CI" = "#FB9851", "95% CI" = "#FCDE83")) +
  facet_grid(age_group ~ region, scales = "free_y", switch = "y") +
  guides(color = "none") +
  labs(
    title = "Fertility Rates per Age Group and Income Group of Country",
    subtitle = "Projected fertility rates with 80% and 95% predicted intervals
    for 2020 to 2100. Note the different scales between age groups",
    caption = "\U00A9 United Nations, August 2019, made available under a Creative 
    Commons license CC BY 3.0 IGO.
    Visualization: Ansgar Wolsing",
    x = NULL,
    y = "Births per 1,000 women",
    fill = NULL 
  ) +
  theme_minimal(base_family = "Fira Sans Condensed") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = 0.3, color = "#DAD9D9"),
    plot.background = element_rect(color = NA, fill = "white"),
    text = element_text(),
    axis.line.x = element_line(color = "black", size = 0.3),
    axis.ticks.x = element_line(color = "black", size = 0.3), 
    axis.ticks.length.x = unit(2, "mm"), 
    axis.title = element_text(family = "Fira Sans Condensed Medium"),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_markdown(),
    plot.title = element_markdown(
      face = "bold", margin = margin(t = 16, b = 4)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(margin = margin(b = 8), lineheight = 1),
    plot.caption = element_textbox(hjust = 0, size = 7, 
                                family = "Fira Sans Condensed Light"),
    plot.caption.position = "plot",
    legend.key.height = unit(2.5, "mm"),
    strip.text = element_text(
      size = 12, family = "Fira Sans Condensed Medium"),
    strip.text.y = element_text(),
    panel.spacing.y = unit(5, "mm")
  )
ggsave(here(base_path, "30-un-pop-fertility-age-groups.png"), 
       dpi = 300, width = 8, height = 7)

