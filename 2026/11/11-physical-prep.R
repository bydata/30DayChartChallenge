library(tidyverse)
library(treemapify)
library(here)


#' Source:
#' Gnann et al. 2026
#' https://www.nature.com/articles/s44458-025-00007-5#Sec12
#' https://www.nature.com/articles/s44458-025-00007-5/figures/2


url <- "https://zenodo.org/records/17108281/files/20250111_Data.xlsx?download=1"
filepath <- here("2026", "11", "krake.xlsx")
download.file(url, filepath)
readxl::excel_sheets(filepath)
df <- readxl::read_xlsx(filepath, sheet = "LT ML Data", .name_repair = janitor::make_clean_names)
colnames(df)

df |> 
  count(material_decoded, wt = weight_g, name = "total_weight") |> 
  mutate(share_by_weight = total_weight / sum(total_weight)) |> 
  arrange(-share_by_weight)


df_counts <- tribble(
  ~material,     ~share,
  "Plastic", 0.697,
  "Worked wood", 0.149,
  "Glass & ceramics", 0.057,
  "Paper & cardboard", 0.026,
  "Metal", 0.026,
  "Rubber", 0.016,
  "Chemicals", 0.016,
  "Foodwaste", 0.008,
  "Textiles", 0.005
)
sum(df_counts$share)

# pal_greys <- c("grey40", "grey50", "black", "black", "black", "black", "grey70", "grey20", "black")
pal_greys <- c("grey40", rep(NA_character_, nrow(df_counts) - 1))
names(pal_greys) <- df_counts$material[order(df_counts$material)]

df_counts |> 
  ggplot(aes(area = share, fill = material)) +
  geom_treemap(
    col = "grey90", size = 3,
    layout = "squarified") +
  scale_fill_manual(values = pal_greys) +
  guides(fill = "none") +
  theme_void()
ggsave(here("2026", "11", "11-physical-treemap-raw.png"), width = 5, height = 5)

Weight
Plastic 422766
Total 1848541
422766 / 1848541

Counts
Plastic 14172
Total 20339
14172 / 20339
