library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2023", "07")

#' Source:
#' https://weltrisikobericht.de/wp-content/uploads/2022/09/WorldRiskReport-2022_Online.pdf

data_url <- "https://weltrisikobericht.de/download/2974/"
local_file <- here(base_path, "wri_2022.xlsx")
download.file(data_url, destfile = local_file)

wri <- readxl::read_xlsx(local_file, sheet = 1, skip = 2)

wri %>% 
  ggplot(aes(WorldRiskIndex)) +
  geom_histogram(bins = 12)


#' Classification: WRI:
#' very low: 0.00-1.84
#' low: 1.85-3.20
#' medium: 3.21-5.87
#' high: 5.88-12.88
#' very high: 12.89-100.00

wri_classes <- tribble(
  ~wri_class_id, ~wri_class, ~lower, ~upper,
  1, "very low",  0.00, 1.84,
  2, "low",       1.85, 3.20,
  3, "medium",    3.21, 5.87,
  4, "high",      5.88, 12.88,
  5, "very high",12.89, 100.000   
)

wri <- wri %>% 
  mutate(match_id = 1) %>% 
  full_join(mutate(wri_classes, match_id = 1)) %>% 
  filter(WorldRiskIndex >= lower & WorldRiskIndex <= upper) %>% 
  select(-match_id, -lower, -upper) 


font_family <- "Nunito Sans"
plot_titles <- list(
  title = "Countries' disaster risk from extreme natural 
  events and climate change impacts",
  subtitle = "The **WorldRiskIndex** indicates the disaster risk from extreme natural 
  events and negative climate change impacts for 193 countries in the world. 
  It is calculated as the geometric mean of exposure and vulnerability. 
  **Exposure** represents the extent to which populations are exposed to the 
  impacts of extreme natural events like earthquakes, tsunamis, and floodings.
  **Vulnerability** maps the societal domain 
  and is composed of three dimensions: susceptibility, coping, and adaptation.
  The limits of the risk classes are calculated based on the median of the 
  quintile limits of the past 20 years.",
  caption = "Source: Ruhr University Bochum – Institute for International Law of
Peace and Armed Conflict (IFHV). Visualisation: Ansgar Wolsing"
)

wri %>% 
  ggplot(aes(x = 1, y = WorldRiskIndex)) +
  geom_rect(
    data = wri_classes,
    aes(xmin = -Inf, xmax = Inf, 
        ymin = lower - 0.01, ymax = pmin(max(wri$WorldRiskIndex) + 5, upper), fill = wri_class_id),
    inherit.aes = FALSE, alpha = 0.6
  ) +
  geom_text(
    data = wri_classes,
    aes(x = 1.5, y = lower + 0.25, label = paste(toupper(wri_class), "")),
    inherit.aes = FALSE, angle = 90, hjust = 1, vjust = 1, size = 2,
    family = paste(font_family, "SemiBold"), color = "grey10"
  ) +
  ggbeeswarm::geom_quasirandom(
    shape = 21, color = "white", fill = "black", stroke = 0.3, size = 2) +
  ggrepel::geom_text_repel(
    data = ~subset(., WorldRiskIndex > 30),
    aes(label = Country),
    family = font_family, size = 1.75, point.padding = 1
  ) +
  scale_fill_distiller(type = "seq", palette = "Reds", direction = 1) +
  coord_flip(expand = FALSE, clip = "off") +
  guides(fill = "none") +
  labs(
    title =  plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption =  plot_titles$caption
  ) +
  theme_minimal(base_family = font_family, base_size = 8) +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(color = "grey28"),
    plot.title = element_markdown(
      color = "grey2", size = 12, family = paste(font_family, "SemiBold")),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 0.95, lineheight = 1.2,
      margin = margin(t = 2, b = 8))
  )
ggsave(here(base_path, "07-hazards-wri.png"), width = 6.5, height = 6.5/5 * 4)
