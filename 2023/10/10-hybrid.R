library(tidyverse)
library(ggtext)
library(here)
library(lubridate)
library(ggstream)

base_path <- here("2023", "10")


#' Source: Kraftfahrtbundesamt
#' https://www.kba.de/DE/Statistik/Fahrzeuge/Neuzulassungen/Umwelt/n_umwelt_node.html
#' Excel - download from the link
#' PDF:  https://www.kba.de/SharedDocs/Downloads/DE/Statistik/Fahrzeuge/FZ14/fz14_2021_pdf.pdf?__blob=publicationFile&v=7
#' 

# Download report Excel file
report_url <- "https://www.kba.de/SharedDocs/Downloads/DE/Statistik/Fahrzeuge/FZ28/fz28_2023_02.xlsx?__blob=publicationFile&v=6"
report_file <- here(base_path, "fz28.xlsx")
download.file(report_url, destfile = report_file)

column_names <- c(
  "monat", "total", "alternativer_antrieb_total", "alternativer_antrieb_anteil",
  "elektro_total", "elektro_anteil", 
  "elektro_elektro", "elektro_brennstoffzelle", "elektro_plugin_hybrid",
  "hybrid_ohne_plugin_total", "hybrid_ohne_plugin_vollhybrid", 
  "hybrid_ohne_plugin_benzinhybrid", "hybrid_ohne_plugin_benzin_vollhybrid",
  "hybrid_ohne_plugin_dieselhybrid", "hybrid_ohne_plugin_diesel_vollhybrid",
  "gas_total", "gas_wasserstoff"
)
report <- readxl::read_excel(report_file, sheet = "FZ 28.2", skip = 12,
                             col_names = column_names, na = "-")
glimpse(report)

# Prepare dataset

month_name_de <- c(
  "Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", 
  "September", "Oktober", "November", "Dezember" 
)
month_name_en <- month.name
names(month_name_en) <- month_name_de

report_prep <- report %>% 
  # delete the last 2 lines
  head(n = nrow(.) - 2) %>% 
  mutate(jahr = str_extract(monat, "20\\d{2}"),
         jahr = as.numeric(jahr)) %>% 
  fill(jahr, .direction = "down") %>% 
  filter(!str_detect(monat, "Jahr .+ insgesamt")) %>% 
  mutate(
    month = month_name_en[monat],
    date = as_date(paste(jahr, month, "01", sep = "-"))) %>% 
  select(date, jahr, everything(), -c(month, monat))

report_prep %>% 
  ggplot(aes(date)) +
  geom_line(aes(y = total)) +
  geom_line(aes(y = alternativer_antrieb_total, col = "Alternativer Antrieb gesamt")) +
  geom_line(aes(y = elektro_plugin_hybrid, col = "Plug-in-Hybrid")) +
  geom_line(aes(y = hybrid_ohne_plugin_total, col = "Hybrid gesamt")) +
  theme(legend.position = "bottom")

df_plot <- report_prep %>% 
  transmute(
    date, jahr, 
    total,
    `Internal combustion engine` = total - alternativer_antrieb_total,
    `Electric` = elektro_total,
    `Electric hybrid (w/o plug-in-hybrid)` = hybrid_ohne_plugin_total,
    `Natural gas` = gas_total
  ) %>% 
  select(-total) %>% 
  pivot_longer(cols = -c(date, jahr), names_to = "type", values_to = "n") %>% 
  mutate(type = factor(type, levels = c("Internal combustion engine", "Natural gas",
                                        "Electric", 
                                        "Electric hybrid (w/o plug-in-hybrid)")))

custom_pal <- c("grey15", "#947BD3", "#5E4AE3", "#05299E")

df_plot %>% 
  ggplot(aes(date, n)) +
  geom_stream(aes(fill = type), bw = 0.45, extra_span = 0.01, type = "ridge") +
  scale_fill_manual(values = custom_pal) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

df_plot %>% 
  ggplot(aes(date, n)) +
  geom_area(aes(fill = type), col = "white", size = 0.2) +
  scale_fill_manual(values = custom_pal) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )


# Custom textbox function
geom_textbox2 <- function(x, y, label, jahr, box.size = 0,
                          hjust = 0, halign = 0, width = 1.1, size = 2.5, ...) {
  geom_textbox(
    data = data.frame(x = x, y = y, label = label, jahr = jahr),
    aes(x, y, label = label),
    hjust = hjust, box.size = box.size, fill = "grey98", size = size, halign = halign, width = width,
    family = "Roboto Condensed", box.padding	= unit(1.5, "mm")
  )
}

year_levels <- c(as.character(2016:2022), "'23")

df_plot %>% 
  mutate(jahr = ifelse(jahr == 2023, "'23", as.character(jahr)),
         jahr = factor(jahr, levels = year_levels),
         month = month(date)) %>% 
  ggplot(aes(month, n)) +
  geom_col(aes(fill = type), width = 1) + 
  # annotations
  geom_textbox2(
    x = 2, y = 350e3, jahr = factor("2022", levels = year_levels), 
    label = "**Dec 2022:**<br>More EVs bought in expectation that 
      government subsidy would be reduced from 9,000 to 6,750 EUR in 2023?"
  ) +
  geom_textbox2(
    x = 3, y = 340e3, jahr = factor("2020", levels = year_levels), 
    label = "**Apr 2020:**<br>Coronavirus pandemic affected car market"
  ) +
  geom_textbox2(
    x = 8, y = 380e3, jahr = factor("2018", levels = year_levels), 
    label = "**Sep 2018:**<br>Some OEM had backlog of cars due to new test requirement."
  ) +
  geom_textbox2(
    x = 6.5, y = 170e3, jahr = factor("2016", levels = year_levels), 
    label = "**INTERNAL COMBUSTION ENGINE**",
    hjust = 0.5, halign = 0.5, width = 0.8, size = 3
  ) +
  geom_textbox2(
    x = 6.5, y = 65e3, jahr = factor("2022", levels = year_levels), 
    label = "**ELECTRIC**",
    hjust = 0.5, halign = 0.5, width = 0.6, size = 3
  ) +
  geom_textbox2(
    x = 6.5, y = 15e3, jahr = factor("2022", levels = year_levels), 
    label = "**HYBRID**",
    hjust = 0.5, halign = 0.5, width = 0.6, size = 3
  ) +
  # scale_x_date(expand = c(0, 0), breaks = "1 year", date_labels = "%Y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  scale_fill_manual(values = custom_pal) +
  coord_cartesian(clip = "off") +
  facet_grid(cols = vars(jahr), scales = "free_x", space = "free_x", switch = "x") +
  labs(
    title = "New car registrations in Germany by type of engine",
    caption = "Source: Kraftfahrtbundesamt, February 2023. Visualisation: Ansgar Wolsing",
    x = NULL, y = "Number of new car registrations per month",
    fill = NULL
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1, "mm"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.title.position = "plot",
    strip.text.x = element_text(
      margin = margin(t = -1), size = 10
    ) 
  )
ggsave(here(base_path, "11-hybrid-vehicles-de.png"), width = 8, height = 6.4)

