library(tidyverse)
library(ggtext)
library(grid)
library(here)

base_path <- here("2024", "22")


#' Source: Kraftfahrtbundesamt
#' https://www.kba.de/DE/Statistik/Fahrzeuge/Neuzulassungen/Umwelt/n_umwelt_node.html

# Download report Excel file
report_url <- "https://www.kba.de/SharedDocs/Downloads/DE/Statistik/Fahrzeuge/FZ28/fz28_2024_03.xlsx?__blob=publicationFile&v=4"
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
    date = as_date(paste(jahr, month, "01", sep = "-")),
    elektro_total_anteil = elektro_total / total * 100,
    elektro_elektro_anteil = elektro_elektro / total * 100) %>% 
  select(date, jahr, everything(), -c(month, monat))


# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 7, hjust = 1),
      axis.text = element_text(family = "Source Code Pro", size = 6),
      axis.text.x = element_blank(),
      axis.text.y = element_text(),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 7.5, width = 0.97, lineheight = 1.2,
        margin = margin(b = 0)),
      plot.caption = element_markdown(hjust = 0, lineheight = 1.1, size = 6),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 7, color = "grey25",
        margin = margin(t = -1, b = 1)),
      panel.spacing.x = unit(0, "mm") 
    )
)


ragg::agg_png(here(base_path, "22-mobility.png"), width = 5, height = 5, 
              res = 300, units = "in")
report_prep |> 
  arrange(date) |> 
  transmute(
    date, jahr, elektro_total, elektro_elektro,
    month = month(date),
    # calculate YoY comparisons
    across(c(elektro_total, elektro_elektro), 
           function(x) x / lag(x, 12) - 1, .names = "{.col}_yoy")) |> 
  filter(!is.na(elektro_elektro_yoy)) |> 
  ggplot(aes(month)) +
  geom_col(aes(y = elektro_elektro_yoy, fill = elektro_elektro_yoy > 0),
           alpha = 0.9, show.legend = FALSE) +
  scale_x_continuous(expand = expansion(add = c(1.75, 1.75))) +
  scale_y_continuous(
    labels = scales::percent_format(), breaks = seq(-10, 10, 1)) +
  scale_fill_manual(values = c("#d602ee", "#021aee")) +
  coord_cartesian(ylim = c(NA, 7)) +
  facet_grid(cols = vars(jahr), space = "free_x", switch = "x") +
  labs(
    title = "How to destroy a trend",
    subtitle = "For years, the number of battery electric car registrations in 
    Germany only knew one direction -
    <span style='color:#3d00e0;font-family:\"Libre Franklin SemiBold\"'>up</span>. 
    When the federal government abruptly cancelled the EV purchase subsidies for 
    businesses in September 2023, the
    <span style='color:#d602ee;font-family:\"Libre Franklin SemiBold\"'>
    registration numbers plummeted immediately</span>. 
    Later in 2023, the government also ended the subsidies for private customers.
    <br><br>
    <span style='font-size:7pt;font-family:\"Libre Franklin SemiBold\"'>\U2193
    %-Change in battery electric car registrations year-over-year</span>",
    caption = "Source: Kraftfahrbundesamt (March 2024).
    Visualization: Ansgar Wolsing",
    x = NULL, y = NULL
  )
grid.text(
  label = "Subsidies cancelled\nfor businesses",
  x = 0.84, y = 0.29,
  hjust = 0, vjust = 0,
  gp = gpar(fontfamily = "Libre Franklin", lineheight = 0.8, cex = 0.5)
)
grid.lines(x = c(0.845, 0.836), y = c(0.285, 0.16), 
           gp = gpar(lwd = 0.3),
           arrow = arrow(length = unit(1, "mm")))
grid.text(
  label = "... and for\neveryone",
  x = 0.89, y = 0.19,
  hjust = 0, vjust = 0,
  gp = gpar(fontfamily = "Libre Franklin", lineheight = 0.8, cex = 0.5)
)
grid.lines(x = c(0.885, 0.86), y = c(0.18, 0.145), 
           gp = gpar(lwd = 0.3),
           arrow = arrow(length = unit(1, "mm")))
dev.off()
