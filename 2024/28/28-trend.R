library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "28")


#' Source: Destatis
#' https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Industrie-Verarbeitendes-Gewerbe/Tabellen/Lkw-Maut-Fahrleistungsindex-Daten.html
#' https://www.balm.bund.de/SharedDocs/Pressemitteilungen/DE/2020/2020_04_09_PM.html

# Download report Excel file
report_url <- "https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Industrie-Verarbeitendes-Gewerbe/Tabellen/Lkw-Maut-Fahrleistungsindex-Daten.xlsx?__blob=publicationFile"
report_file <- here(base_path, "Lkw-Maut-Fahrleistungsindex-Daten.xlsx")
download.file(report_url, destfile = report_file)

report <- readxl::read_excel(report_file, sheet = "Daten", skip = 5, na = "-")
report <- janitor::clean_names(report) |> 
  mutate(datum = as_date(datum))
glimpse(report)


# Maximum values
report |> 
  slice_max(order_by = unbereinigt, n = 10)

# Minimum values
report |> 
  slice_min(order_by = unbereinigt, n = 10)

# ... by weekday
report |> 
  group_by(wochentag) |> 
  summarize(mean_ksb = mean(unbereinigt))


# Create rolling mean and confidence intervals
rolling_interval_days <- 90
report$rolling_mean <- rollmean(
  report$kalender_und_saisonbereinigt_ksb, 
  k = rolling_interval_days, align = "center", fill = NA)

report$sd <- rollapply(report$kalender_und_saisonbereinigt_ksb, 
                       width = rolling_interval_days, FUN = sd, fill = NA)
report$se <- report$sd / sqrt(rolling_interval_days)  
report$lower_ci <- report$rolling_mean - 1.96 * report$se  
report$upper_ci <- report$rolling_mean + 1.96 * report$se  


# Custom theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = gradient_fill, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title.x.top = element_markdown(size = 7, hjust = 0),
      axis.title.y = element_text(size = 7, hjust = 0.8),
      axis.text = element_text(family = "Source Code Pro", size = 6),
      axis.text.x.top = element_text(hjust = 0.5, vjust = -0.2),
      axis.text.y = element_text(),
      # axis.line.x = element_line(linewidth = 0.15),
      plot.title = element_markdown(
        color = "grey8", lineheight = 1.2,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", size = 7.5, width = 1, lineheight = 1.2,
        margin = margin(b = 8)),
      plot.caption = element_markdown(hjust = 0, lineheight = 1.1, size = 7),
      plot.margin = margin(rep(4, 4)),
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.key.height = unit(3, "mm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 10, color = "grey35",
        margin = margin(t = 4, b = 1))
    )
)


# Annotations
annotations <- tribble(
  ~date, ~y, ~label,
  "2009-01-01", 120, "Financial crisis hit industry production<br>**2009<br>\U2193 **",
  "2020-03-20", 70, "**\U2191<br>2020**<br>Coronavirus pandemic"
)
annotations$date <- as_date(annotations$date)
annotations$label <- str_replace_all(str_wrap(annotations$label, 24), "\\n", "<br>")

report |> 
  ggplot(aes(datum, kalender_und_saisonbereinigt_ksb)) +
  geom_point(
    aes(color = "Daily values, calendar and seasonally adjusted"),
    size = 0.05) +
  geom_smooth(
    aes(color = "Smoothed trend"),
    method = "loess", linewidth = 0.3,
    span = 0.07, se = TRUE, fill = "#d602ee66") +
  # Annotations
  geom_richtext(
    data = annotations,
    aes(date, y, label = label),
    family = "Libre Franklin", hjust = 0, vjust = 1, size = 2.5,
    label.size = 0, fill = NA, lineheight = 1.15
  ) +
  scale_x_date(
    date_breaks = "2 years", date_labels = "%Y",
    expand = expansion(mult = c(0.0, 0.01))) +
  scale_color_manual(
    values = c("Smoothed trend" = "#d602ee", 
               "Daily values, calendar and seasonally adjusted" = "grey80")) +
  guides(color = guide_legend(override.aes = list(size = 2, linewidth = 0.5))) +
  labs(
    title = "Truck toll mileage as an early indicator for<br>
    the development of industrial production",
    subtitle = "The index is calculated from digital process data from the 
    lorry toll collection in Germany and reflects the mileage of trucks
    (7.5+ tons) on motorways. Index values are relative to the 
    2021 average (=100).",
    caption = "Source: Destatis, Kraftfahrtbundesamt. Visualization: Ansgar Wolsing",
    x = NULL, y = "Truck toll mileage index",
    color = NULL
  )
ggsave(here(base_path, "28-trend-lowess.png"), width = 5, height = 5)


report |> 
  ggplot(aes(datum, kalender_und_saisonbereinigt_ksb)) +
  geom_point(
    aes(color = "Calendar and seasonal adjusted daily values"),
    size = 0.05) +
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci),
    fill = "#d602ee66") +
  geom_line(
    aes(y = rolling_mean, color = "Smoothed trend"),
    linewidth = 0.3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    values = c("Smoothed trend" = "#d602ee", 
               "Calendar and seasonal adjusted daily values" = "grey80")) +
  guides(color = guide_legend(override.aes = list(size = 2, linewidth = 0.5))) +
  labs(
    title = "",
    subtitle = "",
    caption = "Source: Destatis, Kraftfahrtbundesamt. Visualization: Ansgar Wolsing",
    x = NULL, color = NULL
  )
ggsave(here(base_path, "28-trend-rolling.png"), width = 5, height = 5)

