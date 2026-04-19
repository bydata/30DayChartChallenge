library(tidyverse)
library(ggtext)
library(readxl)
library(here)

#' Source: Kraftfahrtbundesamt (KBA)
#' https://www.kba.de/DE/Statistik/Produktkatalog/produkte/Fahrzeuge/fz28/fz28_gentab.html?nn=864666

url_registrations <- "https://www.kba.de/SharedDocs/Downloads/DE/Statistik/Fahrzeuge/FZ28/fz28_2026_03.xlsx?__blob=publicationFile&v=3"
file_registrations <- tempfile()
download.file(url_registrations, file_registrations)

# The Excel file has a lot of formatting, let's extract only what we really need

months_mapping <- 1:12
names(months_mapping) <- c(
  "Januar", "Februar", "März", "April", "Mai", "Juni", 
  "Juli", "August", "September", "Oktober", "November", "Dezember")

df_registrations <- readxl::read_xlsx(file_registrations, sheet = "FZ 28.2", skip = 11)
df_registrations_prep <- df_registrations |> 
  select(month = 1, total_registrations = 2, n_bev = 7) |> 
  # Extract the year from the subtotal row labels and fill the following rows
  mutate(
    year = str_extract(month, "Jahr (\\d{4}) insgesamt", group = 1),
    year = as.integer(year)
  ) |> 
  fill(year, .direction = "down") |> 
  filter(!str_detect(month, " insgesamt$")) |> 
  mutate(
    month_num = months_mapping[month],
    month_date = as_date(paste(year, month_num, 1, sep = "-")),
    share_bev = n_bev / total_registrations
  ) |> 
  filter(!is.na(month_num)) |> 
  select(year, month = month_num, month_date, total_registrations, n_bev, share_bev)

# Load scraped Tesla data (starting 2021)
df_tesla <- read_csv(here("2026", "19", "kba-fz28-tesla.csv"))

df_tesla_prep <- df_tesla |> 
  select(year = jahr, month = monat, n_tesla = tesla_neuzulassungen) |> 
  mutate(month_date = as_date(paste(year, month, 1, sep = "-")))

df_registrations_combined <- df_registrations_prep |> 
  left_join(df_tesla_prep, by = join_by(month_date, year, month)) |> 
  mutate(share_tesla = n_tesla / total_registrations)


# Annotations
df_annotations <- data.frame(
  date = as_date(c("2019-07-01", "2020-10-01", "2022-09-01", "2023-09-01", "2023-12-01")),
  y = c(0.05, 0.215, 0.34, 0.30, 0.23), 
  label = c(
    "**June 2020:**<br>Purchase<br>incentive<br>introduced",
    "**Fall 2021:** Rush to secure purchase incentive before scheduled 2021 expiry and government change.",
    "**Dec 2022:** Last chance for maximum incentive before rates decreased in Jan 2023.",
    "**Aug 2023:** Final month for incentive for commercial buyers; private buyers only thereafter.",
    "**Dec 2023:** Immediate termination of all EV subsidies following federal budget ruling."
  )
)


df_registrations_combined |> 
  ggplot(aes(month_date, share_bev)) +
  geom_col(
    fill = alpha("grey20", 0.1),
    width = 36
  ) +
  geom_step(
    direction = "mid"
  ) +
  geom_col(
    aes(y = share_tesla),
    fill = "black"
  ) +
  annotate(
    GeomTextBox,
    x = df_annotations$date,
    y = df_annotations$y,
    label = df_annotations$label,
    width = 0.25, fill = NA, box.size = 0, 
    size = 2.5, family = "Instrument Sans", hjust = 0, vjust = 0
  ) +
  annotate(
    "richtext",
    x = as_date("2021-03-01"),
    y = 0.08,
    label = "Share of newly<br>registered **Tesla cars**<br>(starting from Jan 2021<sup>1</sup>)",
    family = "Instrument Sans", fill = "#FFFFFF88", label.size = 0,
    size = 2.5, hjust = 0
  ) +
  annotate(
    GeomCurve,
    x = as_date("2021-04-01"), xend = as_date("2021-03-01"),
    y = 0.06, yend = 0.02,
    linewidth = 0.2,
    arrow = arrow(angle = 25, length = unit(0.2, "cm"), type = "closed")
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "BEV sales driven by government subsidy deadlines and customer anticipations",
    subtitle = "Monthly share of battery electric vehicles (BEV)
      among new car registrations in Germany (in %)",
    caption = "*<sup>1</sup>Data split by manufacturer before 2021 is available, but scattered in monthly PDF files.*
      <br><br>
      **Source:** Kraftfahrtbundesamt (March 2026).
    **Visualization:** Ansgar Wolsing",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Instrument Sans", paper = "white") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25),
    panel.grid.minor.y = element_line(linewidth = 0.1),
    plot.title = element_text(family = "Instrument Sans SemiBold"),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      width = 1, lineheight = 1.33, margin = margin(t = 1, b = 10)),
    plot.caption = element_textbox(width = 0.95, lineheight = 1.1),
    plot.caption.position = "plot"
  )
ggsave(here("2026", "19", "19-evolution.png"), width = 7.2, height = 6)

# df_registrations_combined |> 
#   mutate(share_tesla_bev = share_tesla / share_bev) |> 
#   ggplot(aes(month_date, share_tesla_bev)) +
#   geom_point() +
#   geom_smooth(span = 0.5)
