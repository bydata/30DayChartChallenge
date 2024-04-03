library(tidyverse)
library(ggtext)
library(here)
library(packcircles)
library(rvest)

base_path <- here("2024", "03")

# Scrape historical record transfers
transfermarkt_url <- "https://www.transfermarkt.com/transfers/transferrekordehistorie/statistik?land_id=0&ausrichtung=&spielerposition_id=&altersklasse=&w_s=&plus=1"
page <- read_html(transfermarkt_url)
table <- html_node(page, css = "table.items") |> 
  html_table()

# Prepare the data
transfers <- table |> 
  select(rank = 1, name = 4, position = 5, age = 6, transfer_season = 7, 
         transfer_date = 8, departure_club = 12, departure_league = 13,
         arrival_club = 16, arrival_league = 17, transfer_fee_eur = 18) |> 
  mutate(
    transfer_fee_eur = str_remove(transfer_fee_eur, "€"),
    transfer_fee_eur = case_when(
        str_detect(transfer_fee_eur, "m") ~ as.numeric(str_remove(transfer_fee_eur, "m")) * 1e6,
        str_detect(transfer_fee_eur, "k") ~ as.numeric(str_remove(transfer_fee_eur, "k")) * 1e3,
      ),
    transfer_date = lubridate::mdy(transfer_date)
    ) |> 
  filter(!is.na(rank), !is.na(transfer_fee_eur), transfer_date > as.Date("1984-01-01"))

write_rds(transfers, here(base_path, "record-transfers.rds"))


# ggplot2 theme
colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)
theme_set(
  theme_minimal(base_family = "Libre Franklin Light") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.text = element_text(family = "Source Code Pro"),
      axis.line.x = element_line(linewidth = 0.33),
      plot.title = element_textbox(
        color = "grey8", width = 1, lineheight = 1.1,
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_textbox(
        hjust = 0, color = "grey35", lineheight = 1.25, width = 1,
        margin = margin(b = 6)),
      plot.caption = element_markdown(),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      # panel.grid.major.x = element_line(color = "grey70", linewidth = 0.15),
      # panel.grid.minor.x = element_line(color = "grey70", linewidth = 0.08),
      # panel.grid.major.y = element_blank(),
      # panel.grid.minor.y = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(
        family = "Libre Franklin Medium", size = 8, color = "grey35", hjust = 0,
        margin = margin(t = 3, b = 1))
    )
)


# Longest reign es record transfer?
transfers |> 
  select(name, transfer_date, transfer_fee_eur) |> 
  mutate(reign = lag(transfer_date, 1, default = Sys.Date()) - transfer_date) |> 
  arrange(desc(reign))


## CIRCLEPACK VERSION (new style) ----------------------------------------------



## https://r-graph-gallery.com/307-add-space-in-circle-packing.html
# Generate the layout
packing <- circleProgressiveLayout(transfers$transfer_fee_eur, sizetype = "area")
packing$radius <- 0.95 * packing$radius
transfers_packing <- bind_cols(packing, transfers)
dat.gg <- circleLayoutVertices(transfers_packing, npoints = 50, idcol = "rank")

ggplot() + 
  geom_polygon(
    data = dat.gg, 
    aes(x, y, group = id, 
        fill = case_when(id %in% c(1, 15) ~ as.character(id), TRUE ~ "other")), 
    colour = "white", size = 0.3) +
  geom_richtext(
    data = transfers_packing, 
    aes(x, y, size = transfer_fee_eur, 
        label = 
          paste0(
            ifelse(str_length(name) > 8, str_replace(name, "\\s", "<br>"), name), 
            "<br>", "<b style='font-family:\"Libre Franklin\"'>",
            scales::number(transfer_fee_eur, accuracy = 1, scale = 1e-6, suffix = "m"),
            "</b>")), 
    color = "white", family = "Libre Franklin Medium", fill = NA, label.size = 0) +
  # Annotation: Zidane
  annotate(
    GeomTextBox,
    x = 16000, y = 12350,
    label = "Zidane held the record for the longest period (2001 to 2009)",
    width = 0.15, family = "Libre Franklin", size = 2, hjust = 0,
    fill = NA, box.size = 0
  ) +
  annotate(
    GeomCurve,
    x = 16550, xend = 14700, y = 12350, yend = 12050,
    curvature = 0.2, linewidth = 0.25, color = "grey10",
    arrow = arrow(length = unit(1.5, "mm"))
  ) + 
  # Curve to show the temporal direction
  annotate(
    "text",
    x = -20000, y = 8800, label = "time",
    family = "Libre Franklin SemiBold", size = 3, hjust = 0, color = "#666666",
    angle = 45
  ) +
  annotate(
    GeomCurve,
    x = -15000, xend = -21500, y = 11500, yend = 5000,
    curvature = 0.2, linewidth = 0.5, color = "#666666",
    arrow = arrow(length = unit(1.5, "mm"))
  ) + 
  scale_size_continuous(range = c(1.5, 5.5)) +
  scale_fill_manual(values = c("#03DAC5", "#6200EE", "#78909C")) +
  coord_equal(clip = "off") +
  labs(
    title = "All record-high transfers at their time
    since Maradona to Napoli",
    subtitle = "Each transfer in association football since 1984 that has set a new record",
    caption = "Transfer fees in EUR (at historical prices).<br><br>
    Source: transfermarkt.de. Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Libre Franklin") + 
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(lineheight = 1.1),
    plot.title = element_markdown(hjust = 0.5, family = "Libre Franklin SemiBold"),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, family = "Libre Franklin Light", size = 7),
    plot.margin = margin(rep(2, 4)),
    legend.position = "none")
ggsave(here(base_path, "03-makeover-historical-transfer-records.png"), 
       width = 4, height = 4, scale = 1.4)


## BAR CHART VERSION (new) ----------------------------------------------


transfers |> 
  mutate(name = fct_reorder(name, -transfer_fee_eur)) |> 
  ggplot(aes(x = 1, transfer_fee_eur)) +
  # Light grey background for the bars
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
    fill = "grey93") +
  geom_col(
    aes(fill = as.character(ifelse(rank %in% c(1, 15), rank, 99))), 
    colour = "white", size = 0.3) +
  geom_text(
    aes(
      label = scales::number(
        transfer_fee_eur, scale_cut = scales::cut_short_scale(), accuracy = 1)
      ),
    family = "Libre Franklin SemiBold", size = 2, hjust = 1.2, color = "white"
  ) +
  scale_y_continuous(
    labels = scales::number_format(scale_cut = scales::cut_short_scale()),
    breaks = seq(0, 200e6, 50e6)) +
  # scale_fill_manual(values = c("#1d00db", "#db00be")) +
  scale_fill_manual(values = c("#03DAC5", "#6200EE", "#78909C")) +
  # scale_color_manual(values = c("grey2", "white")) +
  coord_flip(expand = FALSE) +
  facet_wrap(vars(name), ncol = 1) +
  labs(
    title = "All record-high transfers at their time
    since Maradona to Napoli",
    subtitle = "Each transfer in association football since 1984 that has set a new record.
    Transfer fees at historical prices (in EUR).",
    caption = "Source: transfermarkt.de. Visualisation: Ansgar Wolsing"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    plot.margin = margin(t = 2, b = 4, l = 6, r = 6),
    panel.spacing.y = unit(0, "mm")
  )
ggsave(here(base_path, "03-makeover-transfer-records.png"), 
       width = 2, height = 3, scale = 2.2)
