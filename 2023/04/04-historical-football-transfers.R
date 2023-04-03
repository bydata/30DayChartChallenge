library(tidyverse)
library(ggtext)
library(here)
library(packcircles)
library(rvest)

base_path <- here("2023", "04")

# Scrape historical record transfers
transfermarkt_url <- "https://www.transfermarkt.com/transfers/transferrekordehistorie/statistik?land_id=0&ausrichtung=&spielerposition_id=&altersklasse=&w_s=&plus=1"
page <- read_html(transfermarkt_url)
table <- html_node(page, css = "table.items") %>% 
  html_table()

# Prepare the data
transfers <- table %>% 
  select(rank = 1, name = 4, position = 5, age = 6, transfer_season = 7, 
         transfer_date = 8, departure_club = 12, departure_league = 13,
         arrival_club = 16, arrival_league = 17, transfer_fee_eur = 18) %>% 
  mutate(
    transfer_fee_eur = str_remove(transfer_fee_eur, "€"),
    transfer_fee_eur = case_when(
        str_detect(transfer_fee_eur, "m") ~ as.numeric(str_remove(transfer_fee_eur, "m")) * 1e6,
        str_detect(transfer_fee_eur, "k") ~ as.numeric(str_remove(transfer_fee_eur, "k")) * 1e3,
      ),
    transfer_date = lubridate::mdy(transfer_date)
    ) %>% 
  filter(!is.na(rank), !is.na(transfer_fee_eur), transfer_date > as.Date("1984-01-01"))

write_rds(transfers, here(base_path, "record-transfers.rds"))


# Longest reign es record transfer?
transfers %>% 
  select(name, transfer_date, transfer_fee_eur) %>% 
  mutate(reign = lag(transfer_date, 1, default = Sys.Date()) - transfer_date) %>% 
  arrange(desc(reign))


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
            ifelse(str_length(name) > 10, str_replace(name, "\\s", "<br>"), name), 
            "<br>", "**",
            scales::number(transfer_fee_eur, accuracy = 1, scale = 1e-6, suffix = "m"),
            "**")), 
    color = "white", family = "Outfit", fill = NA, label.size = 0) +
  annotate(
    GeomTextBox,
    x = 11400 + 4600, y = 9150 + 3200,
    label = "Zidane held the record for the longest period (2001 to 2009)",
    width = 0.25, family = "Outfit Light", size = 2, hjust = 0,
    fill = NA, box.size = 0
  ) +
  annotate(
    GeomCurve,
    x = 11400 + 5150, xend = 11400 + 3300, y = 9150 + 3200, yend = 9150 + 2900,
    curvature = 0.2, size = 0.25, color = "grey10",
    arrow = arrow(length = unit(1.5, "mm"))
  ) + 
  scale_size_continuous(range = c(1.5, 5.5)) +
  scale_fill_manual(values = c("#2C3E50", "#E67E22", "grey50")) +
  coord_equal(clip = "off") +
  labs(
    title = "All Record-High Transfers<br> in Association Football since
    <span style='color:#E67E22'>Maradona</span> to Napoli",
    caption = "Transfer fees in EUR (at historical prices).<br>
    Source: transfermarkt.de. Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Outfit") + 
  theme(
    plot.background = element_rect(color = "grey84", fill = "grey84"),
    text = element_text(lineheight = 1.1),
    plot.title = element_markdown(hjust = 0.5, family = "Outfit Medium"),
    plot.caption = element_markdown(hjust = 0.5, family = "Outfit Light", size = 7),
    plot.margin = margin(rep(2, 4)),
    legend.position = "none")
ggsave(here(base_path, "04-historical-transfer-records.png"), width = 5.5, height = 4 * 5.5/5)

