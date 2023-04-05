library(tidyverse)
library(worldfootballR)
library(ggtext)
library(here)

base_path <- here("2023", "05")

countries <- c("England", "Spain", "Germany", "Italy", "France")
start_years <- 2010:2022
countries_start_years <- expand.grid(country_name = countries, start_year = start_years)

start_years <- 2000:2009
countries_start_years <- expand.grid(country_name = countries, start_year = start_years)


if (FALSE) {
  team_urls <- pmap(countries_start_years, tm_league_team_urls)
  team_urls_top5 <- reduce(team_urls, c)
  write_rds(team_urls_top5, here(base_path, "team_urls_top5-2010-2022.rds"))
  
  team_urls_2 <- pmap(countries_start_years, tm_league_team_urls)
  team_urls_top5_2 <- reduce(team_urls_2, c)
  write_rds(team_urls_top5_2, here(base_path, "team_urls_top5-2000-2009.rds"))
  
  team_urls_top5_2010 <- team_urls_top5[str_detect(team_urls_top5, "/2010")]
  team_urls_top5_2022 <- team_urls_top5[str_detect(team_urls_top5, "/2022")]
  
  team_urls_top5_2002 <- team_urls_top5_2[str_detect(team_urls_top5_2, "/2002")]
  
  transfers <- map(c(team_urls_top5_2010, team_urls_top5_2022), tm_team_transfers, transfer_window = "all")
  write_rds(transfers, here(base_path, "transfers-2010+2022.rds"))
  
  transfers_2002 <- map(team_urls_top5_2002, tm_team_transfers, transfer_window = "all")
  write_rds(transfers_2002, here(base_path, "transfers-2002.rds"))
} else {
  transfers <- read_rds(here(base_path, "data", "transfers-2022-23.rds"))
}

transfers <- read_rds(here(base_path, "transfers-2010+2022.rds"))
transfers_2002 <- read_rds(here(base_path, "transfers-2002.rds"))
  
transfers_df <- bind_rows(transfers)
transfers_df_2002 <- bind_rows(transfers_2002)

transfers_df <- bind_rows(transfers_df, transfers_df_2002)

league_pal <- c("grey94", "grey70", "grey86", "grey78", "#FF6003")
names(league_pal) <- c("Serie A", "Bundesliga", "Ligue 1", "LaLiga", "Premier League")

leagues <- c("Serie A", "Bundesliga", "Ligue 1", "LaLiga", "Premier League")
names(leagues) <- c("Italy", "Germany", "France", "Spain", "England")

plot_titles <- list(
  "title" = sprintf(
    "<span style='color: %s'>Premier League</span> has left the other 
    top 5 leagues far behind", league_pal["Premier League"]),
  "subtitle" = "Total transfer fees in 2002-03 and 2022-23 (in EUR)",
  "caption" = "Source: transfermarkt.de. Visualisation: Ansgar Wolsing"
  )

#' NB: The league column reflects the current league of the teams at the time of
#' PULLING the data (e.g. 2. Bundesliga)

transfers_df %>% 
  mutate(country = ifelse(team_name == "Chievo Verona", "Italy", country),
         league = leagues[country]) %>% 
  filter(season %in% c("2002", "2022")) %>%
  filter(transfer_type == "Arrivals") %>% 
  mutate(season = case_when(season == "2002" ~ "2002-03", season == "2022" ~ "2022-23")) %>% 
  group_by(league, season) %>% 
  summarize(total_fees = sum(transfer_fee, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(season, total_fees, color = league, group = league)) +
  # "sliding" vertical lines
  geom_segment(
    data = ~group_by(., season) %>% summarize(y = min(total_fees) - 50e6,
                                           yend = max(total_fees) + 50e6),
    aes(x = season, xend = season,
        y = 0, yend = yend),
    inherit.aes = FALSE, col = "grey70", linewidth = 0.8
  ) +
  geom_line(
    aes(linewidth = league == "Premier League"),
    # aes(linewidth = ifelse(league == "Premier League", 2, 0.5)),
    alpha = 0.8, 
    arrow = arrow(type = "closed", angle = 20, length = unit(1.5, "mm"))) +
  geom_point(
    data = ~subset(., season == min(season)),
    size = 1.5
  ) +
  ggrepel::geom_text_repel(
    data = ~subset(., season == min(season)),
    aes(label = scales::number(total_fees, accuracy = 1, scale = 1e-6, suffix = "M")),
    hjust = 1, nudge_x = -0.02, family = "Avenir Next Condensed", size = 2, fontface = "bold",
    direction = "y", box.padding = 0
  ) +
  geom_richtext(
    data = ~subset(., season == max(season)),
    aes(
      y = total_fees - ifelse(league == "Bundesliga", 85e6 , 0),
      label = sprintf("<b>%s</b> %s", 
                        scales::number(total_fees, accuracy = 1, 
                                       scale = 1e-6, suffix = "M", big.mark = ","), league)),
    hjust = 0, nudge_x = 0.02, family = "Avenir Next Condensed", fill = NA, label.size = 0,
    size = 2.5
  ) +
  geom_richtext(
    data = ~subset(., season == max(season) & league == "Premier League"),
    aes(label = sprintf("<b>%s</b> %s", scales::number(total_fees, accuracy = 1, 
                                                       scale = 1e-6, suffix = "M", big.mark = ","), league)),
    hjust = 0, nudge_x = 0.02, family = "Avenir Next Condensed", fill = NA, label.size = 0.2,
    size = 2.5
  ) +
  annotate(
    "text", x = 1.45, y = 2.0e9, label = "\U25B2 817%", color = league_pal[5],
    family = "Avenir Next Condensed Medium", size = 4
  ) +
  scale_x_discrete(position = "top", expand = expansion(add = c(0.1, 0.33))) +
  scale_color_manual(values = league_pal) +
  #scale_size_manual(values = c("FALSE" = 0.5, "TRUE" = 1.2)) +
  scale_linewidth_discrete(range = c(0.5, 1)) +
  coord_cartesian(clip = "off") +
  guides(color = "none", size = "none", linewidth = "none") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  theme_minimal(base_family = "Avenir Next Condensed") +
  theme(
    plot.background = element_rect(color = "#555B6E", fill = "#555B6E"),
    text = element_text(color = "grey95"),
    axis.text.x.top = element_text(family = "Avenir Next Condensed Demi Bold", color = "grey90"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 4, b = 4, l = 4, r = 14),
    # panel.grid.major.x = element_line(color = "grey80", linewidth = 0.8),
    plot.title = element_markdown(lineheight = 1, color = "white",
                                  family = "Avenir Next Condensed Medium")
  )
ggsave(here(base_path, "05-slope-transfers.png"), width = 4, height = 5)

