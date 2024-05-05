library(tidyverse)
library(ggtext)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(here)

base_path <- here("2024", "04")

colors <- c("#FBFAFC", "#FFFFFF")
gradient_fill <- grid::linearGradient(colors, group = FALSE)

theme_set(
  theme_minimal(base_family = "Libre Franklin") +
    theme(
      plot.background = element_rect(color = NA, fill = gradient_fill),
      text = element_text(color = "#090909"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(
        color = "grey8",
        family = "Libre Franklin SemiBold", hjust = 0, size = 14,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        hjust = 0, color = "grey35", 
        margin = margin(b = 8)),
      plot.caption = element_markdown(hjust = 0),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid = element_blank(),
      # panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05)
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(
        family = "Libre Franklin SemiBold", size = 10, color = "#3B1448",
        margin = margin(t = 4, b = 1))
    )
)


# Source: https://www.federalreserve.gov/releases/z1/dataviz/dfa/index.html
df <- read_csv(here(base_path, "dfa-networth-shares.csv"),
               name_repair = janitor::make_clean_names)


# Prepare the data for the plot with shifting intervals
df_plot <- df |> 
  # select the latest data
  filter(date == max(date)) |> 
  select(date, category, net_worth) |>
  mutate(
    category = fct_inorder(category),
    category = factor(
      category, 
      labels = c("Top 0.1%", "Remaining Top 1%", "Next 9%", "Next 40%", "Bottom 50%")), 
    net_worth_cumul = cumsum(net_worth),
    net_worth_cumul_lag = lag(net_worth_cumul, default = 0),
    net_worth_remaining = 100 - net_worth_cumul_lag - net_worth,
    net_worth_remaining = ifelse(net_worth_remaining < 0, 0, net_worth_remaining)) |> 
  select(category, net_worth_cumul_lag, net_worth, net_worth_remaining) |> 
  pivot_longer(cols = c(net_worth_cumul_lag, net_worth, net_worth_remaining),
               names_to = "group", values_to = "value") |> 
  mutate(group = factor(
    group, 
    levels = c("net_worth_cumul_lag", "net_worth", "net_worth_remaining"),
    labels = c("previous", "this", "remaining")))


df_plot |> 
  ggplot(aes(fill = group, values = value)) +
  geom_waffle(na.rm = FALSE, make_proportional = TRUE, color = "white", 
              size = 0.4) +
  scale_fill_manual(values = c("previous" = "grey80", "this" = "#3B1448", "remaining" = "grey80")) +
  coord_equal() +
  facet_wrap(vars(category), labeller = as_labeller(toupper)) +
  guides(fill = "none") +
  labs(
    title = "The top 0.1% have 5 times more total wealth than the bottom 50% combined",
    subtitle = "Net worth distribution in the U.S. (2023 Q3)",
    caption = "Source: Federal Reserve. Visualization: Ansgar Wolsing"
  )
ggsave(here(base_path, "04-waffle.png"), width = 6, height = 4, scale = 1.5)
