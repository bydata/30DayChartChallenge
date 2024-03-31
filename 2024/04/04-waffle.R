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
      axis.text = element_text(family = "Source Code Pro"),
      axis.line.x = element_line(linewidth = 0.33),
      plot.title = element_text(
        color = "grey8",
        family = "Libre Franklin SemiBold", hjust = 0, size = 16,
        margin = margin(t = 4, b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        hjust = 0, color = "grey35", 
        margin = margin(b = 8)),
      plot.margin = margin(rep(4, 4)),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey70", linewidth = 0.15),
      # panel.grid.minor.y = element_line(color = "grey70", linewidth = 0.05)
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(
        family = "Libre Franklin SemiBold", size = 10, color = "grey35",
        margin = margin(t = 4, b = 1))
    )
)

# Source: https://www.federalreserve.gov/releases/z1/dataviz/dfa/index.html
df <- read_csv(here(base_path, "dfa-networth-shares.csv"),
               name_repair = janitor::make_clean_names)

df |> 
  filter(date == max(date)) |> 
  summarize(sum(net_worth))

df |> 
  # select the latest data
  filter(date == max(date)) |> 
  select(date, category, net_worth) |>
  mutate(
    category = fct_inorder(category),
    net_worth_rounded = round(net_worth)) |> 
  # summarize(sum(net_worth_rounded))
  ggplot(aes(fill = category, values = net_worth_rounded)) +
  geom_waffle(na.rm = FALSE, make_proportional = TRUE) +
  # scale_fill_manual(values = pal_office) +
  coord_equal() +
  labs()

df |> 
  # select the latest data
  filter(date == max(date)) |> 
  select(date, category, net_worth) |>
  mutate(
    category = fct_inorder(category),
    category = factor(
      category, 
      labels = c("Top 0.1%", "Remaining Top 1%", "Next 9%", "Next 40%", "Bottom 50%")), 
    net_worth_rounded = round(net_worth),
    net_worth_rounded_remaining = 100 - net_worth_rounded) |> 
  pivot_longer(cols = c(net_worth_rounded, net_worth_rounded_remaining),
               names_to = "group", values_to = "value") |> 
  # View()
  ggplot(aes(fill = group, values = value)) +
  geom_waffle(na.rm = FALSE, make_proportional = TRUE, color = "white", size = 0.3) +
  scale_fill_manual(values = c("#3B1448", "grey80")) +
  coord_equal() +
  facet_wrap(vars(category), labeller = as_labeller(toupper)) +
  labs()



df_plot <- df |> 
  # select the latest data
  filter(date == max(date)) |> 
  select(date, category, net_worth) |>
  mutate(
    category = fct_inorder(category),
    category = factor(
      category, 
      labels = c("Top 0.1%", "Remaining Top 1%", "Next 9%", "Next 40%", "Bottom 50%")), 
    net_worth_rounded = round(net_worth),
    net_worth_rounded_cumul = cumsum(net_worth_rounded),
    net_worth_rounded_cumul_lag = lag(net_worth_rounded_cumul, default = 0),
    net_worth_rounded_remaining = 100 - net_worth_rounded_cumul_lag - net_worth_rounded,
    net_worth_rounded_remaining = ifelse(net_worth_rounded_remaining < 0, 0, net_worth_rounded_remaining)) |> 
  select(category, net_worth_rounded_cumul_lag, net_worth_rounded, net_worth_rounded_remaining) |> 
  pivot_longer(cols = c(net_worth_rounded_cumul_lag, net_worth_rounded, net_worth_rounded_remaining),
               names_to = "group", values_to = "value") |> 
  mutate(group = factor(
    group, 
    levels = c("net_worth_rounded_cumul_lag", "net_worth_rounded", "net_worth_rounded_remaining"),
    labels = c("previous", "this", "remaining")))


# Without rounding
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
    title = "The top 0.1% have 5 times more total wealth than the bottom 50%",
    subtitle = "Net worth distribution in the U.S. (2023 Q3)",
    caption = "Source: Federal Reserve. Visualization: Ansgar Wolsing"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(color = "#3B1448"),
    panel.grid.major.y = element_blank()
  )
ggsave(here(base_path, "04-waffle.png"), width = 6, height = 4, scale = 1.5)
