library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(here)

base_path <- here("2025", "12")

#' Source:
#' https://catalog.data.gov/dataset/u-s-state-life-expectancy-by-sex-2021
#' https://catalog.data.gov/dataset/u-s-state-life-expectancy-by-sex-2019-3ab02

df <- read_csv(here(base_path, "U.S._State_Life_Expectancy_by_Sex__2019.csv"))

df <- df |> rename(LE = LEB)

# Lowest, male
df$State[which.min(df$LE[df$Sex == "Male"])]
min(df$LE[df$Sex == "Male"])
# Highest, male
df$State[which.max(df$LE[df$Sex == "Male"])]
max(df$LE[df$Sex == "Male"])

# Lowest, female
df$State[which.min(df$LE[df$Sex == "Female"])]
min(df$LE[df$Sex == "Female"])
# Highest, female
df$State[which.max(df$LE[df$Sex == "Female"])]
max(df$LE[df$Sex == "Female"])

# Difference highest female to lowest male
max(df$LE[df$Sex == "Female"]) - min(df$LE[df$Sex == "Male"])


ragg::agg_png(here(base_path, "12-data.gov.png"), width = 5, height = 5,
              units = "in", res = 300)
df |> 
  filter(Sex != "Total") |> 
  ggplot(aes(x = LE, y = Sex)) +
  geom_beeswarm(
    aes(fill = Sex),
    shape = 21, color = "grey20", stroke = 0.25, cex = 4, size = 3) +
  ggrepel::geom_text_repel(
    data = bind_rows(
      df[df$Sex == "Male",][which.min(df$LE[df$Sex == "Male"]), c("State", "Sex", "LE")],
      df[df$Sex == "Female",][which.max(df$LE[df$Sex == "Female"]), c("State", "Sex", "LE")]
    ),
    aes(label = sprintf("%s (%s years)", State, LE)),
    nudge_y = 0.5, segment.size = 0.1, family = "Roboto Condensed", size = 3,
    box.padding = 0.1
  ) +
  scale_x_continuous(
    breaks = seq(60, 90, 2),
  ) +
  scale_fill_manual(values = c("Male" = "#F4BA3B", "Female" = "#730C6D")) +
  labs(
    title = "A <span style='color:#730C6D'>girl</span> born in Hawaii can expect
    to live 13 years longer<br>
    than a <span style='color:#F4BA3B'>boy</span> born in Mississippi",
    subtitle = "Life expectancy at birth (2019)",
    caption = "**Source:** NCHS via data.gov. 
      **Visualization:** Ansgar Wolsing"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", size = 8),
    axis.text.y = element_text(
      family = "Roboto Condensed Medium", hjust = 0, size = 10),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.1),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, 
      lineheight = 1.2),
    plot.subtitle = element_markdown(
      margin = margin(t = 4, b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      margin = margin(t = 16), hjust = 0),
    legend.position = "none",
    plot.margin = margin(t = 4, r = 20, b = 4, l = 8)
  )

grid::grid.lines(x = c(0.06, 0.105), y = 0.955, gp = gpar(col = "#730C6D", lwd = 2))
grid::grid.lines(x = c(0.13, 0.18), y = 0.91, gp = gpar(col = "#F4BA3B", lwd = 2))
dev.off()
