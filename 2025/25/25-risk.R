library(tidyverse)
library(ggtext)
library(ggdist)
library(here)

base_path <- here("2025", "25")

df_ess <- read_csv(here(base_path, "ESS11.csv"))

# Select the relevant variables
relevant_vars <- c("essround", "anweight", "cntry", "likrisk")
df_ess_prep <- df_ess |> 
  select(all_of(relevant_vars)) |> 
  mutate(cntry_name = countrycode::countrycode(cntry, "iso2c", "country.name")) |> 
  filter(likrisk <= 6)

df_likrisk_avg <- df_ess_prep |> 
  group_by(cntry_name) |> 
  summarize(mean_likrisk = mean(likrisk))

p <- df_ess_prep |> 
  mutate(cntry_name = fct_reorder(cntry_name, likrisk, .fun = mean)) |> 
  ggplot(aes(cntry_name, likrisk)) +
  geom_hline(yintercept = 3, linewidth = 0.3, color = "grey20") +
  stat_summary(
    fun.data = "mean_cl_boot",
    col = "grey24", size = 0.7) +
  stat_summary(
    aes(col = after_stat(y)),
    geom = "point",
    fun.data = "mean_cl_boot", size = 2.8) +
  stat_summary(
    geom = "label",
    aes(label = cntry_name),
    fun.data = function(x) data.frame(y = mean(x) - 0.15),
    family = "Roboto Condensed", size = 2.5, hjust = 1,
    color = "grey12", fill = "#F8F8F8", label.size = 0
  ) +
  annotate(
    "label",
    x = nrow(df_likrisk_avg) -0.5, y = max(df_likrisk_avg$mean_likrisk) + 0.2,
    label = "Confidence interval",
    family = "Roboto Condensed", fontface = "italic", size = 2.5, col = "grey90",
    fill = "grey12", label.size = 0, hjust = 0
  ) +
  annotate(
    GeomCurve,
    x = nrow(df_likrisk_avg) -0.5, xend = nrow(df_likrisk_avg) - 0.1, 
    y = max(df_likrisk_avg$mean_likrisk) + 0.25,
    yend = max(df_likrisk_avg$mean_likrisk) + 0.1,
    col = "grey20", linewidth = 0.2, curvature = -0.2,
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  scale_y_continuous(position = "right") +
  scale_color_gradient2(midpoint = 3, aesthetics = c("color", "fill")) +
  coord_flip(ylim = c(1.4, 4.6), clip = "off") +
  guides(color = guide_colorbar(title.position = "top")) +
  labs(
    title = "How much of a risk are people willing to take?",
    subtitle = "Country average, scale from 0 - not at all to 6 - completely",
    caption = "***Question text:** (...) please tell me to what extent the 
    following statements describe you: I like to take risks.
    (Scale from 0 - not at all to 6 - completely)*
    <br><br>
    **Source:** European Social Survey wave 11 (2023).
    **Visualization:** Ansgar Wolsing",
    y = "\U2190 Take less risk          Take more risk \U2192",
    col = "I like to take risks, extent:"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"),
    text = element_text(color = "grey10"),
    axis.title.x = element_text(family = "Roboto Condensed SemiBold", size = 8),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "grey10", size = 9),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = "dashed", color = "grey10", linewidth = 0.2),
    panel.grid.minor.x = element_line(
      linetype = "dashed", linewidth = 0.1, color = "grey10"),
    plot.title = element_textbox(
      family = "Roboto Condensed SemiBold", size = 14, width = 0.95, 
      lineheight = 1.1, color = "grey2"),
    plot.subtitle = element_markdown(
      margin = margin(b = 12), lineheight = 1.2
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox(
      width = 0.95, margin = margin(t = 16), hjust = 0, lineheight = 1.1),
    plot.margin = margin(t = 4, r = 4, b = 4, l = 4),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.25),
    legend.direction = "horizontal",
    legend.key.height = unit(2, "mm"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.background = element_rect(linewidth = 0.1, fill = "#F8F8F8")
  )
ggsave(here(base_path, "25-risk.png"), width = 5.5, height = 5)
