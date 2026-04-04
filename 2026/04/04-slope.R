#' https://github.com/vdeminstitute/vdemdata
# devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(tidyverse)
library(ggtext)
library(here)


#' Source:
#' Codebook: https://www.v-dem.net/documents/70/codebook_v16.pdf
#' Report: https://www.v-dem.net/documents/75/V-Dem_Institute_Democracy_Report_2026_lowres.pdf

df_vdem <- vdemdata::vdem 
colnames(df_vdem)

# Mapping index names
index_mapping <- c(
  "v2x_polyarchy" = "Electoral democracy", 
  "v2x_libdem" = "Liberal democracy",
  "v2x_partipdem" = "Participatory democracy",
  "v2x_delibdem" = "Deliberative democracy",
  "v2x_egaldem" = "Egalitarian democracy",
  "v2x_jucon" = "Judicial constraints",
  "v2xlg_legcon" = "Legislative constraints",
  "v2xcl_rol" = "Equality before the law",
  "v2x_freexp" = "Freedom of expression"
)

df_vdem_us <-  df_vdem |> 
  filter(country_text_id == "USA", year %in% c(2016, 2017, 2024, 2025)) |> 
  select(country_text_id, year, 
    v2x_libdem,
    v2x_jucon, v2xlg_legcon, v2xcl_rol #, v2x_freexp
  ) |> 
  pivot_longer(cols = -c(country_text_id, year), names_to = "index", values_to = "value") |> 
  mutate(
    index_label = index_mapping[index]
  )


# str_wrap with <br> instead of \n
str_wrap_html <- function(string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) {
    stringr:::check_number_decimal(width)
    if (width <= 0) {
        width <- 1
    }
    stringr:::check_number_whole(indent)
    stringr:::check_number_whole(exdent)
    stringr:::check_bool(whitespace_only)
    out <- stringi::stri_wrap(string, width = width, indent = indent, 
        exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
    out <- vapply(out, str_c, collapse = "<br>", character(1))
    stringr:::copy_names(string, out)
}


plot_titles <- list(
  title = "Democratic backsliding in the United States",
  subtitle = "The rate at which American democracy is currently being dismantled
    is **without precedent in modern history**.
    Trump's second term has seen a rapid and aggressive concentration of power
    in the presidency. Most severely affected are
    **legislative constraints**, which reached their **lowest point in over 100 years**. 
    Civil rights and equality before the law, as well as freedom of expression,
    are now at their **lowest levels in 60 years**.
    Autocratizing countries in recent history have taken about a decade
    to experience such a significant decline.",
  caption = "Source: Varieties of Democracy (V-Dem) Project, v16 - March 2026.
    Visualization: Ansgar Wolsing"
)

df_vdem_us |> 
  mutate(admin = ifelse(year %in% c(2016, 2017), "Trump I", "Trump II")) |> 
  arrange(admin, year) |> 
  group_by(admin, index) |> 
  mutate(change = last(value) - first(value)) |> 
  ungroup() |> 
  ggplot(aes(factor(year), value, col = index, group = index)) +
  geom_vline(
    aes(xintercept = factor(year)),
    linewidth = 1.2, col = "grey80"
  ) +
  geom_line(
    aes(linewidth = ifelse(index == "v2x_libdem", 1.8, 0.8))
  ) +
  geom_point(
    aes(size = ifelse(index == "v2x_libdem", 3.5, 2))
  ) +
  geom_richtext(
    data = ~filter(., year %in% c(2017, 2025)) |> 
      mutate(index_label_with_change = sprintf(
        "**\U0394%.2f** | %s", change, index_label)),
    aes(
      y = value + case_when(
        admin == "Trump I" & index == "v2xcl_rol" ~ 0.02,
        admin == "Trump I" & index == "v2x_jucon" ~ -0.02,
        admin == "Trump II" & index == "v2xcl_rol" ~ -0.02,
        admin == "Trump II" & index == "v2x_libdem" ~ -0.01,
        TRUE ~ 0
      ),
      label = str_wrap_html(index_label_with_change, 25)
    ),
    hjust = 0, lineheight = 0.9, fill = NA, label.size = 0, 
    nudge_x = 0.02,
    family = "Instrument Sans"
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.025, 0.9))) +
  scale_y_continuous(expand = expansion(add = c(0.02, 0.005))) +
  scale_color_manual(values = c("grey25", "#BF0A30", "grey40", "grey10"))+
  scale_size_identity() +
  scale_linewidth_identity() +
  coord_cartesian(ylim = c(0.55, 1), clip = "off") +
  facet_wrap(vars(admin), scales = "free_x") +
  guides(col = "none", size = "none") + 
  labs(
    title = plot_titles["title"],
    subtitle = plot_titles["subtitle"],
    caption = plot_titles["caption"],
    x = NULL,
    y = "High index value \U2192"
  ) +
  theme_minimal(base_family = "Instrument Sans") +
  theme(
    plot.title = element_text(
      family = "Instrument Sans SemiBold", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(
      lineheight = 1.25, width = 0.95,
      margin = margin(t = 4, b = 12)),
    plot.caption = element_markdown(
      hjust = 1, margin = margin(t = 6)),
    axis.title.y = element_text(hjust = 1),
    strip.text = element_text(
      family = "Instrument Sans SemiBold", size = 14, hjust = 0.2, color = "grey40"),
    panel.grid = element_blank(),
    panel.background = element_rect(color = NA, fill = "#F8F8F8"),
    panel.spacing.x = unit(5, "mm"),
    axis.ticks.y = element_line(linewidth = 0.1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(family = "Instrument Sans SemiBold", size = 10, color = "grey40")
  )
ggsave(here("2026", "04", "04-slope.png"), width = 7.2, height = 6)