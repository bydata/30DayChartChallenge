library(tidyverse)
library(lubridate)
library(ggtext)
library(here)
library(glue)

base_path <- here("2022", "21-down-upwards")

#' Source: Destatis
#' https://www-genesis.destatis.de/genesis/online?sequenz=statistikTabellen&selectionname=61111#abreadcrumb
#' Code: 61111-0004

files <- list.files(here(base_path), pattern = "61111-0004_flat.*")
df_raw <- read_csv2(here(base_path, files), 
                    locale = locale(decimal_mark = ",", grouping_mark = ".",
                                    encoding = "ISO-8859-15"),
                    na = "...")

glimpse(df_raw)


# mapping of month names DE <-> EN
months <- 1:12
months_de <- locale(date_names = "de")$date_names$mon
names(months) <- months_de

# Check product codes
unique(df$product_code)



df <- df_raw %>% 
  select(year = Zeit, month = `2_Auspraegung_Label`, product_code = `3_Auspraegung_Code`, 
         product_label = `3_Auspraegung_Label`, index = `PREIS1__Verbraucherpreisindex__2015=100`) %>% 
  mutate(month = months[month],
         year_month = ymd(paste(year, str_pad(month, 2, pad = "0"), "01", sep = "-")),
         category_level = str_length(str_remove(product_code, "CC13-")) - 1) %>% 
  filter(!is.na(index))

glimpse(df)


## Product names -- get a vector of names for translation to English -----------
product_names_de <- unique(df$product_label)
write_lines(product_names_de, here(base_path, "product_names_de.txt"))

# Read translated product names (e.g. deepl.com)
product_names_en <- read_lines(here(base_path, "product_names_en.txt"))
names(product_names_en) <- product_names_de

# Sanity check if both files have the same length
length(product_names_en) == length(product_names_de) 

# Translate product names in dataframe
df <- df %>% 
  mutate(product_label_en = product_names_en[product_label])


# Assign higher-level categories to each product code
df <- df %>% 
  mutate(
    category_level1_code = str_extract(product_code, "(CC13-\\d{2})"),
    category_level2_code = str_extract(product_code, "(CC13-\\d{3})"),
    category_level3_code = str_extract(product_code, "(CC13-\\d{4})")
  ) %>% 
  left_join(distinct(., product_code, product_label_en), 
            by = c("category_level1_code" = "product_code"), suffix = c("", "_level1")) %>% 
  left_join(distinct(., product_code, product_label_en), 
            by = c("category_level2_code" = "product_code"), suffix = c("", "_level2")) %>% 
  left_join(distinct(., product_code, product_label_en), 
            by = c("category_level3_code" = "product_code"), suffix = c("", "_level3")) 



## EDA -------------------------------------------------------------------------


## Function for a basic plot of trends
plot_trend <- function(code, data = df) {
  data %>% 
    filter(product_code == code) %>% 
    ggplot(aes(year_month, index)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0, max(data$index), 20)) +
    coord_cartesian(ylim = c(min(data$index), max(data$index))) +
    labs(title = data$product_label[data$product_code == code])
}


# Which products have become most expensive since 2015?
df %>% 
  filter(year == 2022, month == 2) %>% 
  arrange(-index)



# Which products have decreased in prince most since 2015?
df %>% 
  filter(year == 2022, month == 2, index < 100) %>% 
  arrange(index)

df %>% 
  filter(year == 2022, month == 2, index < 100, category_level == 3) %>% 
  arrange(index)



## Trend for package tours (level = 3)
plot_trend("CC13-0960") 
# Passenger trains
plot_trend("CC13-0731") 
# Telephones
plot_trend("CC13-0820")
plot_trend("CC13-0531")
# Liquid gas
plot_trend("CC13-04522")
# Diesel
plot_trend("CC13-07221")
# Butter
plot_trend("CC13-01151")
# Erdgas
plot_trend("CC13-04521")
# Gas
plot_trend("CC13-0452")


## Variation per product price
df %>% 
  group_by(product_code, product_label, product_label_en, category_level) %>% 
  summarize(across(index, .fns = list(median = median, mean = mean, sd = sd, 
                                      min = min, max = max)), .groups = "drop") %>% 
  arrange(index_sd) %>% 
  filter(index_sd > 0) %>% 
  filter(category_level <= 3)


plot_trend("CC13-055")
plot_trend("CC13-062")
plot_trend("CC13-01192")
plot_trend("CC13-04541")
plot_trend("CC13-0211")
plot_trend("CC13-053")
plot_trend("CC13-071")


# Selected products to show
products <- list(
  "increase" = c("CC13-04522", "CC13-01151", "CC13-0115"),
  "flat" = c("CC13-062", "CC13-055", "CC13-0211"),
  "decrease" = c("CC13-08202", "CC13-0911", "CC13-08302")
)

# get statistics per product
product_stats <- df %>% 
  filter(year == max(year)) %>% 
  filter(month == max(month)) %>% 
  filter(product_code %in% flatten(products)) %>% 
  transmute(product_code, index_change_pct = index - 100)


df %>% 
  filter(product_code %in% flatten(products)) %>% 
  inner_join(product_stats, by = "product_code") %>% 
  mutate(trend = case_when(
    product_code %in% products$increase ~ "increase",
    product_code %in% products$flat ~ "flat",
    product_code %in% products$decrease ~ "decrease"
  ),
  trend = factor(trend, levels = c("increase", "flat", "decrease")),
  # product_label_en = fct_reorder(product_label_en, as.numeric(trend))
  index_change_pct_fmt = scales::percent(index_change_pct / 100, accuracy = 0.1),
  index_change_pct_fmt = ifelse(index_change_pct > 0, paste0("+", index_change_pct_fmt), index_change_pct_fmt),
  label = glue("{product_label_en}<br> <b style='font-size:15pt;color:grey50'>{index_change_pct_fmt}</span>"),
  label = fct_reorder(label, -index_change_pct),
  product_label_en = fct_reorder(product_label_en, -index_change_pct)
  ) %>% 
  ggplot(aes(year_month, index)) +
  # fill "background" by type of change
  geom_rect(
    data = . %>%  distinct(product_code, product_label_en, trend),
    aes(xmin = min(df$year_month) - duration("60 days"), 
                xmax = max(df$year_month) + duration("60 days"), ymin = -Inf, ymax = Inf, 
                fill = trend), 
    stat = "unique", inherit.aes = FALSE, alpha = 0.2) + 
  geom_line() +
  geom_point(data = . %>% filter(year_month == max(year_month))) +
  geom_richtext(data = . %>% filter(year_month == max(year_month)),
                aes(label = index_change_pct_fmt), 
                nudge_y = 8, hjust = 1, label.size = 0, fill = NA,
                family = "Chivo", color = "grey50", size = 4
                ) +
  scale_y_continuous(breaks = seq(0, max(df$index), 20)) +
  scale_fill_manual(values = c("increase" = "#f02ea9", "flat" = "grey70", "decrease" = "green")) +
  coord_cartesian(clip = "off", expand = FALSE) +
  facet_wrap(vars(product_label_en)) +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    strip.text = element_markdown(hjust = 0, lineheight = 1.75, face = "bold",
                                  color = "white"),
    strip.background = element_rect(color = NA, fill = "grey28"),
    panel.background = element_rect(color = NA, fill = "white"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 6, b = 6, l = 12, r = 12)
  )
ggsave(here(base_path, "21-prices.png"), width = 8, height = 7)



df %>% 
  filter(product_code %in% flatten(products)) %>% 
  inner_join(product_stats, by = "product_code") %>% 
  mutate(trend = case_when(
    product_code %in% products$increase ~ "increase",
    product_code %in% products$flat ~ "flat",
    product_code %in% products$decrease ~ "decrease"
  ),
  trend = factor(trend, levels = c("increase", "flat", "decrease")),
  # product_label_en = fct_reorder(product_label_en, as.numeric(trend))
  index_change_pct_fmt = scales::percent(index_change_pct / 100, accuracy = 0.1),
  index_change_pct_fmt = ifelse(index_change_pct > 0, paste0("+", index_change_pct_fmt), index_change_pct_fmt),
  label = glue("{product_label_en}<br> <b style='font-size:15pt;color:grey50'>{index_change_pct_fmt}</span>"),
  label = fct_reorder(label, -index_change_pct),
  product_label_en = fct_reorder(product_label_en, -index_change_pct)
  ) %>% 
  ggplot(aes(year_month, index)) +
  geom_line(aes(color = trend)) +
  geom_point(data = . %>% filter(year_month == max(year_month)),
             aes(col = trend)) +
  geom_richtext(data = . %>% filter(year_month == max(year_month)),
                aes(label = index_change_pct_fmt), 
                nudge_y = 8, hjust = 1, label.size = 0, fill = NA,
                family = "Chivo", color = "grey50", size = 4
  ) +
  scale_y_continuous(breaks = seq(0, max(df$index), 20)) +
  scale_color_manual(values = c("increase" = "#f02ea9", "flat" = "grey70", "decrease" = "green")) +
  coord_cartesian(clip = "off", expand = TRUE) +
  facet_wrap(vars(product_label_en)) +
  guides(color = "none") +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    strip.text = element_markdown(hjust = 0, lineheight = 1.75, face = "bold",
                                  color = "white"),
    strip.background = element_rect(color = NA, fill = "grey28"),
    panel.background = element_rect(color = NA, fill = "grey98"),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(t = 6, b = 6, l = 12, r = 12)
  )
ggsave(here(base_path, "21-prices-colored-lines.png"), width = 8, height = 7)



## More broader categories ---------

df %>% 
  filter(category_level == 1) %>% 
  filter(year_month == max(year_month)) 


df %>% 
  filter(category_level == 1) %>% 
  mutate(
    index_change_pct = index / 100 - 1,
    index_change_pct_fmt = scales::percent(index_change_pct, accuracy = 0.1),
    product_label_en = fct_reorder(product_label_en, -index_change_pct)) %>% 
  ggplot(aes(year_month, index)) +
  geom_line() +
  geom_richtext(data = . %>% filter(year_month == max(year_month)),
                aes(label = index_change_pct_fmt), 
                nudge_y = 1, hjust = 0.9, label.size = 0, fill = NA,
                family = "Chivo", color = "grey50", size = 3
  ) +
  scale_y_continuous(breaks = 100) +
  facet_wrap(vars(product_label_en)) +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    strip.text = element_markdown(hjust = 0, lineheight = 1.75, face = "bold",
                                  color = "white"),
    strip.background = element_rect(color = NA, fill = "steelblue"),
    panel.background = element_rect(color = NA, fill = "grey98"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey67"),
    # axis.text.y = element_blank(),
    plot.margin = margin(t = 6, b = 6, l = 12, r = 12)
  )
ggsave(here(base_path, "21-prices-category1-colored-lines.png"), width = 8, height = 7)



df %>% 
  filter(category_level <= 2) %>% 
  mutate(
    index_change_pct = index / 100 - 1,
    index_change_pct_fmt = scales::percent(index_change_pct, accuracy = 0.1),
    product_label_en_level1 = fct_reorder(product_label_en_level1, -index_change_pct)) %>% 
  ggplot(aes(year_month, index, group = product_label_en)) +
  geom_line(aes(color = factor(category_level))) +
  scale_y_continuous(breaks = 100) +
  scale_color_manual(values = c("1" = "black", "2" = "grey40")) +
  facet_wrap(vars(product_label_en_level1)) +
  theme_minimal(base_family = "Noto Sans") +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    strip.text = element_markdown(hjust = 0, lineheight = 1.75, face = "bold",
                                  color = "white"),
    strip.background = element_rect(color = NA, fill = "steelblue"),
    panel.background = element_rect(color = NA, fill = "grey98"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey67"),
    # axis.text.y = element_blank(),
    plot.margin = margin(t = 6, b = 6, l = 12, r = 12)
  )
ggsave(here(base_path, "21-prices-category1-w2-colored-lines.png"), width = 8, height = 5)



