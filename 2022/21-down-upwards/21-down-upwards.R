library(tidyverse)
library(lubridate)
library(ggtext)
library(here)

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
