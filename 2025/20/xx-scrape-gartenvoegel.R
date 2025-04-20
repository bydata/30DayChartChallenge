library(tidyverse)
library(rvest)
library(here)

base_path <- here("2025", "20")

years <- 2006:2024
# years <- 2010:2024
base_url <- "https://www.nabu.de/tiere-und-pflanzen/aktionen-und-projekte/stunde-der-gartenvoegel/ergebnisse/15767.html?formchange=1&jahr="
urls <- paste0(base_url, years)

scrape_list <- function(url, year) {
  page <- read_html(url)
  df <- page |> 
    html_node(css = "table.datatable") |> 
    html_table() |> 
    janitor::clean_names()
  
  clean_numbers <- function(x) {
    x <- str_replace(x, ",", ".")
      x <- str_remove_all(x, "\\s")
      if (str_detect(x[1], "%")) {
        x <- str_remove(x, "%")
        x <- as.numeric(x) / 100
      } else {
        x <- as.numeric(x)
      }
      x
  }
  
  if (year < 2010) {
    df <- df |> 
      mutate(
        across(c(vogel_pro_garten, 
                 vergleich_zum_vorjahr_vogel_pro_garten, vergleich_zum_vorjahr_trend),
               clean_numbers
        ),
        rang = as.integer(rang)) |> 
      filter(!is.na(rang))
  } else {
    df <- df |> 
      mutate(
        across(c(anzahl, percent_der_garten, vogel_pro_garten, 
                 vergleich_zum_vorjahr_vogel_pro_garten, vergleich_zum_vorjahr_trend),
               clean_numbers
        ),
        rang = as.integer(rang),
        anteil_voegel = round(anzahl / sum(anzahl, na.rm = TRUE), 3)) |> 
      filter(!is.na(rang))
    
  }
  df
}

dfs <- map2(urls, years, scrape_list)
dfs <- set_names(dfs, years)

df_combined <- bind_rows(dfs, .id = "year")
write_csv(df_combined, here(base_path, "NABU-Stunde-der-Gartenvögel.csv"))
