library(tidyverse)
library(ggtext)
library(here)

base_path <- here("2024", "11")

#' Source: Statcounter, Worldwide, Mobile + Tablet, March 2024
#' https://gs.statcounter.com/
url <- "https://gs.statcounter.com/browser-market-share/mobile-tablet/worldwide/chart.php?bar=1&device=Mobile%20%26%20Tablet&device_hidden=mobile%2Btablet&multi-device=true&statType_hidden=browser&region_hidden=ww&granularity=monthly&statType=Browser&region=Worldwide&fromInt=202403&toInt=202403&fromMonthYear=2024-03&toMonthYear=2024-03&csv=1"


df <- read_csv(url)
colnames(df) <- c("browser", "market_share")
head(df)

df |> 
  mutate(browser2 = ifelse(market_share < 2, "Other", browser)) |> 
  count(browser2, wt = market_share, name = "market_share") |> 
  mutate(browser2 = fct_reorder(browser2, -market_share)) |> 
  arrange(browser2) |> 
  mutate(label_pos_y = lag(market_share, 1, default = 0) + market_share / 2) |> 
  ggplot(aes(x = 1, y = market_share, fill = browser2)) +
  geom_col() +
  geom_text(
    aes(y = label_pos_y, label = scales::number(market_share, suffix = "%"))
  ) +
  coord_radial(theta = "y", expand = FALSE, inner.radius = 0.6)


url <- "https://gs.statcounter.com/os-market-share/mobile/worldwide/chart.php?bar=1&device=Mobile&device_hidden=mobile&statType_hidden=os_combined&region_hidden=US&granularity=monthly&statType=Operating%20System&region=United%20States%20of%20America&fromInt=202403&toInt=202403&fromMonthYear=2024-03&toMonthYear=2024-03&csv=1"
df <- read_csv(url)
colnames(df) <- c("os", "market_share")
head(df)

df |> 
  mutate(os2 = ifelse(market_share < 2, "Other", os)) |> 
  count(os2, wt = market_share, name = "market_share") |> 
  write_csv(here(base_path, "mobile-os-share-202403.csv"))
  


url <- "https://gs.statcounter.com/vendor-market-share/mobile/worldwide/chart.php?bar=1&device=Mobile&device_hidden=mobile&statType_hidden=vendor&region_hidden=ww&granularity=monthly&statType=Device%20Vendor&region=Worldwide&fromInt=202403&toInt=202403&fromMonthYear=2024-03&toMonthYear=2024-03&csv=1"
df <- read_csv(url)
colnames(df) <- c("vendor", "market_share")
head(df)

df |> 
  mutate(
    vendor2 = ifelse(market_share < 5, "Other", vendor)) |> 
  count(vendor2, wt = market_share, name = "market_share") |> 
  mutate(
    vendor2 = fct_reorder(vendor2, -market_share),
    vendor2 = fct_relevel(vendor2, "Unknown", after = 98),
    vendor2 = fct_relevel(vendor2, "Other", after = 99),
    market_share = round(market_share, 1)) |> 
  arrange(vendor2) |> 
  write_csv(here(base_path, "mobile-vendor-share-202403.csv"))
