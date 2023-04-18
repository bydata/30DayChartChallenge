library(tidyverse)
library(ggtext)
library(here)
# remotes::install_github("ropengov/eurostat")
library(eurostat)

base_path <- here("2023", "18")

#' https://ec.europa.eu/eurostat/databrowser/bookmark/677ebaff-6af6-4e9a-b512-3fe4b5c90952?lang=en&page=time:2019
df <- get_eurostat("ilc_caindformal", 
                   time_format = "num", 
                   type = "code"  # get variable codes and labels
)
df <- label_eurostat(df, code = "geo", fix_duplicated = TRUE)

# df_prep <- df %>% 
#   mutate(country_code = str_sub(geo_code, 1, 2),
#          country = countrycode::countrycode(country_code, origin = "iso2c", destination = "country.name"),
#          country = ifelse(country_code == "EL", "Greece", country))
# 
# # which countries?
# unique(df_prep$country_code)
# unique(df_prep$country)

df %>% 
  filter(age == "From 3 years to minimum compulsory school age",
         duration == "30 hours or over",
         time == 2021)


# Employment: lfst_hheredch
emplchild <- get_eurostat("lfst_hheredch", 
                    time_format = "num", 
                    type = "code"  # get variable codes and labels
)
# Part-time employment: lfst_hhptechi
emplchild_pt <- get_eurostat("lfst_hhptechi", 
                    time_format = "num", 
                    type = "code"  # get variable codes and labels
)

emplchild2 <- get_eurostat("lfst_hhacwnc", 
                          time_format = "num", 
                          type = "code"  # get variable codes and labels
)
# emplchild2 <- label_eurostat(emplchild2, 
#                              code = c("geo", "sex", "age", "agechild", "n_child", "wstatus"), 
#                              fix_duplicated = TRUE)
emplchild2$geo_name <- countrycode::countrycode(emplchild2$geo,
                                                origin = "eurostat",
                                                destination = "country.name",
                                                nomatch = NULL)

unique(emplchild2$n_child)
unique(emplchild2$agechild)


emplchild2 %>% 
  filter(age == "Y25-49", agechild == "Y_LT6", sex != "T", n_child != "TOTAL",
         time == 2021) %>% 
  mutate(has_child = n_child %in% c(1, 2, "GE3")) %>% 
  filter(geo == "DE") %>% View()

emplchild2_agg <- emplchild2 %>% 
  filter(age == "Y25-49", agechild == "Y_LT6", sex != "T", n_child != "TOTAL",
         time == 2021) %>% 
  mutate(has_child = n_child %in% c(1, 2, "GE3")) %>% 
  count(sex, age, agechild, wstatus, geo, geo_name, time, has_child, wt = values, name = "values") 

emplchild2_agg_pop <- emplchild2_agg %>% 
  filter(wstatus == "POP") %>% 
  select(-wstatus)

emplchild2_agg %>% 
  filter(geo == "DE") %>% View()

emplchild2_agg_rel <- emplchild2_agg %>% 
  inner_join(emplchild2_agg_pop, 
             by = join_by(sex, age, agechild, geo, time, has_child),
             suffix = c("", ".pop")) %>% 
  mutate(share_of_pop = values / values.pop) 


# custom theme
theme_set(
  theme_minimal(base_family = "Source Sans Pro") +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "top",
      text = element_text(color = "grey30"),
      plot.title = element_markdown(color = "black", family = "Source Sans Pro SemiBold"),
      plot.title.position = "plot",
      axis.text = element_text(color = "grey30")
    )
)

eurostat_colors <- c("#2A6EB4", "#FBAB2E", "#72A8DF", "#F6A27B")


emplchild2_agg_rel %>% 
  filter(wstatus == "EMP") %>% 
  ggplot(aes(geo, share_of_pop, fill = sex)) +
  geom_col(position = "dodge") +
  coord_flip()


emplchild2_agg_rel %>% 
  filter(wstatus == "EMP") %>% 
  select(geo, geo_name, sex, share_of_pop) %>% 
  pivot_wider(id_cols = c(geo, geo_name), names_from = "sex", values_from = "share_of_pop",
              names_prefix = "share_of_pop_") %>% 
  mutate(geo = fct_reorder(geo, share_of_pop_F)) %>% 
  ggplot(aes(geo, share_of_pop)) +
  geom_segment(
    aes(x = geo, xend = geo, y = share_of_pop_F, yend = share_of_pop_M),
    linewidth = 0.3, col = "grey50"
  ) +
  geom_point(
    aes(y = share_of_pop_F, col = "Women with children")
  ) +
  geom_point(
    aes(y = share_of_pop_M, col = "Men with children")
  ) +
  geom_label(
    aes(y = (share_of_pop_F + share_of_pop_M) / 2, label = geo_name),
    label.size = 0, label.r = unit(0, "mm"),
    family = "Source Sans Pro", size = 2.25
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_color_manual(values = eurostat_colors) +
  coord_flip() +
  guides(col = guide_legend(reverse = TRUE)) +
  labs(
    title = "Employment rates for <span style='color:#FBAB2E'>women</span>
    and <span style='color:#2A6EB4'>men</span> with children in pre-school age",
    subtitle = "Share of the total population aged 24 to 49",
    caption = "Source: Eurostat. Visualisation: Ansgar Wolsing",
    x = NULL,
    y = "Share of population (%)",
    col = NULL
  ) +
  theme(
    axis.text.y.left = element_blank()
  )
ggsave(here(base_path, "18-eurostat-employed-child-lt_6y_dumbbell.png"),
       width = 7, height = 7/5 * 4)


emplchild2_agg_rel %>% 
  filter(wstatus == "EMP_PT") %>% 
  select(geo, sex, share_of_pop) %>% 
  pivot_wider(id_cols = geo, names_from = "sex", values_from = "share_of_pop",
              names_prefix = "share_of_pop_") %>% 
  mutate(geo = fct_reorder(geo, share_of_pop_F),
         diff_F_M = share_of_pop_M - share_of_pop_F) %>% 
  inner_join(
    filter(df, age == "From 3 years to minimum compulsory school age",
                    duration == "30 hours or over", 
                    time == 2021),
    by = c("geo" = "geo_code")) %>% 
  # with(., cor(values, diff_F_M, use = "pairwise.complete.obs"))
  ggplot(aes(values, diff_F_M)) +
  geom_point() +
  ggrepel::geom_text_repel(
    aes(label = geo)
  )




## MOON CHART ============================================

library(gggibbous)

emplchild2_agg_rel %>% 
  filter(wstatus == "EMP_FT") %>% 
  select(geo, sex, share_of_pop) %>% 
  pivot_wider(id_cols = geo, names_from = "sex", values_from = "share_of_pop",
              names_prefix = "share_of_pop_") %>% 
  mutate(geo = fct_reorder(geo, share_of_pop_F),
         diff_F_M = share_of_pop_M - share_of_pop_F) %>% 
  inner_join(
    filter(df, age == "From 3 years to minimum compulsory school age",
           duration == "30 hours or over", 
           time == 2021),
    by = c("geo" = "geo_code")) %>% # View()
  # ggplot(aes(values, diff_F_M)) +
  ggplot(aes(values, share_of_pop_F)) +
  geom_moon(
    aes(ratio = 1), 
    fill = eurostat_colors[2], col = "white", size = 7) +
  geom_moon(
    aes(ratio = 0.5), 
    fill = eurostat_colors[3], col = "white", size = 7) +
  ggrepel::geom_text_repel(
    aes(label = geo),
    family = "Source Sans Pro"
  )
ggsave(here(base_path, "18-eurostat-1.png"), width = 8, height = 6.4)




# Experimental scatterplot with segments ===============
emplchild2_agg_rel %>% 
  filter(wstatus == "EMP_FT") %>% 
  select(geo, sex, share_of_pop) %>% 
  pivot_wider(id_cols = geo, names_from = "sex", values_from = "share_of_pop",
              names_prefix = "share_of_pop_") %>% 
  mutate(geo = fct_reorder(geo, share_of_pop_F),
         diff_F_M = share_of_pop_M - share_of_pop_F) %>% 
  inner_join(
    filter(df, age == "From 3 years to minimum compulsory school age",
           duration == "30 hours or over", 
           time == 2021),
    by = c("geo" = "geo_code")) %>% 
  ggplot(aes(values)) +
  geom_segment(
    aes(x = values, xend = values,
        y = share_of_pop_F, yend = share_of_pop_M)
  ) +
  geom_point(
    aes(y = share_of_pop_F, col = "Mothers")
  ) +
  geom_point(
    aes(y = share_of_pop_M, col = "Fathers")
  ) 




# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Statistics_on_employment_characteristics_of_households&oldid=568074#Part-time_employment_and_children
