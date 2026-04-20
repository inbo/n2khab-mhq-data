library(tidyverse)
library(git2rdata)

#data forests
lsvi_plot <- read_vc(file = "lsvi_plot_fs", 
                     root = "processed/lsvi_mhq/forests/result")

lsvi_detail <- read_vc(file = "lsvi_detail_fs", 
                       root = "processed/lsvi_mhq/forests/result")

data_habitat <- read_vc(file = "data_habitat_fs", 
                        root = "processed/lsvi_mhq/forests/input")

# FALSE want lsvi_detail bevat voor sommige meetpunten een lsvi-beoordeling voor verschillende habitattypen
# lsvi_plot bevat enkel de resultaten voor het geobserveerde habitattype
check <- all(lsvi_detail$id %in% lsvi_plot$id)

check_lsvi_plot <- lsvi_plot %>%
  mutate(check_id = id %in% lsvi_detail$id)

# voor elk meetpunt zit er een resultaat in lsvi_plot, behalve voor 3 meetpunten met 9130_end, nog na te kijken
check_lsvi_detail <- lsvi_detail %>%
  mutate(check_id = id %in% lsvi_plot$id,
         check_plot = record_id_circle %in% lsvi_plot$point_code) %>%
  group_by(record_id_square) %>%
  mutate(check_any = any(check_id)) %>%
  ungroup() %>%
  filter(!check_any)

lsvi_detail_report <- lsvi_detail %>%
  filter(id %in% lsvi_plot$id) %>%
  select(id, type_observed, criterium, indicator, belang, voorwaarde, plot_type, waarde, waarde_numeric, referentiewaarde, status_voorwaarde, theoretischmaximum, verschilscore)

lsvi_plot_report <- lsvi_plot %>%
  mutate(scheme = ifelse(str_detect(point_code, "vbi"), "vbi", "mhq")) %>%
  select(scheme, id, date, point_code, x, y, type_observed = habitatsubtype, lsvi, aggregatiemethode, aandeel_gunstig, index_mean_ind)

n_plots <- lsvi_plot_report %>%
  group_by(scheme) %>%
  summarise(n_plots = n_distinct(point_code)) %>%
  ungroup()

lsvi_plot_report %>%
  write_csv2("output/lsvi_globaal_boshabitat.csv")

lsvi_detail_report %>%
  write_csv2("output/lsvi_detail_boshabitat.csv")
