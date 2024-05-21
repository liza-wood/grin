library(ggplot2)
library(dplyr)
library(stringr)

dt <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_cut.RDS")

## Looking for university inventors ----
dt$origin_country <- trimws(str_extract(dt$origin, '(?<=\\– ).*'))
sort(unique(dt$origin_country))
unique(dt$accession_name[!is.na(dt$accession_name) & 
                           !str_detect(dt$accession_name, 'Details for\\:')])
unique(dt$accession_name[!str_detect(dt$accession_name, 'Details for\\:') &
    str_detect(dt$origin_country, "United States")])
public_pattern <- paste(c('[Uu]niversity','USDA', 'Agric\\. Exp\\. Station',
                          'Department of Agriculture', 
                          'Agricultural Experiment Station',
                          'Agriculture?a?l? Research Station',
                          'US Forest Service',
                          'Agronomy Department', 'Coll\\. of',
                          'Expt\\. Hort\\. Sta\\.', 'Univ\\.'),
                        collapse = "|")
# ^ got these patterns by looking through 200 of these above
sort(unique(dt$accession_name[str_detect(dt$accession_name, public_pattern) &
    str_detect(dt$origin_country, "United States")]))

pub_inventors

## Improvement status ----
table(dt$improvement_status)
dt$type <- ifelse(dt$improvement_status %in% c('Cultivar', 'Cultivated material',
                                       'Clone', 'Rootstock'), 'Cultivar',
                      ifelse(dt$improvement_status %in% c('Breeding material', 'Genetic material'), 'Breeding & genetic material', dt$improvement_status))
dt$year <- as.numeric(str_extract(dt$date_received, '\\d{4}'))

dt %>% 
  group_by(year, type) %>% 
  count() %>% 
  filter(!is.na(type) & type != "Uncertain improvement status") %>% 
  ggplot(aes(x = year, y = n, color = type)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "G")

# public cont
dt$public_cont <- ifelse((str_detect(dt$accession_name, public_pattern) |
                           str_detect(dt$accession_type, public_pattern)) &
                           (!is.na(dt$accession_name) | !is.na(dt$accession_type)), 
                         T, F)
dt$public_cont <- ifelse(is.na(dt$public_cont), F, dt$public_cont)

dt %>% 
  group_by(year, public_cont, improvement_status) %>% 
  count() %>% 
  filter(!is.na(improvement_status) & 
           improvement_status != "Uncertain improvement status") %>% 
  ggplot(aes(x = year, y = n, color = public_cont)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(begin = .1, end = .9) +
  facet_wrap(~improvement_status)
