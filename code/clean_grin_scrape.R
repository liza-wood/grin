library(ggplot2)
library(dplyr)
library(stringr)

dt <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_cut.RDS")
colnames(dt)
# Pull out names
trimws(str_extract(dt$heading[c(10,100,1000)], '(?<=Details for:)(\\w|\\s)*(?=,)'))
dt$accession_id <- trimws(str_extract(dt$heading, '(?<=Details for:)(\\w|\\s)*(?=,)'))
set.seed(1991)
smpl <- sample(1:nrow(dt), 500)
dt$accession_id[smpl]
dt$top_name[smpl]


## Improvement status ----
table(dt$improvement_status)
dt$type <- ifelse(dt$improvement_status %in% c('Cultivar', 'Cultivated material',
                                       'Clone', 'Rootstock'), 'Cultivar',
                      ifelse(dt$improvement_status %in% c('Breeding material', 'Genetic material'), 'Breeding & genetic material', dt$improvement_status))
dt$year <- as.numeric(str_extract(dt$date_received, '\\d{4}'))
table(is.na(dt$year))
table(dt$year)
dt <- dt[dt$year > 1882,]
## Is there any relationship between year and accession number? How are these organized in the database?
ggplot(dt, aes(x = year, y = accession)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm')

ggplot(dt, aes(x = year, y = accession, 
               color = improvement_status)) +
  geom_point(alpha = .1) 

table(dt$site_maintained)
dt %>% 
  group_by(type) %>% 
  count()

dt %>% 
  group_by(year, type) %>% 
  count() %>% 
  filter(!is.na(type) & type != "Uncertain improvement status") %>% 
  ggplot(aes(x = year, y = n, fill = type)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d()

dt %>% 
  group_by(year, type) %>% 
  count() %>% 
  filter(!is.na(type) & type != "Uncertain improvement status") %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = year, y = prop, color = type)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d()

?zoo::rollapply
prop_base <- dt %>% 
  group_by(year, type) %>% 
  count() %>% 
  filter(!is.na(type) & type != "Uncertain improvement status") %>% 
  ungroup() %>% 
  group_by(year) %>%
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  group_by(type) %>% 
  mutate(yr5_avg_prop = slider::slide_dbl(prop, mean, .before = 5, .after = 5))
# ^ I manually checked some of this and it seems correct
ggplot(prop_base, aes(x = year, y = yr5_avg_prop, color = type)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d()

## Just varieties
var <- filter(dt, type %in% c('Cultivar', 'Cultivated material', 'Rootstock'))
table(var$form_recieved)

## All types of plants -- 
var_us <- filter(var, str_detect(origin, 'Developed') &
                   str_detect(origin, 'United States'))
ggplot(var_us, aes(x = year)) +
  geom_bar()

## These should be annuals developed
ann_us <- var %>% filter(form_recieved %in% c('Seed', 'Seedling'),
                         str_detect(origin, 'Developed'),
                         str_detect(origin, 'United States'))
ggplot(ann_us, aes(x = year)) +
  geom_bar()

## These should be perennials developed
per_us <- var %>% filter(form_recieved %in% 
                           c('Budwood', 'Cutting', 'Scion'),
                         str_detect(origin, 'Developed'),
                         str_detect(origin, 'United States'))
ggplot(per_us, aes(x = year)) +
  geom_bar()

var_us <- filter(var, str_detect(origin, 'Developed') &
                   str_detect(origin, 'United States'))



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

