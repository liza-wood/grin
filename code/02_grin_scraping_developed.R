library(stringr)
library(rvest)
library(xml2)
library(stringr)
library(data.table)
dt <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_cut.RDS")
# Pull out names
trimws(str_extract(dt$heading[c(10,100,1000)], '(?<=Details for:)(\\w|\\s)*(?=,)'))
dt$accession_id <- trimws(str_extract(dt$heading, '(?<=Details for:)(\\w|\\s)*(?=,)'))

# Identify year
dt$year <- as.numeric(str_extract(dt$date_received, '\\d{4}'))
table(is.na(dt$year))
table(dt$year)
dt <- dt[dt$year > 1882,]

# Not using extratc_all for now
dt$origin_general <- str_extract(dt$origin, "Developed|Collected|Donated")
table(dt$origin_general)
table(is.na(dt$origin_general))

# Choose only those that have been worked on
dtdev <- dt[dt$origin_general == "Developed" & !is.na(dt$origin_general),]
# And focus on more recent history
dtdev <- dtdev[dtdev$year >= 2017 & dtdev$year <= 2021 & !is.na(dtdev$year),]
# I would ha eliked to get rid of PP and PVP but not all IPR columns were correct
# e.g. https://npgsweb.ars-grin.gov/gringlobal/accessiondetail?id=1488702 
# dtdev <- dtdev[!(str_detect(dtdev$ipr, "Plant patent")),]

table(dt$improvement_status)

css_text <- c("#MainContent_ctrlSum_lblLifeForm", # Can help me identify if tree of not?
              "#MainContent_ctrlHistory_pnlDeveloped a", # Developers
              "#MainContent_ctrlHistory_pnlDeveloped li", # Developers link
              "#MainContent_ctrlHistory_pnlDeveloped .panel-body", # as a backup, the whole panel text
              "#MainContent_ctrlNarrative_lblNarrative", # Narrative 
              "#MainContent_ctrlIPR_pnlIPR .panel-body" # All IPR?
) 

base <- "https://npgsweb.ars-grin.gov/gringlobal/accessiondetail?id="
ids <- sort(dtdev$accession)
short_list <- list()
# This comes if too long
short_list <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_l_short.RDS") 
ids <- sort(dtdev$accession)
ul <- names(short_list)
ids <- ids[-which(ids %in% ul)]

text.l <- list()

for(i in 1:length(ids)){ # length(ids)
  Sys.sleep(.5)
  url <- tryCatch(read_html(paste0(base, ids[i])),error = function(e) NULL)
  Sys.sleep(.5)
  # Extracting each item in CSS text
  for(j in css_text){
    if(j == "#MainContent_ctrlHistory_pnlDeveloped a"){
      text.l[[j]] <- url %>% 
        html_nodes(css = j) %>%
        html_attr("href") %>% 
        trimws()
    } else {
      text.l[[j]] <- url %>% 
        html_nodes(css = j) %>% 
        html_text() %>% 
        trimws()
    }
  }
  
  # Remove the link that is to a map location
  if(length(text.l$`#MainContent_ctrlHistory_pnlDeveloped a`) != 0){
    if(TRUE %in% str_detect(text.l$`#MainContent_ctrlHistory_pnlDeveloped a`, 'maps.aspx')){
      text.l$`#MainContent_ctrlHistory_pnlDeveloped a` <- text.l$`#MainContent_ctrlHistory_pnlDeveloped a`[-which(str_detect(text.l$`#MainContent_ctrlHistory_pnlDeveloped a`, 'maps.aspx'))]
    }
  }
  text.l[[length(css_text) + 1]] <- ids[i]
  names(text.l)[[length(css_text) + 1]] <- "accession"
  
  short_list[[length(short_list)+1]] <- text.l
  names(short_list)[[length(short_list)]] <- ids[i]
  # Save every 1,000 runs, about every 1.5 hours
  if(i %in% seq(1000, 100000, by = 1000)){
    saveRDS(short_list, "~/OneDrive - University of Exeter/data/grin/grin_db_l_short.RDS")
    print(paste("Saving after at:", i, Sys.time()))
  }
}

# this is moving at a painful -- it takes 2 hours to save
saveRDS(short_list, "~/OneDrive - University of Exeter/data/grin/grin_db_l_short.RDS")
Sys.time()