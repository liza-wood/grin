library(stringr)
library(rvest)
library(xml2)
library(stringr)
library(data.table)

l <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_l_short.RDS") 

df <- rbindlist(l)

table(is.na(df$`#MainContent_ctrlIPR_pnlIPR .panel-body`))
# Remove any USPTO or PVP
df <- df[is.na(df$`#MainContent_ctrlIPR_pnlIPR .panel-body`) | !(str_detect(df$`#MainContent_ctrlIPR_pnlIPR .panel-body`, "Plant patent|Plant Variety Protection|Utility patent|PVP")),]
# Remove trees and perennials
table(df$`#MainContent_ctrlSum_lblLifeForm`)
df <- df[is.na(df$`#MainContent_ctrlSum_lblLifeForm`) | !(str_detect(df$`#MainContent_ctrlSum_lblLifeForm`, "Perennial|Tree|Shrub|Vine")),]
df$coop_id <- str_extract(df$`#MainContent_ctrlHistory_pnlDeveloped a`, '\\d+')
saveRDS(df, '~/OneDrive - University of Exeter/data/grin/developed_deposits.RDS')

devs <- unique(df$`#MainContent_ctrlHistory_pnlDeveloped a`)
devs <- str_extract(devs, '\\d+')


css_text <- c("#MainContent_lblCoopInfo"#, # Cooperator info
              #".col-md-6 .panel-body" # Cooperator body
) 

base <- "https://npgsweb.ars-grin.gov/gringlobal/cooperator?id="
ids <- sort(devs)
coop_list <- list()
# This comes if too long
coop_list <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_l_cooperators.RDS") 
ids <- sort(devs)
ul <- names(coop_list)
ids <- ids[-which(ids %in% ul)]

text.l <- list()

for(i in 1:length(ids)){ # length(ids)
  Sys.sleep(.5)
  url <- tryCatch(read_html(paste0(base, ids[i])),error = function(e) NULL)
  Sys.sleep(.5)
  # Extracting each item in CSS text
  for(j in css_text){
      text.l[[j]] <- url %>% 
        html_nodes(css = j) %>% 
        html_text() %>% 
        trimws()
  }
  
  text.l[[length(css_text) + 1]] <- ids[i]
  names(text.l)[[length(css_text) + 1]] <- "accession"
  
  coop_list[[length(coop_list)+1]] <- text.l
  names(coop_list)[[length(coop_list)]] <- ids[i]
  # Save every 1,000 runs, about every 1.5 hours
  if(i %in% seq(1000, 100000, by = 1000)){
    saveRDS(coop_list, "~/OneDrive - University of Exeter/data/grin/grin_db_l_cooperators.RDS")
    print(paste("Saving after at:", i, Sys.time()))
  }
}

saveRDS(coop_list, "~/OneDrive - University of Exeter/data/grin/grin_db_l_cooperators.RDS")
Sys.time()

