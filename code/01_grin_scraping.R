library(rvest)
library(xml2)
library(stringr)
library(data.table)

# This is the full scraping of the GRIN database

base <- "https://npgsweb.ars-grin.gov/gringlobal/accessiondetail?id="
# from what I can tell, these are all of the indices with data
# based on manual review
highest <- 2168428 # new highest 2168428, mixed findings in low 2m, like 2093501; so an extra 200k ; 1971593
lowest <- 1000001
ids <- seq(lowest, highest) 

css_text <- c("h1", 
              "#MainContent_ctrlSum_lblTaxon a", 
              "#MainContent_ctrlSum_lblTopName", 
              "#MainContent_ctrlPassport_lblCultivar",
              "#MainContent_ctrlPassport_lblOrigin",
              "#MainContent_ctrlSum_lblSite",
              "#MainContent_ctrlSum_lblReceived", 
              "#MainContent_ctrlSum_lblForm",
              "#MainContent_ctrlSum_lblStatus", 
              "#MainContent_ctrlSum_lblBackup a",
              "#MainContent_ctrlAvailability_lblHistoric",
              "#MainContent_ctrlAvailability_pnlAvailable",
              "#MainContent_ctrlHistory_pnlDonated a",
              "#MainContent_ctrlHistory_pnlDeveloped a",
              #'#MainContent_ctrlHistory_pnlCollected a' currently omitting this but think I should grab it later
              "#MainContent_ctrlNarrative_lblNarrative",
              #"#passport .col-md-12",
              #".row:nth-child(2) .col-md-10",
              #".row:nth-child(3) .col-md-10",
              "#MainContent_ctrlTaxon_lblGenus a",
              "#MainContent_ctrlTaxon_lblSubGenus a",
              "#MainContent_ctrlTaxon_lblFamily a",
              "#MainContent_ctrlTaxon_lblSubfamily a",
              "#MainContent_ctrlTaxon_lblTribe a",
              "#MainContent_ctrlIPR_pnlIPR .col-md-12")


# Need to do this because R keeps bugging
total_list <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_l.RDS") #971k Aug 18
text.l <- list()
# Was starting at 1 but now that R is crashing every so often I am started from
# wherever I left off
ul <- unlist(total_list)
ids <- c((as.numeric(max(ul[which(names(ul) == 'accession')]))+1):highest)

for(i in 1:length(ids)){ # length(ids)
    Sys.sleep(2)
    url <- tryCatch(read_html(paste0(base, ids[i])),error = function(e) NULL)
    Sys.sleep(1)
    # Extracting each item in CSS text
    for(j in css_text){
      text.l[[j]] <- url %>% 
        html_nodes(css = j) %>% 
        html_text() %>% 
        trimws()
    }
    # Running common name separately because it is being finnicky
    common.names <- url %>% 
      html_nodes(css = 'td') %>% 
      html_text()
    name <- which(str_detect(common.names,"English"))[1]+1
    text.l[[length(css_text)+1]] <- trimws(common.names[name])
    
    text.l <- ifelse(lengths(text.l) == 0, NA, text.l)
    text.l <- ifelse(lengths(text.l) > 1, text.l[[1]], text.l)
    # unlist into a datatable
    unlisted.text <- data.table(t(data.table(unlist(text.l))))
    accession <- ids[i]
    DT <- cbind(accession, unlisted.text)
    total_list[[length(total_list)+1]] <- DT
    # Save every 1,000 runs, about every 1.5 hours
    if(i %in% seq(1000, 100000, by = 1000)){
      #saveRDS(total_list, "~/OneDrive - University of Exeter/data/grin/grin_db_l.RDS")
      print(Sys.time())
    }
}
beepr::beep()
# this is moving at a painful -- it takes 2 hours to save
Sys.time()
saveRDS(total_list, "~/OneDrive - University of Exeter/data/grin/grin_db_l.RDS")
Sys.time()
beepr::beep()

total_DT <- data.table::rbindlist(total_list)
# For the first moment, compare to total_DT3 to make sure the list is doing what I think

DT_cut <- total_DT[total_DT$V1 != "" & !is.na(total_DT$V2),]

col.names <- c("heading", "taxonomy", "top_name", "cultivar", "origin",
               "site_maintained", "date_received","form_recieved", "improvement_status", 
               "backup", "historic", #"available", "donator", "developer",
               "narrative", "accession_name", "accession_type", "accession_group",
               "genus", "subgenus", "family", "subfamily", "tribe", "ipr", "common_name")
colnames(DT_cut) <- c("accession",col.names)
DT_cut <- unique(DT_cut)
# if top name is non, cultivar
saveRDS(DT_cut, "~/OneDrive - University of Exeter/data/grin/grin_db_cut.RDS")



