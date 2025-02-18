deposits <- readRDS('~/OneDrive - University of Exeter/data/grin/developed_deposits.RDS')
jpr <- deposits[str_detect(deposits$`#MainContent_ctrlIPR_pnlIPR .panel-body`, 'Journal of Plant Registration'),]
# Journal of Plant Registration. GP-1030, WHEAT. Issued: 01 Sep 2018.                                                                                                              Venegas, J. P., R. A. Graybosch, P. S. Baenziger, G. H. Bai, & P. St. Amand. 2018. Registration of Great Plains adapted reduced phytate winter wheat germplasm. J. Pl. Registr. 12(3):405.

jpr$doi <- trimws(str_extract(jpr$`#MainContent_ctrlIPR_pnlIPR .panel-body`, '(?<=DOI:).*'))
jpr$doi <- str_remove(jpr$doi, '\\.$')
jpr$title <- str_extract(jpr$`#MainContent_ctrlIPR_pnlIPR .panel-body`, 
                         "(Registration|Release) [Oo]f .*(?= J\\. Pl\\. Registr\\.)")
# Title, but allowing for things like L.) to be in the middle of the title because scientific name
jpr$title <- ifelse(is.na(jpr$title),
                    str_extract(jpr$`#MainContent_ctrlIPR_pnlIPR .panel-body`, 
                            "[A-Za-z0-9,;:\\-\\s\\[\\]\\(\\)‘’\\'“”]*(L\\.\\))?[A-Za-z0-9,;:\\-\\s\\[\\]\\(\\)‘’\\'“”]*\\.](?= J\\. Pl\\. Registr\\.)"), jpr$title)
jpr$title <- trimws(jpr$title)

jpr$bib <- str_extract(jpr$`#MainContent_ctrlIPR_pnlIPR .panel-body`, 
                            "(?<=J\\. Pl\\. Registr\\.).*")
jpr$bib <- str_remove(jpr$bib, "DOI.*")
jpr$volume <- str_extract(jpr$bib, "\\d{1,2}(?=\\()")                       
jpr$issue <- str_extract(jpr$bib, "(?<=\\()\\d{1,2}(?=\\))")  
jpr$page <- str_extract(jpr$bib, "(?<=\\:)\\d{1,3}(?=\\.)")  


library(httr2)
library(jsonlite)
source('~/Documents/Davis/R-Projects/scopus_key.R')
base = 'https://api.elsevier.com/content/search/scopus'
url <- httr2::url_parse(base)
count = '25'
jpr$abstract <- NA
for(i in 1:nrow(jpr)){
  doi <- jpr$doi[i]  
  if(is.na(jpr$abstract[i])){
    if(!is.na(doi)){
      query <- paste0('DOI (',doi,')')
      url$query <- list(apiKey = scopus_key,
                        view = 'COMPLETE',
                        query = query)
      qurl = httr2::url_build(url)
      js = request(qurl) %>%  req_perform() %>%  
        resp_body_string() %>%  jsonlite::fromJSON()
      scopus <- js$`search-results`$entry
      jpr$abstract[i] <- scopus$`dc:description`
    } else { # ( SRCTITLE ( "Journal of Plant Registration" ) AND TITLE ( "Registration of CA 4005 and CA 4006 Cotton Germplasm Lines with Partial Resistance to Feeding Injury from Thrips Pests." ) )
      title <- str_remove_all(jpr$title[i], "\\)\\(\\]\\[‘’\\'“”")
      query = paste('( SRCTITLE ( "Journal of Plant Registration" ) AND TITLE-ABS-KEY (',
                    str_replace_all(title, " ", " AND "), "))")
      vol <- jpr$volume[i]
      issue <- jpr$issue[i]
      page <- jpr$page[i]
      page <- str_remove(page, "^0{1,2}")
      # SRCTITLE(*field ornith*) AND VOLUME(75) AND ISSUE(1) AND PAGES(53-66)
      query = paste('SRCTITLE ( "Journal of Plant Registration" ) AND VOLUME (',
                    vol, ") AND ISSUE (", issue, ") AND PAGES( ", page, ")")
      url$query <- list(apiKey = scopus_key,
                        view = 'COMPLETE',
                        query = query)
      qurl = httr2::url_build(url)
      js = request(qurl) %>%  req_perform() %>%  
        resp_body_string() %>%  jsonlite::fromJSON()
      scopus <- js$`search-results`$entry
      if(ncol(scopus) > 2)
      jpr$abstract[i] <- scopus$`dc:description`[1]
    }
  }
}

jpr2 <- jpr[,c('accession', 'coop_id', 'abstract')]
deposits2 <- dplyr::left_join(deposits, jpr2)
deposits2$`#MainContent_ctrlNarrative_lblNarrative` <- ifelse(!is.na(deposits2$abstract),
                                                                     deposits2$abstract,
                                                                     deposits2$`#MainContent_ctrlNarrative_lblNarrative`)

saveRDS(deposits2, '~/OneDrive - University of Exeter/data/grin/developed_deposits_withjpr.RDS')
