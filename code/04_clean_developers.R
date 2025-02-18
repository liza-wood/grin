library(data.table)
coop_list <- readRDS("~/OneDrive - University of Exeter/data/grin/grin_db_l_cooperators.RDS")
dat <- rbindlist(coop_list)
colnames(dat)[2] <- "coop_id"

# Need to recombine with developed accessions and identify those with two contributors...
dat$country <- str_extract(dat$`#MainContent_lblCoopInfo`, 'United States$')
table(dat$country)
country <- countries::country_reference_list
countries <- paste(country$simple[!is.na(country$ISO_code)], collapse = "$|")
countries <- paste0("United States$|", countries, "$")
extras <- paste0(c("Czech Republic", "Korea, South", "Côte D'Ivoire", "Tanzania"),
                 collapse = "$|")
countries <- paste0(countries, "|", extras, "$")
dat$country <- str_extract(dat$`#MainContent_lblCoopInfo`, countries)
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "University of Guelph"),
                      "Canada", dat$country)
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Tennessee Agr\\. Exp\\. Sta\\."),
                      "United States", dat$country)
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Clemson University"),
                      "United States", dat$country)
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Ann Koehler University of NebraskaPanhandle Research \\& Extension Center4502 Avenue I"),
                      "United States", dat$country)
# https://cgspace.cgiar.org/items/70aa3aa9-033f-4ef3-966a-faa053c5786c
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "C\\.G\\. Yallou INRABPlant Breeding"),
                      "Benin", dat$country)
#https://www.seedquest.com/forum/roundtable/industryCEOs/nunza.htm
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Nunza B\\.V\\."),
                      "Netherlands", dat$country)
# https://www.crunchbase.com/organization/malteurop
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Malteurop Group"),
                      "France", dat$country)
# https://sagrainmag.co.za/2017/06/16/news-snippets-nuusbrokkies/
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Sydney Walter Nelson ,"),
                      "South Africa", dat$country)
# https://oregondigital.org/concern/images/kp78h918p
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "George F\\. Waldo"),
                      "United States", dat$country)
# https://portal.nifa.usda.gov/web/crisprojectpages/0077869-vaccinium-breeding-and-genetics.html
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "E\\.B\\. Morrow"),
                      "United States", dat$country)
# https://link.springer.com/content/pdf/10.1007/BF02852509.pdf
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Curtis H\\. Dearborn"),
                      "United States", dat$country)
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Syngenta Seeds, Inc\\."),
                      "United States", dat$country)
# https://news.usask.ca/articles/colleges/2021/celebrating-100-years-of-horticulture-at-usask.php
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "Dr\\. C\\.F\\. Patterson"),
                      "Canada", dat$country)
# https://link.springer.com/article/10.1007/BF00329527
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "J\\.H\\. Lonnquist"),
                      "United States", dat$country)
# https://www.isasunflower.org/fileadmin/documents/Proceedings/8thISC1978/T1978BRE11_004.pdf
# Unclear, he publishes with Lonnquit
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "N\\.E\\. Williams"),
                      "Canada", dat$country)
# https://www.pubhort.org/aps/31/v31_n4_a3.htm
dat$country <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`, "R\\.C\\. Blake"),
                      "United States", dat$country)
table(dat$country)
table(is.na(dat$country))



usmap <- usmapdata::us_map()
statenames <- usmap$full
statenames <- c(statenames, "Puerto Rico")
dat$state <- NA
for(i in 1:nrow(dat)){
  if(dat$country[i] == "United States" & !is.na(dat$country[i])){
    for(j in 1:length(statenames)){
      dat$state[i] <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`[i],
                             statenames[j]), statenames[j], dat$state[i])
    }
  }
}


uni <- "University|Unversity|Univ\\.|Virginia Tech|Virginia Polytechnic Institute|Texas A\\&M|WSU Bread Lab|Board of Regents|Center for Applied Genetic Technologies|NDSU Research Foundation|Virginia Bioinformatics Institute"
govt <- "USDA|DBNRRC"
stategovt <- "Research & Extension Center|Alaska Plant Materials Center|Tidewater Agricultural Research and Ext|Piedmont Agricultural Res\\. and Ext\\.|Virginia Agricultural Res\\. and Ext\\.|Texas Agricultural Experiment Station|Virginia Agric\\. Research and Extension Center|VA Ag. Res\\. \\& Ext\\. Center"
forgovt <- "Agriculture & Agri-Food Canada|Agriculture and Agri-Food Canada|Alberta Agriculture \\& ForestryField Crop Development Centre|Agriculture CanadaResearch Station|National Research Institute|International Center for Agric\\. Research|Agriculture and Agri\\_food Canada|Sugarcane Breeding Station|Wheat Breeding & Genetics SectionCereal Crops Research Institute|Institute of Agriculture|Cotton Research Institute|Alberta Agriculture \\& Rural Development|Instituto Agronomico - Compinas|Alberta AgricultureField Crop|Central Research Institute for Field Crops|National Institute for Biotechnology \\& Genetic Engineering|Lacombe Research Centre|Plant Gene Resource of Canada|Alberta AgricultureFood and Rural|Alberta AgricultureFood \\& Rural|Agriculture CanadaMorden Research Station|Nakhon Sawan Field Crops Center"
co <- "\\bInc\\.|Company|Co\\.|Corporation|AgriPro|Highland Specialty Grains|Nutrien Ag Solutions|Syngenta Crop Protection|Texas AgriScience|KWS Lochow GMBH|KWS Cereals USA|Especialistas En Papayas, S\\.A|Sakata Seed America|Kan\\-Do Plant Breeding Services|Admiral Maltings|RAGT|McKee, Voorhees & Sease|Tom Wagner Seeds|Seminis Vegetable Seeds|Rijk Zwaan|ProGene Plant Research|Limagrain Cereal Seeds|Nunhems"
nonprofit <- "Virginia Foundation Seed Stocks Farm|Crop Improvement Assoc|Colorado Wheat Research Foundation|Frank J. Kutka|Seed We Need|CIMMYT"
dat$type <- NA
# US company = 2, includes uni...
# Foreign company = 3
# Domestic indvl = 4
# For indvl = 5
# US govt = 6
# For govt = 7
# 08 U.S. county government
# 09 U.S. state government
# 10, nonprofit domestic
# 11, nonprofit foreign
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              co) & dat$country == "United States", 2, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              co) & dat$country != "United States", 3, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                        uni) & dat$country == "United States", 2, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              uni) & dat$country != "United States", 3, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              govt), 6, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              stategovt) & dat$country == "United States", 9, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              forgovt) & dat$country != "United States", 7, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              nonprofit) & dat$country == "United States", 10, dat$type)
dat$type <- ifelse(str_detect(dat$`#MainContent_lblCoopInfo`,
                              nonprofit) & dat$country != "United States", 11, dat$type)
table(is.na(dat$type))
dat$type <- ifelse(is.na(dat$type) & dat$country == "United States", 4, dat$type)
dat$type <- ifelse(is.na(dat$type) & dat$country != "United States", 5, dat$type)
table(is.na(dat$type))

dat$country <- ifelse(is.na(dat$country), "Unknown", dat$country)
dat$state <- ifelse(is.na(dat$state), "Unknown", dat$state)
dat$type <- ifelse(is.na(dat$type), "Unknown", dat$type)

saveRDS(dat, '~/OneDrive - University of Exeter/data/grin/cooperators.RDS')
