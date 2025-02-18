library(stringr)
library(dplyr)

setwd("~/Box")
df <- read.csv("~/Box/grin/data_raw/recipient_list.csv")
df$clean <- trimws(str_remove_all(df$organization, "[:punct:]"))

write.csv(df, "grin/raw/recipient_list_punc_rmvd.csv", row.names = F)
