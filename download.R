library(googledrive)

fileid <- "1o5XNIVw48I4HAfYmB-7caxVkJ4umq_t6"

drive_download(as_id(fileid), 
               path = "./data/nsee2017.sav",
               overwrite = TRUE)

nsee <- read_sav("./data/nsee2017.sav")
library(sjlabelled)
nsee %>% select(gw_belief) %>% sjmisc::frq()

