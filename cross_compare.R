library(tidyverse)

source("functions.R")

df <- read_csv("priority_matrix.csv")

ipeds <- read_csv("ipeds/adm2022.csv")

college_names <- read_csv("ipeds/hd2022.csv")

adm_cols <- c('APPLCN','APPLCNM','APPLCNW','APPLCNAN','APPLCNUN','ADMSSN','ADMSSNM','ADMSSNW','ADMSSNAN','ADMSSNUN','ENRLT','ENRLM','ENRLW','ENRLAN','ENRLUN')

ipeds <- ipeds %>% select(UNITID,all_of(adm_cols))

college_names <- college_names %>% select(UNITID,INSTNM)

ipeds <- inner_join(ipeds,college_names,by="UNITID")

ipeds$INSTNM <- standard_clean(ipeds$INSTNM)

df$College <- standard_clean(df$College)

#analysis

colnames(df)[which(colnames(df) == "College")] <- "College_Name"

colnames(ipeds)[which(colnames(ipeds) == "INSTNM")] <- "College_Name"

df <- inner_join(df,ipeds,by="College_Name")
