library(tidyverse)

ipeds <- read_csv("ipeds/hd2022.csv")
ipeds <- ipeds %>% select(UNITID,INSTNM,WEBADDR)

adm <- read_csv("ipeds/adm2022.csv")
adm <- adm %>% select(UNITID,APPLCN,ADMSSN)

ipeds <- left_join(ipeds,adm,by=c("UNITID"))

ipeds <- ipeds %>% filter(!is.na(APPLCN)) %>% filter(!is.na(ADMSSN))

ipeds <- cbind(ipeds,ADM_RATE=NA)
ipeds$ADM_RATE <- ipeds$ADMSSN / ipeds$APPLCN

write.csv(ipeds,"ipeds/trimmed_data.csv",row.names=FALSE)