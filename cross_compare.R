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

df <- cbind(df,admRate=NA)
df$admRate <- df$ADMSSN / df$APPLCN

df <- cbind(df,admMale=NA)
df$admMale <- df$ADMSSNM / df$APPLCNM

df <- cbind(df,admFemale=NA)
df$admFemale <- df$ADMSSNW / df$APPLCNW

df <- cbind(df,admDiff=NA)
df$admDiff <- df$admFemale - df$admMale

stats_tables <- build_means_table(df,colnames(df)[2],"admDiff")
for (i in 2:20) {
  message(i)
  temp_table <- build_means_table(df,colnames(df)[i],"admDiff")
  stats_tables <- rbind(stats_tables,temp_table)
  
  p <- assemble_combined_plot(df,colnames(df)[i],"admDiff")
  plot_name <- paste("plots/",gsub(" ","_",gsub("/"," ",str_to_lower(colnames(df)[i]))),".png",sep="")
  ggsave(plot_name,p,units="px",width=2400,height=1600)
}

stats_tables <- unique(stats_tables)




