library(tidyverse)

source("functions.R")

df <- read_csv("priority_matrix.csv")

ipeds <- read_csv("ipeds/adm2022.csv")

adm_cols <- c(APPLCN,APPLCNM,APPLCNW,APPLCNAN,APPLCNUN,ADMSSN,ADMSSNM,ADMSSNW,ADMSSNAN,ADMSSNUN,ENRLT,ENRLM,ENRLW,ENRLAN,ENRLUN)