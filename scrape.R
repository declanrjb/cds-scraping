library(tidyverse)
library(RSelenium)
library(rvest)
library(RCurl)

source("functions.R")

#initiate server

rD <- rsDriver(
  port = 4460L,
  browser = "firefox",
  version = "latest",
  chromever = "106.0.5249.21",
  geckover = "latest",
  iedrver = NULL,
  phantomver = "2.1.1",
  verbose = TRUE,
  check = TRUE,
)

remDr <- rD[["client"]]
remDr$setTimeout(type = "implicit", 3000)

inst_dict <- read_csv("ipeds/trimmed_data.csv")
stop_trigger <- randInt(30,40)
for (i in 1963:length(inst_dict$INSTNM)) {
  curr_college <- inst_dict[i,]$INSTNM
  curr_url <- inst_dict[i,]$WEBADDR
  
  if ((i %% stop_trigger) == 0) {
    message("TRIGGERED A STOP")
    rD[["server"]]$stop()
    
    Sys.sleep(randFloat(5,10))
    stop_trigger <- randInt(30,40)
    
    rD <- rsDriver(
      port = 4460L,
      browser = "firefox",
      version = "latest",
      chromever = "106.0.5249.21",
      geckover = "latest",
      iedrver = NULL,
      phantomver = "2.1.1",
      verbose = TRUE,
      check = TRUE,
    )
    
    remDr <- rD[["client"]]
    remDr$setTimeout(type = "implicit", 3000)
  }
  
  message(curr_college)
  message(i)
  message(i/length(inst_dict$INSTNM))
  outcome <- download_cds_guess(curr_college,curr_url)
  message(outcome)
}

rD[["server"]]$stop()
