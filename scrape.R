library(tidyverse)
library(RSelenium)
library(rvest)

#initiate server

rD <- rsDriver(
  port = 4450L,
  browser = "firefox",
  version = "latest",
  chromever = "106.0.5249.21",
  geckover = "latest",
  iedrver = NULL,
  phantomver = "2.1.1",
  verbose = TRUE,
  check = TRUE,
)

url_from_result <- function(result_obj) {
  anchor <- result_obj$findChildElements("css","a")
  if (length(anchor) >= 1) {
    anchor <- anchor[[1]]
    url <- unlist(anchor$getElementAttribute("href"))
    return(url)
  } else {
    return(NA)
  }
}

download_from_url <- function(url,dir_path) {
  file_name <- gsub("https://","",url)
  file_name <- gsub("/","_",file_name)
  file_name <- paste(dir_path,"/",file_name,sep="")
  if (file.exists(file_name)) {
    return(FALSE)
  } else if (url.exists(url)) {
    download.file(url,file_name)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

get_page_height <- function() {
  doc_body <- remDr$findElement("css","body")
  return(parse_number(unlist(doc_body$getElementAttribute("scrollHeight"))))
}

scroll_to_bottom <- function() {
  webElem <- remDr$findElement("css", "body")
  past_height <- 0
  curr_height <- parse_number(unlist(webElem$getElementAttribute("scrollHeight")))
  while ((curr_height - past_height) > 0) {
    past_height <- curr_height
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(1)
    curr_height <- parse_number(unlist(webElem$getElementAttribute("scrollHeight")))
  }
}

scrape_page <- function() {
  results <- remDr$findElements("css",".MjjYud")
  
  urls <- unlist(lapply(results,url_from_result))
  urls <- urls[!is.na(urls)]
  urls <- urls[grepl(".edu",urls,fixed=TRUE)]
  
  lapply(urls,download_from_url,"cds_all")
}

expand_results <- function() {
  expand_button <- remDr$findElement("css",".GNJvt.ipz2Oe")
  if (length(expand_button) >= 1) {
    expand_button$clickElement()
    return(TRUE)
  } else {
    return(FALSE)
  }
}

search_url <- "https://www.google.com/search?q=site:.edu+filetype:pdf+common+data+set+2022-2023&start=0"

remDr <- rD[["client"]]
remDr$setTimeout(type = "implicit", 3000)

remDr$navigate(search_url)

i <- 0
while (i < 10) {
  scroll_to_bottom()
  scrape_page()
  if (expand_results()) {
    i <- i + 1 
  } else {
    break
  }
}

rD[["server"]]$stop()
