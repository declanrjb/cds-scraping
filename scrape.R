library(tidyverse)
library(RSelenium)
library(rvest)
library(RCurl)

#initiate server

rD <- rsDriver(
  port = 4470L,
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

generate_search_url_from_name <- function(college_name) {
  college_name <- str_to_lower(college_name)
  college_name <- gsub('[[:punct:] ]+',' ',college_name)
  college_name <- str_squish(college_name)
  college_name <- gsub(" ","+",college_name)
  
  search_url <- paste("https://www.google.com/search?q=site:.edu+filetype:pdf+",college_name,"+common+data+set+2022-2023+admission&start=0",sep="")
  return(search_url)
}

generate_search_url <- function(college_url) {
  college_url <- gsub("https://","",college_url,fixed=TRUE)
  
  search_url <- paste("https://www.google.com/search?q=site:",college_url,"+filetype:pdf+common+data+set+2022-2023&start=0",sep="")
  return(search_url)
}

download_cds_guess <- function(college_name,college_url) {
  search_url <- generate_search_url(college_url)
  
  remDr$navigate(search_url)
  
  result <- remDr$findElements("css",".MjjYud")[[1]]
  
  if (length(result) > 0) {
    url <- url_from_result(result)
    if (!is.na(url)) {
      college_name <- str_to_lower(college_name)
      college_name <- gsub('[[:punct:] ]+',' ',college_name)
      college_name <- str_squish(college_name)
      college_name <- gsub(" ","_",college_name)
      return(download_from_url(url,"cds_all"))    
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

go_forth_and_scrape <- function(j) {
  search_url <- "https://www.google.com/search?q=site:.edu+filetype:pdf+common+data+set+2022-2023&start=0"
  
  remDr$navigate(search_url)
  
  i <- 0
  while (i < j) {
    scroll_to_bottom()
    scrape_page()
    if (expand_results()) {
      i <- i + 1 
    } else {
      break
    }
  }
}

randFloat <- function(min,max) {
  difference <- max - min
  result <- min + (runif(1) * difference)
  return(result)
}

randInt <- function(min,max) {
  difference <- max - min
  result <- min + (runif(1) * difference)
  result <- as.integer(result)
  return(result)
}

inst_dict <- read_csv("ipeds/trimmed_data.csv")
stop_trigger <- randInt(30,40)
for (i in 988:length(inst_dict$INSTNM)) {
  curr_college <- inst_dict[i,]$INSTNM
  curr_url <- inst_dict[i,]$WEBADDR
  
  if ((i %% stop_trigger) == 0) {
    message("TRIGGERED A STOP")
    rD[["server"]]$stop()
    
    Sys.sleep(randFloat(5,10))
    stop_trigger <- randInt(30,40)
    
    rD <- rsDriver(
      port = 4470L,
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
