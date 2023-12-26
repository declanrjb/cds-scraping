library(tidyverse)
library(RSelenium)
library(rvest)
library(RCurl)
library(pdftools)
library(stringr)
library(stringi)

vec_paste <- function(vec) {
  result <- ""
  for (x in vec) {
    result <- paste(result,x,sep="")
  }
  return(unlist(result))
}

first_split <- function(string,sep) {
  vec <- unlist(str_split(string,sep))
  front <- vec[1]
  vec <- vec[-1]
  back <- vec_paste(vec)
  result <- c(front,back)
  result <- unlist(result)
  result <- result[str_length(result) > 0]
  return(result)
}

row_clean <- function(row) {
  row <- first_split(row,":")
  row <- str_squish(row)
  return(row)
}

list_filter <- function(list,filter) {
  i <- 1
  while (i <= length(list)) {
    if (!filter(list[[i]])) {
      list <- list[-i]
    } else {
      i <- i + 1
    }
  }
  return(list)
}

transform_vectorized_list <- function(list) {
  max_cols <- 0
  for (x in list) {
    if (length(x) > max_cols) {
      max_cols <- length(x)
    }
  }
  
  df <- as.data.frame(matrix(nrow=length(list),ncol=max_cols))
  
  for (i in 1:length(list)) {
    vec <- list[[i]]
    for (j in 1:length(vec)) {
      df[i,j] <- vec[j]
    }
  }
  
  return(df)
}

table_from_text <- function(text) {
  rows <- unlist(str_split(text,"\n"))
  rows <- rows[str_length(rows) > 0]
  rows <- rows[grepl(":",rows)]
  row_list <- lapply(rows,row_clean)
  
  evaluator <- function(x) {
    return(length(x) == 2)
  }
  
  row_list <- list_filter(row_list,evaluator)
  
  if (length(row_list) > 0) {
    df <- transform_vectorized_list(row_list)
    colnames(df) <- c("Val_Type","Val")
    
    return(df)
  } else {
    return(FALSE)
  }
}

retrieve_college_name <- function(full_text) {
  index <- which(grepl("Name of College/University",full_text,fixed=TRUE))
  name_sec <- full_text[index]
  name_table <- table_from_text(name_sec)
  return(name_table[['Val']][which(name_table[['Val_Type']] == "Name of College/University")])
}

retrieve_name_table <- function(full_text) {
  index <- which(grepl("Name of College/University",full_text,fixed=TRUE))
  
  if (length(index > 1)) {
    index <- index[1]
  } else if (length(index) == 0) {
    return(FALSE)
  }
  
  name_sec <- full_text[index]
  name_table <- table_from_text(name_sec)
  return(name_table)
}

name_from_name_table <- function(name_table) {
  index <- which(name_table[['Val_Type']] == "Name of College/University")
  if (length(index) > 1) {
    index <- index[1]
  } else if (length(index) == 0) {
    return(FALSE)
  } else {
    return(name_table[['Val']][index])
  }
}

string_index <- function(string,index) {
  return(substr(string,index,index))
}

consec_space_counter <- function(string) {
  i <- 1
  while ((i <= (str_length(string)-1)) & (substr(string,i,i+1) != "  ")) {
    i <- i + 1
  }
  if (i == str_length(string)) {
    # terminate
    return(0)
  } else {
    j <- i
    while ((j <= str_length(string)) & (substr(string,j,j) == " ")) {
      j <- j + 1
    }
    return(j-i)
  }
}

strip_before_consec_space <- function(string) {
  i <- 1
  while ((i <= (str_length(string)-1)) & (substr(string,i,i+1) != "  ")) {
    i <- i + 1
  }
  return(str_trim(substr(string,1,i)))
}

strip_before_consec_space_ending_x <- function(string) {
  string <- gsub(" X","",string,fixed=TRUE)
  return(str_squish(string))
}

generate_priority_table <- function(file_address) {
  text <- pdf_text(file_address)
  merge_text <- vec_paste(text)
  search_string <- "Relative importance of each of the following academic and nonacademic factors"
  termination_string <- "C8"
  splice <- gregexpr(search_string,merge_text,fixed=TRUE)[[1]][1] + str_length(search_string)
  end_splice <- gregexpr(termination_string,merge_text,fixed=TRUE)[[1]][1]
  weight_sec <- substr(merge_text,splice,end_splice)
  
  if (str_length(weight_sec) == 0) {
    return(FALSE)
  }
  
  rows <- str_split(weight_sec,"\n")
  rows <- unlist(rows)
  rows <- str_trim(rows)
  rows <- rows[str_length(rows) > 0]
  rows <- rows[grepl(" X",rows,fixed=TRUE) | grepl(" x",rows,fixed=TRUE)]
  
  priority_table <- as.data.frame(matrix(ncol=2,nrow=length(rows)))
  colnames(priority_table) <- c("Priority","Weight")
  
  priority_table$Priority <- unlist(lapply(rows,strip_before_consec_space_ending_x))
  priority_table$Weight <- str_length(rows)
  
  unique_weights <- unique(priority_table$Weight)
  for (x in unique_weights) {
    count <- length(which(priority_table$Weight == x))
    message(paste(x,": ",count,sep=""))
  }
  
  return(priority_table)
}

bind_backup_csvs <- function(start_path) {
  all_csvs <- list.files(start_path)
  all_csvs <- paste(start_path,"/",all_csvs,sep="")
  result <- read_csv(all_csvs[1])
  for (i in 2:length(all_csvs)) {
    temp_df <- read_csv(all_csvs[i])
    result <- rbind(result,temp_df)
  }
  return(result)
}

vec_count <- function(vector,x) {
  count <- 0
  for (y in vector) {
    if (y == x)
      count <- count + 1
  }
  return(count)
}

freq_table <- function(vector) {
  values <- unique(vector)
  result <- as.data.frame(matrix(ncol=2,nrow=length(values)))
  colnames(result) <- c("Value","N")
  result$Value <- values
  for (i in 1:length(result$Value)) {
    result[i,]$N <- vec_count(vector,result[i,]$Value)
  }
  result <- as.data.frame(result)
  return(result)
}

very <- 41:49
important <- 60:70
considered <- 76:89
not_considered <- 96:106

ranges_values <- list(very,important,considered,not_considered)
ranges_names <- c("Very Important","Important","Considered","Not Considered")

spaces_to_name <- function(x) {
  i <- 1
  while (i <= length(ranges_values)) {
    if (x %in% ranges_values[[i]]) {
      return(ranges_names[i])
    } else {
      i <- i + 1
    }
  }
  return("SCRIPT FAILURE")
}

importance_to_numeric <- function(importance) {
  if (importance %in% ranges_names) {
    index <- which(ranges_names == importance)
    return(4-index)    
  } else {
    return(NA)
  }
}

whittle <- function(df,uni_col,fail_col) {
  units <- unique(df[[{{uni_col}}]])
  for (sort in units) {
    temp_df <- df[(which(df[[{{uni_col}}]] == sort)),]
    if ("SCRIPT FAILURE" %in% temp_df[[{{fail_col}}]]) {
      df <- df[(which(df[[{{uni_col}}]] != sort)),]
    }
  }
  message(paste("Original Unique: ",length(units),sep=""))
  message(paste("Now Unique: ",length(unique(df[[{{uni_col}}]])),sep=""))
  message(length(unique(df[[{{uni_col}}]]))/length(units))
  return(df)
}

double_filter_lookup <- function(df,x_col,y_col,x,y,output_col) {
  first_indices <- which(df[[{{x_col}}]] == x)
  if (length(first_indices) == 0) {
    return(NA)
  }
  df <- df[first_indices,]
  second_indices <- which(df[[{{y_col}}]] == y)
  if (length(second_indices) == 0) {
    return(NA)
  }
  df <- df[second_indices,]
  results <- df[[{{output_col}}]]
  return(results)
}

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

shared_vec <- function(vec1,vec2) {
  combined_vec <- c(vec1,vec2)
  combined_vec <- unique(combined_vec)
  result <- c()
  
  for (x in combined_vec) {
    if ((x %in% vec1) & (x %in% vec2)) {
      result <- c(result,x)
    }
  }
  
  return(result)
}

standard_clean <- function(vec) {
  vec <- gsub('[[:punct:] ]+',' ',vec)
  vec <- str_to_lower(vec)
  vec <- str_squish(vec)
  vec <- stri_trans_general(vec,"Latin-ASCII")
  return(vec)
}

mean_by_filter <- function(df,filter_column,val_column,filter_value) {
  indices <- which(df[[{{filter_column}}]] == filter_value)
  df <- df[indices,]
  return(mean(df[[{{val_column}}]],na.rm=TRUE))
}

build_means_table <- function(df,filter_column,val_column) {
  sorts <- unique(df[[{{filter_column}}]])
  result <- as.data.frame(matrix(ncol=4,nrow=length(sorts)))
  colnames(result) <- c("Filter_Col","Val_Col","Filter_Val","MEAN")
  result$Filter_Col <- filter_column
  result$Val_Col <- val_column
  for (i in 1:length(sorts)) {
    curr_sort <- sorts[i]
    result[i,]$MEAN <- mean_by_filter(df,filter_column,val_column,curr_sort)
    result[i,]$Filter_Val <- curr_sort
  }
  result <- result %>% arrange(Filter_Val)
  return(result)
}

parse_percent <- function(dec) {
  dec <- dec * 100
  dec <- round(dec,1)
  dec <- paste(dec,"%",sep="")
  return(dec)
}

title_wrapper <- function(x,wrap) {
  x <- paste(strwrap(x,wrap),collapse="\n")
  return(x)
}

assemble_combined_plot <- function(df,filter_col,val) {
  means_table <- build_means_table(df,filter_col,val)
  means_table <- means_table %>% filter(!is.na(Filter_Val))
  graph_df <- df %>% select({{filter_col}},{{val}})
  colnames(graph_df) <- c("Filter_Col","Val")
  graph_df <- graph_df %>% filter(!is.na(Filter_Col)) %>% filter(!is.na(Val))
  p <- ggplot() +
    geom_point(data=graph_df, aes(x=Filter_Col,y=Val)) +
    geom_line(data=means_table, aes(x=Filter_Val,y=MEAN), color="blue") +
    geom_point(data=means_table, aes(x=Filter_Val,y=MEAN), color="blue") +
    ggrepel::geom_label_repel(data = means_table,
                              mapping = aes(x = Filter_Val, y = MEAN, label = parse_percent(MEAN))) +
    geom_line(data=means_table, aes(x=Filter_Val,y=0), color="black", linetype="dashed") +
    theme_bw() +
    ylab(gsub("admDiff","Difference in Admission Rates",val)) +
    xlab(paste("Importance of: ",filter_col,sep="")) +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(title_wrapper(paste("Gendered Difference in Admissions Rates by Importance of ",str_to_title(filter_col),sep=""),85)) +
    xlim(0,3)
  return(p)
}