library(tidyverse)
library(pdftools)
library(stringr)

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

very <- 42:48
important <- 61:70
considered <- 77:87
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

all_pdfs <- list.files(path="cds_all")
all_pdfs <- paste("cds_all/",all_pdfs,sep="")

directory <- as.data.frame(matrix(ncol=3,nrow=length(all_pdfs)))
colnames(directory) <- c("College","CDS_loc","Name_Table_Loc")

for (i in 1:length(all_pdfs)) {
  curr_file <- all_pdfs[i]
  message(curr_file)
  
  directory[i,]$CDS_loc <- curr_file
  
  curr_text <- pdf_text(curr_file)
  name_table <- retrieve_name_table(curr_text)
  
  if (is.data.frame(name_table)) {
    college_name <- name_from_name_table(name_table)
    name_table_path <- gsub(" ","_",str_to_lower(college_name))
    name_table_path <- paste("data_tables/name_tables/",name_table_path,".csv",sep="")
    write.csv(name_table,name_table_path,row.names=FALSE)
    
    directory[i,]$College <- college_name
    directory[i,]$Name_Table_Loc <- name_table_path    
  } else {
    directory[i,]$College <- "FAILED"
    directory[i,]$Name_Table_Loc <- "DNE"
  }
}

write.csv(directory,"directory.csv",row.names=FALSE)

directory <- read_csv("directory.csv")

directory <- directory %>% filter(!(College %in% c("FAILED",FALSE)))
errors <- c()

for (i in 1:length(directory$College)) {
  curr_name <- directory[i,]$College
  message(i)
  message(curr_name)
  curr_file <- directory[i,]$CDS_loc
  priority_table <- generate_priority_table(curr_file)
  if (is.data.frame(priority_table)) {
    if (dim(priority_table)[1] > 0) {
      priority_table <- cbind(College=curr_name,priority_table)
      file_path <- paste("data_tables/priorities/",gsub(" ","_",str_to_lower(curr_name)),sep="")
      write.csv(priority_table,file_path,row.names=FALSE)
    } else {
      errors <- c(errors,curr_name)
      message("ERROR")  
    }
  } else {
    errors <- c(errors,curr_name)
    message("ERROR")
  }
}

# break point

df <- bind_backup_csvs("data_tables/priorities")
write.csv(df,"df.csv",row.names=FALSE)

# break point

df <- read_csv("df.csv")

df$Weight <- unlist(lapply(df$Weight,spaces_to_name))

df <- whittle(df,"College","Weight")
df$Priority <- gsub(" x","",df$Priority)
df$Priority <- str_trim(df$Priority)
df$Weight <- unlist(lapply(df$Weight,importance_to_numeric))

unique_cons <- unique(df$Priority)
all_colleges <- unique(df$College)
priority_matrix <- as.data.frame(matrix(ncol=(length(unique_cons)+1),nrow=length(all_colleges)))
column_names <- c("College",unique_cons)
colnames(priority_matrix) <- column_names
priority_matrix[['College']] <- all_colleges

for (i in 1:length(priority_matrix[['College']])) {
  curr_college <- priority_matrix[['College']][i]
  for (j in 2:length(colnames(priority_matrix))) {
    cur_col <- colnames(priority_matrix)[j]
    importance_value <- double_filter_lookup(df,"College","Priority",curr_college,cur_col,"Weight")
    priority_matrix[i,j] <- importance_value
  }
}




