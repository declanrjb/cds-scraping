library(tidyverse)
library(pdftools)
library(stringr)

source("functions.R")

if (file.exists("cds_cleaned/university_of_alabama_in_huntsville.pdf")) {
  file.remove("cds_cleaned/university_of_alabama_in_huntsville.pdf")  
}

if (file.exists("cds_cleaned/www.upb.pitt.edu_sites_default_files_files_2020-10_academics_academic-schedules_Spring-Schedule.pdf")) {
  file.remove("cds_cleaned/www.upb.pitt.edu_sites_default_files_files_2020-10_academics_academic-schedules_Spring-Schedule.pdf")  
}

if (file.exists("cds_cleaned/www.wittenberg.edu_sites_default_files_media_provost_CDS_2022-2023.pdf")) {
  file.remove("cds_cleaned/www.wittenberg.edu_sites_default_files_media_provost_CDS_2022-2023.pdf")  
}

all_pdfs <- list.files(path="cds_cleaned")
all_pdfs <- paste("cds_cleaned/",all_pdfs,sep="")
all_pdfs <- all_pdfs[!grepl("catalog",str_to_lower(all_pdfs),fixed=TRUE)]

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
  file_path <- paste("data_tables/priorities/",gsub(" ","_",str_to_lower(curr_name)),sep="")
  if (!file.exists(file_path)) {
    curr_file <- directory[i,]$CDS_loc
    priority_table <- generate_priority_table(curr_file)
    if (is.data.frame(priority_table)) {
      if (dim(priority_table)[1] > 0) {
        priority_table <- cbind(College=curr_name,priority_table)
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

num_labels <- c("College","Very_Important","Important","Considered","Not_Considered","Total_Considered")
num_cons_matrix <- as.data.frame(matrix(nrow=length(all_colleges),ncol=length(num_labels)))
colnames(num_cons_matrix) <- num_labels
num_cons_matrix[['College']] <- all_colleges
for (i in 1:length(num_cons_matrix[['College']])) {
  curr_college <- num_cons_matrix[['College']][i]
  num_cons_matrix[['Very_Important']][i] <- dim(df %>% filter(College == curr_college) %>% filter(Weight == 3))[1]
  num_cons_matrix[['Important']][i] <- dim(df %>% filter(College == curr_college) %>% filter(Weight == 2))[1]
  num_cons_matrix[['Considered']][i] <- dim(df %>% filter(College == curr_college) %>% filter(Weight == 1))[1]
  num_cons_matrix[['Not_Considered']][i] <- dim(df %>% filter(College == curr_college) %>% filter(Weight == 0))[1]
  num_cons_matrix[['Total_Considered']][i] <- dim(df %>% filter(College == curr_college) %>% filter(Weight != 0))[1]
}



