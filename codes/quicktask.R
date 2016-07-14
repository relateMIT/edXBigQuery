################################################################
#### This script contains the function for the quick task ######
################################################################

library("stringr")

source("setdir.R")
# Obtain the directory setting function from the setdir.R file

print("Please enter the filepath to the folder containing the MITx log files")
data.source <- Set.Dir()

source("loadfiles.R")

Get.Partitions <- function(file = "course_partitionInfo", class = "MITx/8.01r_2"){
  # Function to obtain a list of course partitions
  # One input, file - the name of the csv file - defaults to course_partitionInfo
  # Outputs a three-column matrix containing: group_id, module_id, user_partition_id
  
  class <- paste(class,"/", sep = "")
  # Add trailing dash to aid removal of class from data
  
  course.partition <- Load.CSV(data.source, file)
  # Load the relevant file
  
  find.module <- ': "[^"]*"'
  find.group <- '"[^"]*"[:]'
  
  group.ids <- list()
  module.ids <- list()
  user.partition.ids <- list()
  names <- list()

  for(j in 1:nrow(course.partition)){
    
    conversion <- course.partition[j, "group_id_to_child"]
    # Select the entry in the group_id_to_child column
    
    module.matches <- str_extract_all(conversion, find.module, simplify = FALSE)
    # Find all possible group instances within the selected data
    
    user.partition.id <- course.partition[j, "user_partition_id"]
    # Obtain the user_partition_id for the current test
    
    user.partition.ids <- c(user.partition.ids, user.partition.id, user.partition.id)
    
    name <- as.character(course.partition[j, "url_name"])
    # Obtain the name for the current test
    
    names <- c(names, name, name)

    for(k in module.matches){

      
      k <- gsub(".*[/]{2}", "", k)
      k <- gsub('\"', "", k)
      k <- gsub(class, "", k)
      # For each such match, remove quotations, and remove all excess characters in front of the double slash
      
      group.ids <- c(group.ids,k)
      
    }
  
    group.matches <- str_extract_all(conversion, find.group)
    
    for(k in group.matches){
      k <- gsub('\"', "", k)
      k <- gsub(":", "", k)
      # For each of the group number matches, remove quotations and the stray colon
      
      module.ids <- c(module.ids,k)
    }
  }


  output <- list(as.character(names), as.character(module.ids), as.character(group.ids), as.character(user.partition.ids))
  output <- as.data.frame(output)
  # Convert to data frame
  
  colnames(output) <- c("url_name", "group_id", "module_id", "user.partition_id")
  # Set Column Names

  return(output)
}
