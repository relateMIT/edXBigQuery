###################################################################################
# This script contains a single function to load the necessary files for analysis #
###################################################################################

Load.File <- function(directory, file){
  # Function to load a single JSON table
  # Takes two inputs: directory, file
  # directory is the R-readable filepath to the folder that houses all the JSON log files
  # file is character string of the name of the specific json table necessary e.g. "person_item" NOT "person_item.json"
  
  z <- list.files(directory)
  # Obtain a character list of all the files within the given directory
  
  if (length(z) != 0){
    # Make sure that the directory is not empty
    
    if (!paste(file, ".json", sep = "") %in% z ){
      print(paste("WARNING: The file", file, "is missing", sep = " "))
      return(NULL)
      # If the file specified is not located within the given directory
    }
    
    else{
      name <- paste(directory, "\\", file, ".json", sep = "")
      print(paste("Currently extracting data for the file: ", file, sep = ""))
      return(fromJSON(sprintf("[%s]", paste(readLines(name), collapse=","))))
    }
    
  }
  else{
    print("The given directory is empty or does not exist")
    return(NULL)
    
  }
}

Load.CSV <- function(directory, file){
  # Function to load a single CSV
  # Takes two inputs: directory, file
  # directory is the R-readable filepath to the folder that houses all the CSV log files
  # file is character string of the name of the specific CSV table necessary e.g. "person_item" NOT "person_item.csv"
  
  z <- list.files(directory)
  # Obtain a character list of all the files within the given directory
  
  if (length(z) != 0){
    # Make sure that the directory is not empty
    
    if (!paste(file, ".csv", sep = "") %in% z ){
      print(paste("WARNING: The file", file, "is missing", sep = " "))
      return(NULL)
      # If the file specified is not located within the given directory
    }
    
    else{
      name <- paste(directory, "\\", file, ".csv", sep = "")
      print(paste("Currently extracting data for the file: ", file, sep = ""))
      return(read.csv(name))
    }
    
  }
  else{
    print("The given directory is empty or does not exist")
    return(NULL)
    
  }
}