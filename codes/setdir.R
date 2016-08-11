################################################################################
# This script contains a single function to find the directory of the datasets #
################################################################################


Set.Dir <- function(){
  # Function to set a default directory for finding JSON tables, etc.

  n <- readline(prompt="Enter the directory that contains the relevant data tables: ")
  # Paste in copied directory of folder from file explorer
  
  pattern <- "[.][\\][.]"
  # Find all single slashes in copied directory
  
  replace <- "[.]([\\\\]{2})[.]"
  # Replacement string of double dashes
  
  dir <- gsub(pattern,replace,n)
  # Find and replace all single dashes with doubles for use by R
  
  return(dir)
}

Set.Defaults <- function(){
  # Function to set defaults for all global variables used in the general file
  
  course.problem <<- NULL
  problem.check <<- NULL
  course.item <<- NULL
  person.course <<- NULL
  person.item <<- NULL
  split.test <<- NULL
  gradesheet <<- NULL
}
