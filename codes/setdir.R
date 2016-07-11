################################################################################
# This script contains a single function to find the directory of the datasets #
################################################################################

Set.Dir <- function(){
  n <- readline(prompt="Enter the desired directory: ")
  # Paste in copied directory of folder from file explorer
  
  pattern <- "[.][\\][.]"
  # Find all single slashes in copied directory
  
  replace <- "[.]([\\\\]{2})[.]"
  # Replacement string of double dashes
  
  dir <- gsub(pattern,replace,n)
  # Find and replace all single dashes with doubles for use by R
  
  return(dir)
}