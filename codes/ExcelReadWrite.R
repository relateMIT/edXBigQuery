#####Those functions read and write excel/csv data from clipboard#########
#####Useful when correlating online data with offline data################

Read.clipboardXls <- function(stringsAsFactors = FALSE, header = TRUE){
  #This reads excel data from clipboard
  #String with special charactors (such as displaynames) should not be imported. Causes problems because I used check.names = FALSE.
  #check.names is turned to false to prevent the script from changing strings.
  read.table("clipboard-2048", check.names = FALSE, header = header, sep = "\t", stringsAsFactors = stringsAsFactors, fill = TRUE)
}
Write.clipboardXls <- function(x, writeRow = FALSE){
  write.table(x, "clipboard-2048", sep = "\t", row.names = writeRow)
}