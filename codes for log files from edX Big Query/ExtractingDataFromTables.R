##### This is a collection of functions used to extract information of computed edX tables#######

#This function reads one json table into a dataframe
#filename is the complete file path that points to the json file data
#requires the package jsonlite
ReadJsonTable <- function(filename){
  #load jsonlite if it is not already loaded
  if(!("jsonlite" %in% installed.packages()[,1])){
    library(jsonlite)
  }
  #read from file
  d <- fromJSON(sprintf("[%s]", paste(readLines(filename), collapse=",")))
  return(d)
}


# This function gets problems (items/inputfields) in a given course unit (chapter/sequential/vertical).
# Takes five parameters: (chapterNames, seqNames, vertNum, courseItem, courseAxis).
# chapter, sequentials can be vectors, they are the display names of chapters and sequentials in the course.
# vertNum is the vertical number in the sequence, can be a vector. A single seuqnce name is required for specifying vertical.
# Display names are used because that is used in the course.items table 
# If both chapter name and sequential names are specified, the sequential must be contained in the chapter
# "courseAxis" is the course_axis table (required for checking if a vertical contains a splittest)
# "courseItem" is the course_item table (required).

GetProblems <- function(chapterNames = "", seqNames = "", vertNum = "", courseItem = course.item, courseAxis = course.axis){
  #Note: checking for validity of parameters and error messages are not available right now.
  d <- courseItem
  #Find items in chapter if chapter is specified
  if (!(chapterNames[1] == "")){
    #make everything case insensitive
    d <- d[ toupper(d$chapter_name) %in% toupper(chapterNames), ]
  }
  #Find items in sequentials
  if(!(seqNames[1] == "")){
    d <- d[ toupper(d$section_name) %in% toupper(seqNames), ]
  }
  
  
  if (!(vertNum[1] == "")){
    # 1. Check if sequence name is provided and only one sequential is provided
    if (seqNames[1] == ""){
      print("Error: Must specify one sequential name to access specific verticals")
      return()
    } else {
      if (length(seqNames) > 1) {
        print("Error: Can only specify one sequential name at a time")
        return()
      }
    }
    # 2. Get sequential path from sequential name
    seqPath <- courseAxis[ courseAxis$name == seqNames ,"path"]
    # 3. Construct a list of vertical paths from the given sequential paths
    vertPath <- sapply(X = vertNum, FUN = function(x,y){paste0(y,"/",x)}, y = seqPath)
    
    #create a dictionary for vertical path, vertical url_name, vercal URl names
    allVerticals <- courseAxis[ courseAxis$category == "vertical", c("path", "url_name", "name")]
    #get the vertical URLs and names for all verticals specified, in the order of the vertical paths given.
    verticalURLsandNames <- allVerticals[match(vertPath, allVerticals$path), c("url_name", "name")]
    
    #get all split tests, URL and parent
    splitTest <- courseAxis[ courseAxis$category == "split_test", c("url_name", "parent")]
    
    #get all verticals in split, both name and URL name
    verticalInSplit <- courseAxis[ courseAxis$category == "vertical" & courseAxis$is_split == TRUE ,c("name", "parent")]
    
    #find if any verticals given are parent URLs
    IsParentVertical <- verticalURLsandNames$url_name %in% splitTest$parent
    #if some of the verticals are parent verticals
    if (sum(IsParentVertical)>0) {
      #Find the split URL names from parent vertical
      splitURLInVertical <- splitTest$url_name[splitTest$parent %in% verticalURLsandNames$url_name]
      #For every vertical url that is a parent vertical, return the name of its child vertical
      splitVerticalsNames <- verticalInSplit$name[ verticalInSplit$parent %in% splitURLInVertical]
      
      #remove parent vertical names, and append the corresponding split vertical names.
      vertNames <- c(verticalURLsandNames[!IsParentVertical,"name"],splitVerticalsNames)
    } else {
      vertNames <- verticalURLsandNames$name
    }
    d <- d[ d$vertical_name %in% vertNames, ]
  }
  return(d)
}


