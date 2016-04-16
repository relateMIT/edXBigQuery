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


#This function gets problems in a given course unit (chapter/sequential/vertical).
#Takes three parameters: (names, courseItem, courseAxis).
#"names" is a list in the order of {[chapters],[sequentials],[verticals]}
#"courseAxis" is the course_axis table (required for checking if a vertical contains a splittest)
#"courseItem" is the course_item table (required).

GetProblems <- function(names, courseItem = course.item, courseAxis = course.axis){
  #Note: checking for validity of parameters and error messages are not available right now.
  d <- courseItem
  #Find items in chapter if chapter is specified
  if (!(names[1] == "")){
    d <- d[ d$chapter_name %in% names[1], ]
  }
  #Find items in sequentials
  if(!(names[2] == "")){
    d <- d[ d$section_name %in% names[2], ]
  }
  
  #If a vertical name is supplied, then check the courseAxis if it contains splittest. This is because the splittest vertical name is not displayed
  
  if (!(names[3] == "")){
    #get all verticals in split, both name and URL name
    verticalInSplit <- courseAxis[ courseAxis$category == "vertical" & courseAxis$is_split == TRUE ,c("name", "parent")]
    #create a dictionary for vertical name and vertical url_name
    allVerticals <- courseAxis[ courseAxis$category == "vertical", c("name", "url_name")]
    #get the vertical URLs for all verticals specified, in the order of the vertical names given.
    verticalURLs <- allVerticals$url_name[match(names[3], allVerticals$name)]
    
    #find if any verticals given are parent URLs
    IsParentVertical <- verticalURLs %in% verticalInSplit$parent
    #if some of the verticals are parent verticals
    if (sum(IsParentVertical)>0) {
      #For every vertical url that is a parent vertical, return the name of its child vertical
      splitVerticalsNames <- verticalInSplit$name[ verticalInSplit$parent %in% verticalURLs[IsParentVertical]]
      
      #remove parent vertical names, and append the corresponding split vertical names.
      names[3] <- c(names[3][!IsParentVertical],splitVerticalsNames)
    }
    d <- d[ d$vertical_name %in% names[3], ]
  }
  #If the vertical is the parent of a split test, delete the vertical from the list, and append the list with the split vertical names
  return(d)
}


