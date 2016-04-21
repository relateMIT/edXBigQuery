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


# This function gets problems in a given course unit (chapter/sequential/vertical).
# Takes five parameters: (chapterNames, seqNames, vertNum, courseItem, courseAxis).
# chapter, sequentials can be vectors, they are the display names of chapters, sequentials and verticals in the course.
# vertNum is the vertical number in the sequence, can be a vector. Seuqnce name is required for specifying vertical.
# Display names are used because that is used in the course.items table 
# The search is conducted in hierarchical order, so the verticals must be contained in the sequentials and the sequenatials in chapters.
# "courseAxis" is the course_axis table (required for checking if a vertical contains a splittest)
# "courseItem" is the course_item table (required).

GetProblems <- function(chapterNames = "", seqNames = "", vertNames = "", courseItem = course.item, courseAxis = course.axis){
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
  
  #If a vertical name is supplied, then check the courseAxis if it contains splittest. This is because the splittest vertical name is not displayed
  #Note: The following needs to be re-coded. 
  # 1. Check if sequence name is provided and only one sequential is provided
  # 2. Get sequential path from sequential name
  # 3. Construct a list of vertical paths from the given sequential paths
  # 4. get vertical url names from vertical paths
  # 5. check if any of the vertical urls are parents, if yes, replace with child vertical urls.
  # 6. get vertical names based on vertical urls.
  # 7. proceed as before with vertical url list.
  
  
  if (!(vertNames[1] == "")){
    #get all verticals in split, both name and URL name
    verticalInSplit <- courseAxis[ courseAxis$category == "vertical" & courseAxis$is_split == TRUE ,c("name", "parent")]
    #create a dictionary for vertical name and vertical url_name
    allVerticals <- courseAxis[ courseAxis$category == "vertical", c("name", "url_name")]
    #get the vertical URLs for all verticals specified, in the order of the vertical names given.
    #make everything upper case
    verticalURLs <- allVerticals$url_name[match(toupper(vertNames), toupper(allVerticals$name))]
    
    #find if any verticals given are parent URLs
    IsParentVertical <- verticalURLs %in% verticalInSplit$parent
    #if some of the verticals are parent verticals
    if (sum(IsParentVertical)>0) {
      #For every vertical url that is a parent vertical, return the name of its child vertical
      splitVerticalsNames <- verticalInSplit$name[ verticalInSplit$parent %in% verticalURLs[IsParentVertical]]
      
      #remove parent vertical names, and append the corresponding split vertical names.
      vertNames <- c(vertNames[!IsParentVertical],splitVerticalsNames)
    }
    d <- d[ toupper(d$vertical_name) %in% toupper(vertNames), ]
  }
  #If the vertical is the parent of a split test, delete the vertical from the list, and append the list with the split vertical names
  return(d)
}


