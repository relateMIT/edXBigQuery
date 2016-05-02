##### This is a collection of functions used to extract information of computed edX tables#######

#This function reads one json table into a dataframe
#filename is the complete file path that points to the json file data
#requires the package jsonlite
ReadJsonTable <- function(filename){
  #load jsonlite if it is not already loaded
  if(!("jsonlite" %in% installed.packages()[,1])){
    print("Required package jsonlite is missing.")
    return()
  }
  library(jsonlite)
  #read from file
  d <- fromJSON(sprintf("[%s]", paste(readLines(filename), collapse=",")))
  return(d)
}



GetProblems <- function(chapterNames = "", seqNames = "", vertNum = "", courseItem = course.item, courseAxis = course.axis){
  # This function gets problems (items/inputfields) in a given course unit (chapter/sequential/vertical).
  # Takes five parameters: (chapterNames, seqNames, vertNum, courseItem, courseAxis).
  # chapter, sequentials can be vectors, they are the display names of chapters and sequentials in the course.
  # vertNum is the vertical number in the sequence, can be a vector. A single seuqnce name is required for specifying vertical.
  # Display names are used because that is used in the course.items table 
  # If both chapter name and sequential names are specified, the sequential must be contained in the chapter
  # "courseAxis" is the course_axis table (required for checking if a vertical contains a splittest)
  # "courseItem" is the course_item table (required).
  
  
  #function for trimming trailing white space
  trim.trailing <- function (x) sub("\\s+$", "", x)
  #Note: checking for validity of parameters and error messages are not available right now.
  d <- courseItem
  #Find items in chapter if chapter is specified
  if (!(chapterNames[1] == "")){
    #make everything case insensitive
    d <- d[ trim.trailing(toupper(d$chapter_name)) %in% trim.trailing(toupper(chapterNames)), ]
  }
  #Find items in sequentials
  if(!(seqNames[1] == "")){
    d <- d[ trim.trailing(toupper(d$section_name)) %in% trim.trailing(toupper(seqNames)), ]
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
    seqPath <- courseAxis[ trim.trailing(courseAxis$name) == trim.trailing(seqNames) ,"path"]
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

EmailtoUsername <- function(emailList){
#extracts usernames from a list of e-mail address
  ExtractUsername <- function(email){
    return(substr(email, 1, regexpr("@", email) - 1))
  }
  rslt <- sapply(X = emailList, FUN = ExtractUsername, USE.NAMES = FALSE)
}

CalcAssignmentCompletion <- function(problems, students, studentInfo = person.course, responseMatrix = responseMatrices$binary, 
                                     problemIdentifyer = NULL, problemInfo = course.item) {
  #test if response matrix is properly defined
  tryCatch(
    if (!class(responseMatrix) == "matrix"){
      print("Response Matrix is not of the data type matrix")
      return()
    },
    error = function(e){
      print("Must specify a response matrix (binary response matrix prefered)")
      return()
    }
    )
  print("Note: This function does not yet handle AB experiments properly")
  # from studentInfo get userid
  studentsUserid <- studentInfo$user_id[match(student, studentInfo$username)]
  #check if any student is found
  if (sum(!is.na(studentsUserid)) == 0) {
    print("no students found, check if student names are usernames, not e-mails")
    return()
  }
  # If problem is a matrix, get problem_nid.
  if (length(dim(problem)) == 2) {
    problemNid <- problem$problem_nid
  } else {
    # If problem is a vector, get problem_nid from problemInfo file by problemIdenfier
    if (length(dim(problem)) == 1){
      problemNid <- problemInfo$problem_nid[match(problem,problemInfo[,problemIdentifyer])]
      #check if any problems are found
      if (sum(!is.na(problemNid)) == 0){
        print("no problem found")
        return()
      }
    }
  }
  
  # Subset the response matrix
  problemData <- responseMatrix[match(studentsUserid, row.names(responseMatrix)), problemNid]
  
  #count total, correct, completion
  rslt <- apply(X = problemData, MARGIN = 1, FUN = function(x){
    return(length(x), sum(!is.na(x)), sum(x == 1))
  })
  names(rslt) <- c("total", "attempted", "correct")
  return(rslt)
}


