##### This is a collection of functions used to extract information of computed edX tables#######

#This function reads one json table into a dataframe
#filename is the complete file path that points to the json file data
#requires the package jsonlite
#requires the package stringr

#Todo: Write function to check and install required packages.

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

CalcAssignmentCompletion <- function(problems, students, responseMatrix = responseMatrices$multANeg, attempts = c(0,1), useUserId = FALSE, 
                                     studentInfo = person.course,  
                                    problemInfo = course.item) {
  # This function outputs the total, attempted and correct number of items and problems in each assignment for each student.
  #test if response matrix is properly defined
  #useUserId means that the students variable contains userids not usernames.
  #uses problem_nid, the parameter problems can be a problem matrix
  # Currently only handles situations where "isBinary" is true. 
  
  
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
  print("Note: This function does not yet handle AB experiments properly. Only uses binary response input")
  if (useUserId) {
    studentsUserid <- students
  } 
  else {
    # from studentInfo get userid
    studentsUserid <- studentInfo$user_id[match(students, studentInfo$username)]
    #check if any student is found
    if (sum(!is.na(studentsUserid)) == 0) {
      print("no students found, check if student names are usernames, not e-mails")
      return()
    }
  }

  # If problem is a matrix, get item_nid.
  if (length(dim(problems)) == 2) {
    itemNid <- problems$item_nid
    problemNid <- problems$problem_nid
    problemToItem <- as.data.frame(cbind(problemNid, itemNid))
  } 
  else {
    # If problem is a vector containing problem Idenfiyer, get problem_nid from problemInfo file by problemIdenfier
    if (length(dim(problems)) == 1){
      itemNid <- problemInfo$item_nid[match(problems,problemInfo[,problemIdentifyer])]
      problemNid <- problemInfo$problem_nid[match(problems,problemInfo[,problemIdentifyer])]
      problemToItem <- as.data.frame(cbind(itemNid, problemNid))
      #check if any problems are found
      if (sum(!is.na(itemNid)) == 0){
        print("no problem found")
        return()
      }
    }
  }
  
  ItemtoProblem <- function(itemData) {
    problemData <- NULL
    if (length(unique(problemNid)) < length(problemNid)){
      #if one of the problem contains multiple items
      for (eachProblem in unique(problemNid)){
        itemsInProblem.nId <- problemToItem[ problemToItem$problemNid == eachProblem, "itemNid"]
        itemsInProblem.Data <- itemData[ ,itemsInProblem.nId]
        #Write a function calculating problem data from itemData
        if (is.null(dim(itemsInProblem.Data))){
          oneProblemData <- itemsInProblem.Data
        } else {
          oneProblemData <- apply(X = itemsInProblem.Data, MARGIN = 1, FUN = prod)
        }
        #append problem data to matrix
        assignProblemData <- tryCatch(
          problemData <- cbind(problemData, oneProblemData),
          error = function(e){e}
        )
        if (inherits(assignProblemData, "error")) {
          problemData <- oneProblemData
        }
      } 
    } else {
      #every problems contains only one item each
      problemData <- itemData
    }

    return(problemData)
  }
  
    #subset response matrix
    itemDataRaw <- responseMatrix[match(studentsUserid, row.names(responseMatrix)), itemNid]
    #generate multiple binary matricies and multiple itemdata 
    
    itemData <- list()  #final attempt is list item 1, nth attempt is list item n+1
    problemData <- list()
    for (i in attempts){
      #loop around attempts
      if (i == 0) {
        #final attempt
        itemData[[1]] <- itemDataRaw > 0
        mode(itemData[[1]]) <- "numeric"
        problemData[[1]] <- ItemtoProblem(itemData[[1]])
      }
      else {
        #ith attempt is put into i+1th slot in list
        itemData[[i+1]] <- (itemDataRaw <= i & itemDataRaw > 0)
        mode(itemData[[i+1]]) <- "numeric"
        problemData[[i+1]] <- ItemtoProblem(itemData[[i+1]])
      }
    }


    
  
  #count total, correct, completion
  #handle cases when there's just one item or one problem in the assignment
  #change this into iterating over all attempts
  
  if (class(itemData[[1]]) == "matrix") {
    #from final attempt matrix, find problems assigned and problems attempted
    itemrslt <- apply(X = itemData[[1]], MARGIN = 1, FUN = function(x){
      #return(c(length(x), sum(!is.na(x)), sum(x == 1, na.rm = TRUE)))
      return(c(length(x), sum(!is.na(x))))
      })
    itemrslt <- t(itemrslt)
    for (i in attempts){
      #append each attempt
      itemrslt <- cbind(itemrslt, apply(
        X= itemData[[i+1]], 
        MARGIN = 1, 
        FUN = function(x){return(sum(x == 1, na.rm = TRUE))}
        )
      )
    }
  } 
  else {
    #if only one item
    #itemCorrect <- itemData
    #itemCorrect[is.na(itemCorrect)] <- 0
    #itemrslt <- cbind(rep(1, length(itemData)), as.numeric(!is.na(itemData)), itemCorrect)
    itemrslt <- cbind(rep(1, length(itemData)), as.numeric(!is.na(itemData)))
    #append data for each attempt
    for (i in attempts){
      #replace NA with zero
      itemCorrect <- itemData[[i+1]]
      itemCorrect[is.na(itemCorrect)] <- 0
      itemrslt <- cbind(itemrslt, itemCorrect)
    }
  }
  colnameList <- c("item.total", "item.attempted", "item.correct.final")
  for (i in attempts){
    if (i > 0){
      colnameList <- c(colnameList, paste0("item.correct.", i,"attempt"))
    }
  }
  colnames(itemrslt) <- colnameList
  
  #Do the same thing for problems
  
  if (class(problemData[[1]]) == "matrix") {
    #from final attempt matrix, find problems assigned and problems attempted
    problemrslt <- apply(X = problemData[[1]], MARGIN = 1, FUN = function(x){
      #return(c(length(x), sum(!is.na(x)), sum(x == 1, na.rm = TRUE)))
      return(c(length(x), sum(!is.na(x))))
    })
    problemrslt <- t(problemrslt)
    for (i in attempts){
      #append each attempt
      problemrslt <- cbind(problemrslt, apply(
        X= problemData[[i+1]], 
        MARGIN = 1, 
        FUN = function(x){return(sum(x == 1, na.rm = TRUE))}
      )
      )
    }
  } 
  else {
    problemrslt <- cbind(rep(1, length(problemData)), as.numeric(!is.na(problemData)))
    for (i in attempts){
      #replace NA with zero
      problemCorrect <- problemData[[i+1]]
      problemCorrect[is.na(problemCorrect)] <- 0
      problemrslt <- cbind(problemrslt, problemCorrect)
    }
  }

  colnameList <- c("problem.total", "problem.attempted", "problem.correct.final")
  for (i in attempts){
    if (i > 0){
      colnameList <- c(colnameList, paste0("problem.correct.", i,"attempt"))
    }
  }
  colnames(problemrslt) <- colnameList
 
  
  rslt <- cbind(itemrslt, problemrslt)
  
  return(as.data.frame(rslt))
}


SplitTest.ProblemsAssigned <- function(courseFlat = course.axis.flatten, courseItem = course.item){
  #This function finds the problems that are involved in a split test
  #takes the flattened course axis file (which contains part of the "data" column from the original split test)
  splitTests <- courseFlat[ !is.na(courseFlat$user_partition_id), ]
  
  #here is the function that Alex has written
  #retuqires stringr package
  #########************This is a hack that works for one course only!!!*******************#############
  #########************Need better regexp strategy!!!************############
  library("stringr")
  Get.Partitions <- function(course.partition, className = "MITx/8.01r_2/vertical/"){
    # Function to obtain a list of course partitions
    # One input, file - the name of the csv file - defaults to course_partitionInfo
    # Outputs a three-column matrix containing: group_id, module_id, user_partition_id    
    
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
        k <- gsub(className, "", k)
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
    
  parentVerticals <- Get.Partitions(splitTests)
  
  
  #return problem module_id and group assignment.
  
  problemsInSplit <- courseFlat[ (courseFlat$is_split == TRUE) & (courseFlat$category == "problem"), ]
  #Get all the problems involved in split tests
  
  problemSplitInfo <- cbind(problemsInSplit[ ,"url_name"], 
                            parentVerticals[match(problemsInSplit$parent, parentVerticals$module_id) ,
                                            c("user.partition_id","group_id")])
  colnames(problemSplitInfo) <- c("url_name", "partition_id", "group_id")
  #For each problem get split Info
  
  itemInSplit <- courseItem[ courseItem$is_split == TRUE, ]
  
  itemSplitInfo <- cbind(itemInSplit[,c("item_id","problem_nid","item_nid")], 
                         problemSplitInfo[match(itemInSplit$problem_id, problemSplitInfo$url_name), 
                                                       c("partition_id", "group_id")] )
  #Go from problem to item
    
  return(itemSplitInfo)
}


