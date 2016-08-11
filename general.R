####################################################################################
#### This script contains functions useful for extracting data from log files ######
####################################################################################

# load required packages
library("stringr")
library("date")
library("plyr")
library("jsonlite") 
library("base")


####################################################################################
################### Import data sets from local directories ########################
####################################################################################

source("setdir.R")
# Obtain the directory setting function from the setdir.R file

print("Please enter the filepath to the folder containing the MITx log files")
data.source <<- Set.Dir()
# Obtain file path by copy and pasting location of data tables from file explorer - will be used for each Load.File function call

Set.Class <- function(){
  # Function to set the global class name, if desired
  
  print("Please enter the edX name of the class (e.g. MITx/8.01r_2)")
  class.global <<- readline(prompt = "Enter the class name: ")
  # class variable is used in every instance of going through the problem_check logs
}

source("loadfiles.R")
# Obtain the file loading function from loadfiles.R - to be called whenever a table has to be loaded

####################################################################################
################# Functions for data extraction from log files #####################
####################################################################################

Compare.ProblemTimes <- function(id, prob1, prob2, problems = person.problem, load.files = TRUE){
  # Function to retrieve time difference (in seconds) between two completed problems, given a single student
  # Takes five inputs: id, prob1, prob2, problems, and load.files
  # id - the integer id of the student in question
  # prob1, prob2 - the integer ids of the two problems considered
  # problems - the data set pertaining to problem attempts - defaults to the global variable person.problem
  # load.files - boolean that determines whether or not the user wishes the function to load a file from the directory or not
  # set to TRUE by default - setting it to FALSE allows user to input data set of their choice
  # Returns an integer that specifies the number of seconds elapsed between the completion of each problem
  # Positive integer output implies that the first problem inputted (prob1) was completed after the other (prob2)
  
  if(is.null(person.problem) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    person.problem <<- Load.File(data.source, "person_problem")
    # Load the data file pertaining to problems
    
    problems <- person.problem
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(problems) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file person_problem.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set person_problem.json")
    return(NULL)
  }
  
  if(id %in% problems$user_id){
    # Check to see if student exists within dataset
    
    student.data <- problems[problems$user_id == id,]
    # Select only data pertinent to the student
    
    if(prob1 %in% student.data$problem_nid){
      # Check to see if the first problem exists within the subset
      
      if(prob2 %in% student.data$problem_nid){
        # Check to see if the second problem exists within the subset
        
        p1 <- student.data[student.data$problem_nid == prob1,"date"]
        p2 <- student.data[student.data$problem_nid == prob2,"date"]
        # Extract only the columns containing the times at which the problems are completed
        
        return(Get.TimeDiff(p1, p2))
      } else{
        # If the second problem does not exist within the subset
        print(paste("Invalid problem given/student did not complete problem ", prob2, sep = "", collapse = NULL))
        return()
      }
    } else{
      # If the first problem does not exist within the subset  
      print(paste("Invalid problem given/student did not complete problem ", prob1, sep = "", collapse = NULL))
      return()
    }
  } else{
    # If the student does not exist within the dataset
    print(paste("Student with id number ", id," does not exist", sep = "", collapse = NULL))
    return()
  }
}

Compare.ItemTimes <- function(id, item1, item2, items = person.item, load.files = TRUE){
  # Function to retrieve time difference (in seconds) between two completed items, given a single student
  # Takes five inputs: id, item1, item2, items, and load.files
  # id - the integer id of the student in question
  # item1, item2 - the integer ids of the two items considered
  # items - the data set pertaining to item attempts - defaults to the global variable person.item
  # load.files - boolean that determines whether or not the user wishes the function to load a file from the directory or not
  # set to TRUE by default - setting it to FALSE allows user to input data set of their choice
  # Returns an integer that specifies the number of seconds elapsed between the completion of each item
  # Positive integer output implies that the first item inputted (item1) was completed after the other (item2)  
  
  if(is.null(person.item) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    person.item <<- Load.File(data.source, "person_item")
    # Load the data file pertaining to items
    
    items <- person.item
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(items) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file person_item.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set person_item.json")
    return(NULL)
  }
  
  if(id %in% items$user_id){
    # Check to see if student exists within dataset
    
    student.data <- items[items$user_id == id,]
    # Select only data pertinent to the student
    
    if(item1 %in% student.data$item_nid){
      # Check to see if the first item exists within the subset
      
      if(item2 %in% student.data$item_nid){
        # Check to see if the seconxd item exists within the subset
        
        t1 <- student.data[student.data$item_nid == item1,"date"]
        t2 <- student.data[student.data$item_nid == item2,"date"]
        # Extract only the columns containing the times at which the items are completed
        return(Get.TimeDiff(t1, t2))
        
      } else{
        # If the second item does not exist within the subset
        print(paste("Invalid item given/student did not complete item ", item2, sep = "", collapse = NULL))
        return()
      }
    } else{
      # If the first item does not exist within the subset
      print(paste("Invalid item given/student did not complete problem ", item1, sep = "", collapse = NULL))
      return()
    }
  } else{
    # If the student does not exist within the dataset
    print(paste("Student with id number ", student," does not exist", sep = "", collapse = NULL))
    return()
  }
}

Check.Item <- function(id, item, attempt, check = problem.check, convert.course = person.course, convert.problem = course.problem, load.files = TRUE, class = class.global){
  # Function to extract the integer time that a particular attempt on a problem was made by a student
  # Takes eight inputs: id, item, attempt, check, convert.course, convert.problem, and load.files, and class
  
  # id is the integer value of the student's id number
  # item is the integer problem nid of the problem in question
  # attempt is an integer representing the attempt number that is to be queried
  # check is the data set that contains the check attempts of the students
  # convert.course is the data set that contains the information relating student usernames and ids
  # convert.problem is the data set that contains the information relating problem ids and module ids
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  # class is a string representing the edX designation of the class in question
  
  # Returns a positive or negative integer representing the number of seconds since 1970 the attempt was taken
  # A positive integer represents a correct response, a negative number represents an incorrect response
  
  username <- Convert.Id(id, convert.course, load.files)
  module.id <- Convert.ProblemToModule(item, convert.problem, load.files)
  
  
  if(is.null(class)){
    # If no class is specified / no global class was set - prompt user to input a class
    class <- Set.Class()
  }
  
  if(is.null(problem.check) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    problem.check <<- Load.File(data.source, "problem_check")
    # Load the data file pertaining to items
    
    check <- problem.check
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(check) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file problem_check.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set problem_check.json")
    return(NULL)
  }
  
  student.data = NULL
  item.data = NULL
  
  if(username %in% check$username){
    # Check to see if student exists within dataset
    
    student.data <- check[check$username == username, c("module_id", "time", "success")]
    # Select only data pertinent to the student
  }
  else{
    # If the student does not exist within the dataset
    # print(paste("Student with username", username, "does not exist", sep = "", collapse = NULL))
    return(NULL)
  }
  
  module.id <- paste (class, "/problem/", module.id, sep = "", collapse = NULL)
  # Convert item into standard log file format
  
  if(module.id %in% student.data$module_id){
    # Check to see if student completed selected item
    item.data <- student.data[student.data$module_id == module.id, c("time", "success")]
  }
  
  else{
    # If the student did not complete the item or if the item does not exist
    # print(paste("Item with name ", module.id," does not exist or student did not complete it", sep = "", collapse = NULL))
    return(NULL)
  }
  
  
  if(nrow(item.data) < attempt){
    # Check to see if the student participated in the given number of attempts
    # print(paste("The student did not complete ", attempt," attempts of the item", sep = "", collapse = NULL))
    return(NULL)
  }
  else{
    time <- item.data[attempt, "time"]
    time <- (TimeInteger(time))
    # Convert the date representation into an integer
    
    if (item.data[attempt, "success"] == 'incorrect'){
      time <- time * -1
      # Check if the attempt was successful; if not multiply the integer by -1
    }
    
    return(time)
  }
}

Check.Item.Final <- function(id, item, check = problem.check, convert.course = person.course, convert.problem = course.problem, load.files = TRUE, class= class.global){
  # Function to extract the integer time that the final on a problem was made by a student
  # Takes seven inputs: id, item, skill.challenges, check, convert.course, convert.problem, and load.files, and class
  
  # id is the integer value of the student's id number
  # item is the integer problem nid of the problem in question
  # check is the data set that contains the check attempts of the students
  # convert.course is the data set that contains the information relating student usernames and ids
  # convert.problem is the data set that contains the information relating problem ids and module ids
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  # class is a string representing the edX designation of the class in question
  
  # Returns a positive or negative integer representing the number of seconds since 1970 the final attempt was taken
  # A positive integer represents a correct response, a negative number represents an incorrect response
  
  attempt.number = 1
  # Number of attempt currently being checked by the function
  
  last.attempt <- NULL
  # Variable to store the integer value for the latest attempt checked
  value <- Check.Item(id, item, attempt.number, check, convert.course, convert.problem, load.files, class)
  
  while(!is.null(value)){
    attempt.number <- attempt.number + 1
    # Check the next attempt on the problem
    
    last.attempt <- value
    # Store the previous attempt
    value <- Check.Item(id, item, attempt.number, check, convert.course, convert.problem, load.files, class)
  }
  return(last.attempt)
  # Return the time of the last attempt
}

Get.DueDate <- function(nid, conversion = course.item, load.files = TRUE){
  # Function to take a problem nid number and output the due date of the corresponding problem
  # Takes three parameters - nid, conversion, and load.files
  
  # nid is an integer representing the integer nid of the problem in question
  # conversion is the data table that contains information linking each problem to its due date - defaults to the global course.item
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  
  # Returns an integer representing the due date of the problem
  
  if(is.null(course.item) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    course.item <<- Load.File(data.source, "course_item")
    # Load the data file pertaining to items
    
    conversion <- course.item
    # Allow the function to utilize the imported dataset
  } else if(is.null(conversion) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file course_item.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set course_item.json")
    return(NULL)
  }
  
  if (!nid %in% conversion$problem_nid){
    print(paste("ERROR: The problem with the id number ", nid, " does not exist.", sep = ""))
    # Check to see if the problem with the nid number given actually exists
    return(NULL)
  }
  else{
    due.date <- conversion[conversion$problem_nid == nid, "due_date"]
    # Extract the data pertaining only to the due dates of the problem in question
    
    due.date <- due.date[1]
    # Consider only the first item in the problem (all other items will have same due date)
    
    if (due.date == "null"){
      # If the problem has no assigned due-date e.g. a practice problem
      
      print("ERROR: The called problem does not have a due date")
      return(NULL)
    }
    else{
      return(TimeInteger(due.date))
      # Convert the date representation into an integer for use
    }
    
    # Find the corresponding problem id of the problem with the given nid number
    return(problem.id)
  }
}

Get.All.Attempts <- function (student.id, problems, check = problem.check, convert.course = person.course, convert.problem = course.problem, load.files = TRUE){
  # Function to get all of the times that the students attempts all problems in a given list
  # Takes six inputs: student.id, problems, check, convert.course, convert.problem, and load.files
  
  # student.id is the integer value of the student's id number
  # problems is a list of integers representing the problem nidsE:\UROP\Data
  # check is the data set that contains the check attempts of the students
  # convert.course is the data set that contains the information relating student usernames and ids
  # convert.problem is the data set that contains the information relating problem ids and module ids
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  
  # Returns a list of lists of times that student attempt the given problems
  
  if(is.null(check) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    problem.check <<- Load.File(data.source, "problem_check")
    # Load the data file pertaining to items
    
    check <- problem.check
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(check) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file problem_check.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set problem_check.json")
    return(NULL)
  }
  
  problem.list <- list()
  # list to store the times that skill.challenges were completed
  
  for (j in problems){
    
    attempt <- 1
    # Check for first attempts of each problem
    
    attempt.time <- Check.Item(student.id, j, attempt, check, convert.course, convert.problem, load.files)
    problem.list[[as.character(j)]] <- attempt.time
    # Extract the times for the first attempts and store
    
    
    while(!is.null(attempt.time)){
      # Check to see if the student made further attempts
      
      attempt <- attempt + 1
      # Increment the attempt number to see further attempts
      
      attempt.time <-Check.Item(student.id, j, attempt, check, convert.course, convert.problem, load.files)
      problem.list[[as.character(j)]] = c(problem.list[[as.character(j)]], attempt.time)
      # Extract the times for the subsequent attempts and store
    }
  }
  
  return(problem.list)
  # return the compiled list of attempt times
}

####################################################################################
######################## Miscellaneous Functions ###################################
####################################################################################

Convert.Id <- function(id, conversion = person.course, load.files = TRUE){
  # Function to take a student id number and output the username of the corresponding student
  # Takes three parameters - id, conversion, and load.files
  
  # id is an integer representing the integer id number of the student in question
  # conversion is the data table that contains information linking each student id to a username - defaults to the global person.course
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  
  # Returns a string representing the username of the student
  
  if(is.null(person.course) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    person.course <<- Load.File(data.source, "person_course")
    # Load the data file pertaining to items
    
    conversion <- person.course
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(conversion) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file person_course.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set person_course.json")
    return(NULL)
  }
  
  if (!id %in% conversion$user_id){
    print(paste("ERROR: The student with the id number ", id, " does not exist.", sep = ""))
    # Check to see if the student with the id number given actually exists
    return(NULL)
  }
  else{
    username <- conversion[conversion$user_id == id, "username"]
    # Find the corresponding username of the person with the given user id number
    return(username)
  }
  
}

Convert.Username <- function(username, conversion = person.course, load.files = TRUE){
  # Function to take a student username and output the username of the corresponding student
  # Takes three parameters - username, conversion, and load.files
  
  # id is an integer representing the integer id number of the student in question
  # conversion is the data table that contains information linking each student id to a username - defaults to the global person.course
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  
  # Returns an integer representing the id number of the student
  
  if(is.null(person.course) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    person.course <<- Load.File(data.source, "person_course")
    # Load the data file pertaining to items
    
    conversion <- person.course
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(conversion) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file person_course.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set person_course.json")
    return(NULL)
  }
  
  if (!username %in% conversion$username){
    print(paste("ERROR: The student with the username ", username, " does not exist.", sep = ""))
    return(NULL)
    # Check to see if the student with the username given actually exists
  }
  else{
    id <- conversion[conversion$username == username, "user_id"]
    # Find the corresponding id of the person with the given username
    return(id)
  }
  
}

Convert.ProblemToModule <- function(nid, conversion = course.problem, load.files = TRUE){
  # Function to take a problem nid number and output the name of the corresponding module
  # Takes two parameters - nid, conversion, and load.files
  
  # nid is an integer representing the integer nid of the problem in question
  # conversion is the data table that contains information linking each problem to a module name - defaults to the global course.problem
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  
  # Returns a string representing the full module name of the problem
  
  if(is.null(course.problem) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded
    
    course.problem <<- Load.File(data.source, "course_problem")
    # Load the data file pertaining to items
    
    conversion <- course.problem
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(conversion) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file course_problem.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set course_problem.json")
    return(NULL)
  }
  
  if (!nid %in% conversion$problem_nid){
    print(paste("ERROR: The problem with the id number ", nid, " does not exist.", sep = ""))
    # Check to see if the problem with the nid number given actually exists
    return(NULL)
  }
  else{
    problem.id <- conversion[conversion$problem_nid == nid, "problem_id"]
    
    # Find the corresponding problem id of the problem with the given nid number
    return(problem.id)
  }
}

####################################################################################
#### Functions for converting between integer time and standard format time ########
####################################################################################

Get.TimeDiff <- function(attempt1, attempt2){
  # Function to calculate the time difference between two separate standard format time entries
  # Returns an integer representing the number of seconds elapsed between the two instances
  
  day <- "([0-9]{4})[-.]([0-9]{2})[-.]([0-9]{2})"
  time <- "([0-9]{2})[:.]([0-9]{2})[:.]([0-9]{2})"
  
  time1 <- paste (str_extract(attempt1, day), str_extract(attempt1, time), sep = " ", collapse = NULL)
  time2 <- paste (str_extract(attempt2, day), str_extract(attempt2, time), sep = " ", collapse = NULL)
  
  x <- as.POSIXct( time1 )
  y <- as.POSIXct( time2 )
  
  return(as.integer(x) - as.integer(y))
}

TimeInteger <- function(attempt){
  # Function to convert standard format time to an integer representing seconds since 1970
  
  day <- "([0-9]{4})[-.]([0-9]{2})[-.]([0-9]{2})"
  time <- "([0-9]{2})[:.]([0-9]{2})[:.]([0-9]{2})"
  
  
  time1 <- paste (str_extract(attempt, day), str_extract(attempt, time), sep = " ", collapse = NULL)
  
  return(as.integer(as.POSIXct( time1 )))
}

IntegerTime <- function(time){
  # Function to return a standard format time given an integer representing time since 1970
  
  return(as.POSIXct(time, origin = "1970-01-01", tz = "EST"))
}

