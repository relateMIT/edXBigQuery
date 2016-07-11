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
###################################### TODO ########################################
####################################################################################

# Replace load.files parameters with something that prompts the user
# Revise Compare.ItemSets

####################################################################################
################################# FUNCTIONS ########################################
####################################################################################

# FOR USER USE:
# Compare.ProblemTimes - Finds the difference between two different problem attempt instances
# Compare.ItemTimes - Does the same but for items
# Compare.ItemSets - WIP
# Classify.Student -Returns the classification of a student's attempt behavior given two sets of problems
  # Calls on Check.Item as a subroutine
# Check.Item - Finds whether or not a specific instance of an item was completed and the correctness of the student's answer

# All other functions are subroutines of the others

####################################################################################
################### Import data sets from local directories ########################
####################################################################################

source("testset.R")
# Call Load.All() to initialize all necessary data table variables

source("setdir.R")
# Obtain the directory setting function from the setdir.R file

print("Please enter the filepath to the folder containing the MITx log files")
data.source <<- Set.Dir()
# Obtain file path by copy and pasting location of data tables from file explorer - will be used for each Load.File function call

Set.Class <- function(){
  # Function to set the global class name, if desired
  
  print("Please enter the edX name of the class (e.g. MITx/8.012r_2)")
  class.global <<- Set.Dir()
  return(class.global)
  # class variable is used in every instance of going through the problem_check logs
}

source("loadfiles.R")
# Obtain the file loading function from loadfiles.R - to be called whenever a table has to be loaded

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

Compare.ItemSets <- function(students, items1, items2){
  #####################################
  ######## TO BE REVISED - WIP ########
  #####################################
  
  # Function to output a histogram/ cumulative line graph of the completion of two sets of items given a set of students
  # Takes three inputs: students, items1, items2
  # students - a list of the integer ids of the students in question
  # items1, items2 - lists of the integer ids of the sets of items in question
  # TODO: problems - when true forces function to operate over the problem, rather than item, dataset
  
  # Returns two graphs:
  # First, A histogram that shows the frequency of item completion over time 
  # Second, A cumulative line graph that shows the percentage completion of the set of items over time
  
  if(is.null(person.item)){
    person.item <<- Load.File(data.source, "person_item")
    # Check if the person.item table is loaded and load if not
  }
  
  num.students <- length(students)
  set1.num <- length(items1)
  set2.num <- length(items2)
  # Extract statistics on the number of students/problems in each of the three inputs
  
  set1.tot <- set1.num * num.students
  set2.tot <- set2.num * num.students
  # Calculate the total number of problems to be potentially completed by the students for each set of problems
  
  student.data = NULL
  # Data frame to contain the subset of the data containing only the students in question
  
  missing.students = NULL
  # Vector to contain a list of integers of all the students who were inputted but failed to shows up in the dataset
  
  for (id in students){
    # Iterate through each student in the list and add their data to student.data
    
    if(id %in% person.item$user_id){
      new.data <- person.item[person.item$user_id == id,]
      student.data <- rbind(student.data, new.data)
  
    }
    else{
      missing.students <- c(missing.students, id)
    }
  }
  print(paste(c("WARNING: Students with id number", missing.students,"do not exist"), collapse = " "))
  # Print list of missing students
  
  
  item.set1 = NULL
  item.set2 = NULL
  # Subsets to contain only the data for the items in question
  
  missing.item1 = NULL
  missing.item2 = NULL
  # Lists to contain the item ids that are missing in the dataset
  
  for (item.id in items1){
    # Iterate through each item in the first set and accumulate a subset of data containing only relevant entries
   
    if(item.id %in% student.data$item_nid){

      new.item <- student.data[student.data$item_nid == item.id, c(1,3,6)]
  

      item.set1 <- rbind(item.set1, new.item)
    }
    
    else{
      # If some items of the first set are missing
      missing.item1 <- c(missing.item1, item.id)
    }
  }
  print(paste(c("WARNING: Items with numbers", missing.item1,"do not exist or were not completed by any students"), collapse = " "))
  # Print list of missing items in first set
  
  for (item.id in items2){
    # Iterate through each item in the second set and accumulate a subset of data containing only relevant entries
    
    if(item.id %in% student.data$item_nid){
      new.item <- student.data[student.data$item_nid == item.id, c(1,3,6)]

      
      item.set2 <- rbind(item.set2, new.item)
    }
    
    else{
      # If items of the second set are missing
      missing.item2 <- c(missing.item2, item.id)
    }
  }
  print(paste(c("WARNING: Items with numbers", missing.item2,"do not exist or were not completed by any students"), collapse = " "))
  # Print list of missing items in second set
  
  times1 = NULL
  times2 = NULL
  # Lists for the times of the completion of the items
  
  completed.set1 <- nrow(item.set1)

  completed.set2 <- nrow(item.set2)

  # The number of items completed by the students for each set
  
  if (is.null(item.set1)){
    print("ERROR: None of items in the first set were completed by any student")
    # Check to see if any items were actually completed (so that there is data to compare)
    return()
    break
  }
  
  else{
    
    for (i in 1:completed.set1){
      times1 = c(times1, time.integer(item.set1[i,3]))
      # Extract the times for each relevant item in first set
    }
  }
  
  if (is.null(item.set2)){
    print("ERROR: None of the items in the first set were completed by any student")
    # Check again to see if any items in the second set were completed
    return()
  }
  else{
    for (j in 1:completed.set2){
      times2 = c(times2, time.integer(item.set2[j,3]))
      # Extract the times for each relevant item in second set
    }
  }
  
  min.time = min(min(times2), min(times1))
  # Extract the earliest time of any item completion
  max.time = max(max(times2), max(times1))
  # Extract the maximum time of any item completion
  
  #print(min.time)
  #print(max.time)

  times = seq(min.time, max.time, length.out = 20)
  # Create a series of timestamps between the earliest and latest item completion
  
  timestamps = NULL
  # A list to store the standard-time format conversions of each timestamp
  for (j in times){
    timestamps <- rbind(timestamps, integer.to.time(j))
  }
  
  print(timestamps)
  # Lists the timestamps for the tick marks
  
  time1.cut = cut(times1, times, right=FALSE)
  time2.cut = cut(times2, times, right=FALSE)
  time1.freq = table(time1.cut)
  time2.freq = table(time2.cut)
  freq1 = c(0,cumsum(time1.freq)) / set1.tot
  freq2 = c(0,cumsum(time2.freq)) / set2.tot
  # Obtain the data lists for the cumulative line graph  
  
  plot(freq1,type="b", ylim=c(0,1),col="red",lty=1,ylab="Percentage Completed",lwd=2,xlab="Time",xaxt="n")
  lines(freq2,type="b",col="black",lty=2,lwd=2)
  # Create the cumulative line graph

  hist(times1, col=rgb(1,0,0,0.5), xlim = c(min.time, max.time) , main = "Histogram of Completion Frequency", xlab = "Time")
  hist(times2, col=rgb(0,0,1,0.5), add=T)
  box()
  # Create the double histogram
}

Classify.Student <- function (student.id, practice, skill.challenges, check = problem.check, convert.course = person.course, convert.problem = course.problem, load.files = TRUE){
  # Function to characterize student into one of several (currently unpolished) classifications
  # Takes seven inputs: student.id, practice, skill.challenges, check, convert.course, convert.problem, and load.files
  
  # student.id is the integer value of the student's id number
  # practice is a list of integers comprising of the problem ids of the practice problems
  # skill.challenges is a list of integers comprising of the problem ids of the challenge problems
  # check is the data set that contains the check attempts of the students
  # convert.course is the data set that contains the information relating student usernames and ids
  # convert.problem is the data set that contains the information relating problem ids and module ids
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data
  
  # Returns a string representing the classification that the student falls into
  
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
  
  challenge.list <- list()
  # list to store the times that skill.challenges were completed
  
  first.incorrect.challenge <- Inf
  # Variable to store the integer time of the first correct challenge completed
  
  minimum.challenge <- Inf
  # Variable to store the earliest attempt of any challenge
  
  maximum.challenge <- -Inf
  # Variable to store the latest attempt of any challenge
  failed.challenge <- FALSE
  # Boolean to test to see if a student failed any one of the challegnes
  
  all.attempted <- TRUE
  # Boolean to test to see if a student at least attempted all of the challenges
  
  all.correct <- TRUE
  # Boolean to test to see if the student ended up with all of the items correct
  
  correct.number <- 0
  # Variable representing the number of skill challenges the student completed
  
  never.wrong <- TRUE
  # Variable representing whether or not the student ever had an incorrect response
  
  challenge.number <- 0
  # Variable representing the number of skill challenges attempted
  
  for (j in skill.challenges){
    
    attempt <- 1
    # Check for first attempts of each problem
    
    attempt.time <- Check.Item(student.id, j, attempt, check, convert.course, convert.problem, load.files)
    challenge.list[[as.character(j)]] <- attempt.time
    # Extract the times for the first attempts and store
    
    if(is.null(attempt.time)){
      all.attempted <- FALSE
      # A NULL means that this skill challenge was not even attempted once
      
    } else{
      
      challenge.number <- challenge.number + 1
      # Increment the challenge.number variable as a challenge was attempted
      
      minimum.challenge <- min(abs(attempt.time), minimum.challenge)
      maximum.challenge <- max(abs(attempt.time), maximum.challenge)
      # Regardless of correctness of attempt - find earliest and latest attempts of challenges
      
      if (attempt.time > 0){
        correct.number <- correct.number + 1
        # Positive times means correct response
        
      } else{
        
        first.incorrect.challenge = min(abs(first.incorrect.challenge), attempt.time)
        # If the student answered the challenge incorrectly on the first  attempt
        
        never.wrong <- FALSE
        # The student has failed a first.attempt of a challenge and so does not satisfy the conditions for this variable
        
        failed.challenge <- TRUE
        # If the attempt.time is negative, the student failed his/her first attempt of the challenge
        
        while(!is.null(attempt.time)){
          # Check to see if the student made further attempts
          
          attempt <- attempt + 1
          # Increment the attempt number to see further attempts
          
          attempt.time <-Check.Item(student.id, j, attempt, check, convert.course, convert.problem, load.files)
          challenge.list[[as.character(j)]] = c(challenge.list[[as.character(j)]], attempt.time)
          # Extract the times for the subsequent attempts and store
          
          if(!is.null(attempt.time)){
            # Make sure the attempt was taken before doing comparisons
            
            minimum.challenge <- min(abs(attempt.time), minimum.challenge)
            maximum.challenge <- max(abs(attempt.time), maximum.challenge)
            
            if (attempt.time > 0){
              correct.number <- correct.number + 1
              # Positive times means correct response
              
           
              
            }
          }
        }
      }
    }
  }
  
  practice.list <- list()
  # List to store the times that the student attempted any practice problems
  
  first.practice <- Inf
  # Variable representing the integer time of the first instance of practice completed
  
  last.practice <- -Inf
  # Variable representing the integer time of the last instance of practice completed

  practice.number <- 0
  # Variable representing the number of problems the student attempted to practice
  
  pratice.before <- 0
  # Variable representing the number of problems the student attempted to practice before he/she attempted the first skill challenge
  
  for (k in practice){
   
    attempt <- 1
    # Check for first attempts of each problem
    
    attempt.time <- Check.Item(student.id, k, attempt, check, convert.course, convert.problem, load.files)
    practice.list[[as.character(k)]] <- attempt.time
    # Extract the times for the first attempts and store
    
    if(!is.null(attempt.time)){
      practice.number <- practice.number + 1 
      # If the student attempted the practice problem, the attempt time will not be NULL
      
      if(attempt.time < minimum.challenge){
        pratice.before <- pratice.before + 1
        # If the student attempted the practice problem before the first skill challenge was attempted
      }
      
      first.practice <- min(abs(attempt.time), first.practice)
      last.practice <- max(abs(attempt.time), last.practice)
      # Use absolute values since attempted practice (regardless of correctness) is what is important
      
      while(!is.null(attempt.time)){
        attempt <- attempt + 1
        # Increment attempt to check for further attempts on the item
        
        attempt.time <- Check.Item(student.id, k, attempt, check, convert.course, convert.problem, load.files)
        practice.list[[as.character(k)]] = c(practice.list[[as.character(k)]], attempt.time)
        # Extract the times for the subsequent attempts and store
        if(!is.null(attempt.time)){
          first.practice <- min(abs(attempt.time), first.practice)
          last.practice <- max(abs(attempt.time), last.practice)
        }
      }
    }
  }

  
  if(practice.number == 0){
    # If the student attempt no practice questions whatsoever
    
    if(correct.number == length(skill.challenges)){
      # If the student got all of the skill challenges correct
      ##### Classification 0.1 - Perfect - NO Practice, All Correct #####
      
      if(never.wrong == TRUE){
        # If the student got all the problems correct on the first try
        
        return("Perfect")
      }
      ##### Classification 0.2 - Practiceless - NO Practice, All Correct #####
      else{
        return("Practiceless")
      }  
   
      
    ##### Classification -1 - Nonexistent - NO Practice, No Attempts on Challenge Either #####
    } else if (challenge.number == 0 ){
      
      return("Nonexistent")
      
    ##### Classification 1 - Careless - NO Practice, Some Attempts (Not all Correct) #####
    } else{
      
      return("Careless")
    }
    
    ##### Classification 2 - Only Practice - Only Practice, No Attempts of Skill Challenges #####
  } else if(challenge.number == 0){
    
    return("Only Practice")
    
  } else if(first.practice > maximum.challenge){
    # If the student practiced only after completing all the skill challenges
    
    ##### Classification 3.1 - Enthusiast - Practice After Succesfully Completing All Challenges #####
    if(never.wrong){
      # If the student perfectly passed all of the challenges
      return("Enthusiast")
      
    ##### Classification 3.2 - Guilt - Practice After All Challenges, Some Unsuccessfully ##### 
    } else{
      return("Guilt")
    }
    
  } else if(first.practice < minimum.challenge){
    # If the student practiced before attempting a single challenge problem
    
    ##### Classification 4.1 - Very Safe - >50% Practice Before First Skill Challenge #####
    if(pratice.before > length(practice)/2){
      # If the student practiced more than half of the problems inputted before attempting a single challenge problem
      return("Very Safe")
      
    ##### Classification 4.2 - Safe - Some Practice Before First Skill Challenge #####   
      
    } else{
      return("Safe")
    }
    
  } else if(first.practice > first.incorrect.challenge){
    # If the student practiced only after first completing unsuccessfully at least one challenge
    
    ##### Classification 5.1 - Contained - Interspersed   - Practice After First Successful Skil Challenge Attempt, Finish Practice Before Last Challenge Attempt #####
    if(last.practice < maximum.challenge){
      # If the student's practice was contained in between the first successful challenge attempt and the last challenge attempt
      return("Interspersed.C")
      
    ##### Classification 5.2 - Interspersed   - Practice After First Successful Skil Challenge Attempt, Continue Practice After Last Challenge Attempt #####
      
    } else{
      return("Interspersed")
    }
    
  } else if (first.practice < first.incorrect.challenge){
    # If the student started practicing before unsuccesfully attempting at least one challenge but perhaps after getting some correct
    
    ##### Classification 6.1 - Contained - Second Thoughts   - Practice After Starting Challenges, But Before Any Correct Challenges; Finish Practice Before Last Challenge Attempt #####
    if(last.practice < maximum.challenge){

      return("Second Thoughts.C")
      
      ##### Classification 6.2 - Second Thoughts   - Practice After Starting Challenges, But Before Any Correct Challenges; Continue Practice After Last Challenge Attempt #####
      
    } else{
      return("Second Thoughts")
    }
  }
}

Check.Item <- function(id, item, attempt, check = problem.check, convert.course = person.course, convert.problem = course.problem, load.files = TRUE, class = class.global){
  # Function to extract the integer time that a particular attempt on a problem was made by a student
  # Takes eight inputs: student.id, practice, skill.challenges, check, convert.course, convert.problem, and load.files, and class
  
  # student.id is the integer value of the student's id number
  # practice is a list of integers comprising of the problem ids of the practice problems
  # skill.challenges is a list of integers comprising of the problem ids of the challenge problems
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
####################################################################################
######################## AB Experiment Functions ###################################
####################################################################################

Classify.AB <- function(test.breakdown = split.test, grades = gradesheet, conversion = person.course, course.i = course.item, person.i = person.item, load.files = TRUE){
  # Function to retrieve the group and partitiod ids of every student given an input containing the relevant breakdown per problem
  # Takes 6 inputs: test.breakdown, grades, conversion, course.i, person.i, and load.Files

  # test.breakdown is the file containing the information detailing the partition/group ids of each problem
  # grades is the gradesheet of the course containing the ids of each student
  # conversion is the data set that contains the information relating student usernames and ids - defaults to the global person.course
  # course.i is the data set that contains the information detailing all items in the course - defaults to the global course.item
  # person.i is the data set that contains the information relating individual students and items - defaults to the global person.item
  # load.files is a boolean that determines whether or not the user wishes to have the script load the files with the default names for data

  # Returns a list of matrices for each student - the matrices contain the group and partition ids for the corresponding students

  if(is.null(person.item) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded for person.item
  
    person.item <<- Load.File(data.source, "person_item")
    # Load the data file pertaining to items
  
    person.i <- person.item
    # Allow the function to utilize the imported dataset
  
  } else if(is.null(person.i) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
  
    print("ERROR: User did not supply dataset/file person_item.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set person_item.json")
    return(NULL)
  }
  
  if(is.null(course.item) && load.files == TRUE){
    # If the user wished for the script to load files and the default data set has not yet been loaded for course.item
    
    course.item <<- Load.File(data.source, "course_item")
    # Load the data file pertaining to items
    
    course.i <- course.item
    # Allow the function to utilize the imported dataset
    
  } else if(is.null(course.i) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file person_item.json has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set person_item.json")
    return(NULL)
  }
  if (is.null(grades) && load.files == TRUE){
    gradesheet <<- Read.Gradesheet()
    grades <- gradesheet
  } else if(is.null(grades) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file of the course gradesheet has not yet been loaded. 
          Please call the function again and supply dataset or set load.files = TRUE to call default data set gradesheet.xlsx")
    return(NULL)
  }
  if (is.null(test.breakdown)  & load.files == TRUE){
    split.test <<- Load.CSV(data.source, "splitTest_itemAssigned")
    test.breakdown <- split.test
  } else if(is.null(test.breakdown) && load.files == FALSE){
    # If the user did not supply a data set or the default data was not loaded and the user did not wish to load a file
    
    print("ERROR: User did not supply dataset/file containing the AB experiment split has not yet been loaded. 
          Please call the function again and supply dataset/set load.files = TRUE to call default data set splitTest_itemAssigned.CSV")
    return(NULL)
  }
  source("ExtractingDataFromTables.R")
  usernames <- EmailtoUsername(gradesheet$Username)
  # Takes the gradesheet and extracts all the usernames of the students in the course
  
  
  id.list <- list()
  # List to contain the individual ids of the students
  
  for (username in usernames){
    # Convert all of the usernames from the gradesheet to integer ids for use 
    id.list <- c(id.list, Convert.Username(username, conversion, load.files))
  }
  
  source("createResponseMatrix_function.R")
  response.matrix <- Create.ResponseMatrix(conversion, course.i, person.i, FALSE)
  # Creates a nonbinary matrix that contains information to determine whether or not a student completed any of the problems relevant to the AB experiments
  
  classification.list <- list()
  # List to contain the matrices for the AB classification for each students
  for (student.id in id.list){
    
    student.groups <- matrix(ncol = 2)
    # Initialize matrix to contain pairs of partition/group ids
    student.responses <- response.matrix[student.id,]
    # Find the row of the response matrix corresponding to the relevant student
    
    for(j in test.breakdown$item_nid){
      # Check each item in the AB conversion sheet - esp. if a student was assigned to a set of problems but only completed some
      if(!is.na(student.responses[j])){
        # If the student actually completed or attempted the relevant problem
        identifier <- test.breakdown[test.breakdown$item_nid == j, c("partition_id", "group_id")]
        # The partition_id, group_id pairing to identify a problem and thus students
        if(!identifier[1] %in% student.groups[,1]){
          # If the partition id has already been placed in the student matrix - prevent duplicate entries in the matrices
          if(all(is.na(student.groups))){
            # If the matrix for the student is still empty (i.e. no classification has been given)
            student.groups[1,] <- as.numeric(as.list(identifier))
            # Fill the empty matrix's first row
          } else{
            # If some classification has already given to the student
            rbind(student.groups, as.numeric(as.list(identifier)))
            # Append on to the matrix
          }
        }
      }
    }
    
    classification.list[[student.id]] <- student.groups
    # Append the student matrix to the classification list
    
  }
  return(classification.list)
  # Return the list of matrices
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
  # problems is a list of integers representing the problem nids
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

Get.Attempt.Distribution <- function(student.id, practices, challenge, check = problem.check, convert.course = person.course, convert.problem = course.problem, load.files = TRUE){
  # Function to get a distribution of attempts for a student regarding a single problem
  
  challenge.times <- Get.All.Attempts(student.id, challenge, check, convert.course, convert.problem, load.files)

  
  if(length(challenge.times) == 0){
    # If the student never completed the challenge
    return(NULL)
  } else{

    attempts = length(challenge.times[[1]])

    if (challenge.times[[1]][attempts] < 0){
      # If the student never succesffuly completed the challenge - i.e. their last attempt was incorrect
      return("Unsuccessful")
    } else if (attempts == 1){
      # If the student completed the challenge succesfully on the first try
      return("Perfect")
    }
    else {
      practice.times <- Get.All.Attempts(student.id, practices, check, convert.course, convert.problem, load.files)
      # Get all the times practice questions were attempted
      
      practice.start <- NULL
      # Variable to store the number of the challenge attempt after which practice (if any) was attempted
      
      for (j in 2:attempts){
        
        for (k in practice.times){
          
          if(abs(k) <= challenge.times[[1]][j] && abs(k) >= challenge.times[[1]][j-1]){
            if(is.null(practice.start)){
              practice.start <- j-1
            }
          }
        }
      }
      
      if(is.null(practice.start)){
        return(c("Practiceless", attempts))
      } else{
        return(c(practice.start, attempts))
      }
    }
  }
}