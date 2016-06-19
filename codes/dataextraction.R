####################################################################################
# This script contains several functions useful for extracting data from log files #
####################################################################################

# load required packages
library("stringr")
library("date")
library("plyr")
library("jsonlite") 

####################################################################################
### Functions regarding conversion between standard date format and integer time ###
####################################################################################

time_diff <- function(attempt1, attempt2){
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

time_integer <- function(attempt){
  # Function to convert standard format time to an integer representing seconds since 1970
  
  day <- "([0-9]{4})[-.]([0-9]{2})[-.]([0-9]{2})"
  time <- "([0-9]{2})[:.]([0-9]{2})[:.]([0-9]{2})"
  
  
  time1 <- paste (str_extract(attempt, day), str_extract(attempt, time), sep = " ", collapse = NULL)
  
  return(as.integer(as.POSIXct( time1 )))
}


integer_to_time <- function(time){
  # Function to return a standard format time given an integer representing time since 1970
  
  return(as.POSIXct(time, origin = "1970-01-01", tz = "EST"))
}

####################################################################################
################### Import data sets from local directories ########################
####################################################################################


items <- fromJSON(sprintf("[%s]", paste(readLines("C:\\Users\\Alex\\Documents\\R_Project\\person_item.json"), collapse=",")))

problems <- fromJSON(sprintf("[%s]", paste(readLines("C:\\Users\\Alex\\Documents\\R_Project\\person_problem.json"), collapse=",")))

problem_check <- fromJSON(sprintf("[%s]", paste(readLines("C:\\Users\\Alex\\Documents\\R_Project\\problem_check.json"), collapse=",")))


####################################################################################
################# Functions for data extraction from log files #####################
####################################################################################




problem_time_compare <- function(id, num1, num2){
  # Function to retrieve time difference (in seconds) between two completed problems, given a single student
  # Takes three inputs: id, num1, num2
  # id - the integer id of the student in question
  # num1, num2 - the integer ids of the two problems considered
  # Returns an integer that specifies the number of seconds elapsed between the completion of each problem
  # Positive integer output implies that the first problem inputted (num1) was completed after the other (num2)

  if(id %in% problems$user_id){
    # Check to see if student exists within dataset
    
    student_data <- problems[problems$user_id == id,]
    # Select only data pertinent to the student

    if(num1 %in% student_data$problem_nid){
      # Check to see if the first problem exists within the subset
      
      if(num2 %in% student_data$problem_nid){
        # Check to see if the second problem exists within the subset
        
        p1 <- student_data[student_data$problem_nid == num1,6]
        p2 <- student_data[student_data$problem_nid == num2,6]
        # Extract only the columns containing the times at which the problems are completed
        
        return(time_diff(p1, p2))
      }
      
      else{
        # If the second problem does not exist within the subset
        print(paste("Invalid problem given/ Student did not complete problem ", num2, sep = "", collapse = NULL))
        return()
      }
    }  
    else{
      # If the first problem does not exist within the subset  
      print(paste("Invalid problem given/Student did not complete problem ", num1, sep = "", collapse = NULL))
      return()
    }
  }  
  else{
    # If the student does not exist within the dataset
    print(paste("Student with id number ", id," does not exist", sep = "", collapse = NULL))
    return()
  }
}


item_time_compare <- function(id, item1, item2){
  # Function to retrieve time difference (in seconds) between two completed items, given a single student
  # Takes three inputs: id, item1, item2
  # id - the integer id of the student in question
  # item1, item2 - the integer ids of the two items considered
  # Returns an integer that specifies the number of seconds elapsed between the completion of each item
  # Positive integer output implies that the first item inputted (item1) was completed after the other (item2)  
  
  if(id %in% items$user_id){
    # Check to see if student exists within dataset
    
    student_data <- items[items$user_id == id,]
    # Select only data pertinent to the student
    
    if(item1 %in% student_data$item_nid){
      # Check to see if the first item exists within the subset
      
      if(item2 %in% student_data$item_nid){
        # Check to see if the seconxd item exists within the subset
        
        t1 <- student_data[student_data$item_nid == item1,6]
        t2 <- student_data[student_data$item_nid == item2,6]
        # Extract only the columns containing the times at which the items are completed
        return(time_diff(t1, t2))
      }
      
      else{
        # If the second item does not exist within the subset
        print(paste("Invalid item given/ Student did not complete item ", item2, sep = "", collapse = NULL))
        return()
      }
    }
 
    else{
      # If the first item does not exist within the subset
      print(paste("Invalid item given/Student did not complete problem ", item1, sep = "", collapse = NULL))
      return()
    }
  }  
  else{
    # If the student does not exist within the dataset
    print(paste("Student with id number ", student," does not exist", sep = "", collapse = NULL))
    return()
  }
}

item_set_comparison <- function(students, items1, items2){
  # Function to output a histogram/ cumulative line graph of the completion of two sets of items given a set of students
  # Takes three inputs: students, items1, items2
  # students - a list of the integer ids of the students in question
  # items1, items2 - lists of the integer ids of the sets of items in question
  # TODO: problems - when true forces function to operate over the problem, rather than item, dataset
  
  # Returns two graphs:
  # First, A histogram that shows the frequency of item completion over time 
  # Second, A cumulative line graph that shows the percentage completion of the set of items over time
  
  num_students <- length(students)
  set1_num <- length(items1)
  set2_num <- length(items2)
  # Extract statistics on the number of students/problems in each of the three inputs
  
  set1_tot <- set1_num * num_students
  set2_tot <- set2_num * num_students
  # Calculate the total number of problems to be potentially completed by the students for each set of problems
  
  student_data = NULL
  # Data frame to contain the subset of the data containing only the students in question
  
  missing_students = NULL
  # Vector to contain a list of integers of all the students who were inputted but failed to shows up in the dataset
  
  for (id in students){
    # Iterate through each student in the list and add their data to student_data
    
    if(id %in% items$user_id){
      new_data <- items[items$user_id == id,]
      student_data <- rbind(student_data, new_data)
  
    }
    else{
      missing_students <- c(missing_students, id)
    }
  }
  print(paste(c("WARNING: Students with id number", missing_students,"do not exist"), collapse = " "))
  # Print list of missing students
  
  
  item_set1 = NULL
  item_set2 = NULL
  # Subsets to contain only the data for the items in question
  
  missing_item1 = NULL
  missing_item2 = NULL
  # Lists to contain the item ids that are missing in the dataset
  
  for (item_id in items1){
    # Iterate through each item in the first set and accumulate a subset of data containing only relevant entries
   
    if(item_id %in% student_data$item_nid){

      new_item <- student_data[student_data$item_nid == item_id,c(1,3,6)]
  

      item_set1 <- rbind(item_set1, new_item)
    }
    
    else{
      # If some items of the first set are missing
      missing_item1 <- c(missing_item1, item_id)
    }
  }
  print(paste(c("WARNING: Items with numbers", missing_item1,"do not exist or were not completed by any students"), collapse = " "))
  # Print list of missing items in first set
  
  for (item_id in items2){
    # Iterate through each item in the second set and accumulate a subset of data containing only relevant entries
    
    if(item_id %in% student_data$item_nid){
      new_item <- student_data[student_data$item_nid == item_id,c(1,3,6)]

      
      item_set2 <- rbind(item_set2, new_item)
    }
    
    else{
      # If items of the second set are missing
      missing_item2 <- c(missing_item2, item_id)
    }
  }
  print(paste(c("WARNING: Items with numbers", missing_item2,"do not exist or were not completed by any students"), collapse = " "))
  # Print list of missing items in second set
  
  times1 = NULL
  times2 = NULL
  # Lists for the times of the completion of the items
  
  completed_set1 <- nrow(item_set1)

  completed_set2 <- nrow(item_set2)

  # The number of items completed by the students for each set
  
  if (is.null(item_set1)){
    print("ERROR: None of items in the first set were completed by any student")
    # Check to see if any items were actually completed (so that there is data to compare)
    return()
    break
  }
  
  else{
    
    for (i in 1:completed_set1){
      times1 = c(times1, time_integer(item_set1[i,3]))
      # Extract the times for each relevant item in first set
    }
  }
  
  if (is.null(item_set2)){
    print("ERROR: None of the items in the first set were completed by any student")
    # Check again to see if any items in the second set were completed
    return()
  }
  else{
    for (j in 1:completed_set2){
      times2 = c(times2, time_integer(item_set2[j,3]))
      # Extract the times for each relevant item in second set
    }
  }
  
  min_time = min(min(times2), min(times1))
  # Extract the earliest time of any item completion
  max_time = max(max(times2), max(times1))
  # Extract the maximum time of any item completion
  
  #print(min_time)
  #print(max_time)

  times = seq(min_time, max_time, length.out = 20)
  # Create a series of timestamps between the earliest and latest item completion
  
  timestamps = NULL
  # A list to store the standard-time format conversions of each timestamp
  for (j in times){
    timestamps <- rbind(timestamps, integer_to_time(j))
  }
  
  print(timestamps)
  # Lists the timestamps for the tick marks
  
  time1_cut = cut(times1, times, right=FALSE)
  time2_cut = cut(times2, times, right=FALSE)
  time1_freq = table(time1_cut)
  time2_freq = table(time2_cut)
  freq1 = c(0,cumsum(time1_freq)) / set1_tot
  freq2 = c(0,cumsum(time2_freq)) / set2_tot
  # Obtain the data lists for the cumulative line graph  
  
  plot(freq1,type="b", ylim=c(0,1),col="red",lty=1,ylab="Percentage Completed",lwd=2,xlab="Time",xaxt="n")
  lines(freq2,type="b",col="black",lty=2,lwd=2)
  # Create the cumulative line graph

  hist(times1, col=rgb(1,0,0,0.5), xlim = c(min_time, max_time) , main = "Histogram of Completion Frequency", xlab = "Time")
  hist(times2, col=rgb(0,0,1,0.5), add=T)
  box()
  # Create the double histogram
}

item_completion <- function(id, item, attempt){
  # Function to extract the integer time that a particular attempt on a problem was made by a student
  # Takes three inputs id, item, attempt
  # id is a string representing the username of the student in question
  # item is the name of the module in question
  # attempt is an integer representing the number of the attempt in consideration
  
  student_data = NULL
  item_data = NULL
  
  if(id %in% problem_check$username){
    # Check to see if student exists within dataset
    
    student_data <- problem_check[problem_check$username == id,]
    # Select only data pertinent to the student
  }
  else{
    # If the student does not exist within the dataset
    print(paste("Student with id ", id," does not exist", sep = "", collapse = NULL))
    return()
  }
  
  item <- paste ("MITx/8.01r_2/problem/", item, sep = "", collapse = NULL)
  # Convert item into standard log file format
  
  if(item %in% student_data$module_id){
    # Check to see if student completed selected item
    item_data <- student_data[student_data$module_id == item,c(4,6)]
  }
  
  else{
    # If the student did not complete the item or if the item does not exist
    print(paste("Item with name ", item," does not exist or student did not complete it", sep = "", collapse = NULL))
    return()
  }
  
  
  if(nrow(item_data) < attempt){
    # Check to see if the student participated in the given number of attempts
    print(paste("The student did not complete ", attempt," attempts of the item", sep = "", collapse = NULL))
    return()
  }
  else{
    time <- item_data[attempt,2]
    return(time_integer(time))
  }
}


