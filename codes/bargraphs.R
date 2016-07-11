###########################################################################
########## Create Box Plots of the the Student Classifications ############
###########################################################################
library("ggplot2")
library("xlsx")
source("ExtractingDataFromTables.R")
source("dataextraction.R")

sk.1 <- c(1:6)
sk.2 <- c(34:37)
sk.4 <- c(72:73)
sk.7 <- c(129)



lp.1 <- c(7:17)
lp.2 <- c(38:46)
lp.4 <- c(74:78)
lp.7 <- c(130:132)

Classify.Week<- function(skill.challenges, practices, gradesheet = data.sheet){
  
  if (is.null(gradesheet)){
    gradesheet <- Read.Gradesheet()
  }
  
  usernames <- EmailtoUsername(gradesheet$Username)
  
  print("here")
  
  id.list <- list()
  for (username in usernames){
    id.list <- c(id.list, Convert.Username(username))
  }
  
  classes <- c("Careless", "Enthusiast", "Guilt", "Interspersed", "Interspersed.C", "Nonexistent", "Only Practice", "Perfect", "Practiceless", "Safe", "Second Thoughts", "Second Thoughts.C", "Very Safe")
  # Various classes 
  
  list.classifications <- list()

  for (id in id.list){
    classification <- Classify.Student(id, practices, skill.challenges)
    list.classifications <- c(list.classifications, classification)
  
  }
  
  table.classifications <- table(unlist(list.classifications))
  

  
  df <- as.data.frame(table.classifications)
  colnames(df) <- c("Classification","Frequency")
  
  
  df.test <- data.frame(classes)
  colnames(df.test) <- c("Classification")
  new.1 <- list()
  for (j in classes){
    if (j %in% df$Classification){
      new.1 <- c(new.1, df[df$Classification == j, "Frequency"])
      
    } else{
      new.1 <- c(new.1, 0)
    }
  }
  
  df <- data.frame(classes)
  df[["Frequency"]] = as.integer(new.1)
  colnames(df) <- c("Classification","Frequency")
  
  return(df)
}

Classify.Week<- function(partition_id, matrix.AB, matrices, gradesheet = data.sheet){
  
  if (is.null(gradesheet)){
    gradesheet <- Read.Gradesheet()
  }
  
  usernames <- EmailtoUsername(gradesheet$Username)
  
  print("here")
  
  id.list <- list()
  for (username in usernames){
    id.list <- c(id.list, Convert.Username(username))
  }
  
  classes <- c("Careless", "Enthusiast", "Guilt", "Interspersed", "Interspersed.C", "Nonexistent", "Only Practice", "Perfect", "Practiceless", "Safe", "Second Thoughts", "Second Thoughts.C", "Very Safe")
  # Various classes 
  
  list.classifications <- list()
  
  for (id in id.list){
    classification <- Classify.Student(id, practices, skill.challenges)
    list.classifications <- c(list.classifications, classification)
    
  }
  
  table.classifications <- table(unlist(list.classifications))
  
  
  
  df <- as.data.frame(table.classifications)
  colnames(df) <- c("Classification","Frequency")
  
  
  df.test <- data.frame(classes)
  colnames(df.test) <- c("Classification")
  new.1 <- list()
  for (j in classes){
    if (j %in% df$Classification){
      new.1 <- c(new.1, df[df$Classification == j, "Frequency"])
      
    } else{
      new.1 <- c(new.1, 0)
    }
  }
  
  df <- data.frame(classes)
  df[["Frequency"]] = as.integer(new.1)
  colnames(df) <- c("Classification","Frequency")
  
  return(df)
}

Classify.Distribution<- function(skill.challenge, practices, gradesheet = data.sheet){
  
  if (is.null(gradesheet)){
    gradesheet <- Read.Gradesheet()
  }
  
  usernames <- EmailtoUsername(gradesheet$Username)

  id.list <- list()
  for (username in usernames){
    id.list <- c(id.list, Convert.Username(username))
  }
  
  practiceless <- list()
  practice.before <- list()
  practice.after <- list()
  unsuccessful <- 0
  perfect <- 0
  
  for (id in id.list){
    output <- Get.Attempt.Distribution(id, practices, skill.challenge)
    if(length(output) == 2){
      if(output[1] == "Practiceless"){
        practiceless <- c(practiceless, output[2])
      } else{
        practice.before <- c(practice.before, output[1])
        practice.after <- c(practice.after, output[2])
      }
      
    } else if (!is.null(output)){
      if (output == "Unsuccessful"){
        unsuccesful <- unsuccessful + 1
      } else if(output == "Perfect"){
        perfect <- perfect+1
      }
    }
  }
  
  practiceless <- table(unlist(practiceless))
  practice.before <- table(unlist(practice.before))
  practice.after <- table(unlist(practice.after))
  df.1 <- as.data.frame(practiceless)
  colnames(df.1) <- c("Attempts","Frequency")
  df.2 <- as.data.frame(practice.before)
  colnames(df.2) <- c("Attempts","Frequency")
  df.3 <- as.data.frame(practice.after)
  colnames(df.3) <- c("Attempts","Frequency")
  print("Without Practice")
  print(df.1)
  df.1 <<- df.1
  print("With Practice Before")
  print(df.2)
  df.2 <<- df.2
  print("With Practice After")
  print(df.3)
  df.3 <<- df.3
  print("Perfect")
  print(perfect)
  perfect <<- perfect
  print("Unsuccessful")
  print(unsuccessful)
  unsuccessful <<- unsuccessful
  return(df.1)
}

Make.Plot <- function(data, title.graph){
  rearranged.data <- data[c(13,10, 4, 5, 11, 12, 3, 2, 7, 1, 9, 8, 6),]
  row.names(rearranged.data) <- 1:nrow(rearranged.data)
  rearranged.data$Classification <- as.character(rearranged.data$Classification)
  rearranged.data$Classification <- factor(rearranged.data$Classification, levels=unique(rearranged.data$Classification))

  plot <- (ggplot(data=rearranged.data, aes(x=Classification, y=Frequency, fill=Classification)) 
           +     geom_bar(colour="black", stat="identity") 
           +     guides(fill=FALSE)
           + labs(title = title.graph)
           + scale_y_continuous(limits=c(0, 250)))
  return(plot)
}