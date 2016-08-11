###########################################################################
########## Create Box Plots of the the Student Classifications ############
###########################################################################
library("ggplot2")
library("xlsx")
source("ExtractingDataFromTables.R")
source("dataextraction.R")
Load.Weeks <- function(){
  sk.1 <<- c(1:6)
  sk.2 <<- c(34:37)
  sk.4 <<- c(72:73)
  sk.7 <<- c(129)
  
  
  
  lp.1 <<- c(7:17)
  lp.2 <<- c(38:46)
  lp.4 <<- c(74:78)
  lp.7 <<- c(130:132)
}

Classify.Week<- function(skill.challenges, practices, xgradesheet = gradesheet){
  
  if (is.null(xgradesheet)){
    gradesheet <<- Read.Gradesheet("gradesheet")
    xgradesheet <- gradesheet
  }
  
  usernames <- EmailtoUsername(xgradesheet$Username)
  
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
    list.classifications[[id]] <-  classification
  
  }
  
  table.classifications <- table(unlist(list.classifications))
  cc <<- list.classifications

  
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

Correct.Number.Get <- function(skill.challenges, classifications){
  first.correct.number <- 0
  final.correct.number <- 0
  for (i in names(classifications)){
    print(i)
    final.correct <- FALSE
    current.attempt <- 1
 
    for(j in skill.challenges){
      x <- Check.Item(i, j, current.attempt)
      print(x)
      
      if(!is.null(x) && x > 0){
        final.correct <- TRUE
        first.correct.number <- first.correct.number + 1
      }
      while(!is.null(x)){
        current.attempt <- current.attempt + 1
        x <- Check.Item(i,j,current.attempt)
        if(!is.null(x) && x>0){
          final.correct <- TRUE
        }
      }
    if (final.correct == TRUE){
      final.correct.number <- final.correct.number + 1
    } 
    }
  }
  print(final.correct.number)
  print(first.correct.number)
}

Correct.Number.Get.Practice <- function(skill.challenges, classifications){
  first.correct.number <- 0
  final.correct.number <- 0
  for (i in names(classifications)){
    if(classifications[i] == "Safe" || classifications[i] == "Very Safe" || classifications[i] == "Interspersed" || classifications[i] == "Interspersed.C" || classifications[i] == "Second Thoughts" || classifications[i] == "SecondThoughts.C"){
      final.correct <- FALSE
      current.attempt <- 1
      
      for(j in skill.challenges){
        x <- Check.Item(i, j, current.attempt)
        print(x)
        
        if(!is.null(x) && x > 0){
          final.correct <- TRUE
          first.correct.number <- first.correct.number + 1
        }
        while(!is.null(x)){
          current.attempt <- current.attempt + 1
          x <- Check.Item(i,j,current.attempt)
          if(!is.null(x) && x>0){
            final.correct <- TRUE
          }
        }
        if (final.correct == TRUE){
          final.correct.number <- final.correct.number + 1
        } 
      }
    }
    
    
  }
  print(final.correct.number)
  print(first.correct.number)
}

Correct.Number.Get.Practice.After <- function(skill.challenges, classifications){
  first.correct.number <- 0
  final.correct.number <- 0
  for (i in names(classifications)){
    if(classifications[i] == "Enthusiast" || classifications[i] == "Guilt" ){
      final.correct <- FALSE
      current.attempt <- 1
      
      for(j in skill.challenges){
        x <- Check.Item(i, j, current.attempt)
        print(x)
        
        if(!is.null(x) && x > 0){
          final.correct <- TRUE
          first.correct.number <- first.correct.number + 1
        }
        while(!is.null(x)){
          current.attempt <- current.attempt + 1
          x <- Check.Item(i,j,current.attempt)
          if(!is.null(x) && x>0){
            final.correct <- TRUE
          }
        }
        if (final.correct == TRUE){
          final.correct.number <- final.correct.number + 1
        } 
      }
    }
    
    
  }
  print(final.correct.number)
  print(first.correct.number)
}

Correct.Number.Change.Practice <- function(skill.challenges, classifications1, classifications2){
  first.correct <- list()
  last.correct <- list()
  for (i in names(classifications1)){
    if(classifications1[i] == "Safe" || classifications1[i] == "Very Safe" || classifications1[i] == "Interspersed" || classifications1[i] == "Interspersed.C" || classifications1[i] == "Second Thoughts" || classifications1[i] == "SecondThoughts.C"){
      if(classifications2[i] == "Practiceless" || classifications2[i] == "Careless" || classifications2[i] == "Perfect"){
        first.correct.number <- 0
        last.correct.number <- 0
        for(j in skill.challenges){
          a <- Check.Item(i,j,1)
          b <- Check.Item.Final(i,j)
          
          if(!is.null(a) && a > 0){
            first.correct.number <- first.correct.number + 1
          }
          if(!is.null(b) && b > 0){
            last.correct.number <- last.correct.number + 1
          }
          
        }
        first.correct[[i]] <- first.correct.number
        last.correct[[i]] <- last.correct.number
        
      }
    }
      
    
  }
  set1 <<- first.correct
  set2 <<- last.correct
}

Classify.Week.AB<- function(partition.id, module.id, skill.challenges, practices, test.breakdown = split.test, data.sheet = gradesheet, breakdown = students.AB){
  
  classes <- c("Careless", "Enthusiast", "Guilt", "Interspersed", "Interspersed.C", "Nonexistent", "Only Practice", "Perfect", "Practiceless", "Safe", "Second Thoughts", "Second Thoughts.C", "Very Safe")
  # Various classes 
  
  if (is.null(data.sheet)){
    gradesheet <<- Read.Gradesheet("gradesheet")
    data.sheet <- gradesheet
    
  }
  
  if(is.null(breakdown)){
    students.AB <<- Classify.AB()
    breakdown <- students.AB
  }
  
  usernames <- EmailtoUsername(data.sheet$Username)
  
  print("here")
  
  id.list <- list()
  for (username in usernames){
    id.list <- c(id.list, Convert.Username(username))
  }
  
  list.classifications <- list()
  
  for (id in id.list){
    
    z <- breakdown[id][[1]]
    if(partition.id %in% z[,1]){
      print("First")
      print(id)
      if(module.id == z[z[,1] == partition.id, 2]){
        print(z[z[,1] == partition.id, 2])
        print(id)
        classification <- Classify.Student(id, practices, skill.challenges)
        list.classifications[[id]] <- classification
      }
    }
    
    
  }
  b <<- list.classifications
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
    gradesheet <- Read.Gradesheet("gradesheet")
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