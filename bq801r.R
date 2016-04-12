# I added this commments
# here 1
# here 3
##################################################################################
####### This is the R script for extracting information for 8.01r 2015 Fall course
##################################################################################

### load required package
library(jsonlite)   # for fromJSON()

##################################################################################
##################### read json files into R dataframe ###########################
##################################################################################

### read 'person_course.json' to get person information 
person_course <- fromJSON(sprintf("[%s]", paste(readLines("D:\\Dropbox (MIT)\\BigQueryData\\data\\MITx__8_01r_2__2015_Fall\\person_course.json"), collapse=",")))
dim(person_course) # 782 30, so there are 782 students' IDs
user_id <- person_course$user_id

### read 'course_item.json' to get item information
course_item <- fromJSON(sprintf("[%s]", paste(readLines("D:\\Dropbox (MIT)\\BigQueryData\\data\\MITx__8_01r_2__2015_Fall\\course_item.json"), collapse=",")))
dim(course_item)  # 634 36, so there are 634 items
item_nid <- course_item$item_nid
item_id <- course_item$item_id
# here2
### read 'person_item.json' to get person-item information
person_item <- fromJSON(sprintf("[%s]", paste(readLines("D:\\Dropbox (MIT)\\BigQueryData\\data\\MITx__8_01r_2__2015_Fall\\person_item.json"), collapse=",")))

##################################################################################
########## create item response matrix (binary version: correct or not) ##########
##################################################################################

### create item response matrix (binary version: correct or not)
responseBin <- matrix(NA, nrow=length(user_id), ncol=length(item_nid))  # create matrixi with NAs
colnames(responseBin) <- item_nid
rownames(responseBin) <- user_id
length(unique(person_item$user_id))  # the number of unique user_id in person_item is 652 out of 782 students
for (index_user_id in unique(person_item$user_id)) {
  responseBin[which(user_id==index_user_id), item_nid %in% person_item$item_nid[person_item$user_id==index_user_id]] <- person_item$item_grade[person_item$user_id==index_user_id]
}
class(responseBin) <- "numeric"    # convert character matrix to numeric matrix 

### create item response matrix (multiple attempts version)
responseMA <- matrix(NA, nrow=length(user_id), ncol=length(item_nid))  # create matrixi with NAs
colnames(responseMA) <- item_nid
rownames(responseMA) <- user_id
length(unique(person_item$user_id))  # the number of unique user_id in person_item is 652 out of 782 students
for (index_user_id in unique(person_item$user_id)) {
  responseMA[which(user_id==index_user_id), item_nid %in% person_item$item_nid[person_item$user_id==index_user_id]] <- person_item$n_attempts[person_item$user_id==index_user_id]
}
class(responseMA) <- "numeric"    # convert character matrix to numeric matrix 

### negative coding = a student attempted a item multiple times but got the item wrong
# e.g., -3 = try 3 times but wrong
responseMAneg <- responseMA
for (row_index in 1:dim(responseBin)[1]) {
  for (col_index in 1:dim(responseBin)[2]) {
    if (!is.na(responseBin[row_index, col_index]) & responseBin[row_index, col_index]==0) {
      responseMAneg[row_index, col_index] <- -1*responseMA[row_index, col_index] 
    } 
  }
}

### export response matrices to CSV files
write.csv(responseBin, "responseBin.csv")
write.csv(responseMA, "responseMA.csv")
write.csv(responseMAneg, "responseMAneg.csv")



### check some basic statistics
# overall missing percentage 
missingOverall <- sum(is.na(as.vector(responseBin)))/length(as.vector(responseBin))   
missingOverall  # 0.62

# missing percentage by persons
missingPerson <- apply(responseBin, 1, function(x) sum(is.na(x))/length(item_nid))
missingPerson
sum(missingPerson==1)   # persons with no data = 130 
sum(missingPerson==1)/length(missingPerson)  # percentage of persons with no data = 0.17

# frequency table for missing by person
br = seq(0,1,by=0.1)
ranges = paste(br[-11], br[-1], sep=" - ")
freq = hist(missingPerson, breaks = seq(0,1,0.1), plot=TRUE)
cbind(range=ranges, frequency=freq$counts)

# missing percentage by items
missingItem <- apply(responseBin, 2, function(x) sum(is.na(x))/length(user_id))
missingItem
sum(missingItem==1)   # persons with no data = 130 
sum(missingItem==1)/length(missingItem)  # percentage of persons with no data = 0.17

# frequency table for missing by person
br = seq(0,1,by=0.1)
ranges = paste(br[-11], br[-1], sep=" - ")
freq = hist(missingItem, breaks = seq(0,1,0.1), plot=TRUE)
cbind(range=ranges, frequency=freq$counts)

















