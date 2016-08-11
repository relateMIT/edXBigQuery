###########R function for creating Response Matrix from Computed Tables############
###########Based on Sunbok's R script, so it does not strictly adhere to Google's R-style guide##### 

# Requires 3 data tables: person_course, course_item, person_item
# if returnAll is TRUE, then returns a list containing all three matrices.
# otherwise simply return the multiple-attempt-negative-coding matrics

Create.ResponseMatrix <- function(person_course, course_item, person_item, returnAll = TRUE){
  user_id <- person_course$user_id
  item_nid <- course_item$item_nid
  item_id <- course_item$item_id
  
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
  
  if (returnAll) {
    responseMatrices <- list(binary = responseBin, multipleAttempt = responseMA, multANeg = responseMAneg)
    return(responseMatrices)
  } else {
    return(responseMAneg)
  }
}