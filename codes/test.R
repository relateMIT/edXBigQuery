#############################################################
#### This script is used as a test bed for functions ########
#############################################################
data_source <- setdir()
source("loadfiles.R")

course_problem <- load_file(data_source, "course_problem")


problem_nid_to_module_id <- function(nid){
  
  if(is.null(course_problem)){
    course_problem <<- load_file(data_source, "course_problem")
    # Check if the course_problem table is loaded and load if not
  }
  if (!nid %in% course_problem$problem_nid){
    print(paste("ERROR: The problem with the id number ", nid, " does not exist.", sep = ""))
    # Check to see if the problem with the nid number given actually exists
  }
  else{
    problem_id <- course_problem[course_problem$problem_nid == nid, "problem_id"]
    # Find the corresponding problem id of the problem with the given nid number
    return(problem_id)
  }
  
}
