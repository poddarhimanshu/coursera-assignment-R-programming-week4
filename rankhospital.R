rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!state %in% unique(outcome_data[[7]])){
    stop("invalid state")
  }
  outcome_list <- c('heart attack','heart failure','pneumonia')
  outcome_index <- c(11, 17, 23)
  names(outcome_index) <- outcome_list
  if(!outcome %in% names(outcome_index)){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  new_data <- outcome_data[outcome_data$State==state,][c(outcome_index[outcome],2)]
  new_data[1] <- as.numeric(new_data[[1]])
  
  ordered_data <- new_data[order(new_data[1],new_data[2]),] 
  if(num=="best"){
    return (ordered_data[1,2])
  }
  else if(num=="worst"){
    return (ordered_data[complete.cases(ordered_data),][nrow(ordered_data[complete.cases(ordered_data),]),][,2])
  }
  else
    return (ordered_data[num,2])
  ## 30-day death rate

}