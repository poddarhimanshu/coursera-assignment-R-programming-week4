best <- function(state, outcome) {
  
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
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  new_data <- outcome_data[outcome_data$State==state,][c(outcome_index[outcome],2)]
  new_data[1] <- as.numeric(new_data[[1]])
  return (new_data[order(new_data[1],new_data[2]),][1,2])
}

## hospital <- read.csv('hospital-data.csv')