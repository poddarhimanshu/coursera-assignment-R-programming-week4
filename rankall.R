rankall <- function(outcome, num = "best") {
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
  
  hospital = c()
  ## For each state, find the hospital of the given rank
  for (state in unique(outcome_data[[7]])) {
  new_data <- outcome_data[outcome_data$State==state,][c(outcome_index[outcome],2)]
  new_data[1] <- as.numeric(new_data[[1]])
  
  ordered_data <- new_data[order(new_data[1],new_data[2]),]
            h <-  if(num=="best"){
                   ordered_data[1,2]
                }
                else if(num=="worst"){
                  ordered_data[complete.cases(ordered_data),][nrow(ordered_data[complete.cases(ordered_data),]),][,2]
                }
                else
                  ordered_data[num,2]
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
hospital <- c(hospital,h)
  }
  return (data.frame(state = unique(outcome_data[[7]]),hospital_name = hospital))
}