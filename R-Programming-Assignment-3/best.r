best <- function(state, outcome) {
    ## Read outcome data
    full_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    full_data[,c(11,17,23)] <- sapply(full_data[,c(11,17,23)] , as.numeric)  
    clear_data <- na.omit(full_data)
    outcomes <- c("heart attack","heart failure","pneumonia")
    states <- clear_data[,7]
    
    ## Check that state and outcome are valid
    if ((state %in% states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    if (outcome == "heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    new_data <- clear_data[clear_data$State==state,]
    desired_data <- which.min(new_data[,outcome_column])
    desired_hospital <- new_data[desired_data,"Hospital.Name"]
    
    if (length(desired_hospital) > 1) {
        hospitals_sorted <- sort(desired_hospital)
        hospitals_sorted[1]
    }
    else {
        desired_hospital
    }
}