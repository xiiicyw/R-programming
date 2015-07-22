rankhospital <- function(outcome, num = "best") {
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
    
    
    # order data by outcome
    new_data <- clear_data[clear_data$State==state,]
    sorted_data <- new_data[order(as.numeric(new_data[[outcome_column]]),
                                  new_data[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    if (num=="best") num = 1
    if (num=='worst') num = nrow(sorted_data)
    #will automatically return NA if num > nrow, as well as if it's some other text value
    # if someone passes num < 1, they'll get what's expected
    #if (is.numeric(num) & num > nrwo(sorted.data.state) return(NA)
    
    sorted_data[num,"Hospital.Name"]
}