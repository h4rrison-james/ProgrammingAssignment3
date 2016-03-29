best <- function(state, outcome) {
        ## Read outcome data
        outcome.file <- read.csv("outcome-of-care-measures.csv", colClasses = "character", strip.white = TRUE)
        
        outcome.file[, 11] <- as.numeric(outcome.file[, 11])
        outcome.file[, 17] <- as.numeric(outcome.file[, 17])
        outcome.file[, 23] <- as.numeric(outcome.file[, 23])
        
        ## Check that state and outcome are valid
        states <- unique(outcome.file[, 7]) # Store the states seperately
        if(!state %in% states) {
                stop("invalid state")
        }
        
        # Define the possible outcome names
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% valid.outcomes) {
                stop("invalid outcome")
        }

        ## Return hospital name in that state with lowest 30-day death rate
        outcome.filtered <- outcome.file[outcome.file$State == state, ]
        if (outcome == "heart attack") {
                outcome.filtered <- arrange(outcome.filtered, outcome.filtered[,11], outcome.filtered[,2])
        }
        if (outcome == "heart failure") {
                outcome.filtered <- arrange(outcome.filtered, outcome.filtered[,17], outcome.filtered[,2])
        }
        if (outcome == "pneumonia") {
                outcome.filtered <- arrange(outcome.filtered, outcome.filtered[,23], outcome.filtered[,2])
        }
        
        # Return the hospital name of the first row of the filtered frame
        return(outcome.filtered[1,2])
}