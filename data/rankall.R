rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome.file <- read.csv("outcome-of-care-measures.csv", colClasses = "character", strip.white = TRUE)
        
        # Convert key columns to numeric
        outcome.file[, 11] <- as.numeric(outcome.file[, 11])
        outcome.file[, 17] <- as.numeric(outcome.file[, 17])
        outcome.file[, 23] <- as.numeric(outcome.file[, 23])
        
        # Define the possible outcome names
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% valid.outcomes) {
                stop("invalid outcome")
        }
        
        # Build a list of the possible states, and order it
        states <- sort(unique(outcome.file[, 7])) # Store the states seperately

        r = data.frame()

        ## Find the hospital in each state with the specified ranking
        for (state in states) {
                o <- outcome.file[outcome.file$State == state, ]
                    
                if (outcome == "heart attack") {
                        o <- arrange(o, o[,11], o[,2])
                        o <- o[complete.cases(o[,11]),]
                }
                if (outcome == "heart failure") {
                        o <- arrange(o, o[,17], o[,2])
                        o <- o[complete.cases(o[,17]),]
                }
                if (outcome == "pneumonia") {
                        o <- arrange(o, o[,23], o[,2])
                        o <- o[complete.cases(o[,23]),]
                }
                
                if (is.numeric(num)) {
                        r = rbind(r, data.frame(o[num,2], state))
                } else if (num == "worst") {
                        r = rbind(r, data.frame(o[nrow(o),2], state))
                } else if (num == "best") {
                        r = rbind(r, data.frame(o[1,2], state))
                }
        }
        
        colnames(r) <- c("hospital", "state")
        return(r)
}