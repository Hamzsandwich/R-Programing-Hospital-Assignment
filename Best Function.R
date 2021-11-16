best <- function(state, admit_reas) {
    ## read in the data from CSV to Data frame
    hospitaldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character",stringsAsFactors = FALSE, na.strings = "NA")
    
    ## Clean the data by keeping only needed col
    coltokeep <- c("Hospital.Name", "State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    cleanhdwithna <- subset(hospitaldata, select = coltokeep)
    
    ## removing NAs
    cleanhd <- cleanhdwithna[complete.cases(cleanhdwithna),]
    
    ## renaming columns to fit user input ask   30 day outcome for 
       ## heart attach
       ## heart failure
       ## pneumonia
    names(cleanhd)[3] <- "heart attack"
    names(cleanhd)[4] <- "heart failure"
    names(cleanhd)[5] <- "pneumonia"
    
    # set these columns as numerics
    cleanhd[,3] <- as.numeric(as.character(cleanhd[,3]))
    cleanhd[,4] <- as.numeric(as.character(cleanhd[,4]))
    cleanhd[,5] <- as.numeric(cleanhd[,5])
   
    ## Create a df for the called state
    dataforstate <- subset(cleanhd, State == state)
    
    ## sort by admit_reason
    sorted_data <- dataforstate[order(subset(dataforstate, select = admit_reas)),]
    
    ## call best hospital name (lowest 30 mort) for that 
    
    sorted_data[1,1]
}
