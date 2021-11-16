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
    names(cleanhd)[3] <- "heart_attack"
    names(cleanhd)[4] <- "heart_failure"
    names(cleanhd)[5] <- "pneumonia"
    
   
    
    
    # set these columns as numerics
    cleanhd$heart_attack <- as.numeric(as.character(cleanhd$heart_attack))
    cleanhd$heart_failure <- as.numeric(as.character(cleanhd$heart_failure))
    cleanhd$pneumonia <- as.numeric(cleanhd$pneumonia)
    
    
    
    ## Create a df for the called state
    dataforstate <- subset(cleanhd, State == state)
    
    ## sort by admit_reason
    sorted_data <- dataforstate[order(subset(dataforstate, select = admit_reas)),]
    
    ## call best hospital name (lowest 30 mort) for that 
    
    head(sorted_data)
    ##sorted_data[1,1]
}

#Hospital.30.Day.Death..Mortality..Rates.from.