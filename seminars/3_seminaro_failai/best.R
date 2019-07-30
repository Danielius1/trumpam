best <- function(state, outcome){
        
        # reads in csv file
        df <- read.csv("outcome-of-care-measures.csv",
                       header = TRUE,
                       sep=",",
                       stringsAsFactors = FALSE,
                       na.strings = "Not Available")
        
        if(!state %in% unique(df$State)){
                stop("invalide state")
        }
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalide outcome")
        }
        
        if(outcome=="heart attack"){
                df <- df[,c(2,7,11)]
        } else if(outcome=="heart failure"){
                df<- df[,c(2,7,17)]
        } else {
                df<- df[,c(2,7,23)]
        }
        
        # reduces active datafreme to the selected state
        df <- df[df$State==state,]
        
        #selects only rows with existing values
        df <- df[!is.na(df[,3]),]
        
        #orders rows by value and by hospital name in descdening order
        df <- df[order(df[,3], df[,1]),]
        # prints first row first column value
        df[1,1]
}