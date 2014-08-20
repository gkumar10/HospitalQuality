rankhospital <- function(state, outcome, num="best"){
  ## Read outcome data
  #initialize
  r <- NULL
  c <- NULL
  rf1 <- NULL
  rf2 <- NULL 
  n <- NULL #rank
  readfile <- NULL
  rfile <- NULL
  f <- FALSE # flag for condition where num > nrow
  
  ## Read outcome data
  readfile <- read.csv("~/Coursera/HospitalQuality/outcome-of-care-measures.csv", colClasses="character", header=TRUE)
  rfile <- readfile[,c(2,7,11,17,23)] #read 5 columns relevant to this function
  
  ## Check that state and outcome are valid
  if (length(grep(state, rfile[,2], ignore.case=TRUE)) == 0) {    
    geterrmessage()
    stop("invalid state")
  } else {
    r <- grep(state, rfile[,2], ignore.case=TRUE) #returns row numbers
  }
  
  if (outcome == "heart attack") {
    c <- 3 #column #3 in rfile
  } else {
    if (outcome == "heart failure") {
      c <- 4
    } else {
      if (outcome == "pneumonia") {
        c <- 5
      } else {
        geterrmessage()
        stop("invalid outcome")
      }
    }
  }
  
  if (is.character(num) & (as.character(num) == "best")) {
    n <- 1
  } else {
      if (is.character(num) & (as.character(num) == "worst")) {
        n <- length(r) - length(grep("not available", rfile[r,c], ignore.case=TRUE))
      } else {
          if (is.numeric(num) & (as.numeric(num) > length(r))) {
            f <- TRUE
            #n <- as.numeric(num)
            n <- 1
          } else {
              n <- as.numeric(num)
          }
      }
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  rf1 <- rfile[r,]
  #rf2 <- suppressWarnings(na.omit(rf1[order(as.numeric(rf1[,c]),rf1[,1], na.last=TRUE),]))
  
  if (f) {
    rf2 <- suppressWarnings(rf1[order(as.numeric(rf1[,c]),rf1[,1]),])
    paste(n,"+", rf2[n,2])
    
  } else {
      rf2 <- suppressWarnings(na.omit(rf1[order(as.numeric(rf1[,c]),rf1[,1], na.last=TRUE),]))  
      paste("~", rf2[n,1])
  }
    
}