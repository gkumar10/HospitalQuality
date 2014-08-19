rankhospital <- function(state, outcome, num="best"){
  ## Read outcome data
  #initialize
  r <- NULL
  c <- NULL
  lowest <- NULL
  r1 <- NULL
  o1 <- NULL 
  n <- NULL #rank
  
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
  
  if (num == "best") {
    n <- 1
  } else {
      if (num == "worst") {
        n <- length(r)
      } else {
          if (num > length(r)) {
            print(NA)
          }
      }
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  
  
}