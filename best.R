best <- function(state, outcome){
  #initialize
  r <- NULL
  c <- NULL
  lowest <- NULL
  r1 <- NULL
  o1 <- NULL
  
  ## Read outcome data
  readfile <- read.csv("~/Coursera/HospitalQuality/outcome-of-care-measures.csv", colClasses="character", header=TRUE)
  rfile <- readfile[,c(2,7,11,17,23)] #read 5 columns relevant to this function
  
  ## Check that state and outcome are valid
  if (length(grep(state, rfile[,2], ignore.case=TRUE)) == 0) {    
    geterrmessage()
    stop("invalid state")
  } else {
      r <- grep(state, rfile[,2], ignore.case=TRUE) #returns row numbers with
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  ###use r1 to get row number in outcome-of-care-measure file. retrieve hospital name from column #1
  lowest <- suppressWarnings(min(as.numeric(rfile[r,c]), na.rm=TRUE))
  r1 <- grep(lowest, rfile[r,c]) # find row number of the lowest value. this is row number in r vector.
  if (length(r1) > 1) {
    o1 <- order(rfile[r[r1],1])
    rfile[r[r1][o1[1]],1]
  } else {
      rfile[r[r1],1]
  }
}