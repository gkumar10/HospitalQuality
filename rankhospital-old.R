rankhospital <- function(state, outcome, num="best"){
  ## Read outcome data
  #initialize
  r <- NULL
  c <- NULL
  lowest <- NULL
  r1 <- NULL
  o1 <- NULL 
  n <- NULL #rank
  readfile <- NULL
  rfile <- NULL
  g1 <- NULL
  
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
        n <- length(r)
      } else {
          if (is.numeric(num) & (as.numeric(num) > length(r))) {
            print(NA)
          } else {
              n <- as.numeric(num)
          }
      }
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  rf1 <- suppressWarnings(order(as.numeric(rfile[r,c]))) # returns ordered data.frame
  #rfile[r[rf1[n]],1] #will return hospital name at n row
  c1 <- rfile[r[rf1[n]],c] #get column value at n row to look for ties
  print(c1)
  g1 <- grep(c1,rfile[r[rf1],c]) #check for ties in column value; returns integer vector
  print(length(g1))
  if (length(g1) > 1) {
    o1 <- order(rfile[r[rf1][g1],1])
    print(o1)
    rfile[r[rf1[n]],1]  
  } else {
      rfile[r[rf1[n]],1]
  }
  rf2[order(as.numeric(rf2[,4]), rf2[,1]),]  
  
}