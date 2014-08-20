rankall <- function(outcome, num = "best") {
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
  u <- NULL #unique states
  hos <- NULL
  st <- NULL
  
  
  ## Read outcome data
  readfile <- read.csv("~/Coursera/HospitalQuality/outcome-of-care-measures.csv", colClasses="character", header=TRUE)
  rfile <- readfile[,c(2,7,11,17,23)] #read 5 columns relevant to this function
  u <- unique((rfile[,2])) #returns vector of length 54; unique ordered
  
  ## Check that state and outcome are valid
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
    
  for (i in 1:length(u)) {
    
    r <- NULL
    f <- FALSE
    
    r <- grep(u[i], rfile[,2], ignore.case=TRUE)
    
    if (is.character(num) & (as.character(num) == "best")) {
      n <- 1
    } else {
      if (is.character(num) & (as.character(num) == "worst")) {
        n <- length(r) - length(grep("not available", rfile[r,c], ignore.case=TRUE))
      } else {
        if (is.numeric(num) & (as.numeric(num) > length(r))) {
          f <- TRUE
          n <- 1
        } else {
          n <- as.numeric(num)
        }
      }
    }
    
    rf1 <- rfile[r,]
    rf2 <- suppressWarnings(na.omit(rf1[order(as.numeric(rf1[,c]),rf1[,1]),]))
    
    if (f) {
      hos[i] <- "<NA>"
      st[i] <- as.character(rf2[n,2])
    } else {
      hos[i] <- as.character(rf2[n,1])
      st[i] <- as.character(rf2[n,2])
    }
  }
  df <- data.frame(hos, st, stringsAsFactors = FALSE)
  df[order(df[,2]),]
}