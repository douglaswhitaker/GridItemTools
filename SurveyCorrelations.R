#Package to create scatterplot, histograms and calculate correlation coefficient
library("PerformanceAnalytics")

###Read the csv file
dat.raw <-  read.csv("data/real/results-survey945827-202004302207-codes.csv",
                     stringsAsFactors = FALSE)

#Function to assign grid position to respondents' grid answers
col2xy <- function(gc){
  return(which(matrix(1:25,nrow=5,byrow = TRUE)==gc, arr.ind = TRUE))
}

# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2
# Convert grid value to 1 to 9 value
grid2nine <- function(gc,b = -0.5){
  i <- col2xy(gc)[2] # this is the X value (positive axis), so the column in our format
  j <- col2xy(gc)[1] # the Y value, the row in our format
  return((b+2)*i+b*j-1-6*b)
}

###Function to read data frome -codes.csv file for non-grid items - 
###NOTE: Revised function for improved functionality (turns empty cells into NA's)
read.lime.nongrid <- function (dat){
  code <- c('D4','D3','D2','D1','N0','A1','A2','A3','A4',"")
  numcode <- c(1,2,3,4,5,6,7,8,9,NA)
  
  #Removes grid columns
  dat <- dat[,which(!sapply(names(dat),
                            grepl,pattern="_",
                            simplify=TRUE))]
  
  #Choose likert columns
  dat <- dat[,c(13:21,25:34)]
  
  #Loop to assign numbers to codes
  for (i in 1:ncol(dat)){
    for (j in 1:nrow(dat)){
      for (k in 1:length(code)){
        if(dat[j,i]==code[k]){
          dat[j,i] <- numcode[k]
        }
      }
    }
  }
  return(dat)
}

#Create variable to hold likert-like results
likertresults <- read.lime.nongrid(dat.raw)

###Function to read data frome -codes.csv file for grid items - 
###NOTE: Revised function to add 1-25 values based on respondents' answers to grid questions
read.lime.grid <- function(dat){
  #Removes non-grid columns
  dat <- dat[,which(sapply(names(dat),
                           grepl,pattern="_",
                           simplify=TRUE))]
  
  #Loop to go through each row and assign grid answers 1-25 values based on
  #grid location
  index <- 1
  for (i in 1:nrow(dat)){
    for (j in 1:ncol(dat)){
      if (!is.na(dat[i,j])){
        dat[i,j] <- index
      }
      index <- index + 1
      if (index == 26){
        index <- 1
      }
    }
  }
  return(dat)
}

#Create variable to hold grid results
gridresults <- read.lime.grid(dat.raw)
gridresults <- gridresults[,-c(476:550)]

gridholder <- c() #List to hold all grid answers
likertholder <- c() #List to hold all likert-type answers
matchingresults <- 1 #How many matching results are present (pairs of likewise data)
grid2likert3length <- 0
grid4likert1length <- 0

#Loop to go through each grid AND likert-type answer and stores results only if
#each corresponding grid and likert-type answer exists (the respondent answered both)
for (k in 1:19){ #These span through the number of grid/likert questions asked
  for (i in (1+(k-1)*25):(25+(k-1)*25)){ #This corresponds to the column number for grid answers
    for (j in 1:nrow(gridresults)){ #This corresponds to the row for grid answers (each respondent)
      if(k<=10){ #For grid page 2 & likert page 3 questions
        if (!is.na(gridresults[j,i]) && !is.na(likertresults[j,k+9])){ #Make sure both grid AND likert answer exists, so correlation can be calculated
          gridholder[matchingresults] <- gridresults[j,i]
          likertholder[matchingresults] <- likertresults[j,k+9] #k+9 since that's where page 3 starts
          matchingresults <- matchingresults + 1
          grid2likert3length <- grid2likert3length + 1
        }
      }
      if(k>=11){ #For grid page 4 & likert page 1 questions
        if (!is.na(gridresults[j,i]) && !is.na(likertresults[j,k-10])){ #Make sure both grid AND likert answer exists, so correlation can be calculated
          gridholder[matchingresults] <- gridresults[j,i]
          likertholder[matchingresults] <- likertresults[j,k-10] #k-10 since that's where page 1 starts
          matchingresults <- matchingresults + 1
          grid4likert1length <- grid4likert1length + 1
        }
      }
    }
  }
}

#Find the total length of all pairs of data
grid4likert1length <- grid4likert1length + grid2likert3length

#Applies grid2nine function to all grid data to assign 1-9 values
for (i in 1:length(gridholder)){
  gridholder[i] <- grid2nine(gridholder[i], b=-1)
}

#Calculate correlation coefficient without jitter (data can overlap)
grid2likert3 <- cbind(page2=(gridholder[1:grid2likert3length]),
                      page3=(as.numeric(likertholder[1:grid2likert3length])))
grid4likert1 <- cbind(page4=(gridholder[(grid2likert3length+1):grid4likert1length]),
                      page1=(as.numeric(likertholder[(grid2likert3length+1):grid4likert1length])))

#Calculate correlation coefficient with jitter (data is slightly moved randomly)
grid2likert3jit <- cbind(page2=jitter(gridholder[1:grid2likert3length]),
                         page3=jitter(as.numeric(likertholder[1:grid2likert3length])))
grid4likert1jit <- cbind(page4=jitter(gridholder[(grid2likert3length+1):grid4likert1length]),
                         page1=jitter(as.numeric(likertholder[(grid2likert3length+1):grid4likert1length])))

#Creates scatterplot, histograms and calculates correlation coefficient without jitter
chart.Correlation(grid2likert3)
chart.Correlation(grid4likert1)

#Creates scatterplot, histograms and calculates correlation coefficient with jitter
chart.Correlation(grid2likert3jit)
chart.Correlation(grid4likert1jit)
