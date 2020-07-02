###Read the csv file
dat.raw <-  read.csv("data/real/results-survey945827-202004302207-codes.csv",
                     stringsAsFactors = FALSE)

###Function to read data frome -codes.csv file for likert items
read.lime.nongrid <- function (dat){
  code <- c('D4','D3','D2','D1','N0','A1','A2','A3','A4')
  numcode <- c(1,2,3,4,5,6,7,8,9)

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



#Test out read.lime.nongrid function
nongriddata <- read.lime.nongrid(dat.raw)

###Function to read data frome -codes.csv file for grid items
read.lime.grid <- function(dat){
  #Removes non-grid columns
  dat <- dat[,which(sapply(names(dat),
                              grepl,pattern="_",
                              simplify=TRUE))]
}

#Test out read.lime.grid function
griddata <- read.lime.grid(dat.raw)