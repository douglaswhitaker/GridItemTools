source("helper.R")

library("tidyverse")
library("ggpubr")
library("rstatix")
library("nortest")
library("MASS")
library("pander")

#Read csv file
surveytime <-  read.csv("data/real/results-survey945827-202004302207.csv",
                        stringsAsFactors = FALSE)

#Only display columns of time completion data
surveytime <- surveytime[,c(604,607,620,623,635)]

#Rename columns to meaningful names
colnames(surveytime) <- c("pg1","pg2","pg3","pg4","pg5")

#Remove outlier
surveytime[9,3] <- NA

#Create 5 vectors of each page times
vec1 <- surveytime[,1]
vec2 <- surveytime[,2]
vec3 <- surveytime[,3]
vec4 <- surveytime[,4]
vec5 <- surveytime[,5]

#Combine vectors for 1st column
total <- c(vec1,vec2,vec3,vec4,vec5)

#Create a data frame with the vectors
total <- data.frame(total)

#Loop to create 2nd column assigning each time to the page it came from
for (j in 1:5){
  for (i in (1+(65*(j-1))):(65+(65*(j-1)))){
    total[i,2] <- colnames(surveytime[j])
  }
}

#Removes rows with NA
total <- total[complete.cases(total),]

#Rename data frame columns (response: time, factor: pages)
colnames(total) <- c("time", "pages")

total[22,1] <- NA
total <- total[complete.cases(total),]

#Box-Cox Transformation
total[,1] <- log((total[,1]))

#Do One-Way ANOVA
res.aov <- aov(time ~ pages, data = total)


######################################################################################################################################

library("PerformanceAnalytics")

###Read the csv file
dat.raw <-  read.csv("data/real/results-survey945827-202004302207-codes.csv",
                     stringsAsFactors = FALSE)

#Function to assign grid position to respondents' grid answers
col2xy <- function(gc){
  return(which(matrix(1:25,nrow=5,byrow = TRUE)==gc,arr.ind = TRUE))
}

# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2
# Convert grid value to 1 to 9 value
grid2nine <- function(gc,b = -0.5){
  i <- col2xy(gc)[2] # this is the X value (positive axis), so the column in our format
  j <- col2xy(gc)[1] # the Y value, the row in our format
  return((b+2)*i+b*j-1-6*b)
}

###Function to read data frome -codes.csv file for non-grid items - NOTE: Revised function for improved functionality
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
likertresults <- likertresults[complete.cases(likertresults),] #Removes observations with missing data values

###Function to read data frome -codes.csv file for grid items - NOTE: Revised function to add 1-25 values based on respondents' answers to grid questions
read.lime.grid <- function(dat){
  #Removes non-grid columns
  dat <- dat[,which(sapply(names(dat),
                           grepl,pattern="_",
                           simplify=TRUE))]
  index <- 1
  #Loop to go through each row and assign grid answers 1-25 values based on grid location
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
gridresults <- gridresults[c(as.numeric(row.names(likertresults))),] #Removes rows to match likert-type results, so plots can be made
gridresults <- gridresults[-c(5,6,19),]
likertresults <- likertresults[-c(5,6,19),] #Manually removed rows since there is missing data

#######
#Tells if any data is missing for grid items
total <- 0
testnumber <- 1
for (j in 1:25){
  for (k in 1:19){
    for (i in (1+(k-1)*25):(25+(k-1)*25)){
      if (is.na(gridresults[j,i])){
        testnumber <- testnumber +1
      }
    }
    if (testnumber==26){
      print("ohno")
      total <- total +1
    }
    print(j)
    print(i)
    testnumber <- 1
  }
}


#######


#Loop to assign each grid answer a 1-9 scale number using grid2nine function
gridscale <- c()
k <- 1
for (i in 1:nrow(gridresults)){
  for (j in 1:ncol(gridresults)){
    if (!is.na(gridresults[i,j])){
      gridscale[k] <- grid2nine(gridresults[i,j], b=-1)
      k <- k+1
    }
  }
}

#Create a specified-sized matrix to fill it with respondents' data
gridresults <- data.frame(matrix(gridscale,nrow=25,byrow = TRUE))
######gridresults <- gridresults[-c(20,21)] #Remove data for grid-item only page, so gridresults and likertresults match up  

###

k <- 1

gridpages <- c()

for (i in 1:10){
  for (j in 1:nrow(gridresults)){
    gridpages[k] <- gridresults[j,i]
    k <- k +1
  }
}

gridpage2 <- data.frame(matrix(gridpages,nrow=length(gridpages)))

###

k <- 1

gridpages <- c()

for (i in 11:19){
  for (j in 1:nrow(gridresults)){
    gridpages[k] <- gridresults[j,i]
    k <- k +1
  }
}

gridpage4 <- data.frame(matrix(gridpages,nrow=length(gridpages)))

###
k <- 1

likertpages <- c()

for (i in 1:9){
  for (j in 1:nrow(likertresults)){
    likertpages[k] <- likertresults[j,i]
    k <- k +1
  }
}

likertpage1 <- data.frame(matrix(likertpages,nrow=length(likertpages)))
###

k <- 1

likertpages <- c()

for (i in 10:19){
  for (j in 1:nrow(likertresults)){
    likertpages[k] <- likertresults[j,i]
    k <- k +1
  }
}

likertpage3 <- data.frame(matrix(likertpages,nrow=length(likertpages)))

colnames(likertpage1) <- "page1"
colnames(gridpage2) <- "page2"
colnames(likertpage3) <- "page3"
colnames(gridpage4) <- "page4"


#######
likertpage1[,1] <- as.numeric(likertpage1[,1])
likertpage3[,1] <- as.numeric(likertpage3[,1])

data1 <- cbind(page4=(gridpage4[,1]), page1=(likertpage1[,1]))
chart.Correlation(data1)

data2 <- cbind(page2=(gridpage2[,1]), page3=(likertpage3[,1]))
chart.Correlation(data2)

data1 <- cbind(page4=jitter(gridpage4[,1]), page1=jitter(likertpage1[,1]))
chart.Correlation(data1)

data2 <- cbind(page2=jitter(gridpage2[,1]), page3=jitter(likertpage3[,1]))
chart.Correlation(data2)

#########################################

install.packages("PerformanceAnalytics")







num <- likertpage1[1,1]



#Test out read.lime.nongrid function
nongriddata <- read.lime.nongrid(dat.raw)
