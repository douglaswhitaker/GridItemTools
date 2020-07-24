

###Trinomial Test Function 
trinomial.test <- function(col1, col2, alternative=c("two.sided", "greater", "less")){
  columns <- cbind(col1, col2) #Merge the two columns
  dat <- data.frame(columns) #Create data frame with the columns
  numrow1 <- nrow(dat) #Find the length of the data frame to see number of observations
  dat <- dat[complete.cases(dat),] #Remove empty/NA columns
  numrow2 <- nrow(dat) #Find the length of the data frame after clearing any rows with missing data values
  
  #If-statement to tell user if any rows were removed due to empty cells - not working
  # if(numrow1 > numrow2){
  #  print((numrow1 - numrow2), " observations have been removed due to missing values.")
  #}
  
  #Create variables
  n <- numrow2
  n_pos <- 0 #Number of observations with positve differences
  n_neg <- 0 #Number of observations with negative differences
  n_tie <- 0 #Number of observations with ties (zero differences)
  
  #Loop to assign each observation (row) as a positve, negative or tie (zero) difference
  for (i in 1:n){
    diff <- dat[i, 1] - dat[i, 2]
    if(diff>0){
      n_pos <- n_pos+1
    }
    else if (diff<0){
      n_neg <- n_neg+1
    }
    else{
      n_tie <- n_tie+1
    }
  }
  
  n_diff <- n_pos - n_neg #This is n_d in the paper
  p_tie <- n_tie/n #This is p_o in the paper - we can consider making this adjustable further on if the user has any theoretical knowledge of p_o
  p_value <- 0 #Create p-value variable
  
  if(alternative=="greater"){ #One-sided test
    #Loop to compute the p-value
    for (j in n_diff:n){
      for (k in 0:((n-j)/2)){
        prob <- factorial(n)/(factorial(j+k)*factorial(k)*factorial(n-j-2*k))*((1-p_tie)/2)^(j+2*k)*p_tie^(n-j-2*k)
        p_value <- p_value + prob
      }
    }
  }
  if(alternative=="two.sided"){ #Two-sided test
    #Loop to compute the p-value
    for (j in n_diff:n){
      for (k in 0:((n-j)/2)){
        prob <- factorial(n)/(factorial(j+k)*factorial(k)*factorial(n-j-2*k))*((1-p_tie)/2)^(j+2*k)*p_tie^(n-j-2*k)
        p_value <- p_value + 2*prob
      }
    }
  }
  
  return(p_value) #Return p-value to user
}
