
#' Perform the trinomial hypothesis test
#' 
#' @param col1 first data vector
#' @param col2 second data vector
#' @param alternative the type of hypothesis test to be performed
#' @param p_tie theoretical proportion of ties; calculated from the data by default
#' @return the p-value from the hypothesis test

###Trinomial Test Function 
trinomial.test <- function(col1, col2, alternative=c("two.sided", "greater", "less"), p_tie=NULL){
  columns <- cbind(col1, col2) #Merge the two columns
  dat <- data.frame(columns) #Create data frame with the columns
  numrow1 <- nrow(dat) #Find the length of the data frame to see number of observations
  dat <- dat[stats::complete.cases(dat),] #Remove empty/NA columns
  numrow2 <- nrow(dat) #Find the length of the data frame after clearing any rows with missing data values
  
  #If-statement to tell user if any rows were removed due to empty cells - not working
  # if(numrow1 > numrow2){
  #  print((numrow1 - numrow2), " observations have been removed due to missing values.")
  #}
  
  #Create variables
  n <- numrow2
  ns <- calculate.ns(col1,col2)
  n_diff <- ns$n_pos - ns$n_neg #This is n_d in the paper
  reverse.flag <- FALSE
  
  if (n_diff < 0){
    print("Calculated test statistic is negative.")
    print("Continuing with the roles of col1 and col2 switched.")
    reverse.flag <- TRUE
    n_diff <- ns$n_neg - ns$n_pos
  }
  
  if (is.null(p_tie)){
    p_tie <- ns$n_tie/n #This is p_o in the paper - we can consider making this adjustable further on if the user has any theoretical knowledge of p_o
  }
  p_value <- 0 #Create p-value variable
  pivot_prob <- 0
  
  #Loop to compute the p-value
  for (j in n_diff:n){
    for (k in 0:((n-j)/2)){
      prob <- prob.nd(n=n,nd=j,k=k,p_tie=p_tie)
      p_value <- p_value + prob
    }
    if (j == n_diff){
      pivot_prob <- p_value # store the value of the probability that is included in both greater and less
    }
  }
  
  prob.nd(n=n,nd=n_diff,k=k,p_tie=p_tie)
  
  if(alternative=="greater"){ #One-sided test
    if (reverse.flag){
      print(1-p_value+pivot_prob)
      
    }
    else{
      print(p_value)
    }
  }
  else if(alternative=="less"){
    if (reverse.flag){
      print(p_value)
      
    }
    else{
      print(1-p_value+pivot_prob)
    }
  }
  else if(alternative=="two.sided"){
    print(p_value*2)
  }
}

# The probability distribution of Nd, the test statistic.
# Nd is the difference between N+ and N-
# The formula is given by (Bian et al., 2011, p. 1156)
prob.nd <- function(n,nd,k,p_tie){
  factorial(n)/(factorial(nd+k)*factorial(k)*factorial(n-nd-2*k))*
    ((1-p_tie)/2)^(nd+2*k)*
    p_tie^(n-nd-2*k)
}

calculate.ns <- function(col1,col2){
  n_pos <- sum((col1-col2) > 0)
  n_neg <- sum((col1-col2) < 0)
  n_tie <- sum((col1-col2) == 0)
  return(list(n_pos=n_pos,n_neg=n_neg,n_tie=n_tie))
}
