
#' Perform the trinomial hypothesis test
#' 
#' @param col1 first data vector
#' @param col2 second data vector
#' @param alternative the type of hypothesis test to be performed
#' @return the p-value from the hypothesis test

###Trinomial Test Function 
trinomial.test <- function(col1, col2, alternative=c("two.sided", "greater", "less")){
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
  
  n_pos <- sum((col1-col2) > 0)
  n_neg <- sum((col1-col2) < 0)
  n_tie <- sum((col1-col2) == 0)
  
  n_diff <- n_pos - n_neg #This is n_d in the paper
  p_tie <- n_tie/n #This is p_o in the paper - we can consider making this adjustable further on if the user has any theoretical knowledge of p_o
  p_value <- 0 #Create p-value variable
  
  if(alternative=="greater"){ #One-sided test
    #Loop to compute the p-value
    for (j in n_diff:n){
      for (k in 0:((n-j)/2)){
        prob <- prob.nd(n=n,nd=j,k=k,p_tie=p_tie)
        p_value <- p_value + prob
      }
    }
  }
  else if(alternative=="two.sided"){ #Two-sided test
    #Loop to compute the p-value
    for (j in n_diff:n){
      for (k in 0:((n-j)/2)){
        prob <- prob.nd(n=n,nd=j,k=k,p_tie=p_tie)
        p_value <- p_value + 2*prob
      }
    }
  }
  else if(alternative=="less"){
    p_value <- trinomial.test(col1=col2,col2=col1,alternative="greater")
  }
  
  return(p_value) #Return p-value to user
}

# The probability distribution of Nd, the test statistic.
# Nd is the difference between N+ and N-
# The formula is given by (Bian et al., 2011, p. 1156)
prob.nd <- function(n,nd,k,p_tie){
  factorial(n)/(factorial(nd+k)*factorial(k)*factorial(n-nd-2*k))*
    ((1-p_tie)/2)^(nd+2*k)*
    p_tie^(n-nd-2*k)
}
