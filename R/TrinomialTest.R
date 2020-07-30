
#' Perform the trinomial hypothesis test
#' 
#' @param col1 first data vector
#' @param col2 second data vector
#' @param alternative the type of hypothesis test to be performed
#' @param p_tie theoretical proportion of ties; calculated from the data by default
#' @return the p-value from the hypothesis test

#########
# To-do #
#########
# Return an object containing the following: 
#   p-value
#   test statistic (n_diff)
#   critical value
#   n_pos, n_tie, n_neg
#   reverse.flag
# Add a flag to also run the sign.test
# Have nice output processing of the returned object
###Trinomial Test Function 
trinomial.test <- function(col1, col2=NULL, 
                           alternative=c("two.sided", "greater", "less"), 
                           p_tie=NULL){
  if (is.null(col2)){
    ns <- list(n_pos=col1[[1]],n_tie=col1[[2]],n_neg=col1[[3]])
    n <- col1[[1]] + col1[[2]] + col1[[3]]
  }
  else{
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
  }
  
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


################################################################################

# Generate information about the trinomial test under various settings
#' Title
#'
#' @param n 
#' @param alpha 
#' @param include.one 
#' @param find.RR 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
gen.probs.obj <- function(n,alpha=0.05,include.one=TRUE,find.RR=TRUE,digits=NULL){
  tmp.sum <- c()
  probs.table <- c()
  xs <- 0:n
  if (include.one){
    P0s <- (0:(n))/n
  }
  else {
    P0s <- (0:(n-1))/n
  }
  for (p0 in P0s){
    for (i in 1:length(xs)){
      tmp.sum[i] <- prob.nd.cumsum(n=n,nd=xs[i],p_tie=p0)
    }
    probs.table <- rbind(probs.table,tmp.sum[(n+1):1])
  }
  rownames(probs.table) <- paste("p0=",P0s,sep="")
  colnames(probs.table) <- paste("nd=",n:0,sep="")
  
  if (!is.null(digits)){
    probs.table <- round(probs.table,digits)
  }
  
  
  probs.obj <- list(probs.table=probs.table,nd=n:0,P0s=P0s)
  probs.obj <- find.critical.values(probs.obj)
  
  if (find.RR){
    probs.obj$RejectionRegion <- find.rejection.region(probs.obj)
  }
  
  return(probs.obj)
}

