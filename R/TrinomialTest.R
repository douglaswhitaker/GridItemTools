#########
# To-do #
#########
# Add a flag to also run the sign.test
# Have nice output processing of the returned object
###Trinomial Test Function 
#' Title
#'
#' @param col1 first data vector or a vector of (positive, tie, negative) counts of differences
#' @param col2 second data vector
#' @param alternative the type of hypothesis test to be performed
#' @param p_tie theoretical proportion of ties; calculated from the data by default
#' @param print.info 
#'
#' @return
#' @export
#'
#' @examples
trinomial.test <- function(col1, 
                           col2=NULL, 
                           alternative=c("two.sided", "greater", "less"), 
                           p_tie=NULL,
                           print.info=TRUE){
  
  if (length(alternative) > 1) {
    alternative <- alternative[1]
  }
  
  ##############################################################################
    #### Section 1: process the data 
  
  if (is.null(col2)){ # user has entered a vector of counts; the sum of the values is n
    ns <- list(n_pos=col1[1],
               n_tie=col1[2],
               n_neg=col1[3])
               # order determined by the Bian et al. (2011) article
    n <- sum(col1)
  }
  else{ # user has entered two vectors of raw data to be subtracted; the length of the vectors is n
    columns <- cbind(col1, col2) #Merge the two columns
    dat <- data.frame(columns) #Create data frame with the columns
    numrow1 <- nrow(dat) #Find the length of the data frame to see number of observations
    dat <- dat[stats::complete.cases(dat),] #Remove empty/NA columns
    numrow2 <- nrow(dat) #Find the length of the data frame after clearing any rows with missing data values
    
    if(numrow1 > numrow2){
      print(paste((numrow1 - numrow2), " observations have been removed due to missing values."))
      col1 <- dat$col1 #Makes new versions of col1 and col2 without NA values,
      col2 <- dat$col2 # but the observations are still paired properly.
    }
    
    #Create variables
    n <- numrow2
    ns <- calculate.ns(col1,col2)
  }
  
  ##############################################################################
  #### Section 2: Compute summary statistic n_diff and do sanity checks
  
  n_diff <- ns$n_pos - ns$n_neg #This is n_d in the paper
  reverse.flag <- FALSE
  if (n_diff < 0){
    print("Calculated test statistic is negative.")
    print("Continuing with the roles of col1 and col2 switched.") #Different statement for when col2=NULL?
    print("Test statistic must be positive; provided data had negative differences count exceeding positive differences count")
    reverse.flag <- TRUE
    n_diff <- ns$n_neg - ns$n_pos
  }
  
  if (is.null(p_tie)){
    p_tie <- ns$n_tie/n #This is p_o in the paper - we can consider making this adjustable further on if the user has any theoretical knowledge of p_o
  }
  p_value <- 0 #Create p-value variable
  pivot_prob <- 0
  
  ##############################################################################
  #### Section 3: Compute the initial p-value
  
  # Note that prob.nd.cumsum is not used to account for greater, less, or two.sided tests

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
  
  # This line should do nothing, so comment it out
  #prob.nd(n=n,nd=n_diff,k=k,p_tie=p_tie)
  
  ##############################################################################
  #### Section 4: Make adjustments based on the sidedness of the test and sanity checks, return the result
  
  if(alternative=="greater"){ #One-sided test
    if (reverse.flag){
      true_p_val <- 1-p_value+pivot_prob
      
    }
    else{
      true_p_val <- p_value
    }
  }
  else if(alternative=="less"){
    if (reverse.flag){
      true_p_val <- p_value
      
    }
    else{
      true_p_val <- 1-p_value+pivot_prob
    }
  }
  else if(alternative=="two.sided"){
    true_p_val <- p_value*2
    if (n_diff == 0){
      true_p_val <- true_p_val - pivot_p
    }
  }
  
  critval <- gen.probs.obj(n = n, P0s = p_tie, find.RR = FALSE)$critvals
  
  # TO DO: make this look nicer (i.e., mimic the output of, say, t.test)
  if (print.info){
    print("***   ***   ***   ***   ***   ***   ***")
    print("Trinomial Test (Ganesalingam, 1994; Bian et al., 2011)")
    print("")
    print("Null hypothesis:        prob_pos = prob_neg")
    if (alternative == "greater"){
      print("Alternative hypothesis: prob_pos > prob_neg")
    }
    else if (alternative == "less"){
      print("Alternative hypothesis: prob_pos < prob_neg")
    }
    else if (alternative == "two.sided"){
      print("Alternative hypothesis: prob_pos != prob_neg")
    }
    
    # if (reverse.flag){
    #   print("")
    #   print("NOTE: Sanity check of data and alternative triggered.")
    #   print("reported p-value is for the OTHER sided test")
    # }
    print("")
    print(paste("p-value:",true_p_val))
  }
  
  # Work on a better return value
  return(list(N=n,
              statistic=n_diff,
              p.value=true_p_val,
              alternative=alternative,
              Ns=ns,
              critval=critval,
              reverse.flag=reverse.flag))
}

# We can probably move this to internal for now.
################################################################################

# Generate information about the trinomial test under various settings
#' Title
#'
#' @param n 
#' @param alpha 
#' @param include.one 
#' @param find.RR 
#' @param digits 
#' @param P0s 
#'
#' @return
#' @export
#'
#' @examples
gen.probs.obj <- function(n,
                          alpha=0.05,
                          include.one=TRUE,
                          find.RR=TRUE,
                          digits=NULL,
                          P0s=NULL){
  
  # Set up basic storage vectors
  
  tmp.sum <- c()
  probs.table <- c()
  xs <- 0:n
  
  # set up possible probabilities of a tie
  # P0 is the probability of a tie; an unbiased estimate is n_tie / n (see last paragraph of section 3 p. 1156)
  # So, for each possible number of ties, compute the estimate P0
  # This currently makes no provision for the user to specify the P0s
  
  if (is.null(P0s)){
    if (include.one){
      P0s <- (0:(n))/n 
    }
    else {
      P0s <- (0:(n-1))/n
    }
  }

  # For each possible probability of a tie, compute the probability of observing 0 to n successes
  # This uses the distribution function given by Bian et al. (2011) on page 1156
  
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
  # try replacing with next line to get rid of $critvals$critvals in find.rejection.region
  #probs.obj <- c(probs.obj, find.critical.values(probs.obj, alpha = alpha))
  probs.obj$critvals <- find.critical.values(probs.obj,alpha = alpha)
  
  if (find.RR){
    probs.obj$RejectionRegion <- find.rejection.region(probs.obj)
  }
  
  return(probs.obj)
}
