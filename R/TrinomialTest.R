#########
# To-do #
#########
# Add a flag to also run the sign.test
# Have nice output processing of the returned object
###Trinomial Test Function 
#' Trinomial Test
#' 
#' Performs the trinomial test on either two data vectors or a vector of containing 
#' the counts of positive, tied, and negative differences.
#'
#' @param col1 first data vector or a vector of (positive, tie, negative) counts of differences.
#' @param col2 second data vector.
#' @param alternative the type of hypothesis test to be performed.
#' @param p_tie theoretical proportion of ties; calculated from the data by default.
#' @param print_info logical. If \code{TRUE} the hypotheses and p-value of the test are returned.
#'
#' @return An output object containing the number of observations; test statistic; 
#'   p-value; hypotheses; number of positive, tied, or negative differences; 
#'   critical value; and reverse flag notice for the trinomial test.
#' @export
#'
#' @examples
trinomial_test <- function(col1, 
                           col2 = NULL, 
                           alternative = c("two.sided", "greater", "less"), 
                           p_tie = NULL,
                           print_info = TRUE) {
  
  if (length(alternative) > 1) {
    alternative <- alternative[1]
  }
  
  ##############################################################################
    #### Section 1: process the data 
  
  if (is.null(col2)) { # user has entered a vector of counts; the sum of the values is n
    ns <- list(n_pos = col1[1],
               n_tie = col1[2],
               n_neg = col1[3])
               # order determined by the Bian et al. (2011) article
    n <- sum(col1)
  } else { # user has entered two vectors of raw data to be subtracted; the length of the vectors is n
    columns <- cbind(col1, col2) #Merge the two columns
    dat <- data.frame(columns) #Create data frame with the columns
    numrow1 <- nrow(dat) #Find the length of the data frame to see number of observations
    dat <- dat[stats::complete.cases(dat), ] #Remove empty/NA columns
    numrow2 <- nrow(dat) #Find the length of the data frame after clearing any rows with missing data values
    
    if (numrow1 > numrow2) {
      print(paste((numrow1 - numrow2), " observations have been removed due to missing values."))
      col1 <- dat$col1 #Makes new versions of col1 and col2 without NA values,
      col2 <- dat$col2 # but the observations are still paired properly.
    }
    
    #Create variables
    n <- numrow2
    ns <- calculate_ns(col1, col2)
  }
  
  ##############################################################################
  #### Section 2: Compute summary statistic n_diff and do sanity checks
  
  n_diff <- ns$n_pos - ns$n_neg #This is n_d in the paper
  reverse_flag <- FALSE
  if (n_diff < 0) {
    print("Calculated test statistic is negative.")
    print("Continuing with the roles of col1 and col2 switched.") #Different statement for when col2=NULL?
    print("Test statistic must be positive; provided data had negative differences count exceeding positive differences count")
    reverse_flag <- TRUE
    n_diff <- ns$n_neg - ns$n_pos
  }
  
  if (is.null(p_tie)){
    p_tie <- ns$n_tie / n #This is p_o in the paper - we can consider making this adjustable further on if the user has any theoretical knowledge of p_o
  }
  p_value <- 0 #Create p-value variable
  pivot_prob <- 0
  
  ##############################################################################
  #### Section 3: Compute the initial p-value
  
  # Note that prob_nd_cumsum is not used to account for greater, less, or two.sided tests

  #Loop to compute the p-value
  for (j in n_diff:n) {
    for (k in 0:((n - j) / 2)) {
      prob <- prob_nd(n = n, nd = j, k = k, p_tie = p_tie)
      p_value <- p_value + prob
    }
    if (j == n_diff) {
      pivot_prob <- p_value # store the value of the probability that is included in both greater and less
    }
  }
  
  # This line should do nothing, so comment it out
  #prob_nd(n=n,nd=n_diff,k=k,p_tie=p_tie)
  
  ##############################################################################
  #### Section 4: Make adjustments based on the sidedness of the test and sanity checks, return the result
  
  if (alternative == "greater") { #One-sided test
    if (reverse_flag) {
      true_p_val <- 1 - p_value + pivot_prob
      
    } else {
      true_p_val <- p_value
    }
  } else if (alternative == "less") {
    if (reverse_flag) {
      true_p_val <- p_value
      
    } else {
      true_p_val <- 1 - p_value + pivot_prob
    }
  } else if (alternative == "two.sided") {
    true_p_val <- p_value * 2
    if (n_diff == 0) {
      true_p_val <- true_p_val - pivot_prob
    }
  }
  
  critval <- generate_trinomial_info(n = n, P0s = p_tie, find_RR = FALSE)$critvals
  
  # TO DO: make this look nicer (i.e., mimic the output of, say, t.test)
  if (print_info) {
    if (alternative == "greater") {
      alternative_output <- c("Alternative hypothesis: prob_pos > prob_neg")
    } else if (alternative == "less") {
      alternative_output <- c("Alternative hypothesis: prob_pos < prob_neg")
    } else if (alternative == "two.sided") {
      alternative_output <- c("Alternative hypothesis: prob_pos != prob_neg")
    }
    
    # if (reverse_flag){
    #   print("")
    #   print("NOTE: Sanity check of data and alternative triggered.")
    #   print("reported p-value is for the OTHER sided test")
    # }
    
    cat("",
        "        Trinomial Test (Ganesalingam, 1994; Bian et al., 2011)",
        "",
        "Null hypothesis:        prob_pos = prob_neg",
        alternative_output,
        "",
        paste("statistic:", n_diff), paste("p-value:", true_p_val), sep = "\n")
  }
  
  # Work on a better return value
  invisible(list(N = n,
              statistic = n_diff,
              p_value = true_p_val,
              alternative = alternative,
              Ns = ns,
              critval = critval,
              reverse_flag = reverse_flag))
}

# We can probably move this to internal for now.
################################################################################

# Generate information about the trinomial test under various settings
#' Show Trinomial Test Information
#' 
#' Returns a range of information relevant to a trinomial test subject to various settings.
#'
#' @param n number of paired observations.
#' @param alpha significance level.
#' @param include_one logical. If \code{FALSE} the probability that all pairs are tied is excluded from \code{P0s}.
#' @param find_RR logical. If \code{TRUE} the output will include the rejection region.
#' @param digits number of significant digits to be returned.
#' @param P0s numeric vector of all possible probabilities of a tie.
#'
#' @return A list containing possible probability distributions, numbers of ties,
#'   probabilities of a tie, critical values, and rejection regions for a given 
#'   number of observations and alpha level of a theoretical trinomial test.
#' @export
#'
#' @examples
generate_trinomial_info <- function(n,
                          alpha = 0.05,
                          include_one = TRUE,
                          find_RR = TRUE,
                          digits = NULL,
                          P0s = NULL) {
  
  # Set up basic storage vectors
  
  tmp_sum <- c()
  probs_table <- c()
  xs <- 0:n
  
  # set up possible probabilities of a tie
  # P0 is the probability of a tie; an unbiased estimate is n_tie / n (see last paragraph of section 3 p. 1156)
  # So, for each possible number of ties, compute the estimate P0
  # This currently makes no provision for the user to specify the P0s
  
  if (is.null(P0s)) {
    if (include_one) {
      P0s <- (0:(n)) / n 
    } else {
      P0s <- (0:(n - 1)) / n
    }
  }

  # For each possible probability of a tie, compute the probability of observing 0 to n successes
  # This uses the distribution function given by Bian et al. (2011) on page 1156
  
  for (p0 in P0s) {
    for (i in 1:length(xs)) {
      tmp_sum[i] <- prob_nd_cumsum(n = n, nd = xs[i], p_tie = p0)
    }
    probs_table <- rbind(probs_table, tmp_sum[(n + 1):1])
  }
  rownames(probs_table) <- paste("p0=", P0s, sep = "")
  colnames(probs_table) <- paste("nd=", n:0, sep = "")
  
  if (!is.null(digits)) {
    probs_table <- round(probs_table, digits)
  }
  
  
  probs_obj <- list(probs_table = probs_table, nd = n:0, P0s = P0s)
  # try replacing with next line to get rid of $critvals$critvals in find_rejection_region
  #probs_obj <- c(probs_obj, find_critical_values(probs_obj, alpha = alpha))
  probs_obj$critvals <- find_critical_values(probs_obj, alpha = alpha)
  
  if (find_RR) {
    probs_obj$RejectionRegion <- find_rejection_region(probs_obj)
  }
  
  return(probs_obj)
}

## This seems to only work for n <= 170
#
# If n > 170, then the following error occurs:
# Error in if (any(x < 0)) stop("'x' must be non-negative") : 
#   missing value where TRUE/FALSE needed
# Called from: FUN(newX[, i], ...)
#
## The problem seems to be due to prob_nd_cumsum
#
# > GridItemTools:::prob_nd_cumsum(n=171,nd=0,p_tie=0)
# [1] NaN
#
## Which in turn is because of factorial(n)
#
# > factorial(170)
# [1] 7.257416e+306
# > factorial(171)
# [1] Inf
#
## The answer might be to cleverly re-write that code using the choose function (multiplied by a value to make it work right)
