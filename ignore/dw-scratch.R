source("R/TrinomialTest.R")
source("R/Trinomial-Internal.R")

trinomial.test(col1=c(7,1,2), alternative = "greater")

# Computes S, equation (1) from Bian et al. (2011)
nue.s <- function(col1, verbose = TRUE, checkRR = TRUE, alpha = 0.05){
  ns <- list(n_pos=col1[1],
             n_neg=col1[3],
             n_tie=col1[2]) # order determined by the Bian et al. (2011) article
  n <- sum(col1)
  s <- ns$n_pos + ns$n_tie/2 
  if (verbose) { print(paste("S is",s)) }
  
  # check the rejection region for this test:
  # H0: prob_pos = prob_neg
  # HA: prob_pos > prob_neg
  if (checkRR) {
    critval <- qbinom(p = alpha, size = n, prob = 0.50, lower.tail = FALSE) 
    if (verbose) { print(paste("critval is",critval))}
    print(ifelse(s > critval, "Reject", "Fail to reject"))
  }
}

### Selected examples from p. 1156
# > nue.s(c(8,1,1))
# [1] "S is 8.5"
# [1] "critval is 8"
# [1] "Reject"
# > nue.s(c(8,2,0))
# [1] "S is 9"
# [1] "critval is 8"
# [1] "Reject"
# > nue.s(c(8,0,2))
# [1] "S is 8"
# [1] "critval is 8"
# [1] "Fail to reject"