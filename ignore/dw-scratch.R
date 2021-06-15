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



#### Testing trinomial function
# Data: 6 pos, 4 tie, 0 neg
# HA: greater

# > trinomial.test(col1=c(6,4,0),alternative = "greater")
# [1] "p0=0.4"
# [1] "nd=4"
# [1] "***   ***   ***   ***   ***   ***   ***"
# [1] "Trinomial Test (Bian et al., 2011)"
# [1] ""
# [1] "Null hypothesis:        prob_pos = prob_neg"
# [1] "Alternative hypothesis: prob_pos > prob_neg"
# [1] ""
# [1] "p-value: 0.0109682424"

#### Testing trinomial function
# Data: 6 pos, 4 tie, 0 neg
# HA: not equal

# > trinomial.test(col1=c(6,4,0),alternative = "two.sided")
# [1] "p0=0.4"
# [1] "nd=4"
# [1] "***   ***   ***   ***   ***   ***   ***"
# [1] "Trinomial Test (Bian et al., 2011)"
# [1] ""
# [1] "Null hypothesis:        prob_pos = prob_neg"
# [1] "Alternative hypothesis: prob_pos != prob_neg"
# [1] ""
# [1] "p-value: 0.0219364848"

#### Testing trinomial function
# Data: 6 pos, 3 tie, 1 neg
# HA: not equal

# > trinomial.test(col1=c(6,3,1),alternative = "two.sided")
# 1] "***   ***   ***   ***   ***   ***   ***"
# [1] "Trinomial Test (Bian et al., 2011)"
# [1] ""
# [1] "Null hypothesis:        prob_pos = prob_neg"
# [1] "Alternative hypothesis: prob_pos != prob_neg"
# [1] ""
# [1] "p-value: 0.0872985911382812"

#### Testing trinomial function
# Data: 6 pos, 3 tie, 1 neg
# HA: greater

# > trinomial.test(col1=c(6,3,1),alternative = "greater")
# [1] "***   ***   ***   ***   ***   ***   ***"
# [1] "Trinomial Test (Bian et al., 2011)"
# [1] ""
# [1] "Null hypothesis:        prob_pos = prob_neg"
# [1] "Alternative hypothesis: prob_pos > prob_neg"
# [1] ""
# [1] "p-value: 0.0436492955691406"

#### Testing trinomial function
# Data: 1 pos, 3 tie, 6 neg
# HA: greater [p_pos > p_neg]
# test statistic is 1-6 = -5, so this triggers the reverse flag

# > trinomial.test(col1=c(1,3,6),alternative = "greater")
# [1] "Calculated test statistic is negative."
# [1] "Continuing with the roles of col1 and col2 switched."
# [1] "Test statistic must be positive; provided data had negative differences count exceeding positive differences count"
# [1] "***   ***   ***   ***   ***   ***   ***"
# [1] "Trinomial Test (Bian et al., 2011)"
# [1] ""
# [1] "Null hypothesis:        prob_pos = prob_neg"
# [1] "Alternative hypothesis: prob_pos > prob_neg"
# [1] ""
# [1] "p-value: 0.982671175476172"