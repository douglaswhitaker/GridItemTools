# With two.sided sometimes p-values exceed 1 - huge problem

trinomial.test(col1 = c(1,8,1), alternative = "two.sided")
trinomial.test(col1 = c(1,8,2), alternative = "two.sided")


# # These replicate Table 1 
# > sum(sapply(X = 5:10, FUN = function(x){prob.nd.cumsum(10, nd = x, p_tie = 0.20)}))
# [1] 0.05539103
# > sum(sapply(X = 4:10, FUN = function(x){prob.nd.cumsum(10, nd = x, p_tie = 0.30)}))
# [1] 0.09355447
# > sum(sapply(X = 5:10, FUN = function(x){prob.nd.cumsum(10, nd = x, p_tie = 0.30)}))
# [1] 0.0436493
# > 

# # More replicating of Table 1
# > sapplyvecouttmp <- sapply(X = 0:10, FUN = function(x){prob.nd.cumsum(10, nd = x, p_tie = 0.20)})
# > obsprobtmp4 <- prob.nd.cumsum(n = 10, nd = 4, p_tie = 0.20)
# > sum(sapplyvecouttmp[sapplyvecouttmp < obsprobtmp4])
# [1] 0.05539103
# > obsprobtmp6 <- prob.nd.cumsum(n = 10, nd = 6, p_tie = 0.20)
# > obsprobtmp5 <- prob.nd.cumsum(n = 10, nd = 5, p_tie = 0.20)
# > sum(sapplyvecouttmp[sapplyvecouttmp < obsprobtmp4])
# [1] 0.05539103
# > sum(sapplyvecouttmp[sapplyvecouttmp < obsprobtmp5])
# [1] 0.02468086
# > sum(sapplyvecouttmp[sapplyvecouttmp < obsprobtmp6])
# [1] 0.009148826
# > sum(sapplyvecouttmp[sapplyvecouttmp < obsprobtmp5])
# [1] 0.02468086
# 
# > sum(sapplyvecouttmp[sapplyvecouttmp <= obsprobtmp5])
# [1] 0.05539103

################################################################################


# Misc scratch

probs_vec <- c()
for (index in 1:(n + 1)){
  j <- index - 1
  for (k in 0:((n - j)/ 2)){
    probs_vec[index] <- prob.nd(n = n, nd = j, k = k, p_tie = p_tie)
  }
}
prob_obs <- 
  
gen.probs.obj(n = 10, alpha = 0.05)
gen.probs.obj(n = 10, alpha = 0.025)

tmpRR <- gen.probs.obj(n = 10, alpha = 0.025)$RejectionRegion


pncRR <- function(RRrow){
  n <- sum(RRrow)
  nd <- RRrow[1]-RRrow[3]
  
}