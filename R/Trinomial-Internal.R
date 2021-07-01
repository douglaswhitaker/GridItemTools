# The probability distribution of Nd, the test statistic.
# Nd is the difference between N+ and N-
# The formula is given by (Bian et al., 2011, p. 1156)
prob.nd <- function(n,nd,k,p_tie){
  factorial(n)/(factorial(nd+k)*factorial(k)*factorial(n-nd-2*k))*
    ((1-p_tie)/2)^(nd+2*k)*
    p_tie^(n-nd-2*k)
}

prob.nd.cumsum <- function(n,nd,p_tie){
  tmp.prob <- 0
  for (k in 0:((n-nd)/2)){
    tmp.prob <- tmp.prob + prob.nd(n=n,nd=nd,k=k,p_tie=p_tie)
  }
  return(tmp.prob)
}

## This might correspond with (4,6,0) [Reject at alpha = 0.05]
# > prob.nd.cumsum(10,nd=4,p_tie=0.50)
# [1] 0.03696442
## This might correspond with (3,7,0) [Fail to reject at alpha = 0.05]
# > prob.nd.cumsum(10,nd=3,p_tie=0.50)
# [1] 0.07392883

# Helper function for trinomial.test
# This calculates the number of positive, negative, and tied observations
calculate.ns <- function(col1,col2){
  n_pos <- sum((col1-col2) > 0)
  n_neg <- sum((col1-col2) < 0)
  n_tie <- sum((col1-col2) == 0)
  return(list(n_pos=n_pos,n_neg=n_neg,n_tie=n_tie))
}

# Helper function for gen.probs.obj
# Identifies which values are on either side of alpha for each value of P0
find.critical.values <- function(probs.obj,alpha=0.05,verbose=FALSE){
  critvals <- c()
  for (i in 1:nrow(probs.obj$probs.table)){
    if (verbose){
      print(rownames(probs.obj$probs.table)[i])
      print(colnames(probs.obj$probs.table)[which(cumsum(probs.obj$probs.table[i,]) > alpha)[1]])
    }
    # The key thing in this is checking the cumulative sum to see at which point it exceeds the alpha level
    # The which, $nd, and [1] are all just to account for the order of the columns in correctly identifying the correct nd that is the critical value
    # The column NAMES would easily work, but algorithmically storing the value requires cross-referencing the which value against the vector of nds
    critvals[i] <- probs.obj$nd[which(cumsum(probs.obj$probs.table[i,]) > alpha)[1]]
  }
  #probs.obj$critvals <- critvals 
  #return(probs.obj)
  return(critvals)
}

# Helper function for gen.probs.obj
# Identifies the rejection region for various settings
find.rejection.region <- function(probs.obj){
  n <- probs.obj$nd[1]
  grid <- expand.grid(0:n,0:n,0:n)
  colnames(grid) <- c("Pos","Tie","Neg")
  grid <- grid[which(rowSums(grid)==n),]
  nd <- grid$Pos - grid$Neg
  p0 <- grid$Tie/n
  inRR <- c()
  for (i in 1:nrow(grid)){
    inRR[i] <- nd[i] > probs.obj$critvals[which(probs.obj$P0s == p0[i])]
  }
  return(grid[inRR,])
}

# Helper function for gen.probs.obj
# Prints the information from Table 1 (Bian et al., 2011)
# No return value
find.cutpoints <- function(probs.obj){
  for (i in 1:nrow(probs.obj$probs.table)){
    print(probs.obj$P0s[i])
    cv.match <- which(probs.obj$critvals[i]==probs.obj$nd)
    
    print(
      paste("P0=",probs.obj$P0s[i],
            ", P(nd > Ca) for nd=",probs.obj$nd[cv.match],
            ": ",
            round(cumsum(probs.obj$probs.table[i,])[c(cv.match-1)],3),
            ", P(nd >= Ca) for nd=",probs.obj$nd[cv.match],
            ": ",
            round(cumsum(probs.obj$probs.table[i,])[c(cv.match)],3),
            sep=""))
  }
}

# Eventually move this to public
# Compute the power of the trinomial test under given parameters
# Note that the negative values are computed from the positive and tie values.
ttpow <- function(n = NULL, p_pos = NULL, p_tie = NULL, alpha = NULL){
  sum(apply(gen.probs.obj(n = n, alpha = alpha)$RejectionRegion, 
            MARGIN = 1, 
            FUN = dmultinom,
            prob = c(p_pos, p_tie, 1 - p_pos - p_tie)))
}