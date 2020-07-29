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
    critvals[i] <- probs.obj$nd[which(cumsum(probs.obj$probs.table[i,]) > alpha)[1]]
  }
  probs.obj$critvals <- critvals 
  return(probs.obj)
}

# Helper function for gen.probs.obj
# Identifies the rejection region for various settings
find.rejection.region <- function(probs.obj){
  n <- probs.obj$nd[1]
  grid <- expand.grid(0:n,0:n,0:n)
  colnames(grid) <- c("Pos","Zero","Neg")
  grid <- grid[which(rowSums(grid)==10),]
  nd <- grid$Pos - grid$Neg
  p0 <- grid$Zero/n
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

