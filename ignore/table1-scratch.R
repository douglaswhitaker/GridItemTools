prob.nd <- function(n,nd,k,p_tie){
  factorial(n)/(factorial(nd+k)*factorial(k)*factorial(n-nd-2*k))*
    ((1-p_tie)/2)^(nd+2*k)*
    p_tie^(n-nd-2*k)
}

prob.nd2 <- function(n,nd,p_tie){
  tmp.prob <- 0
  for (k in 0:((n-nd)/2)){
    tmp.prob <- tmp.prob + prob.nd(n=n,nd=nd,k=k,p_tie=p_tie)
  }
  return(tmp.prob)
}

# ttpower <- function(p_pos,p_tie,alpha,n){
#   
# }

n <- 10
alpha <- 0.05

P0s <- seq(from=0,to=.9,by=.1)

calc.ca <- function(vec,alpha,p0){
  np <- vec[1]
  nt <- vec[2]
  nn <- vec[3]
  n <- sum(vec)
  nd <- np-nn
  return(prob.nd2(n=n,nd=nd,p_tie=p0))
}

RR <- list()
RR[[1]] <- c(10,0,0)
RR[[2]] <- c(9,0,1)
RR[[3]] <- c(9,1,0)
RR[[4]] <- c(8,1,1)
RR[[5]] <- c(8,2,0)
RR[[6]] <- c(7,2,1)
RR[[7]] <- c(7,3,0)
RR[[8]] <- c(6,3,1)
RR[[9]] <- c(6,4,0)
RR[[10]] <- c(5,5,0)
RR[[11]] <- c(4,6,0)


calc.ca(vec=c(10,0,0),alpha=0.05,p0=0)

tmp.sum <- 0
for (i in 1:length(RR)){
  p0 <- RR[[i]][2]/10
  tmp.sum <- tmp.sum + calc.ca(vec=RR[[i]],alpha=0.05,p0=p0)
}


xs <- 0:10

gen.probs.table <- function(n,alpha=0.05,include.one=TRUE,find.RR=TRUE){
  tmp.sum <- c()
  probs.table <- c()
  xs <- 0:n
  #P0s <- seq(from=0,to=.9,by=.1)
  if (include.one){
    P0s <- (0:(n))/n
  }
  else {
    P0s <- (0:(n-1))/n
  }
  for (p0 in P0s){
    for (i in 1:length(xs)){
      tmp.sum[i] <- prob.nd2(n=n,nd=xs[i],p_tie=p0)
    }
    probs.table <- rbind(probs.table,tmp.sum[(n+1):1])
  }
  rownames(probs.table) <- paste("p0=",P0s,sep="")
  colnames(probs.table) <- paste("nd=",n:0,sep="")
  
  # for (i in 1:nrow(probs.table)){
  #   print(paste(rownames(probs.table)[i],"Critical Value:",colnames(probs.table)[which((probs.table[i,] > alpha) == TRUE)[1] - 1]))
  # }
  probs.obj <- list(probs.table=probs.table,nd=n:0,P0s=P0s)
  probs.obj <- find.critical.values(probs.obj)
  
  if (find.RR){
    probs.obj$RejectionRegion <- find.rejection.region(probs.obj)
  }
  
  return(probs.obj)
}



#tab1 <- gen.table1(10)
myprobobj <- gen.probs.table(10)


for (i in 1:nrow(tab1)){
  print(rownames(tab1)[i])
  #print(cumsum(tab1[i,]) > alpha)
  print(colnames(tab1)[which(cumsum(tab1[i,]) > alpha)[1]])
  #print(round(cumsum(tab1[i,]),3))
  #print("###########################################################")
}


find.critical.values <- function(probs.obj,alpha=0.05,verbose=FALSE){
  critvals <- c()
  for (i in 1:nrow(probs.obj$probs.table)){
    if (verbose){
      print(rownames(probs.obj$probs.table)[i])
      #print(cumsum(probs.table[i,]) > alpha)
      print(colnames(probs.obj$probs.table)[which(cumsum(probs.obj$probs.table[i,]) > alpha)[1]])
    }
    critvals[i] <- probs.obj$nd[which(cumsum(probs.obj$probs.table[i,]) > alpha)[1]]
    #print(round(cumsum(probs.table[i,]),3))
    #print("###########################################################")
  }
  probs.obj$critvals <- critvals 
  return(probs.obj)
}

myprobobj2 <- find.critical.values(myprobobj)

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

#cumsum(tmp.sum[10:1])

# Note that  0.031824568 0.076242052 are the (rounded) table entries for the .4 column of Table 1 of Bian (2011)

# > cumsum(tmp.sum[10:1])
# [1] 0.000078732 0.000610173 0.002998377 0.010962338 0.031824568 0.076242052 0.154788724 0.271792657 0.419916793 0.580071397