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


gentable2 <- function(p_tie, alpha, inc=0.05){
  for (j in p_tie){
    vec.p_pos <- seq(from=(1-j)/2,to=(1-j-inc),by=inc)
    pows <- c()
    for (i in 1:length(vec.p_pos)){
      pows[i] <- ttpow(n = 10, p_pos = vec.p_pos[i], p_tie = j, alpha = alpha)
    }
    print("#####################################")
    print(paste("p_tie =",j))
    print(paste("p_pos =",vec.p_pos))
    print(paste("power =",pows))
  }
}
gentable2(p_tie = c(.1,.2,.3), alpha = 0.05)

# older version output
### Results are pretty close
# > gentable2(p_tie = 0.10, alpha = 0.05)
# [1] 0.02193922 0.04442000 0.08327637 0.14581788 0.23974291 0.37090360 0.53915514 0.73140224 0.91070260
# > gentable2(p_tie = 0.20, alpha = 0.05)
# [1] 0.03214213 0.06490235 0.12022719 0.20652503 0.33094656 0.49541281 0.68986180 0.88157334
# > gentable2(p_tie = 0.30, alpha = 0.05)
# [1] 0.03619317 0.07590016 0.14382117 0.24975125 0.40048097 0.59376525 0.80846255
# > gentable2(p_tie = 0.40, alpha = 0.05)
# [1] 0.03569770 0.07944379 0.15707668 0.28116382 0.46032969 0.69118503

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

gen.probs.obj <- function(n,alpha=0.05,include.one=TRUE,find.RR=TRUE,digits=NULL){
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
  
  #probs.table <- ifelse(is.null(digits),probs.table,round(probs.table,digits))
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

# Helper function for gen.probs.obj
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
# Maybe make the output like this:
# P0=value, Ca=value, P(nd > Ca)=value, P(nd >= Ca)=value

find.cutpoints(mbo)
#cumsum(tmp.sum[10:1])

# Note that  0.031824568 0.076242052 are the (rounded) table entries for the .4 column of Table 1 of Bian (2011)

# > cumsum(tmp.sum[10:1])
# [1] 0.000078732 0.000610173 0.002998377 0.010962338 0.031824568 0.076242052 0.154788724 0.271792657 0.419916793 0.580071397