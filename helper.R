# Data Cleaning Functions 
# (may not be widely useful)

rename.cols <- function(grid.items){
  new.names <- c()
  for (i in 1:length(grid.items)){
    new.names[i] <- paste(strsplit(names(grid.items)[i],"[...]")[[1]][1:2],collapse=".")
  }
  return(new.names)
}

grid.item.names <- function(names.vec){
  new.names <- c()
  for (i in 1:length(names.vec)){
    new.names[i] <- paste(strsplit(names.vec[i],"[.]")[[1]][1],collapse=".")
  }
  return(unique(new.names))
}

col2xy <- function(gc){
  return(which(matrix(1:25,nrow=5,byrow = TRUE)==gc,arr.ind = TRUE))
}

# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2
# Convert grid value to 1 to 9 value
grid2nine <- function(gc,b = -0.5){
  i <- col2xy(gc)[2] # this is the X value (positive axis), so the column in our format
  j <- col2xy(gc)[1] # the Y value, the row in our format
  return((b+2)*i+b*j-1-6*b)
}

make.grid9s <- function(grid.items.names){
  grid9s <- t(data.frame(rep(NA,length(grid.items.names))))
  colnames(grid9s) <- grid.items.names
  grid9s <- grid9s[-1,]
  return(grid9s)
}

# This is wrong! Need to write grid.tr() because diagonal is (1,5) through (5,1)
# trace + trace of two submatrices
library(psych)
within1diag <- function(mat,col=5){
  count <- 0
  count <- count + psych::tr(mat)
  count <- count + psych::tr(mat[1:(col-1),2:col])
  count <- count + psych::tr(mat[2:col,1:(col-1)])
  return(count)
}