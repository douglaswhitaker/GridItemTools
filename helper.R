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

# Essentially just creating an empty data.frame with the appropriate column names
make.grid9s <- function(grid.items.names){
  grid9s <- t(data.frame(rep(NA,length(grid.items.names))))
  colnames(grid9s) <- grid.items.names
  grid9s <- grid9s[-1,]
  return(grid9s)
}

grid.tr <- function(mat, col = NULL){
  if (is.null(col)) col <- ncol(mat)
  val <- 0
  for (i in 1:col){
    val <- val + mat[i,col+1-i]
  }
  return(val)
}

# In the future make number of off-diagonal diagonals selected (i.e. more than 1)
within1diag <- function(mat,col=5){
  count <- 0
  count <- count + grid.tr(mat)
  count <- count + grid.tr(mat[1:(col-1),1:(col-1)])
  count <- count + grid.tr(mat[2:col,2:col])
  return(count)
}

delete.empty.mat <- function(resp.list){
  for (i in 1:length(resp.list)){
    if (sum(resp.list[[i]])==0){
      resp.list[[i]] <- NA
    }
  }
  return(resp.list)
}

mat2df <- function(mat,col=5){
  return(data.frame(x=rep(1:col,each=col),y=rep(1:col,col),count=as.vector(mat)))
}