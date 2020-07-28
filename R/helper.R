# Data Cleaning Functions 
# (may not be widely useful)

rename.cols <- function(grid.items){
  new.names <- c()
  for (i in 1:length(grid.items)){
    new.names[i] <- paste(strsplit(names(grid.items)[i],"[...]")[[1]][1:2],collapse=".")
  }
  return(new.names)
}

# This function processes the LimeSurvey column names. 
# For example, there are 25 variables of the format Page4Grid1.Y1_X1. for the one item Page4Grid1.
# This function will identify that Page4Grid1 is an item name. 
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

display.grid2nine <- function(gcvals=c(1:25),b=-0.5,match.lit = FALSE){
  mat <- matrix(NA,nrow=5,ncol=5)
  for (gc in gcvals){
    i <- col2xy(gc)[2] # this is the X value (positive axis), so the column in our format
    j <- col2xy(gc)[1] # the Y value, the row in our format
    mat[j,i] <- grid2nine(gc,b)
  }
  colnames(mat) <- paste("Agree",1:5,sep="")
  rownames(mat) <- paste("Disagree",1:5,sep="")
  if (!match.lit){
    mat <- as.table(mat)
    return(mat)
  }
  else{
    return(mat[5:1,])
  }
}

# Essentially just creating an empty data.frame with the appropriate column names
make.grid9s <- function(grid.items.names){
  grid9s <- t(data.frame(rep(NA,length(grid.items.names))))
  colnames(grid9s) <- paste("c9",grid.items.names,sep="")
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

sum.resp.mats <- function(mat.list,items=NULL){
  tmp <- matrix(0,nrow=dim(mat.list[[1]])[1],ncol=dim(mat.list[[1]])[2])
  if (is.null(items)){
    items <- 1:length(mat.list)
  }
  for (i in items){
    tmp <- tmp + mat.list[[i]]
  }
  return(tmp)
}

# to do: give option to return as table for easy use of chi-sq
# and/or build testing into this function
# Note that this is a crude classifier
# See Figure A2 (p. 47; pdf p. 19)
#     Negative gets a triangular region of 6 cells
#     Positive gets a triangular region of 6 cells
#     Indifferent is half of the middle region
#     Ambivalent is half of the middle region and includes (3,3)
make4cats <- function(grid,poscut=3,negcut=3){
  neg <- sum(grid[negcut:5,1:(poscut-1)])
  pos <- sum(grid[1:(negcut-1),poscut:5])
  ind <- sum(grid[1:(negcut-1),1:(poscut-1)])
  amb <- sum(grid[negcut:5,poscut:5])
  return(list(pos=pos,neg=neg,ind=ind,amb=amb))
}


fixLimeSurveyLikert <- function(dat, cols, ...){
  for (i in cols){
    dat[,i] <- as.numeric(substr(dat[,i],1,1)) # as ong as there are 9 or fewer scale points "1,1" should work
  }
  return(dat)
}
