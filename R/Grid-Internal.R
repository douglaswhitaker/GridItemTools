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

# This function returns the row and column indices that correspond to the selected response based on the LimeSurvey columns
col2xy <- function(gc, mat.rows, mat.cols, diag.info.warning = FALSE){
  if (diag.info.warning){
    warning("Diagonal associated with a reciprocal relationship is bottom left to top right.")
    warning(paste("Focus on the diagonal from (", mat.rows, ",1) to (1,", mat.cols, "). (row,col)", sep=""))
  }
  return(which(matrix(1:(mat.rows*mat.cols), 
                      nrow = mat.rows, 
                      byrow = TRUE) == gc,
               arr.ind = TRUE))
}

# Essentially just creating an empty data.frame with the appropriate column names
make.grid9s <- function(grid.items.names){
  grid9s <- t(data.frame(rep(NA,length(grid.items.names))))
  colnames(grid9s) <- paste("c9",grid.items.names,sep="")
  grid9s <- grid9s[-1,]
  return(grid9s)
} # This function seems to be designed for a 9 point scale; could it be adapted?


# Data Cleaning Functions 
# (may not be widely useful)

rename.cols <- function(grid.items){
  new.names <- c()
  for (i in 1:length(grid.items)){
    new.names[i] <- paste(strsplit(names(grid.items)[i],"[...]")[[1]][1:2],collapse=".")
  }
  return(new.names)
}

display.grid2nine <- function(gcvals=c(1:25), rows=5, cols=5, b=-0.5,match.lit = FALSE){
  mat <- matrix(NA,nrow=5,ncol=5)
  for (gc in gcvals){
    i <- col2xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
    j <- col2xy(gc, rows, cols)[1] # the Y value, the row in our format
    mat[j,i] <- grid2nine(gc, rows, cols, b)
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
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale.

mat2df <- function(mat, col=5){
  return(data.frame(x=rep(1:col,each=col),y=rep(1:col, col),count=as.vector(mat)))
} # Could be adapted for non-5x5 grids.

# to do: give option to return as table for easy use of chi-sq
# and/or build testing into this function
# Note that this is a crude classifier
# See Figure A2 (p. 47; pdf p. 19)
#     Negative gets a triangular region of 6 cells
#     Positive gets a triangular region of 6 cells
#     Indifferent is half of the middle region
#     Ambivalent is half of the middle region and includes (3,3)
make4cats <- function(grid,poscut=3,negcut=3,table=FALSE){
  neg <- sum(grid[negcut:5,1:(poscut-1)])
  pos <- sum(grid[1:(negcut-1),poscut:5])
  ind <- sum(grid[1:(negcut-1),1:(poscut-1)])
  amb <- sum(grid[negcut:5,poscut:5])
  if (!table){
    return(list(pos=pos,neg=neg,ind=ind,amb=amb))
  }
  else if (table){
    tmp.tab <- as.table(matrix(c(ind,pos,
                                 neg,amb),nrow=2,byrow=TRUE))
    colnames(tmp.tab) <- c("LowPos","HighPos")
    rownames(tmp.tab) <- c("LowNeg","HighNeg")
    return(tmp.tab)
  }
} # This function can only be used on a 5-by-5 grid.

