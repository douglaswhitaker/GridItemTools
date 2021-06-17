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
  if (diag.info.message){
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


