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
# This should be modified to account for non 5x5 grids
col2xy <- function(gc){
  return(which(matrix(1:25,nrow=5,byrow = TRUE)==gc,arr.ind = TRUE))
}