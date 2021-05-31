

# This generates a list of matrices where each cell is the count of the number of responses
#' Title
#'
#' @param x 
#' @param gridinfo 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
grid.cell.counts <- function(x,gridinfo,type="items",return.table = FALSE){
  grid.resp.list <- list()
  
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  if (type=="items"){
    for (i in 1:length(gridinfo$names)){
      
      grid.resp.list[[gridinfo$names[i]]] <- matrix(0,nrow=rows,ncol=cols)
      
      for (current.row in 1:nrow(x)){
        
        grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
        grid.xy <- col2xy(grid.column, rows, cols)
        grid.resp.list[[gridinfo$names[i]]][grid.xy] <- grid.resp.list[[gridinfo$names[i]]][grid.xy] + 1
        
      }
      
      if (return.table){
        grid.resp.list[[gridinfo$names[i]]] <- as.table(grid.resp.list[[gridinfo$names[i]]])
        rownames(grid.resp.list[[gridinfo$names[i]]]) <- c("LowNeg",
                                                          rep("",nrow(grid.resp.list[[gridinfo$names[i]]])-2),
                                                          "HighNeg")
        colnames(grid.resp.list[[gridinfo$names[i]]]) <- c("LowPos",
                                                          rep("",ncol(grid.resp.list[[gridinfo$names[i]]])-2),
                                                          "HighPos")
      }
    }
  }
  
  else if (type=="respondents"){
    for (current.row in 1:nrow(x)){
      
      grid.resp.list[[current.row]] <- matrix(0,nrow=rows,ncol=cols)
      
      for (i in 1:length(gridinfo$names)){
        
        grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
        grid.xy <- col2xy(grid.column, rows, cols)
        grid.resp.list[[current.row]][grid.xy] <- grid.resp.list[[current.row]][grid.xy] + 1
        
      }
    }
  }
  
  return(grid.resp.list)
}

# This function produces a list that contains information about the grid items
# Either raw data or grid-only data may be used
# This presupposes LimeSurvey formatted data
# In the future, develop a way to algorithmically determine rows and cols
#' Title
#'
#' @param x 
#' @param rows 
#' @param cols 
#'
#' @return
#' @export
#'
#' @examples
grid.item.info.ls <- function(x,rows=5,cols=5){
  gridinfo <- list()
  gridinfo$cols <- sapply(names(x),grepl,pattern="_",simplify=TRUE) # the columns that contain grid items
  gridinfo$names <- grid.item.names(names(x)[which(gridinfo$cols)]) # vector of names
  gridinfo$dim <- c(rows,cols)
  
  return(gridinfo)
} # Could be adapted for non-5x5 grids.


# This function processes raw grid data into approximate scores from a Likert-type scale
#' Title
#'
#' @param x 
#' @param gridinfo 
#'
#' @return
#' @export
#'
#' @examples
rawgrid2uni <- function(x,gridinfo){
  grid9s <- make.grid9s(gridinfo$names)
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  for (current.row in 1:nrow(x)){
    vals <- c()
    for (i in 1:length(gridinfo$names)){
      grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
      vals[i] <- grid2nine(grid.column)
    }
    grid9s <- rbind(grid9s,current.row=vals)
  }
  rownames(grid9s) <- rownames(x)
  return(grid9s)
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale.

# In the future make number of off-diagonal diagonals selected (i.e. more than 1)
# To-do: Make this an internal function, and create a function that does more summary statistic processing than this
# (This requires sapply to work)
#' Title
#'
#' @param mat 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
within1diag <- function(mat,col=5){
  count <- 0
  count <- count + grid.tr(mat)
  count <- count + grid.tr(mat[1:(col-1),1:(col-1)])
  count <- count + grid.tr(mat[2:col,2:col])
  return(count)
} # Could be adapted for non-5x5 grids.

# This function does the correct trace for grid items. 
# It is used by within1diag()
# Note that this sums along the other diagonal that tr() does not do, 
# i.e. this function sums the bottom-left to top-right diagonal.
# To-do: Make this internal as with above
#' Title
#'
#' @param mat 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
grid.tr <- function(mat, col = NULL){
  if (is.null(col)) col <- ncol(mat)
  val <- 0
  for (i in 1:col){
    val <- val + mat[i,col+1-i]
  }
  return(val)
}


# This will be a simple version at first. We'll account for within1diag later
grid.tri.summary <- function(mat, rows = 5, cols = 5, offdiag = 0){
  if (offdiag == 0){
    mat <- mat[,ncol(mat):1]
    tie <- sum(diag(mat))
    upper <- sum(mat[upper.tri(mat,diag=FALSE)])
    lower <- sum(mat[lower.tri(mat,diag=FALSE)])
  }
  return(list(upper=upper,lower=lower,tie=tie))
} # Could be adapted for non-5x5 grids.
