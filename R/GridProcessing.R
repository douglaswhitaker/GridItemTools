

# This generates a list of matrices where each cell is the count of the number of responses
#' Title
#'
#' @param x 
#' @param gridinfo 
#' @param type 
#' @param return.table 
#'
#' @return
#' @export
#'
#' @examples
grid.cell.counts <- function(x, gridinfo, type = "items", return.table = FALSE){
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
#' @param b
#'
#' @return
#' @export
#'
#' @examples
rawgrid2uni <- function(x, gridinfo, b = -0.5){
  grid9s <- make.grid9s(gridinfo$names)
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  for (current.row in 1:nrow(x)){
    vals <- c()
    for (i in 1:length(gridinfo$names)){
      grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
      vals[i] <- grid2nine(grid.column, b = b)
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

#' Title
#'
#' @param mat 
#' @param rows 
#' @param cols 
#' @param offdiag 
#' @param return.table 
#'
#' @return
#' @export
#'
#' @examples
grid.tri.summary <- function(mat, rows = 5, cols = 5, offdiag = 0, 
                             return.table = TRUE){
  if (offdiag == 0){
    mat <- mat[,ncol(mat):1]
    tie <- sum(diag(mat))
    upper <- sum(mat[upper.tri(mat,diag=FALSE)])
    lower <- sum(mat[lower.tri(mat,diag=FALSE)])
  }
  else {
    stop("Not yet implemented.")
  }
  
  if (return.table){
    tmp.tab <- as.table(c(upper,tie,lower))
    names(tmp.tab) <- c("upper","diagonal","lower")
    return(tmp.tab)
  }
  else { #if we don't return a table, we return a lit
    return(list(upper=upper,diagonal=tie,lower=lower))
  }
} # Could be adapted for non-5x5 grids.


# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2
# Convert grid value to 1 to 9 value
#' Title
#'
#' @param gc 
#' @param rows 
#' @param cols 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
grid2nine <- function(gc, rows=5, cols=5, b = -0.5){
  i <- col2xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
  j <- col2xy(gc, rows, cols)[1] # the Y value, the row in our format
  return((b+2)*i+b*j-1-6*b)
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale. 

# This function is intended to be applied to a list that is the output of 
# grid.cell.counts with type = "respondents"
# If remove = TRUE, respondent summary matrices with sum 0 (no responses) are removed from the list
# If remove = FALSE, such matrices are replaced with NA
#' Title
#'
#' @param resp.list 
#' @param remove 
#'
#' @return
#' @export
#'
#' @examples
delete.empty.mat <- function(resp.list, remove = TRUE){
  if (remove){
    tmp.list <- list()
    for (i in length(resp.list):1){
      if (sum(resp.list[[i]])==0){
        resp.list[[i]] <- NULL # removes the index and closes the hole; must loop backward through the list
      }
    }
    for (i in length(resp.list):1){
      tmp.list[[(length(resp.list)+1)-i]] <- resp.list[[i]]
    }
    resp.list <- tmp.list
  }
  else{
    for (i in 1:length(resp.list)){
      if (sum(resp.list[[i]])==0){
        resp.list[[i]] <- NA
      }
    }
  }
  return(resp.list)
}

# This function accepts a list (the output from grid.cell.counts) and sums across the list.
# It returns a single matrix that is the sum of the individual matrices.
# Note that it uses the first matrix in the list to determine the dimensions:
# if this matrix is missing (an NA), there will be an error.
# It is best to use this after delete.empty.mat.
#' Title
#'
#' @param mat.list 
#' @param items 
#'
#' @return
#' @export
#'
#' @examples
sum_resp_mats <- function(mat.list,items=NULL){
  tmp <- matrix(0,nrow=dim(mat.list[[1]])[1],ncol=dim(mat.list[[1]])[2])
  if (is.null(items)){
    items <- 1:length(mat.list)
  }
  for (i in items){
    tmp <- tmp + mat.list[[i]]
  }
  return(tmp)
}

#' Title
#'
#' @param dat 
#' @param lsvals 
#' @param livals 
#'
#' @return
#' @export
#'
#' @examples
fixLimeSurveyLikert <- function(dat, 
                                lsvals = c("D4","D3","D2","D1",
                                           "N0","A1","A2","A3","A4"), 
                                livals = 1:9){
  for (i in 1:length(livals)){
    dat[dat==lsvals[i]] <- livals[i]
  }
  return(as.data.frame(sapply(dat, as.numeric)))
}

# This is hard-coded for 5x5 grid displayed in the manner this package uses
#' Title
#'
#' @param grid 
#'
#' @return
#' @export
#'
#' @examples
classify_responses <- function(grid){
  grid <- t(grid) # based the counts on the one in the appendix, but it's really the transpose
  # These are based on Audrezet et al. (2016, p. 47) Figure A2
  indifferent_cells <- c(1,2,6,7,8,12)
  ambivalent_cells <- c(13,14,18,19,24,20,25)
  negative_cells <- c(21,22,16,17,11,23)
  positive_cells <- c(5,4,10,3,9,15)
  
  indifferent_counts <- sum(as.vector(grid)[indifferent_cells])
  ambivalent_counts <- sum(as.vector(grid)[ambivalent_cells])
  negative_counts <- sum(as.vector(grid)[negative_cells])
  positive_counts <- sum(as.vector(grid)[positive_cells])
  
  return(list(indifferent_counts, positive_counts,
              negative_counts, ambivalent_counts))
}
