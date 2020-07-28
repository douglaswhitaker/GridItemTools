# This generates a list of matrices where each cell is the count of the number of responses
grid.cell.counts <- function(x,gridinfo,type="items"){
  grid.resp.list <- list()
  
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  if (type=="items"){
    for (i in 1:length(gridinfo$names)){
      
      grid.resp.list[[gridinfo$names[i]]] <- matrix(0,nrow=rows,ncol=cols)
      
      for (current.row in 1:nrow(x)){
        
        grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
        grid.xy <- col2xy(grid.column)
        grid.resp.list[[gridinfo$names[i]]][grid.xy] <- grid.resp.list[[gridinfo$names[i]]][grid.xy] + 1
        
      }
    }
  }
  
  else if (type=="respondents"){
    for (current.row in 1:nrow(x)){
      
      grid.resp.list[[current.row]] <- matrix(0,nrow=rows,ncol=cols)
      
      for (i in 1:length(gridinfo$names)){
        
        grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
        grid.xy <- col2xy(grid.column)
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
grid.item.info.ls <- function(x,rows=5,cols=5){
  gridinfo <- list()
  gridinfo$cols <- sapply(names(x),grepl,pattern="_",simplify=TRUE) # the columns that contain grid items
  gridinfo$names <- grid.item.names(names(x)[which(gridinfo$cols)]) # vector of names
  gridinfo$dim <- c(rows,cols)
  
  return(gridinfo)
}


# This function processes raw grid data into approximate scores from a Likert-type scale
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
}

# In the future make number of off-diagonal diagonals selected (i.e. more than 1)
# To-do: Make this an internal function, and create a function that does more summary statistic processing than this
# (This requires sapply to work)
within1diag <- function(mat,col=5){
  count <- 0
  count <- count + grid.tr(mat)
  count <- count + grid.tr(mat[1:(col-1),1:(col-1)])
  count <- count + grid.tr(mat[2:col,2:col])
  return(count)
}

# This function does the correct trace for grid items. 
# It is used by within1diag()
# To-do: Make this internal as with above
grid.tr <- function(mat, col = NULL){
  if (is.null(col)) col <- ncol(mat)
  val <- 0
  for (i in 1:col){
    val <- val + mat[i,col+1-i]
  }
  return(val)
}