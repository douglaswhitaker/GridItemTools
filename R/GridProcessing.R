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