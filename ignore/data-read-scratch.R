
source("R/helper.R")

# read data
dat.raw <- read.csv("data/all-data-code.csv",stringsAsFactors = FALSE)


all.grid.item.cols <- sapply(names(dat.raw),grepl,pattern="_",simplify=TRUE)
all.grid.item.names <- grid.item.names(names(dat.raw)[which(all.grid.item.cols)]) # length 22 vector of names


dat.grid <- dat.raw[,which(all.grid.item.cols)]
write.csv(dat.grid,file="data/grid-data-code.csv")

# empty results vector
grid.resp.items <- list()


##########################################################
# This should produce counts in each cell for all 22 items 
for (i in 1:length(all.grid.item.names)){
  grid.resp.items[[all.grid.item.names[i]]] <- matrix(0,nrow=5,ncol=5)
  for (current.row in 1:nrow(dat.grid)){
    grid.column <- which(!is.na(dat.grid[current.row,((i-1)*25+1):(i*25)])) # should only be one value
    grid.xy <- col2xy(grid.column)
    grid.resp.items[[all.grid.item.names[i]]][grid.xy] <- grid.resp.items[[all.grid.item.names[i]]][grid.xy] + 1
  }
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
}

# This generates a list of matrices where each cell is the count of the number of responses
grid.cell.counts <- function(x,gridinfo,type="items"){
  grid.resp.list <- list()
  
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  for (i in 1:length(gridinfo$names)){
    
    grid.resp.list[[gridinfo$names[i]]] <- matrix(0,nrow=rows,ncol=cols)
    
    for (current.row in 1:nrow(x)){
      
      grid.column <- which(!is.na(x[current.row,((i-1)*(rows*cols)+1):(i*(rows*cols))])) # should only be one value
      grid.xy <- col2xy(grid.column)
      grid.resp.list[[gridinfo$names[i]]][grid.xy] <- grid.resp.list[[gridinfo$names[i]]][grid.xy] + 1
    
    }
  }
  return(grid.resp.list)
}