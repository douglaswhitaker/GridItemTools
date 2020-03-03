
dat.raw <- read.csv("data/real/results-survey945827-202003030207-code.csv")

all.grid.item.cols <- sapply(names(dat.raw),grepl,pattern="_",simplify=TRUE)


all.grid.item.names <- grid.item.names(names(dat.raw)[which(all.grid.item.cols)]) # length 22 vector of names

dat.grid <- dat.raw[,which(all.grid.item.cols)]


grid.resp.items <- grid.resp.participant <- list()

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

## ## ## Diagonal direction is WRONG - it isn't (1,1) through (5,5)
## It is (1,5) through (5,1)
## Write a grid.tr() function to do this instead.

grid.count.all <- sapply(grid.resp.items,sum,simplify=TRUE)

library(psych)
grid.count.diag <- sapply(grid.resp.items,tr,simplify=TRUE)

round(grid.count.diag/grid.count.all,2)


grid.count.within1 <- sapply(grid.resp.items,within1diag,simplify = TRUE)
round(grid.count.within1/grid.count.all,2)


##################################
# Next: for each of the respondents, get 22 converted 9-point scales so we can check (spearman) correlations