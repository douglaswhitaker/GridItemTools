
source("R/helper.R")
source("R/Grid-Internal.R")

# read data
dat.raw <- read.csv("data/all-data-code.csv",stringsAsFactors = FALSE)


all.grid.item.cols <- sapply(names(dat.raw),grepl,pattern="_",simplify=TRUE)
gridinfo$names <- grid.item.names(names(dat.raw)[which(all.grid.item.cols)]) # length 22 vector of names


dat.grid <- dat.raw[,which(all.grid.item.cols)]
#write.csv(dat.grid,file="data/grid-data-code.csv")

# # empty results vector
# grid.resp.items <- list()


# ##########################################################
# # This should produce counts in each cell for all 22 items 
# for (i in 1:length(gridinfo$names)){
#   grid.resp.items[[gridinfo$names[i]]] <- matrix(0,nrow=5,ncol=5)
#   for (current.row in 1:nrow(dat.grid)){
#     grid.column <- which(!is.na(dat.grid[current.row,((i-1)*25+1):(i*25)])) # should only be one value
#     grid.xy <- col2xy(grid.column)
#     grid.resp.items[[gridinfo$names[i]]][grid.xy] <- grid.resp.items[[gridinfo$names[i]]][grid.xy] + 1
#   }
# }




mygridinfo <- grid.item.info.ls(x=dat.grid)
item.tester <- grid.cell.counts(x=dat.grid,gridinfo=mygridinfo,type="items")
person.tester <- grid.cell.counts(x=dat.grid,gridinfo=mygridinfo,type="respondents")


# 
# 
# grid.resp.participant <- list()
# 
# for (current.row in 1:nrow(dat.grid)){
#   grid.resp.participant[[current.row]] <- matrix(0,nrow=5,ncol=5)
#   for (i in 1:length(gridinfo$names)){
#     grid.column <- which(!is.na(dat.grid[current.row,((i-1)*25+1):(i*25)])) # should only be one value
#     grid.xy <- col2xy(grid.column)
#     grid.resp.participant[[current.row]][grid.xy] <- grid.resp.participant[[current.row]][grid.xy] + 1
#   }
# }


# grid9s <- make.grid9s(gridinfo$names)
# for (current.row in 1:nrow(dat.grid)){
#   vals <- c()
#   for (i in 1:length(gridinfo$names)){
#     grid.column <- which(!is.na(dat.grid[current.row,((i-1)*25+1):(i*25)])) # should only be one value
#     vals[i] <- grid2nine(grid.column)
#   }
#   grid9s <- rbind(grid9s,current.row=vals)
# }


rawgrid2uni(x=dat.grid,gridinfo=mygridinfo)


# Examining the proportion of responses to each item that are ON the diagonal or NEAR the diagonal
grid.count.all <- sapply(item.tester,sum,simplify=TRUE)
grid.count.diag <- sapply(item.tester,grid.tr,simplify=TRUE)
grid.count.within1 <- sapply(item.tester,within1diag,simplify = TRUE)
round(grid.count.diag/grid.count.all,2)
round(grid.count.within1/grid.count.all,2)
