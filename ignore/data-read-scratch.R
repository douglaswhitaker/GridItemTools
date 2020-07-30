
source("R/helper.R")
source("R/Grid-Internal.R")
source("R/GridProcessing.R")

# read data
dat.raw <- read.csv("data/all-data-code.csv",stringsAsFactors = FALSE)

mygridinfo <- grid.item.info.ls(x=dat.raw)

dat.grid <- dat.raw[,mygridinfo$cols]



item.tester <- grid.cell.counts(x=dat.grid,gridinfo=mygridinfo,type="items",return.table = TRUE)
person.tester <- grid.cell.counts(x=dat.grid,gridinfo=mygridinfo,type="respondents",return.table = TRUE)

mytrisummary <- lapply(item.tester,grid.tri.summary)

lapply(mytrisummary,trinomial.test, alternative="greater")

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
