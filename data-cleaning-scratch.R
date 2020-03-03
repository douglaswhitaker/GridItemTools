
source("helper.R")

dat <- read.csv("data/grid-pilot-2209-format-NOT-data.csv")

#grepl(pattern="Page2Grid")

# this will be a vector to help us identify the correct columns of dats
grid.items.page2 <- sapply(names(dat),grepl,pattern="Page2Grid",simplify=TRUE)
grid.items.page2.dat <- dat[,which(grid.items.page2)]

# length(grid.items.page2) # there are 10 columns of timings at the end
# which(grid.items.page2)
# 
# grid.items.page2 <- grid.items.page2[-c(length(grid.items.page2)-10,length(grid.items.page2))]
# 
# (names(grid.items.page2)[1])

colnames(grid.items.page2.dat) <- rename.cols(grid.items.page2[which(grid.items.page2)])

grid.items.page2.dat



Page2Grid1.mat <- matrix(0,nrow=5,ncol=5)
grid.items.page2.names <-  grid.item.names(names(grid.items.page2.dat))[1:(length(grid.item.names(names(grid.items.page2.dat)))/2)]
# should have K grid names and K grid timings so take the first half
# We know that there are 5*5=25 sequential columns for each item

###################### This code will increment over rows (participants) computing the number of responses in each cell

Page2Grids <- list()

for (i in 1:length(grid.items.page2.names)){
  Page2Grids[[grid.items.page2.names[i]]] <- matrix(0,nrow=5,ncol=5)
  for (current.row in 3:3){
    grid.column <- which(!is.na(grid.items.page2.dat[current.row,((i-1)*25):(i*25)])) # should only be one value
    grid.xy <- col2xy(grid.column)
    Page2Grids[[grid.items.page2.names[i]]][grid.xy] <- Page2Grids[[grid.items.page2.names[i]]][grid.xy] + 1
  }
}

# This is code to convert the grid responses to 9-point scale

grid9s <- make.grid9s(grid.items.page2.names)
for (current.row in 3:3){
  vals <- c()
  for (i in 1:length(grid.items.page2.names)){
    grid.column <- which(!is.na(grid.items.page2.dat[current.row,((i-1)*25):(i*25)])) # should only be one value
    vals[i] <- grid2nine(grid.column)
  }
  grid9s <- rbind(grid9s,current.row=vals)
}