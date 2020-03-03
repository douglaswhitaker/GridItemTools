
# read data
dat.raw <- read.csv("data/real/results-survey945827-202003030207-code.csv")

# To do: build in a better consent checking mechanism. Right now we ONLY have item data from people who did consent 
# The ONLY columns that we need to deal with are:
#         interviewtime. Total time	
#         groupTime18654. Group time: Informed Consent
# Quotas were used to ensure that anyone who did not select (Yes, Yes, 19+, Yes) was not presented the survey.


# identify the columns of the dataset that contain the data for the grid items
# each grid item occupies 25 columns; 24 values are NA and one value is 1
all.grid.item.cols <- sapply(names(dat.raw),grepl,pattern="_",simplify=TRUE)
all.grid.item.names <- grid.item.names(names(dat.raw)[which(all.grid.item.cols)]) # length 22 vector of names

# subset the dataset so that we can step through in increments of 25
# it might be better to not subset this and instead have a more sophisticated looping mechanism, but that's for later 
dat.grid <- dat.raw[,which(all.grid.item.cols)]

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

# Examining the proportion of responses to each item that are ON the diagonal or NEAR the diagonal
grid.count.all <- sapply(grid.resp.items,sum,simplify=TRUE)
grid.count.diag <- sapply(grid.resp.items,grid.tr,simplify=TRUE)
grid.count.within1 <- sapply(grid.resp.items,within1diag,simplify = TRUE)
round(grid.count.diag/grid.count.all,2)
round(grid.count.within1/grid.count.all,2)


##########################################################
# This should produce counts in each cell for all participants

grid.resp.participant <- list()

for (current.row in 1:nrow(dat.grid)){
  grid.resp.participant[[current.row]] <- matrix(0,nrow=5,ncol=5)
  for (i in 1:length(all.grid.item.names)){
    grid.column <- which(!is.na(dat.grid[current.row,((i-1)*25+1):(i*25)])) # should only be one value
    grid.xy <- col2xy(grid.column)
    grid.resp.participant[[current.row]][grid.xy] <- grid.resp.participant[[current.row]][grid.xy] + 1
  }
}

# Account for people who did not respond to the items (i.e. those who do not consent)
delete.empty.mat(grid.resp.participant)


#################
# Making graphs
library(ggplot2)
library(gridExtra)

heat.list <- list()
for (i in 1:length(grid.resp.items)){
  heat.list[[i]] <- ggplot(data=mat2df(grid.resp.items[[i]]),
                         mapping=aes(x=x,y=y,fill=count)) +
    geom_tile() + scale_fill_gradient(name = "Count",
                                      low = "#FFFFFF",
                                      high = "#012345") +
    labs(x="Positive",y="Negative") + 
    ggtitle(paste(names(grid.resp.items[i]),": Heat Map - Grid Pilot March 2020",sep=""))
}

# Calling this n=10 on morning of March 3rd because there are 10 complete
# A more accurate description would be n=13 because of the 3 partial
grid.arrange(grobs=heat.list[1:10],nrow=3) # Page 2 = Group B (10 items)
grid.arrange(grobs=heat.list[11:19],nrow=3) # Page 4 = Group A (9 items)
grid.arrange(grobs=heat.list[20:22],nrow=3) # Page 5 = Group C (3 items)
grid.arrange(grobs=heat.list[20:22],nrow=2) # Page 5 = Group C (3 items)



# testing.heat <- ggplot(data=mat2df(grid.resp.items[[1]]),
#                        mapping=aes(x=x,y=y,fill=count)) +
#   geom_tile() + scale_fill_gradient(name = "Count",
#                                     low = "#FFFFFF",
#                                     high = "#012345") +
#   ggtitle(paste(names(grid.resp.items[1]),"Heat Map - Grid Pilot March 2020"))
# 
# testing.heat

##################################
# Next: for each of the respondents, get 22 converted 9-point scales so we can check (spearman) correlations