# Note that b = -1 violates Constraint 6
grid9s.b1 <- make.grid9s(all.grid.item.names)
for (current.row in 1:nrow(dat.grid)){
  vals <- c()
  for (i in 1:length(all.grid.item.names)){
    grid.column <- which(!is.na(dat.grid[current.row,((i-1)*25+1):(i*25)])) # should only be one value
    vals[i] <- grid2nine(grid.column,b=-1)
  }
  grid9s.b1 <- rbind(grid9s.b1,current.row=vals)
}

dat.nogrid9.b1 <- cbind(dat.raw[,which(!all.grid.item.cols)],grid9s.b1)

names(dat.nogrid9.b1)
# Columns 13-21 match with 107-115
# Columns 25-34 match with 97-106

# Check correlations within people and within items

####
### We need to be careful here - it looks like NA's are getting changed to vectors of 1's - not good!
###

#dat.nogrid9.b1[which((dat.nogrid9.b1==""))] <- NA

fixLimeSurveyLikert <- function(dat, cols, ...){
  for (i in cols){
    dat[,i] <- as.numeric(substr(dat[,i],1,1)) # as ong as there are 9 or fewer scale points "1,1" should work
  }
  return(dat)
}

dat.nogrid9.b1 <- fixLimeSurveyLikert(dat.nogrid9.b1,cols=c(13:21,25:34))

# Try these again with b=-1
# These are correlations within items
for (i in 13:21){
  print(round(cor(dat.nogrid9.b1[,i],dat.nogrid9.b1[,i+94],method="spearman",use="pairwise.complete.obs"),3))
}

for (i in 25:34){
  print(round(cor(dat.nogrid9.b1[,i],dat.nogrid9.b1[,i+72],method="spearman",use="pairwise.complete.obs"),3))
}




# These are correlations within people
cor.within.participants <- c()
for (j in 1:nrow(dat.nogrid9.b1)){
  tmpcor <- cor(as.numeric(dat.nogrid9.b1[j,c(13:21,25:34)]),
                as.numeric(dat.nogrid9.b1[j,c(107:115,97:106)]),
                method="spearman",use="pairwise.complete.obs")
  cor.within.participants[j] <- tmpcor
  if (!is.na(tmpcor)){
    print(round(tmpcor,3))
  }
}


library(corrplot)
all.items.cor <- cor(dat.nogrid9.b1[,c(13:21,107:115,25:34,97:106)],method="spearman",use="pairwise.complete.obs")
corrplot(all.items.cor)
