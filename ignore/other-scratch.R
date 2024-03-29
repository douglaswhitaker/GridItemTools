# Need to fix this

# > trinomial.test(frame[,2],frame[,1],alternative="greater")
# [1] NaN
# There were 21 warnings (use warnings() to see them)

# Expected behaviour: p-value is 1 - the greater alternative




###Example Data
row1 <- c(30,15,35,12,35,8,21,8,29,17)
row2 <- c(23,13,31,15,35,8,18,7,22,13)
frame <- cbind(row1,row2)
frame <- data.frame(frame)

alternative <- "greater"

col1 <- row1; col2 <- row2


col1 <- col2 <- 1:5

columns <- cbind(col1, col2) #Merge the two columns
dat <- data.frame(columns) #Create data frame with the columns
numrow1 <- nrow(dat) #Find the length of the data frame to see number of observations
dat <- dat[stats::complete.cases(dat),] #Remove empty/NA columns
numrow2 <- nrow(dat) #Find the length of the data frame after clearing any rows with missing data values

#If-statement to tell user if any rows were removed due to empty cells - not working
#if(numrow1 > numrow2){
# print((numrow1 - numrow2), " observations have been removed due to missing values.")
#}

# I believe this will fix the if-statement so long as the vectors have NA values in the missing positions.--JB
if(numrow1 > numrow2){
 print(paste((numrow1 - numrow2), "observations have been removed due to missing values."))
  # Here is a fix to remove any NAs from col1 and col2:--JB
  col1 <- dat$col1
  col2 <- dat$col2
}

#Create variables
n <- numrow2

# n_pos <- sum((col1-col2) > 0)
# n_neg <- sum((col1-col2) < 0)
# n_tie <- sum((col1-col2) == 0)

ns <- calculate.ns(col1,col2)

reverse.flag <- FALSE

n_diff <- ns$n_pos - ns$n_neg #This is n_d in the paper
if (n_diff < 0){
  print("Calculated test statistic is negative.")
  print("Continuing with the roles of col1 and col2 switched.")
  reverse.flag <- TRUE
  n_diff <- ns$n_neg - ns$n_pos
}

p_tie <- ns$n_tie/n #This is p_o in the paper - we can consider making this adjustable further on if the user has any theoretical knowledge of p_o

p_value <- 0 #Create p-value variable

#Loop to compute the p-value
for (j in n_diff:n){
  for (k in 0:((n-j)/2)){
    prob <- prob.nd(n=n,nd=j,k=k,p_tie=p_tie)
    p_value <- p_value + prob
  }
}

if(alternative=="greater"){ #One-sided test
  if (reverse.flag){
    print(1-p_value)
    
  }
  else{
    print(p_value)
  }
}
if(alternative=="less"){
  if (reverse.flag){
    print(p_value)
    
  }
  else{
    print(1-p_value)
  }
}
if(alternative=="two.sided"){
  print(p-value*2)
}


###############################################################################
# identifying rejection region

probs.obj <- gen.probs.table(10,include.one = TRUE)
probs.obj <- find.critical.values(mpo)

n <- probs.obj$nd[1]
grid <- expand.grid(0:n,0:n,0:n)
colnames(grid) <- c("Pos","Zero","Neg")
grid <- grid[which(rowSums(grid)==10),]
nd <- grid$Pos - grid$Neg
p0 <- grid$Zero/n
inRR <- c()
for (i in 1:nrow(grid)){
  inRR[i] <- nd[i] > probs.obj$critvals[which(probs.obj$P0s == p0[i])]
}
#return(grid[inRR,])
grid[inRR,]

##########
# draft of possible output for the trinomial test
output <- list("statistic" = n_diff, "p-value" = add_p_value_variable)
return(output)

#######
# alternative version of mat2df
# works for matrices in which byrow = TRUE

mat2dfalt <- function(mat){
  return(data.frame(x=rep(1:nrow(mat),each=ncol(mat)),y=rep(1:ncol(mat), nrow(mat)),count=as.vector(t(mat))))
}

#######
# possible adaptation of within1diag (works on any square matrix)
withinxdiag <- function(mat, x, col){
  count <- 0
  count <- count + grid.tr(mat)
  for (i in 1:x) {
    count <- count + grid.tr(mat[1:(col-i), 1:(col-i)])
    count <- count + grid.tr(mat[(i+1):col, (i+1):col])
  }
  return(count)
}  

#######
# An alternate version of grid2nine, which allows more than just 5x5 grids.
# Transforms a dimxdim square grid into a 2dim-1 linear scale.
# The validity of non-5x5 grids is untested, and the transformation may not be accurate.
grid2likert <- function(gc, dim = 5, b = -0.5){
  i <- col2xy(gc, dim, dim)[2] # this is the X value (positive axis), so the column in our format
  j <- col2xy(gc, dim, dim)[1] # the Y value, the row in our format
  return((b+2)*i+b*j-1-(dim+1)*b)
}

# An alternate version of display.grid2nine
display.grid2likert <- function(dim=5, b=-0.5,match.lit = FALSE){
  mat <- matrix(NA,nrow=dim,ncol=dim)
  gcvals <- c(1:dim^2)
  for (gc in gcvals){
    i <- col2xy(gc, dim, dim)[2] # this is the X value (positive axis), so the column in our format
    j <- col2xy(gc, dim, dim)[1] # the Y value, the row in our format
    mat[j,i] <- grid2likert(gc, dim, b)
  }
  colnames(mat) <- paste("Agree",1:dim,sep="")
  rownames(mat) <- paste("Disagree",1:dim,sep="")
  if (!match.lit){
    mat <- as.table(mat)
    return(mat)
  }
  else{
    return(mat[dim:1,])
  }
}

#######
# An alternate version of make4cats intended for any square grid
# needs more work
make4catsalt <- function(grid,table=FALSE){
  dim <- nrow(grid)
  poscut <- ceiling(dim/2)
  negcut <- ceiling(dim/2)
  neg <- sum(grid[negcut:dim,1:(poscut-1)])
  pos <- sum(grid[1:(negcut-1),poscut:dim])
  ind <- sum(grid[1:(negcut-1),1:(poscut-1)])
  amb <- sum(grid[negcut:dim,poscut:dim])
  if (!table){
    return(list(pos=pos,neg=neg,ind=ind,amb=amb))
  }
  else if (table){
    tmp.tab <- as.table(matrix(c(ind,pos,
                                 neg,amb),nrow=2,byrow=TRUE))
    colnames(tmp.tab) <- c("LowPos","HighPos")
    rownames(tmp.tab) <- c("LowNeg","HighNeg")
    return(tmp.tab)
  }
}