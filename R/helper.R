# Data Cleaning Functions 
# (may not be widely useful)

rename.cols <- function(grid.items){
  new.names <- c()
  for (i in 1:length(grid.items)){
    new.names[i] <- paste(strsplit(names(grid.items)[i],"[...]")[[1]][1:2],collapse=".")
  }
  return(new.names)
}




# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2
# Convert grid value to 1 to 9 value
grid2nine <- function(gc, rows=5, cols=5, b = -0.5){
  i <- col2xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
  j <- col2xy(gc, rows, cols)[1] # the Y value, the row in our format
  return((b+2)*i+b*j-1-6*b)
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale. 

display.grid2nine <- function(gcvals=c(1:25), rows=5, cols=5, b=-0.5,match.lit = FALSE){
  mat <- matrix(NA,nrow=5,ncol=5)
  for (gc in gcvals){
    i <- col2xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
    j <- col2xy(gc, rows, cols)[1] # the Y value, the row in our format
    mat[j,i] <- grid2nine(gc, rows, cols, b)
  }
  colnames(mat) <- paste("Agree",1:5,sep="")
  rownames(mat) <- paste("Disagree",1:5,sep="")
  if (!match.lit){
    mat <- as.table(mat)
    return(mat)
  }
  else{
    return(mat[5:1,])
  }
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale.





# This function is intended to be applied to a list that is the output of 
# grid.cell.counts with type = "respondents"
# If remove = TRUE, respondent summary matrices with sum 0 (no responses) are removed from the list
# If remove = FALSE, such matrices are replaced with NA
delete.empty.mat <- function(resp.list, remove = TRUE){
  if (remove){
    tmp.list <- list()
    for (i in length(resp.list):1){
      if (sum(resp.list[[i]])==0){
        resp.list[[i]] <- NULL # removes the index and closes the hole; must loop backward through the list
      }
    }
    for (i in length(resp.list):1){
      tmp.list[[(length(resp.list)+1)-i]] <- resp.list[[i]]
    }
    resp.list <- tmp.list
  }
  else{
    for (i in 1:length(resp.list)){
      if (sum(resp.list[[i]])==0){
        resp.list[[i]] <- NA
      }
    }
  }
  return(resp.list)
}

mat2df <- function(mat, col=5){
  return(data.frame(x=rep(1:col,each=col),y=rep(1:col, col),count=as.vector(mat)))
} # Could be adapted for non-5x5 grids.

# This function accepts a list (the output from grid.cell.counts) and sums across the list.
# It returns a single matrix that is the sum of the individual matrices.
# Note that it uses the first matrix in the list to determine the dimensions:
# if this matrix is missing (an NA), there will be an error.
# It is best to use this after delete.empty.mat.
sum.resp.mats <- function(mat.list,items=NULL){
  tmp <- matrix(0,nrow=dim(mat.list[[1]])[1],ncol=dim(mat.list[[1]])[2])
  if (is.null(items)){
    items <- 1:length(mat.list)
  }
  for (i in items){
    tmp <- tmp + mat.list[[i]]
  }
  return(tmp)
}

# to do: give option to return as table for easy use of chi-sq
# and/or build testing into this function
# Note that this is a crude classifier
# See Figure A2 (p. 47; pdf p. 19)
#     Negative gets a triangular region of 6 cells
#     Positive gets a triangular region of 6 cells
#     Indifferent is half of the middle region
#     Ambivalent is half of the middle region and includes (3,3)
make4cats <- function(grid,poscut=3,negcut=3,table=FALSE){
  neg <- sum(grid[negcut:5,1:(poscut-1)])
  pos <- sum(grid[1:(negcut-1),poscut:5])
  ind <- sum(grid[1:(negcut-1),1:(poscut-1)])
  amb <- sum(grid[negcut:5,poscut:5])
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
} # This function can only be used on a 5-by-5 grid.


fixLimeSurveyLikert <- function(dat, cols, ...){
  for (i in cols){
    dat[,i] <- as.numeric(substr(dat[,i],1,1)) # as long as there are 9 or fewer scale points "1,1" should work
  }
  return(dat)
}
