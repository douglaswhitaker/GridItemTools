# This function processes the LimeSurvey column names. 
# For example, there are 25 variables of the format Page4Grid1.Y1_X1. for the one item Page4Grid1.
# This function will identify that Page4Grid1 is an item name. 
grid_item_names <- function(names_vec) {
  new_names <- c()
  for (i in 1:length(names_vec)) {
    new_names[i] <- paste(strsplit(names_vec[i], "[.]")[[1]][1], collapse = ".")
  }
  return(unique(new_names))
}

# This function returns the row and column indices that correspond to the selected response based on the LimeSurvey columns
# Consider changing the name - looks like col2xy already exists (Does col_to_xy exist?)
col_to_xy <- function(gc, mat_rows, mat_cols, diag_info_warning = FALSE, rc = FALSE) {
  if (diag_info_warning) {
    warning("Diagonal associated with a reciprocal relationship is bottom left to top right.")
    warning(paste("Focus on the diagonal from (", mat_rows, ",1) to (1,", mat_cols, "). (row,col)", sep = ""))
  }
  if (rc) {
    return(which(t(matrix(1:(mat_rows * mat_cols), 
                        nrow = mat_rows, 
                        byrow = TRUE)) == gc,
                 arr.ind = TRUE))
  } else {
    return(which(matrix(1:(mat_rows * mat_cols), 
                      nrow = mat_rows, 
                      byrow = TRUE) == gc,
               arr.ind = TRUE))
  }
}

# Essentially just creating an empty data.frame with the appropriate column names
make_grid9s <- function(grid_items_names) {
  grid9s <- t(data.frame(rep(NA, length(grid_items_names))))
  colnames(grid9s) <- paste("c9", grid_items_names, sep = "")
  grid9s <- grid9s[-1, ]
  return(grid9s)
} # This function seems to be designed for a 9 point scale; could it be adapted?


# Data Cleaning Functions 
# (may not be widely useful)

rename_cols <- function(grid_items) {
  new_names <- c()
  for (i in 1:length(grid_items)) {
    new_names[i] <- paste(strsplit(names(grid_items)[i], "[...]")[[1]][1:2], collapse = ".")
  }
  return(new_names)
}

display_grid_to_nine <- function(gcvals=c(1:25), rows=5, cols=5, b=-0.5, match_lit = FALSE) {
  mat <- matrix(NA, nrow = 5, ncol = 5)
  for (gc in gcvals) {
    i <- col_to_xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
    j <- col_to_xy(gc, rows, cols)[1] # the Y value, the row in our format
    mat[j, i] <- grid_to_nine(gc, rows, cols, b)
  }
  colnames(mat) <- paste("Agree", 1:5, sep = "")
  rownames(mat) <- paste("Disagree", 1:5, sep = "")
  if (!match_lit) {
    mat <- as.table(mat)
    return(mat)
  } else {
    return(mat[5:1, ])
  }
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale.

mat_to_df <- function(mat, col = 5) {
  return(data.frame(x = rep(1:col, each=col), y = rep(1:col, col), count = as.vector(mat)))
} # Could be adapted for non-5x5 grids.


