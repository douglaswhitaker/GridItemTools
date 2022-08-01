

# This generates a list of matrices where each cell is the count of the number of responses
#' Form Grid Cell Counts
#' 
#' Creates grids with aggregate cell counts (summed across items or respondents)
#' out of LimeSurvey formatted grid item data.
#'
#' @param x data frame of grid-only LimeSurvey formatted data.
#' @param gridinfo list. The output of \code{grid_item_info}.
#' @param type character. Indicates whether to create cell counts for \code{"items"} or \code{"respondents"}.
#' @param reverse_code integer or logical. Values of 0 (\code{FALSE}) indicate items that are not reverse-coded, 
#'   while values of 1 (\code{TRUE}) indicate items that are reverse coded.
#' @param return_table logical. If \code{TRUE} a list of tables rather than matrices is returned.
#' @param chosen_items integer vector indicating which grid items to include.
#'
#' @return A list of matrices, each of which is either the sum of all responses to a certain item (the default)
#'   or the sum of a certain respondent's answers for all items.
#' @export
#'
#' @examples
grid_cell_counts <- function(x, gridinfo, type = "items", reverse_code = NULL,
                             return_table = FALSE, chosen_items = NULL) {
  grid_resp_list <- list()
  
  if (is.null(reverse_code)){
    reverse_code <- rep(0, length(gridinfo$names))
  }
  
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  if (type == "items") {
    if (is.null(chosen_items)) {
      chosen_items <- 1:length(gridinfo$names)
    }
    for (i in 1:length(chosen_items)) {      
      grid_resp_list[[gridinfo$names[chosen_items[i]]]] <- matrix(0, nrow = rows, ncol = cols)
      
      for (current_row in 1:nrow(x)) {
        grid_column <- which(!is.na(x[current_row, 
                                      grepl(x = names(x), 
                                            fixed = TRUE, 
                                            pattern = paste(
                                              gridinfo$names[chosen_items[i]], 
                                              ".", 
                                              sep = ""))]))
        #grid_column <- which(!is.na(x[current_row, ((i - 1) * (rows * cols) + 1):(i * (rows * cols))])) # should only be one value
        grid_xy <- col_to_xy(grid_column, rows, cols, rc = reverse_code[chosen_items[i]])
        grid_resp_list[[gridinfo$names[chosen_items[i]]]][grid_xy] <- grid_resp_list[[gridinfo$names[chosen_items[i]]]][grid_xy] + 1
      }
      
      if (return_table) {
        grid_resp_list[[gridinfo$names[chosen_items[i]]]] <- as.table(grid_resp_list[[gridinfo$names[chosen_items[i]]]])
        rownames(grid_resp_list[[gridinfo$names[chosen_items[i]]]]) <- c("LowNeg",
                                                           rep("", nrow(grid_resp_list[[gridinfo$names[chosen_items[i]]]]) - 2),
                                                           "HighNeg")
        colnames(grid_resp_list[[gridinfo$names[chosen_items[i]]]]) <- c("LowPos",
                                                           rep("", ncol(grid_resp_list[[gridinfo$names[chosen_items[i]]]]) - 2),
                                                           "HighPos")
      }
    }
  } else if (type == "respondents") {
    for (current_row in 1:nrow(x)) {
      
      grid_resp_list[[current_row]] <- matrix(0, nrow = rows, ncol = cols)
      
      if (is.null(chosen_items)) {
        chosen_items <- 1:length(gridinfo$names)
      }
      for (i in 1:length(chosen_items)) {      
        grid_column <- which(!is.na(x[current_row, 
                                      grepl(x = names(x), 
                                            fixed = TRUE, 
                                            pattern = paste(
                                              gridinfo$names[chosen_items[i]], 
                                              ".", 
                                              sep = ""))]))
        #grid_column <- which(!is.na(x[current_row, ((i - 1) * (rows * cols) + 1):(i * (rows * cols))])) # should only be one value
        grid_xy <- col_to_xy(grid_column, rows, cols, rc = reverse_code[i])
        grid_resp_list[[current_row]][grid_xy] <- grid_resp_list[[current_row]][grid_xy] + 1
      }
    }
  }
  
  return(grid_resp_list)
}

# This function produces a list that contains information about the grid items
# Either raw data or grid-only data may be used
# This presupposes LimeSurvey formatted data
# In the future, develop a way to algorithmically determine rows and cols
#' Show Grid Item Information
#' 
#' Displays the LimeSurvey columns, item name, and dimensions of each grid item.
#'
#' @param x data frame of raw LimeSurvey formatted data.
#' @param rows,cols the dimensions of the grid items.
#'
#' @return A list containing information on which raw data columns contain grid items, 
#'   as well as the item names and dimensions of the grid items.
#' @export
#'
#' @examples
grid_item_info <- function(x, rows = 5, cols = 5) {
  gridinfo <- list()
  gridinfo$cols <- sapply(names(x), grepl, pattern = "_", simplify = TRUE) # the columns that contain grid items
  gridinfo$names <- grid_item_names(names(x)[which(gridinfo$cols)]) # vector of names
  gridinfo$dim <- c(rows, cols)
  
  return(gridinfo)
} # Could be adapted for non-5x5 grids.


# This function processes raw grid data into approximate scores from a Likert-type scale
#' Form Likert-type Grid Scores
#' 
#' For each item and respondent, creates scores equivalent to a Likert-type scale
#' out of LimeSurvey formatted grid item data. 
#'
#' @param x data frame of grid-only LimeSurvey formatted data.
#' @param gridinfo list. The output of \code{grid_item_info}.
#' @param b a parameter between -1 and 0, default is a value satisfying Audrezet's (2016) six constraints. 
#' @param uni.min,uni.max integers specifying the minimum and maximum value for the unidimensional (Likert-type) mapping. Default is a 9-point scale.
#' @param reverse_code a vector indicating which grids are reverse-coded. 
#' @param subset a vector indiciating which items to include. By default (NULL) items are used.
#' @param prefix a string which is the prefix used for column names for the converted score data frame. 
#'   a value of 0 means not reverse coded; a value of 1 means reverse coded.
#' @param classify_response Logical indicating if Positive (P), Negative (N), Indifferent (I), or Ambivalent (A) should be returned instead of a numeric value computed with S(i,j). Default is FALSE.
#'
#' @return A data frame in which the grid scores for each respondent have been 
#'   converted to values equivalent to those on Likert-type scale.
#' @export
#'
#' @examples
create_grid_score <- function(x, gridinfo, b = (uni.min - uni.max)/16, 
                              uni.min = 1, uni.max = 9, 
                              reverse_code = NULL, 
                              subset = NULL,
                              prefix = "conv_uni_",
                              classify_response = FALSE) {
  if (is.null(subset)){
    subset <- 1:length(gridinfo$names)
  }
  grid9s <- make_grid9s(gridinfo$names[subset], prefix = prefix)
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  if (is.null(reverse_code)) {
    reverse_code <- rep(0, length(gridinfo$names[subset]))
  }
  
  for (current_row in 1:nrow(x)) {
    vals <- c()
    for (i in 1:length(gridinfo$names[subset])){
      grid_column <- which(!is.na(x[current_row, grepl(x = names(x), fixed = TRUE, 
                                                       pattern = paste(gridinfo$names[subset][i], ".", sep = ""))]))
      vals[i] <- grid_to_uni(grid_column, b = b, rc = reverse_code[i],
                            uni.min = uni.min, uni.max = uni.max, classify_response = classify_response)
    }
    # for (i in 1:length(gridinfo$names[subset])) {
    #   grid_column <- which(!is.na(x[current_row, ((i - 1) * (rows * cols) + 1):(i * (rows * cols))])) # should only be one value, the indicator of which of 25 columns the response is in
    #   vals[i] <- grid_to_uni(grid_column, b = b, rc = reverse_code[i],
    #                          uni.min = uni.min, uni.max = uni.max)
    # }
    grid9s <- rbind(grid9s, current_row = vals)
  }
  rownames(grid9s) <- rownames(x)
  return(grid9s)
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale.

# In the future make number of off-diagonal diagonals selected (i.e. more than 1)
# To-do: Make this an internal function, and create a function that does more summary statistic processing than this
# (This requires sapply to work)
#' Sum of the Diagonal and Off-diagonals
#' 
#' Returns the sum of all cells on a grid's main diagonal (trace) and on its two off-diagonals.
#'
#' @param mat a square matrix.
#' @param col the number of columns of the matrix.
#'
#' @return The sum of the matrix's diagonal and two off-diagonals within one cell from the diagonal.
#' @export
#'
#' @examples
within_diag <- function(mat, col = 5) {
  count <- 0
  count <- count + grid_trace(mat)
  count <- count + grid_trace(mat[1:(col - 1), 1:(col - 1)])
  count <- count + grid_trace(mat[2:col, 2:col])
  return(count)
} # Could be adapted for non-5x5 grids.

# This function does the correct trace for grid items. 
# It is used by within_diag()
# Note that this sums along the other diagonal that tr() does not do, 
# i.e. this function sums the bottom-left to top-right diagonal.
# To-do: Make this internal as with above
#' Sum of the Diagonal
#' 
#' Returns the sum of all cells on either a grid's main diagonal (trace) or its secondary diagonal.
#'
#' @param mat a square matrix.
#' @param col the number of columns of the matrix.
#' @param limesurvey logical. If \code{FALSE} the function calculates the sum of the diagonal from bottom-left to top-right.
#'
#' @return The sum of the matrix's diagonal, either from top-left to bottom-right 
#'   (the default) or from bottom-left to top-right.
#' @export
#'
#' @examples
grid_trace <- function(mat, col = NULL, limesurvey = TRUE) {
  if (is.null(col)) col <- ncol(mat)
  if (limesurvey) {
    return(sum(diag(mat)))
  } else {
    val <- 0
    for (i in 1:col) {
      val <- val + mat[i, col + 1 - i]
    }
    return(val)
  }
}


# This will be a simple version at first. We'll account for within_diag later

#' Form Counts for Three Grid Areas
#' 
#' Returns the total cell counts for each of three grid regions: above the diagonal,
#' on the diagonal, and below the diagonal. (The diagonal may include the off-diagonals.)
#'
#' @param mat a matrix.
#' @param rows,cols the dimensions of the matrix.
#' @param offdiag integer. the number of off-diagonals to treat as part of the diagonal.
#' @param return_table logical. If \code{TRUE} the function returns a table rather than a list.
#' @param limesurvey logical. If \code{FALSE} the function uses the sum of the diagonal from bottom-left to top-right.
#'
#' @return A list (or table) containing the grid cell counts for each of three categories:
#'   above the diagonal, on the diagonal, and below the diagonal.
#' @export
#'
#' @examples
grid_summary_tri <- function(mat, rows = NULL, cols = NULL, offdiag = 0, 
                             return_table = TRUE,
                             limesurvey = TRUE) {
  if (is.null(rows)) {
    rows <- nrow(mat)
  }  
  if (is.null(cols)) {
    cols <- ncol(mat)
  }
  if (rows != cols) {
    stop("Only square matrices are supported now.")
  }
  
  if (!limesurvey) {
    mat <- mat[, ncol(mat):1]
  }
  
  if (offdiag == 0) {
    tie <- sum(diag(mat))
    upper <- sum(mat[upper.tri(mat, diag = FALSE)])
    lower <- sum(mat[lower.tri(mat, diag = FALSE)])
  } else if (offdiag == 1) {
    matL <- mat[2:rows, 1:(cols-1)]
    matU <- mat[1:(rows-1), 2:cols]
    tie <- sum(diag(mat)) + sum(diag(matL)) + sum(diag(matU))
    upper <- sum(matU[upper.tri(matU,diag=FALSE)])
    lower <- sum(matL[lower.tri(matL,diag=FALSE)]) 
  } else {
    stop("Not yet implemented.")
  }
  
  if (return_table) {
    tmp_tab <- as.table(c(upper, tie, lower))
    names(tmp_tab) <- c("upper", "diagonal", "lower")
    if (sum(tmp_tab) != sum(mat)) {
      stop("Calculation error.")
    }
    return(tmp_tab)
  } else { #if we don't return a table, we return a list
    return(list(upper = upper, diagonal = tie, lower = lower))
  }
} # Could be adapted for non-5x5 grids.


# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2 with modifications for different unidimensional scales.
# Convert grid value to unidimensional value
#' Convert Grid Value to Likert-type Value
#' 
#' Converts a grid cell's position to a score equivalent to a Likert-type scale point.
#'
#' @param gc integer indicating a grid cell's position as though the grid were a
#'   matrix filled by row with sequential integers.
#' @param rows,cols the dimensions of the grid item.
#' @param b a parameter between -1 and 0, default is a value satisfying Audrezet's (2016) six constraints. 
#' @param uni.min,uni.max integers specifying the minimum and maximum value for the unidimensional (Likert-type) mapping. Default is a 9-point scale.
#' @param rc logical. If \code{TRUE} the function will correct for a reverse-coded grid item.
#' @param classify_response Logical indicating if Positive (P), Negative (N), Indifferent (I), or Ambivalent (A) should be returned instead of a numeric value computed with S(i,j). Default is FALSE.
#' 
#' @return A number derived from a grid cell's position
#'   and transformed to be equivalent to a value on a 9 point Likert-type scale.
#' @export
#'
#' @examples
grid_to_uni <- function(gc, rows = 5, cols = 5, b = (uni.min - uni.max)/16, uni.min = 1, uni.max = 9, rc = FALSE, classify_response = FALSE) {
  if (rc) { # reverse-coded
    i <- col_to_xy(gc, rows, cols)[1] 
    j <- col_to_xy(gc, rows, cols)[2]
  } else { # not reverse-coded
    i <- col_to_xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
    j <- col_to_xy(gc, rows, cols)[1] # the Y value, the row in our format
  }
  if (classify_response){
    if (rows != 5 | cols != 5){
      stop("Only implemented for 5x5 grids.")
    }
    tmp_grid <- matrix(0, nrow = rows, ncol = cols)
    tmp_grid[i,j] <- 1
    tmp_grid <- t(tmp_grid)
    #print(tmp_grid)
    tmp_resp <- classify_responses(tmp_grid)

    #print(tmp_resp)
    # if (sum(sapply(classify_responses(tmp_resp), FUN = I)) != 1){
    #   stop("Error: different than 1 response when 1 response expected.")
    # }
    if (tmp_resp$indifferent_counts == 1){
      return("I")
    } else if (tmp_resp$ambivalent_counts == 1) {
      return("A")
    } else if (tmp_resp$positive_counts == 1) {
      return("P")
    } else if (tmp_resp$negative_counts == 1) {
      return("N")
    } 
  }
  else {
    return((i-1)*((uni.max - uni.min)/4) + (i + j - 6)*b + uni.min)
    #return((b + 2) * i + (b * j) - 1 - (6 * b)) # Audrezet's fixed 9-point version
  }

} 

# This function is intended to be applied to a list that is the output of 
# grid_cell_counts with type = "respondents"
# If remove = TRUE, respondent summary matrices with sum 0 (no responses) are removed from the list
# If remove = FALSE, such matrices are replaced with NA
#' Remove Empty Grids
#' 
#' Deletes empty grids from a list, or replaces them with \code{NA}.
#' 
#' @param resp_list list of matrices.
#' @param remove logical. If \code{TRUE} empty matrices are removed from the list.
#' 
#' @return A list of grids with empty grids removed or replaced by \code{NA}.
#' @export
#'
#' @examples
delete_empty_grid <- function(resp_list, remove = TRUE) {
  if (remove) {
    tmp_list <- list()
    for (i in length(resp_list):1) {
      if (sum(resp_list[[i]]) == 0) {
        resp_list[[i]] <- NULL # removes the index and closes the hole; must loop backward through the list
      }
    }
    for (i in length(resp_list):1) {
      tmp_list[[(length(resp_list) + 1) - i]] <- resp_list[[i]]
    }
    resp_list <- tmp_list
  } else {
    for (i in 1:length(resp_list)) {
      if (sum(resp_list[[i]]) == 0) {
        resp_list[[i]] <- NA
      }
    }
  }
  return(resp_list)
}

# This function accepts a list (the output from grid_cell_counts) and sums across the list.
# It returns a single matrix that is the sum of the individual matrices.
# Note that it uses the first matrix in the list to determine the dimensions:
# if this matrix is missing (an NA), there will be an error.
# It is best to use this after delete_empty_grid.
#' Sum of Grids
#' 
#' Sums across all (or a selection) of grids in a list.
#' 
#' @param mat_list list of matrices.
#' @param items indicates which matrices to sum.
#'
#' @return A matrix created by summing some or all of the inputted list of grids.
#' @export
#'
#' @examples
sum_resp_mats <- function(mat_list, items = NULL) {
  tmp <- matrix(0, nrow = dim(mat_list[[1]])[1], ncol = dim(mat_list[[1]])[2])
  if (is.null(items)) {
    items <- 1:length(mat_list)
  }
  for (i in items) {
    tmp <- tmp + mat_list[[i]]
  }
  return(tmp)
}

# Intended to eventually replace sum_resp_mats as it has a more accurate name.
#' Sum of Grids
#' 
#' Sums across all (or a selection) of grids in a list.
#'
#' @param mat_list list of matrices.
#' @param items indicates which matrices to sum.
#'
#' @return A matrix created by summing some or all of the inputted list of grids.
#' @export
#'
#' @examples
sum_grid_mat_list <- function(mat_list, items = NULL) {
  return(sum_resp_mats(mat_list = mat_list, items = items)) 
}

#' Format LimeSurvey Likert-type Data
#' 
#' Replaces LimeSurvey's formatting of Likert-type data with the standard numerical scale points.
#'
#' @param dat data frame of LimeSurvey formatted Likert-type data.
#' @param mapping character indicating which pre-generated mapping from LimeSurvey notation to numeric values should be used. 
#' @param lsvals character vector of LimeSurvey's notation for Likert-type scale points.
#' @param livals integer vector of Likert-type scale points.
#' @param cols integer vector indicating which columns/items the fix should be applied to. If default (NULL), all columns/items will be used.
#'
#' @return A data frame of numeric Likert-type values for each response.
#' @export
#'
#' @examples
fix_limesurvey_likert <- function(dat, 
                                  mapping = "DA9",
                                  lsvals = NULL, 
                                  livals = NULL,
                                  cols = NULL) {
  if (mapping == "DA9"){
    lsvals <- c("D4", "D3", "D2", "D1",
                "N0", "A1", "A2", "A3", "A4")
    livals <- 1:9
  } else if (mapping == "bA7"){
    lsvals <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7")
    livals <- 1:7
  } else if (mapping == "bA5"){
    lsvals <- c("A1", "A2", "A3", "A4", "A5")
    livals <- 1:5
  } else if (mapping == "other") {
    cat(paste("Using custom mapping: ", paste(lsvals, livals, sep = "=", collapse = ","), sep = ""))
  }
  
  if (is.null(cols)){
    cols <- 1:ncol(dat)
    other_cols <- NULL
  } else {
    other_cols <- which(!(1:ncol(dat) %in% cols))
  }
  dat_cols <- dat[,cols]
  for (i in 1:length(livals)) {
    dat_cols[dat_cols == lsvals[i]] <- livals[i]
    #dat[dat[,cols] == lsvals[i], cols] <- livals[i]
  }
  dat_cols <- as.data.frame(sapply(dat_cols, as.numeric))
  if (is.null(other_cols)){
    return(dat_cols)
  }
  else {
    full_dat <- cbind(dat[,other_cols], dat_cols)
    full_dat <- full_dat[,sort(c(other_cols, cols), index.return = TRUE)$ix]
    return(full_dat)
  }
}

# This is hard-coded for 5x5 grid displayed in the manner this package uses
#' Form Counts for Four Grid Areas
#' 
#' Returns the total cell counts for each of four grid regions corresponding to 
#' attitudes that can be measured using grid items.
#'
#' @param grid a matrix.
#' @param print_cell_class_mapping A logical indicating whether a matrix showing the mapping of response types to cells should be printed. If TRUE, then no computation occurs and any option passed to grid is ignored.
#'
#' @return A list containing the grid cell counts for each of four categories:
#'   indifferent, ambivalent, positive, and negative. 
#' @export
#'
#' @examples
classify_responses <- function(grid, print_cell_class_mapping = FALSE) {
  # These are based on Audrezet et al. (2016, p. 47) Figure A2
  indifferent_cells <- c(1, 2, 6, 7, 8, 12)
  ambivalent_cells <- c(13, 14, 18, 19, 24, 20, 25)
  negative_cells <- c(21, 22, 16, 17, 11, 23)
  positive_cells <- c(5, 4, 10, 3, 9, 15)
  
  if (print_cell_class_mapping){
    tmp <- matrix(NA, nrow = 5, ncol = 5)
    tmp[indifferent_cells] <- "I"
    tmp[ambivalent_cells] <- "A"
    tmp[negative_cells] <- "N"
    tmp[positive_cells] <- "P"
    return(t(tmp))
  }

  grid <- t(grid) # based the counts on the one in the appendix, but it's really the transpose
  
  indifferent_counts <- sum(as.vector(grid)[indifferent_cells])
  ambivalent_counts <- sum(as.vector(grid)[ambivalent_cells])
  negative_counts <- sum(as.vector(grid)[negative_cells])
  positive_counts <- sum(as.vector(grid)[positive_cells])
  
  return(list(indifferent_counts = indifferent_counts, 
              ambivalent_counts = ambivalent_counts,
              positive_counts = positive_counts,
              negative_counts = negative_counts))
}
