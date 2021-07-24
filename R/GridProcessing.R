

# This generates a list of matrices where each cell is the count of the number of responses
#' Title
#'
#' @param x data frame of grid-only LimeSurvey formatted data.
#' @param gridinfo list. The output of \code{grid_item_info}.
#' @param type character. Indicates whether to create cell counts for \code{"items"} or \code{"respondents"}.
#' @param reverse_code a value of 1 indicates that the grid items are reverse coded, and will correct for it.
#' @param return_table logical. If \code{TRUE} a list of tables rather than matrices is returned.
#' @param chosen_items integer vector indicating which grid items to include.
#'
#' @return
#' @export
#'
#' @examples
grid_cell_counts <- function(x, gridinfo, type = "items", reverse_code = NULL,
                             return_table = FALSE, chosen_items = NULL) {
  grid_resp_list <- list()
  
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  if (type == "items") {
    if (is.null(chosen_items)) {
      chosen_items <- 1:length(gridinfo$names)
    }
    for (i in chosen_items) {      
      grid_resp_list[[gridinfo$names[i]]] <- matrix(0, nrow = rows, ncol = cols)
      
      for (current_row in 1:nrow(x)) {
        
        grid_column <- which(!is.na(x[current_row, ((i - 1) * (rows * cols) + 1):(i * (rows * cols))])) # should only be one value
        xy_tmp <- col_to_xy(grid_column, rows, cols)
        if (!is.null(reverse_code)) {
          if (reverse_code == 1) {
            xy_tmp <- xy_tmp[2:1]
          }
        }
        grid_xy <- xy_tmp
        grid_resp_list[[gridinfo$names[i]]][grid_xy] <- grid_resp_list[[gridinfo$names[i]]][grid_xy] + 1
        
      }
      
      if (return_table) {
        grid_resp_list[[gridinfo$names[i]]] <- as.table(grid_resp_list[[gridinfo$names[i]]])
        rownames(grid_resp_list[[gridinfo$names[i]]]) <- c("LowNeg",
                                                           rep("", nrow(grid_resp_list[[gridinfo$names[i]]]) - 2),
                                                           "HighNeg")
        colnames(grid_resp_list[[gridinfo$names[i]]]) <- c("LowPos",
                                                           rep("", ncol(grid_resp_list[[gridinfo$names[i]]]) - 2),
                                                           "HighPos")
      }
    }
  } else if (type == "respondents") {
    for (current_row in 1:nrow(x)) {
      
      grid_resp_list[[current_row]] <- matrix(0, nrow = rows, ncol = cols)
      
      if (is.null(chosen_items)) {
        chosen_items <- 1:length(gridinfo$names)
      }
      for (i in chosen_items) {
        
        grid_column <- which(!is.na(x[current_row, ((i - 1) * (rows * cols) + 1):(i * (rows * cols))])) # should only be one value
        xy_tmp <- col_to_xy(grid_column, rows, cols)
        if (!is.null(reverse_code)) {
          if (reverse_code == 1) {
            xy_tmp <- xy_tmp[2:1]
          }
        }
        grid_xy <- xy_tmp        
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
#' Title
#'
#' @param x data frame of raw LimeSurvey formatted data.
#' @param rows,cols the dimensions of the grid items.
#'
#' @return
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
#' Title
#'
#' @param x data frame of grid-only LimeSurvey formatted data.
#' @param gridinfo list. The output of \code{grid_item_info}.
#' @param b a parameter between -1 and 0.
#' @param reverse_code a vector indicating which grids are reverse-coded. 
#'   a value of 0 means not reverse coded; a value of 1 means reverse coded.
#'
#' @return
#' @export
#'
#' @examples
create_grid_score <- function(x, gridinfo, b = -0.5, reverse_code = NULL) {
  grid9s <- make_grid9s(gridinfo$names)
  rows <- gridinfo$dim[1]
  cols <- gridinfo$dim[2]
  
  if (is.null(reverse_code)) {
    reverse_code <- rep(0, length(gridinfo$names))
  }
  
  for (current_row in 1:nrow(x)) {
    vals <- c()
    for (i in 1:length(gridinfo$names)) {
      grid_column <- which(!is.na(x[current_row, ((i - 1) * (rows * cols) + 1):(i * (rows * cols))])) # should only be one value, the indicator of which of 25 columns the response is in
      vals[i] <- grid_to_nine(grid_column, b = b, rc = reverse_code[i])
    }
    grid9s <- rbind(grid9s, current_row = vals)
  }
  rownames(grid9s) <- rownames(x)
  return(grid9s)
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale.

# In the future make number of off-diagonal diagonals selected (i.e. more than 1)
# To-do: Make this an internal function, and create a function that does more summary statistic processing than this
# (This requires sapply to work)
#' Title
#'
#' @param mat a square matrix.
#' @param col the number of columns of the matrix.
#'
#' @return
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
#' Title
#'
#' @param mat a square matrix.
#' @param col the number of columns of the matrix.
#' @param limesurvey logical. If \code{FALSE} the function calculates the sum of the diagonal from bottom-left to top-right.
#'
#' @return
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

#' Title
#'
#' @param mat a matrix.
#' @param rows,cols the dimensions of the matrix.
#' @param offdiag integer. the number of off-diagonals to treat as part of the diagonal.
#' @param return_table logical. If \code{TRUE} the function returns a table rather than a list.
#' @param limesurvey logical. If \code{FALSE} the function uses the sum of the diagonal from bottom-left to top-right.
#'
#' @return
#' @export
#'
#' @examples
grid_summary_tri <- function(mat, rows = 5, cols = 5, offdiag = 0, 
                             return_table = TRUE,
                             limesurvey = TRUE) {
  if (offdiag == 0) {
    if (!limesurvey) {
      mat <- mat[, ncol(mat):1]
    }
    tie <- sum(diag(mat))
    upper <- sum(mat[upper.tri(mat, diag = FALSE)])
    lower <- sum(mat[lower.tri(mat, diag = FALSE)])
  } else {
    stop("Not yet implemented.")
  }
  
  if (return_table) {
    tmp_tab <- as.table(c(upper, tie, lower))
    names(tmp_tab) <- c("upper", "diagonal", "lower")
    return(tmp_tab)
  } else { #if we don't return a table, we return a list
    return(list(upper = upper, diagonal = tie, lower = lower))
  }
} # Could be adapted for non-5x5 grids.


# This function implements the model proposed in Audrezet, Olsen, and Tudoran (2016)'s Appendix 2
# Convert grid value to 1 to 9 value
#' Title
#'
#' @param gc integer indicating a grid cell as formatted by LimeSurvey.
#' @param rows,cols the dimensions of the grid item.
#' @param b a parameter between -1 and 0.
#' @param rc logical. If \code{TRUE} the function will correct for a reverse-coded grid item.
#'
#' @return
#' @export
#'
#' @examples
grid_to_nine <- function(gc, rows = 5, cols = 5, b = -0.5, rc = FALSE) {
  if (rc) { # reverse-coded
    i <- col_to_xy(gc, rows, cols)[1] 
    j <- col_to_xy(gc, rows, cols)[2]
  } else { # not reverse-coded
    i <- col_to_xy(gc, rows, cols)[2] # this is the X value (positive axis), so the column in our format
    j <- col_to_xy(gc, rows, cols)[1] # the Y value, the row in our format
  }
  return((b + 2) * i + (b * j) - 1 - (6 * b))
} # This function can only be used to transform a 5-by-5 grid to a 9 point scale. 

# This function is intended to be applied to a list that is the output of 
# grid_cell_counts with type = "respondents"
# If remove = TRUE, respondent summary matrices with sum 0 (no responses) are removed from the list
# If remove = FALSE, such matrices are replaced with NA
#' Title
#' 
#' @param resp_list list of matrices.
#' @param remove logical. If \code{TRUE} empty matrices are removed from the list.
#' 
#' @return
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
#' Title
#' 
#' @param mat_list list of matrices.
#' @param items indicates which matrices to sum.
#'
#' @return
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

#' Title
#'
#' @param dat data frame of LimeSurvey formatted Likert-type data.
#' @param lsvals character vector of LimeSurvey's notation for Likert-type scale points.
#' @param livals integer vector of Likert-type scale points.
#'
#' @return
#' @export
#'
#' @examples
fix_limesurvey_likert <- function(dat, 
                                lsvals = c("D4", "D3", "D2", "D1",
                                           "N0", "A1", "A2", "A3", "A4"), 
                                livals = 1:9) {
  for (i in 1:length(livals)) {
    dat[dat == lsvals[i]] <- livals[i]
  }
  return(as.data.frame(sapply(dat, as.numeric)))
}

# This is hard-coded for 5x5 grid displayed in the manner this package uses
#' Title
#'
#' @param grid a matrix.
#'
#' @return
#' @export
#'
#' @examples
classify_responses <- function(grid) {
  grid <- t(grid) # based the counts on the one in the appendix, but it's really the transpose
  # These are based on Audrezet et al. (2016, p. 47) Figure A2
  indifferent_cells <- c(1, 2, 6, 7, 8, 12)
  ambivalent_cells <- c(13, 14, 18, 19, 24, 20, 25)
  negative_cells <- c(21, 22, 16, 17, 11, 23)
  positive_cells <- c(5, 4, 10, 3, 9, 15)
  
  indifferent_counts <- sum(as.vector(grid)[indifferent_cells])
  ambivalent_counts <- sum(as.vector(grid)[ambivalent_cells])
  negative_counts <- sum(as.vector(grid)[negative_cells])
  positive_counts <- sum(as.vector(grid)[positive_cells])
  
  return(list(indifferent_counts = indifferent_counts, 
              ambivalent_counts = ambivalent_counts,
              positive_counts = positive_counts,
              negative_counts = negative_counts))
}
