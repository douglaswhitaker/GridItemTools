make_grid_labels <- function(grid, labels = c("agree_disagree", "satisfied_dissatisfied", "positive_negative", "other"), 
                             pos_labels = NULL, neg_labels = NULL, ggplot = FALSE) {

  disagreement <- c("No disagreement \nat all",
                    "Slightly disagree",
                    "Moderately \ndisagree",
                    "Greatly disagree",
                    "Completely \ndisagree")
  agreement <- c("No agreement \nat all",
                 "Slightly agree",
                 "Moderately \nagree",
                 "Greatly agree",
                 "Completely \nagree")
  dissatisfaction <- c("Not at all \ndissatisfied",
                       "Slightly \ndissatisfied",
                       "Moderately \ndissatisfied",
                       "Quite a bit \ndissatisfied",
                       "Extremely \ndissatisfied")
  satisfaction <- c("Not at all \nsatisfied",
                    "Slightly \nsatisfied",
                    "Moderately \nsatisfied",
                    "Quite a bit \nsatisfied",
                    "Extremely \nsatisfied")
  negativity <- c("Not at all \nnegative",
                  "Slightly negative",
                  "Moderately \nnegative",
                  "Quite a bit \nnegative",
                  "Extremely negative")
  positivity <- c("Not at all \npositive",
                  "Slightly positive",
                  "Moderately \npositive",
                  "Quite a bit \npositive",
                  "Extremely positive")
  
  if (ggplot) {
    
    if (labels == "agree_disagree") {
      row_labels <- disagreement
      col_labels <- agreement
    } else if (labels == "satisfied_dissatisfied") {
      row_labels <- dissatisfaction
      col_labels <- satisfaction
    } else if (labels == "positive_negative") {
      row_labels <- negativity
      col_labels <- positivity
    } else if (labels == "other") {
      row_labels <- neg_labels
      col_labels <- pos_labels
    }
    
    return(list(row_labels, col_labels))
    
  } else {
    
    if (labels == "agree_disagree") {
      rownames(grid) <- disagreement
      colnames(grid) <- agreement
    } else if (labels == "satisfied_dissatisfied") {
      rownames(grid) <- dissatisfaction
      colnames(grid) <- satisfaction
    } else if (labels == "positive_negative") {
      rownames(grid) <- negativity
      colnames(grid) <- positivity
    } else if (labels == "other") {
      rownames(grid) <- neg_labels
      colnames(grid) <- pos_labels
    }
    
    return(grid)
  }
  
}

#' Make a Heatmap for a Grid
#' 
#' The function makes a heatmap showing cell counts for a grid. By default, 
#' darker colours indicate larger counts, while lighter colours indicate smaller counts.
#'
#' @param grid a matrix.
#' @param labels a character string specifying which set of row names and col names
#'   are to be used.
#' @param title the title of the plot.
#' @param pos_labels character. Custom column names.
#' @param neg_labels character. Custom row names.
#' @param colours a character string indicating what colour the heatmap will be.
#'   Choosing \code{"custom"} allows for setting the \code{custom_palette} variable
#'   to a palette other than \code{"purple"}, \code{"green"}, or \code{"red"}.
#' @param custom_palette a colour palette, the output of \code{colorRampPalette}.
#' @param breaks A sequence of numbers that covers the range of values in the grid.
#'   If the value is \code{NA}, the breaks are calculated automatically.
#' @param show_counts logical. If \code{TRUE}, counts are shown for each cell.
#' @param fontsize base fontsize for the heatmap.
#' @param fontsize_names fontsize for row and column names.
#'
#' @return a heatmap showing counts in each grid cell.
#' @export
#'
#' @examples
make_heatmap <- function(grid, labels = c("agree_disagree", "satisfied_dissatisfied",
                                          "positive_negative", "other"),
                         title = "Heatmap for Grid Item", pos_labels = NULL, 
                         neg_labels = NULL, colours = c("purple", "green", "red", "custom"),
                         custom_palette, show_counts = TRUE, breaks = NA, 
                         fontsize = 20, fontsize_names = 11) {

  if (colours == "purple") {
    heatmap_colours <- grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(n = 7, name = "Purples"))(100)
  } else if (colours == "green") { 
    heatmap_colours <- c("white", grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(n = 7, name = "Greens"))(32)[2:32])
  } else if (colours == "red") {
    heatmap_colours <- grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(n = 7, name = "Reds"))(100)
  } else if (colours == "custom"){
    heatmap_colours <- custom_palette
  }

  
  grid <- make_grid_labels(grid, labels, pos_labels, neg_labels)

  pheatmap::pheatmap(grid, display_numbers = show_counts, number_format = "%i", 
           fontsize = fontsize, fontsize_col = fontsize_names, fontsize_row = fontsize_names, 
           cluster_rows = FALSE, cluster_cols = FALSE, main = title, 
            color = heatmap_colours, breaks = breaks)
}

make_axis_names <- function(labels = c("agree_disagree", "satisfied_dissatisfied",
                                       "positive_negative", "other"),
                            x_axis_label = NULL, y_axis_label = NULL) {
  if (labels == "agree_disagree") {
    y_name <- "Disagreement"
    x_name <- "Agreement"
  } else if (labels == "satisfied_dissatisfied") {
    y_name <- "Dissatisfied Response"
    x_name <- "Satisfied Response"
  } else if (labels == "positive_negative") {
    y_name <- "Negative Response"
    x_name <- "Positive Response"
  } else if (labels == "other") {
    y_name <- y_axis_label
    x_name <- x_axis_label
  }
  names <- c(y_name, x_name)
  return(names)
}


#' Make a Path Diagram for Two Grids
#' 
#' The function \code{make_path_diagram} constructs a diagram showing the changes
#' in response from one grid-type item to another for all respondents. 
#' The function can be used in two ways: either with LimeSurvey formatted data, 
#' or with two lists of grids.
#'
#' @param grid_data data frame of grid-only LimeSurvey formatted data.
#' @param grid_info list. The output of \code{grid_item_info}.
#' @param chosen_item_1 a number, OR, if \code{limesurvey = FALSE}, a list of matrices. The first item of interest.
#' @param chosen_item_2 a number, OR, if \code{limesurvey = FALSE}, a list of matrices. The second item of interest.
#' @param labels a character string specifying which set of row names and col names
#'   are to be used.
#' @param pos_labels character. Custom column names.
#' @param neg_labels character. Custom row names.
#' @param x_axis_label a custom label for the x-axis.
#' @param y_axis_label a custom label for the y-axis.
#' @param item_1_name the name of the first chosen item.
#' @param item_2_name the name of the second chosen item.
#' @param limesurvey logical. If \code{TRUE}, \code{grid_data} and \code{grid_info} must be inputted, 
#' and \code{chosen_item_1} and \code{chosen_item_2} should be integers. If \code{FALSE}, 
#' input \code{chosen_item_1} and \code{chosen_item_2} as lists of matrices, and do not input
#' \code{grid_data} and \code{grid_info}.
#'
#' @return a path diagram showing the shift in each respondent's answer from one
#'   item to the other.
#' @export
#' @importFrom rlang .data
#'
#' @examples
make_path_diagram <- function(grid_data, grid_info, chosen_item_1, chosen_item_2, 
                              labels = c("agree_disagree", 
                                         "satisfied_dissatisfied",
                                         "positive_negative", "other"),
                              pos_labels = NULL, neg_labels = NULL, 
                              x_axis_label = NULL, y_axis_label = NULL, 
                              item_1_name = "Item 1", item_2_name = "Item 2", limesurvey = TRUE) {
  if (limesurvey) {
    item1_resps <- grid_cell_counts(x = grid_data, gridinfo = grid_info, 
                                    type = "respondents", return_table = TRUE, chosen_items = chosen_item_1)
    item1_name <- grid_info$names[chosen_item_1]
    
    item2_resps <- grid_cell_counts(x = grid_data, gridinfo = grid_info, 
                                    type = "respondents", return_table = TRUE, chosen_items = chosen_item_2)
    item2_name <- grid_info$names[chosen_item_2]
  } else {
    item1_resps <- chosen_item_1
    item1_name <- item_1_name
    item2_resps <- chosen_item_2
    item2_name <- item_2_name
  }
  
  paths <- data.frame(x0 = double(), y0 = double(), x1 = double(), y1 = double(),
                      ID = character())
  
  for (i in 1:length(item1_resps)) {
    if (sum(item1_resps[[i]] > 0)) {
      item1x <- which(item1_resps[[i]] != 0, arr.ind = TRUE)[2]
      item1y <- which(item1_resps[[i]] != 0, arr.ind = TRUE)[1]
      item2x <- which(item2_resps[[i]] != 0, arr.ind = TRUE)[2]
      item2y <- which(item2_resps[[i]] != 0, arr.ind = TRUE)[1]
      
      tmp_paths <- data.frame(x0 = item1x, y0 = item1y, x1 = item2x, y1 = item2y,
                              ID = as.character(i))
      paths <- rbind(paths, tmp_paths)
    }
  }
  
  row_labels <- make_grid_labels(labels = labels, pos_labels = pos_labels, neg_labels = neg_labels, ggplot = TRUE)[[1]]
  col_labels <- make_grid_labels(labels = labels, pos_labels = pos_labels, neg_labels = neg_labels, ggplot = TRUE)[[2]]
  y_name <- make_axis_names(labels, x_axis_label, y_axis_label)[1]
  x_name <- make_axis_names(labels, x_axis_label, y_axis_label)[2]
  
  ggplot2::ggplot(paths) +
    ggforce::geom_link(ggplot2::aes(x = rlang::.data$x0, y = rlang::.data$xy0, xend = rlang::.data$xx1, yend = rlang::.data$xy1,
                  colour = rlang::.data$xID, group = rlang::.data$xID,
                  alpha = ggplot2::after_stat(rlang::.data$xindex), size = ggplot2::after_stat(rlang::.data$xindex)),
                  n = 1000,
                  show.legend = FALSE) +
    ggplot2::scale_y_reverse(name = y_name, breaks = 1:5, labels = row_labels) +
    ggplot2::scale_x_continuous(name = x_name, breaks = 1:5, labels = col_labels, position = "top") + 
    ggplot2::ggtitle(label = paste("Change in Response from ", item1_name, " to ", item2_name, sep = ""))
}
