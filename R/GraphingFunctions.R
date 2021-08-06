make_grid_labels <- function(grid, labels, pos_labels = NULL, neg_labels = NULL) {
  
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
  dissatisfaction <- c("Not at all dissatisfied",
                       "Slightly dissatisfied",
                       "Moderately dissatisfied",
                       "Quite a bit dissatisfied",
                       "Extremely dissatisfied")
  satisfaction <- c("Not at all satisfied",
                    "Slightly satisfied",
                    "Moderately satisfied",
                    "Quite a bit satisfied",
                    "Extremely satisfied")
  negativity <- c("Not at all negative",
                  "Slightly negative",
                  "Moderately negative",
                  "Quite a bit negative",
                  "Extremely negative")
  positivity <- c("Not at all positive",
                  "Slightly positive",
                  "Moderately positive",
                  "Quite a bit positive",
                  "Extremely positive")
  
  if (labels == "agree_disagree") {
    rownames(grid) <- disagreement
    colnames(grid) <- agreement
  } else if (labels == "satisfied_dissatisfied") {
    rownames(grid) <- dissatisfaction
    colnames(grid) <- satisfaction
  } else if (labels == "negative_positive") {
    rownames(grid) <- negativity
    colnames(grid) <- positivity
  } else if (labels == "other") {
    rownames(grid) <- neg_labels
    colnames(grid) <- pos_labels
  }
}

#' Make a Heatmap for a Grid
#'
#' @param grid a matrix.
#' @param title the title of the plot.
#' @param labels a character string specifying which set of rownames and colnames
#'   are to be used.
#' @param pos_labels character. Custom column names.
#' @param neg_labels character. Custom row names.
#' @param show_counts logical. If \code{TRUE}, counts are shown for each cell.
#' @param colours A colour palette, the output of \code{colorRampPalette}.
#' @param fontsize base fontsize for the heatmap.
#' @param fontsize_names fontsize for row and column names.
#'
#' @return a heatmap showing counts in each grid cell.
#' @export
#'
#' @examples
make_heatmap <- function(grid, title = "Heatmap for Grid Item", 
                         labels = c("agree_disagree", "satisfied_dissatisfied",
                                    "negative_positive", "other"),
                         pos_labels = NULL, neg_labels = NULL, show_counts = TRUE, 
                         colours = NULL, fontsize = 20, fontsize_names = 11) {
  
  if (is.null(colours)) {
    heatmap_colours <- grDevices::colorRampPalette(
    RColorBrewer::brewer.pal(n = 7, name = "Purples"))(100)
  } else { 
    heatmap_colours <- colours
  }
  
  grid <- make_grid_labels(grid, labels, pos_labels, neg_labels)

  pheatmap::pheatmap(grid, display_numbers = show_counts, number_format = "%i", 
           fontsize = fontsize, fontsize_col = fontsize_names, fontsize_row = fontsize_names, 
           cluster_rows = FALSE, cluster_cols = FALSE, main = title, 
            color = heatmap_colours)
}


# A check brings up this note for this function.
# make_path_diagram: no visible binding for global variable 'x0'
# make_path_diagram: no visible binding for global variable 'y0'
# make_path_diagram: no visible binding for global variable 'x1'
# make_path_diagram: no visible binding for global variable 'y1'
# make_path_diagram: no visible binding for global variable 'ID'
# make_path_diagram: no visible binding for global variable 'index'
# Undefined global functions or variables:
#  ID index x0 x1 y0 y1

#' Make a Path Diagram for Two Grids
#'
#' @param grid_data data frame of grid-only LimeSurvey formatted data.
#' @param grid_info list. The output of \code{grid_item_info}.
#' @param chosen_item_1 number. The first item of interest.
#' @param chosen_item_2 number. The second item of interest.
#' @param labels a character string specifying which set of rownames and colnames
#'   are to be used.
#' @param pos_labels character. Custom column names.
#' @param neg_labels character. Custom row names.
#' @param custom_x_name Custom name for the x-axis.
#' @param custom_y_name Custom name for the y-axis.
#'
#' @return a path diagram showing the shift in each respondent's answer from one
#'   item to the other.
#' @export
#'
#' @examples
make_path_diagram <- function(grid_data, grid_info, chosen_item_1, chosen_item_2, 
                              labels = c("agree_disagree", 
                                         "satisfied_dissatisfied",
                                         "negative_positive", "other"),
                              pos_labels = NULL, neg_labels = NULL, 
                              custom_x_name = NULL, custom_y_name = NULL) {
  
  item1_resps <- grid_cell_counts(x = grid_data, gridinfo = grid_info, 
                                   type = "respondents", return_table = TRUE, chosen_items = chosen_item_1)
  item1_name <- grid_info$names[chosen_item_1]
  
  item2_resps <- grid_cell_counts(x = grid_data, gridinfo = grid_info, 
                                   type = "respondents", return_table = TRUE, chosen_items = chosen_item_2)
  item2_name <- grid_info$names[chosen_item_2]
  
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
  dissatisfaction <- c("Not at all dissatisfied",
                       "Slightly dissatisfied",
                       "Moderately dissatisfied",
                       "Quite a bit dissatisfied",
                       "Extremely dissatisfied")
  satisfaction <- c("Not at all satisfied",
                    "Slightly satisfied",
                    "Moderately satisfied",
                    "Quite a bit satisfied",
                    "Extremely satisfied")
  negativity <- c("Not at all negative",
                  "Slightly negative",
                  "Moderately negative",
                  "Quite a bit negative",
                  "Extremely negative")
  positivity <- c("Not at all positive",
                  "Slightly positive",
                  "Moderately positive",
                  "Quite a bit positive",
                  "Extremely positive")
  
  if (labels == "agree_disagree") {
    row_labels <- disagreement
    col_labels <- agreement
    y_name <- "Disagreement"
    x_name <- "Agreement"
  } else if (labels == "satisfied_dissatisfied") {
    row_labels <- dissatisfaction
    col_labels <- satisfaction
    y_name <- "Dissatisfied Response"
    x_name <- "Satisfied Response"
  } else if (labels == "negative_positive"){
    row_labels <- negativity
    col_labels <- positivity
    y_name <- "Negative Response"
    x_name <- "Positive Response"
  } else if (labels == "other") {
    row_labels <- neg_labels
    col_labels <- pos_labels
    y_name <- custom_y_name
    x_name <- custom_x_name
  }
  
  ggplot2::ggplot(paths) +
    ggforce::geom_link(ggplot2::aes(x = x0, y = y0, xend = x1, yend = y1,
                  colour = ID, group = ID,
                  alpha = ggplot2::stat(index), size = ggplot2::stat(index)),
              n = 1000,
              show.legend = FALSE) +
    ggplot2::scale_y_reverse(name = y_name, breaks = 1:5, labels = row_labels) +
    ggplot2::scale_x_continuous(name = x_name, breaks = 1:5, labels = col_labels, position = "top") + 
    ggplot2::ggtitle(label = paste("Change in Response from ", item1_name, " to ", item2_name, sep = ""))
}
