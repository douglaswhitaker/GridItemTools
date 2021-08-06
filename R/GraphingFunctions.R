#' Make a Heatmap for a Grid
#'
#' @param grid a matrix.
#' @param title the title of the plot.
#' @param labels a character string specifying which set of rownames and colnames
#'   are to be used.
#'
#' @return a heatmap showing counts in each grid cell.
#' @export
#'
#' @examples
make_heatmap <- function(grid, title, labels = c("agree_disagree", 
                                                "satisfied_dissatisfied", 
                                                "negative_positive",
                                                "other"),
                         pos_labels = NULL, neg_labels = NULL) {
  
  heatmap_colours <- colorRampPalette(
    brewer.pal(n = 7, name = "Purples"))(100)
  
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
  } else if (labels == "satisfied-dissatisfied") {
    rownames(grid) <- dissatisfaction
    colnames(grid) <- satisfaction
  } else if (labels == "negative_positive"){
    rownames(grid) <- negativity
    colnames(grid) <- positivity
  }

  pheatmap(grid, display_numbers = TRUE, number_format = "%i", 
           fontsize = 20, fontsize_col = 11, fontsize_row = 11, 
           cluster_rows = FALSE, cluster_cols = FALSE, main = title, 
            color = heatmap_colours)
}

#' Title
#'
#' @param grid_data data frame of grid-only LimeSurvey formatted data.
#' @param grid_info list. The output of \code{grid_item_info}.
#' @param chosen_item_1 number. The first item of interest.
#' @param chosen_item_2 number. The second item of interest.
#' @param labels a character string specifying which set of rownames and colnames
#'   are to be used.
#'
#' @return a path diagram showing the shift in each respondent's answer from one
#'   item to the other.
#' @export
#'
#' @examples
make_path_diagram <- function(grid_data, grid_info, chosen_item_1, chosen_item_2, 
                              labels = c("agree_disagree", 
                                         "satisfied_dissatisfied",
                                         "negative_positive")) {
  
  item1_resps <- grid_cell_counts(x = grid_data, gridinfo = grid_info, 
                                   type = "respondents", return_table = TRUE, chosen_items = chosen_item_1)
  item1_name <- grid_info$names[chosen_item_1]
  
  item2_resps <- grid_cell_counts(x = grid_data, gridinfo = grid_info, 
                                   type = "respondents", return_table = TRUE, chosen_items = chosen_item_2)
  item2_name <- grid_info$names[chosen_item_2]
  
  paths <- data.frame(x0 = double(), y0 = double(), x1 = double(), y1 = double(),
                      ID = character())
  
  for (i in 1:length(item1_resps)){
    if (sum(item1_resps[[i]] > 0)){
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
    y_label <- "Disagreement"
    x_label <- "Agreement"
  } else if (labels == "satisfied-dissatisfied") {
    row_labels <- dissatisfaction
    col_labels <- satisfaction
    y_label <- "Dissatisfied Response"
    x_label <- "Satisfied Response"
  } else if (labels == "negative_positive"){
    row_labels <- negativity
    col_labels <- positivity
    y_label <- "Negative Response"
    x_label <- "Positive Response"
  }
  
  ggplot(paths) +
    geom_link(aes(x = x0, y = y0, xend = x1, yend = y1, 
                  colour = ID, group = ID,
                  alpha = stat(index), size = stat(index)),
              n = 1000,
              show.legend = FALSE) +
    scale_y_reverse(name = y_label, breaks = 1:5, labels = row_labels) +
    scale_x_continuous(name = x_label, breaks = 1:5, labels = col_labels, position = "top") + 
    ggtitle(label = paste("Change in Response from ", item1_name, " to ", item2_name, sep = ""))
}
