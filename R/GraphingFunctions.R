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
                                                "negative_positive")) {
  
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