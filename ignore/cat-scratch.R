
library(RColorBrewer) # need for changing the colours
library(pheatmap) #Package to creat heatmaps with value in center of cell

heatmap.colours <- colorRampPalette(
  brewer.pal(n = 7, name = "Purples"))(100)

dat.raw <- read.csv(file = "../GridItemAnalysis/data/real/results-survey945827-202004302207-codes.csv")
#print(paste("Using GridItemTools version ",packageVersion("GridItemTools"), sep=""))
# Identify which columns are grid items 
pilot.gii <-  GridItemTools::grid.item.info.ls(x = dat.raw, rows = 5, cols = 5)
# subset the data to include grid-only items
dat.grid <- dat.raw[,pilot.gii$cols] 
# compute summaries for each item and respondent
counts.item <- GridItemTools::grid.cell.counts(x = dat.grid, gridinfo = pilot.gii, type = "items", return.table = TRUE)
counts.respondent <- GridItemTools::grid.cell.counts(x = dat.grid, gridinfo = pilot.gii, type = "respondents", return.table = TRUE)
counts.respondent <- GridItemTools:::delete.empty.mat(counts.respondent)

print(paste("There are ", length(counts.item), " items to be analyzed.", sep = ""))
print(paste("There are ", length(counts.respondent), " respondents in the dataset.", sep = ""))

# Because these two lines both sum across all the matrices in their respective lists, the output will be the same! 
summary.counts.item <- GridItemTools:::sum.resp.mats(counts.item) # we will focus on this object
print(summary.counts.item)
# this object won't be used for now, but creating a similar object for each scale might be useful.
summary.counts.respondent <- GridItemTools:::sum.resp.mats(counts.respondent)

labels.disagree <- c("No disagreement \nat all",
                     "Slightly disagree",
                     "Moderately \ndisagree",
                     "Greatly disagree",
                     "Completely \ndisagree")
labels.agree <- c("No agreement \nat all",
                  "Slightly agree",
                  "Moderately \nagree",
                  "Greatly agree",
                  "Completely \nagree")

rownames(summary.counts.item) <- labels.disagree
colnames(summary.counts.item) <- labels.agree
pheatmap(summary.counts.item, display_numbers = TRUE, number_format = "%i", 
         fontsize = 15, cluster_rows = FALSE, cluster_cols = FALSE, 
         main = "Frequency of responses for \neach cell across all items", 
         fontsize_col = 11, fontsize_row = 11, color = heatmap.colours)

make4cats(summary.counts.item)
# correct overall sum, incorrect counts

# still wrong
classify_responses(summary.counts.item)
