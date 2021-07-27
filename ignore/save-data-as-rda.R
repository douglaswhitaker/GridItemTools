griddata_all_code <- read.csv(file = "ignore/all-data-code.csv")
griddata_all_text <- read.csv(file = "ignore/all-data-text.csv")
griddata_gridonly_code <- read.csv(file = "ignore/grid-data-code.csv")

save(griddata_all_code, griddata_all_text, griddata_gridonly_code, file = "data/grid-data.rda")
