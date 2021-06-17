all.data.code <- read.csv(file = "data/all-data-code.csv")
all.data.text <- read.csv(file = "data/all-data-text.csv")
grid.data.code <- read.csv(file = "data/grid-data-code.csv")

save(all.data.code, all.data.text, grid.data.code, file = "data/grid-data.rda")
