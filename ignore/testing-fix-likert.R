source("R/GridProcessing.R")

dat_raw_tmp <- read.csv(file = "../GridItemAnalysis/data/real/results-survey768545-study-A-codes.csv")
dat_likertplus <- dat_raw_tmp[,c(which(grepl(x = names(dat_raw_tmp), pattern = ".SQ")),1)] # include the ID column last
scale_tool <- 1:6
scale_util <- 7:17
scale_bigf <- c(18:21,23:28) # item 22 is validation


dat_likertplus <- fix_limesurvey_likert(dat = dat_likertplus,
                                        mapping = "bA7",
                                        cols = scale_util)
dat_likertplus <- fix_limesurvey_likert(dat = dat_likertplus,
                                        mapping = "bA5",
                                        cols = scale_tool)
dat_likertplus <- fix_limesurvey_likert(dat = dat_likertplus,
                                        mapping = "bA5",
                                        cols = scale_bigf)
