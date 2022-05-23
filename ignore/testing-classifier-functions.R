

dat_raw <- read.csv(file = "../GridItemAnalysis/data/real/results-survey768545-study-A-codes.csv")

drop_rows <- (dat_raw$ConstentYN1 != "YES" | dat_raw$ConsentYN2 != "YES" | dat_raw$Screening1Age < 18 | dat_raw$Screening2Mobile != "NoMD")
dat_raw <- dat_raw[!drop_rows,]

# Need to remove respondents who did not reach last page per REB application.
# We can check this easily based on the presence/absence of a time for the last page: !is.na(dat_raw$groupTime19811)
dat_raw <- dat_raw[!is.na(dat_raw$groupTime19811),]
# We could maybe salvage 4 observations if using the presence of a time on the penultimate page (19819) but need to understand the timing statistics better to do this.

print(paste("Using GridItemTools version ",packageVersion("GridItemTools"), sep=""))
# Identify which columns are grid items 
pilot_gii <- grid_item_info(x = dat_raw, rows = 5, cols = 5)
# subset the data to include grid-only items
dat_grid <- dat_raw[,pilot_gii$cols] 
# subset the data to extract Likert-only items
dat_likertplus <- dat_raw[,c(which(grepl(x = names(dat_raw), pattern = ".SQ")),1)] # include the ID column last

valid_grid <- !is.na(dat_grid$Page5ControlItem.Y5_X2.)
valid_likert <- dat_likertplus[,22] == "A4"
valid_resps <- which(!!(valid_likert * valid_grid))
dat_grid_vo <- dat_grid[valid_resps,]
dat_likertplus_vo <- dat_likertplus[valid_resps,]
dat_raw_vo <- dat_raw[valid_resps,]

scale_util_grid <- 18:28
rc_util_grid <- rep(0, length(scale_util_grid))
rc_util_grid[c(3,7,8)] <- 1

dat_grid_util_7_1 <- as.data.frame(create_grid_score(dat_grid_vo, 
                                                     gridinfo = pilot_gii, 
                                                     reverse_code = rc_util_grid,
                                                     subset = scale_util_grid,
                                                     uni.max = 7))
dat_grid_util_7_2 <- as.data.frame(create_grid_score(dat_grid_vo, 
                                                   gridinfo = pilot_gii, 
                                                   reverse_code = rc_util_grid,
                                                   subset = scale_util_grid,
                                                   uni.max = 7,
                                                   classify_response = TRUE))
cbind(dat_grid_util_7_1[,1],dat_grid_util_7_2[,1])