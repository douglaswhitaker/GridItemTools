###Example Data
row1 <- c(30,15,35,12,35,8,21,8,29,17)
row2 <- c(23,13,31,15,35,8,18,7,22,13)
frame <- cbind(row1,row2)
frame <- data.frame(frame)

source("R/TrinomialTest.R")

###Example Data
trinomial.test(frame[,1],frame[,2],alternative="greater")

trinomial.test(frame[,2],frame[,1],alternative="less")

trinomial.test(frame[,1],frame[,2],alternative="less")
