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

trinomial.test(frame[,2],frame[,1],alternative="two.sided")

trinomial.test(frame[,1],frame[,2],alternative="two.sided")


trinomial.test(col1=c(1,2,3,4,5),
               col2=c(1,2,3,4,5),
               alternative = "two.sided")

library(BSDA)
SIGN.test(x=row1,y=row2)


