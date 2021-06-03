###Example Data
row1 <- c(30,15,35,12,35,8,21,8,29,17)
row2 <- c(23,13,31,15,35,8,18,7,22,13)
frame <- cbind(row1,row2)
frame <- data.frame(frame)

source("R/TrinomialTest.R")
source("R/Trinomial-Internal.R")


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

a <- rep(0,10)
p4nd2 <- c(0,0,0,0,1,1,1,1,-1,-1)
p4nd4 <- c(0,0,0,0,1,1,1,1,1,-1)
p4nd6 <- c(0,0,0,0,1,1,1,1,1,1)

trinomial.test(p4nd2,a,alternative="greater")
trinomial.test(p4nd4,a,alternative="greater")
trinomial.test(p4nd6,a,alternative="greater")

p3nd3 <- c(0,0,0,1,1,1,1,1,-1,-1)
p3nd5 <- c(0,0,0,1,1,1,1,1,1,-1)
p3nd7 <- c(0,0,0,1,1,1,1,1,1,1)

trinomial.test(p3nd3,a,alternative="greater")
trinomial.test(p3nd5,a,alternative="greater")
trinomial.test(p3nd7,a,alternative="greater")


trinomial.test(p3nd3,a,alternative="less")
trinomial.test(p3nd5,a,alternative="less")
trinomial.test(p3nd7,a,alternative="less")

trinomial.test(a,p3nd3,alternative="less")
trinomial.test(a,p3nd5,alternative="less")
trinomial.test(a,p3nd7,alternative="less")

trinomial.test(a,p3nd3,alternative="greater")
trinomial.test(a,p3nd5,alternative="greater")
trinomial.test(a,p3nd7,alternative="greater")
