source("helper.R")
library(psych)
library(corrplot)

dat.raw.ecots <-  read.csv("data/real/results-survey945827-202004302207.csv",stringsAsFactors = FALSE)

# Items 1-10 are on LikertTypePage1
# Items 11-19 are on LikertTypePage3
#task.effort <- c(4,8,9,14,16)
task.effort <- c(14,16,8,4,9)
#emotional <- c(1,10,11,12,17,19)
emotional <- c(11,12,1,17,10,19)
#somas.cost.rc <- c(-2,3,5,6,-7,13,-15,-18) # high is high cost
somas.cost.rc <- c(-18,5,13,6,3,-2,-7,-15)
somas.cost <- abs(somas.cost.rc)

dat.likert <- dat.raw.ecots[,which(!sapply(names(dat.raw),grepl,pattern="_",simplify=TRUE))]
dat.likert <- dat.likert[,c(13:21,25:34)]
dat.likert <- fixLimeSurveyLikert(dat.likert,cols=1:ncol(dat.likert))


colnames(dat.likert)[task.effort] <- paste(rep("Task Effort",5),1:5)
colnames(dat.likert)[emotional] <- paste(rep("Emotional",6),1:6)
colnames(dat.likert)[somas.cost] <- paste(rep("SOMAS Cost",8),1:8)

rc.keys <- rep(1,ncol(dat.likert))
rc.keys[somas.cost.rc[which(somas.cost.rc < 0)]*(-1)] <- rc.keys[somas.cost.rc[which(somas.cost.rc < 0)]*(-1)] * -1




# c(paste(rep("Task Effort",5),1:5),paste(rep("Emotional",6),1:6),paste(rep("SOMAS Cost",8),1:8))[order(colnames(dat.likert.rc)[c(task.effort,emotional,somas.cost)],colnames(dat.likert.rc))]

# colnames(dat.likert.rc)[c(task.effort,emotional,somas.cost)] <- c(paste(rep("Task Effort",5),1:5),paste(rep("Emotional",6),1:6),paste(rep("SOMAS Cost",8),1:8))

dat.likert.rc <- reverse.code(keys=rc.keys,items=dat.likert,mini=1,maxi=9)




# Now we have raw data in dat.likert.rc
# No reverse coding or anything yet (do we need that?)

#pilot.keys <- make.keys(nvars = 19, keys.list = list(task.effort,emotional,somas.cost))
keys.list <- list(task.effort=task.effort,emotional=emotional,somas.cost=somas.cost)
scores <- scoreItems(keys.list, items = dat.likert.rc, impute = "none", min = 1, max = 9)
describe(scores$scores)
hist(scores$scores[,1],xlim=c(0,10),col="gray")
hist(scores$scores[,2],xlim=c(0,10),col="gray")
hist(scores$scores[,3],xlim=c(0,10),col="gray")

cor(scores$scores,use="pairwise.complete.obs",method="spearman")
corrplot(cor(scores$scores,use="pairwise.complete.obs",method="spearman"))


cor(dat.likert.rc[,c(task.effort,emotional,somas.cost)],use="pairwise.complete.obs",method="spearman")
corrplot(cor(dat.likert.rc[,c(task.effort,emotional,somas.cost)],use="pairwise.complete.obs",method="spearman"))


