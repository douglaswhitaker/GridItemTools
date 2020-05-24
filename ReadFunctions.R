dat.raw <-  read.csv("data/real/results-survey945827-202004302207-codes.csv",
                     stringsAsFactors = FALSE)

FixNonGridCode <- function(dat, cols, rows, ...){
  for (i in cols){
    for (j in rows){
      if (dat[j,i]=='D4'){
        dat[j,i] <- 1
      }
      if (dat[j,i]=='D3'){
        dat[j,i] <- 2
      }
      if (dat[j,i]=='D2'){
        dat[j,i] <- 3
      }
      if (dat[j,i]=='D1'){
        dat[j,i] <- 4
      }
      if (dat[j,i]=='A1'){
        dat[j,i] <- 6
      }
      if (dat[j,i]=='A2'){
        dat[j,i] <- 7
      }
      if (dat[j,i]=='A3'){
        dat[j,i] <- 8
      }
      if (dat[j,i]=='A4'){
        dat[j,i] <- 9
      }
      if (dat[j,i]=='N0'){
        dat[j,i] <- 5
      }
    }
  }
  return(dat)
}

read.lime.nongrid <- function(dat1){
  dat1 <- dat1[,which(!sapply(names(dat1),
                              grepl,pattern="_",
                              simplify=TRUE))]
  dat1 <- dat1[,c(13:21,25:34)]
  dat1 <- FixNonGridCode(dat1,
                         cols=1:ncol(dat1),
                         rows=1:nrow(dat1))
  return (dat1)
}

dat.raw <- read.lime.nongrid(dat.raw)
