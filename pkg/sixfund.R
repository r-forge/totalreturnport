nl <- length(symbols)+1
px <- as.matrix(sixfund[,2:nl])
r <- portfolio.optim(px)
r$pw <- round(r$pw,digits=2)
names(r$pw) <-symbols