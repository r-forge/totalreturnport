syms <- c("VBMFX","VGTSX","VTSMX","SPY","IEF")
p <-portfolio(syms)
nl <- length(syms)+1
px <- as.matrix(p[,2:nl])
r <- portfolio.optim(px)
r$pw <- round(r$pw,digits=2)
names(r$pw) <-syms
#next construct a portfolio with equal weights of each fund
ws <- c(.2,.2,.2,.2,.2)
ws.port <- apply(px*ws,1,sum)
#ws.port is a vector of daily returns for an equal weight portfolio

#calculate annual return for equal weight portfolio
print(mean(ws.port)^255)
#calulate risk for equal weight portfolio
sd(ws.port)*(255^.5)
#result is 12% standard dev for equal weight portfolio
#caluclate standard deviation for optim portfolio
r$ps* (255^.5) 