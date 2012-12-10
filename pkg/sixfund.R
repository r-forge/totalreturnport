syms <- c("VBMFX","VGTSX","VTSMX","SPY","IEF","GLD")
sixfund <-portfolio(syms,"2010-10-01")
nl <- length(syms)+1
#remove date column and create matrix
px <- as.matrix(sixfund[,2:nl])
#calculate minimum risk port folio
r <- portfolio.optim(px)
#roud off the weights
r$pw <- round(r$pw,digits=2)
names(r$pw) <-syms
#caculate annual risk
risk <-r$ps*255^.5
#caculate portfolio return
rtn <- r$pm^255
