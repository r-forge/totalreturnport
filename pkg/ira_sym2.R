library('fImport')
library('tseries')
#syms <- c("VTSMX","VGTSX","VBMFX")  #vangurard index fund
#wt <-c(.42,.18,.4)
#names(wt) <- syms
#wtm <- as.matrix(wt,colnum=1)#use matrix multiplication to get mid ris portfolio

#my fidelity ira
syms <-c("IEF","SHY","IYR","SPY","IWD","IWN","EFA")
yahoo <- yahooSeries(syms,nDaysBack=3660)#get up to 10 years of data
n <- length(syms)
yahoo.Dates <- rownames(yahoo)
rts <- matrix(yahoo,ncol=n)
ix <- seq(1:n)*6 #vector to extract .Adj.Close
yahoo <- yahoo[,ix]
yn <- length(yahoo[,1])
names(yahoo) <- syms

write.csv(yahoo,"yahoo.csv")#save to file
rts <- matrix(yahoo,ncol=n)

yahoo.returns <- rts[-1,]/rts[-yn,]
ndays <- length(yahoo.returns[,1])
colnames(yahoo.returns)<-syms
#medrisk <- yahoo.returns %*% wtm  #generate medium risk porotfolio daily returns
#medrisk.artn <- prod(medrisk[,1])^(255/ndays)
#medrisk.artn
#medrisk.sd <- sd(medrisk[,1])*(255^.5)
#medrisk.sd #risk

yahoo.prod <- apply(yahoo.returns,2,prod)
avrtn <- yahoo.prod^(1/ndays)#day return rate geo average
names(avrtn)<- syms
avrtnYear <- yahoo.prod^(255/ndays) #annual  return rate
avrtnYear
names(avrtnYear)<-syms
rmax <- max(avrtn)
rmin <- min(avrtn)
sq <- seq(rmin+.00001,rmax,length=20)

opt <- portfolio.optim(yahoo.returns) 
opt.sd <-opt$ps*(255^.5)#annual risk of optimum portfilio
opt.sd

opt.pm <- opt$pm^255
opt.pm

hrtn <- NULL
hsd  <- NULL
opt.df <-NULL
opt.df <- data.frame(opt$pw)

for(i in sq)
{
  opt <- portfolio.optim(yahoo.returns,i)
  hrtn <- c(hrtn,opt$pm)
  hsd <- c(hsd,opt$ps)
  opt.df <-cbind(opt.df,opt$pw)
}
opt.df <- round(opt.df,digits=2)
hrtn <- hrtn^255
hsd <- hsd*255^.5
opt.df <-round(rbind(opt.df,hsd,hrtn),digits=2)
rownames(opt.df)<-c(syms,"risk","rtn")
 plot(hsd,hrtn,type="l",main ="efficent horizon ",xlab ="risk",ylab="return")