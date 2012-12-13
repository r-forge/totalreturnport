#to download dividens load the library quoantmod
#install.package(quantmod)
#call function getDividens with fund symbol
#to get historical price data use get.hist.quote
library('quantmod')
library('tseries')
fundReturn <-function( fund ="IEF",first="2011-01-1")
{
#get dividens for each trading day
ief.dividends <- getDividends(fund,from=first,to=Sys.Date()) 
ief.dividends <-data.frame(ief.dividends)
ief.dividends.date <- as.Date(rownames(ief.dividends))  
ief.div <- as.numeric(ief.dividends$ief.dividends)
ief.dividends <- data.frame(date=ief.dividends.date,div=ief.div)


#requires library tseries
library('tseries')
library('quantmod')
ief.price.days <-get.hist.quote(fund,start=first,quote="AdjClose")

ief.price.days <-data.frame(ief.price.days)
ief.price.days.date <- as.Date(rownames(ief.price.days))
ief.price.days.close <- as.numeric(ief.price.days$AdjClose)
ief.price.days.df <- data.frame(date=ief.price.days.date,price=ief.price.days.close)
#we now have a data frame with two columns. Date and closing price
#combine price and dividends into a single data frame
#all.x =TRUE set left join includes dates with no dividends.those dates show na
ief <- merge(ief.price.days.df,ief.dividends,all.x=TRUE)
ief$div[is.na(ief$div)] <- 0  #change NA to zero
n<- length(ief$price)
#calculate daily total return. combin dividende and change in price to determine total return
return <- (ief$div[-1]+ ief$price[-1])/ief$price[-n]
#add retun column 
ief.return <-cbind(ief[-1,],return)#eliminate 1st row

return(ief.return)
#annualGeometricReturn <-(year10.return)^.1 

}
	