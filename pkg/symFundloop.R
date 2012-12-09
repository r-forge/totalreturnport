portfolio <- function( syms=c("VBMFX","VGTSX","VTSMX"),start_date ="2002-10-01")
{
d <- data.frame()
tmp <- data.frame() 
nl <- length(syms)
d <- fundReturn(syms[1],start_date)
d <- d[,c(1,4)]
names(d) <-c("date",syms[1])

for (i in 2:nl)
{
 tmp <- fundReturn(syms[i],start_date) 
 tmp <- tmp[,c(1,4)]
 names(tmp)<- c("date",syms[i])
d <-merge(d,tmp)
  
} 
  
return(d)
}