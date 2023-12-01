#file that cleans the ellsworth lwd data from 2007 and 2020
lwd2020<-lwd2020[,1:8]
lwd2007<-cbind(lwd2007[,1:8],lwd2007[,27])

#Clean species names 
lwd2007$SPP[lwd2007$SPP=="NONE "]<-"NONE"
lwd2020$SPP[lwd2020$SPP==""]<-"NONE"
