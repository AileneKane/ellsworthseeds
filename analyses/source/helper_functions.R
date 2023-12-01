#helper functions used to analyze Ellsworth data

#calculate species richness, mean height, and mean cover in each plot and time period

get.sum<-function(yeardat){
  allnumsp<-c()
  allmnht<-c()
  allmncov<-c()
  alltotcov<-c()
  for (i in 1:length(plots)){
    plotdat<-yeardat[yeardat$SITE_ID==plots[i],]
    numsp<-length(unique(plotdat$SPP))
    mnht<-mean(plotdat$HT,na.rm=TRUE)
    mncov<-mean(plotdat$COVER,na.rm = TRUE)
    totcov<-sum(plotdat$COVER, na.rm=TRUE)
    allnumsp<-c(allnumsp,numsp)
    allmnht<-c(allmnht,mnht)
    allmncov<-c(allmncov,mncov)
    alltotcov<-c(alltotcov,totcov)
    
  }
  sumdat<-as.data.frame(cbind(plots,allnumsp,allmnht,allmncov,alltotcov))
  return(sumdat)
  
}
