#Prep mistletow data for models
##table(mist2020$SITE_ID,mist2020$TOTAL)
#sum mist across plots in two different ways:
#turn NAs into 0s
mist2020$LOW[which(is.na(mist2020$LOW))]<-0
mist2020$MID[which(is.na(mist2020$MID))]<-0
mist2020$TOP[which(is.na(mist2020$TOP))]<-0
mist2020$TOTAL[which(is.na(mist2020$TOTAL))]<-0
mist2007$LOW[which(is.na(mist2007$LOW))]<-0
mist2007$MID[which(is.na(mist2007$MID))]<-0
mist2007$TOP[which(is.na(mist2007$TOP))]<-0
mist2007$TOTAL[which(is.na(mist2007$TOTAL))]<-0

#create a new column for merging that removes B and replaces with V in all cases
mist2020$plot2<-gsub("VB","V",mist2020$SITE_ID)
mist2007$plot2<-gsub("VB","V",mist2007$SITE_ID)

mist2020sub<-mist2020[mist2020$plot2 %in% plots,]
mist2007sub<-mist2007[mist2007$plot2 %in% plots,]

mist2007sub$YR<-2007
mist2020sub$YR<-2020

mist<-rbind(mist2020sub,mist2007sub)
mist$plot_id<-mist$plot2
misttrt<-left_join(mist,treatdatsub,by=c("plot2"),copy=TRUE)
#Add column for simply presence/absence of mistletoe 
misttrt$PRES<-0
misttrt$PRES[misttrt$TOTAL>0]<-1

#my num obs
mist2020$subplot<-paste(mist2020$plot2,mist2020$PLOT,sep="-")
mist2007$subplot<-paste(mist2007$plot2,mist2007$PLOT,sep="-")

summist2020<-aggregate(mist2020$TOTAL,by=list(mist2020$subplot,mist2020$plot2,mist2020$PLOT),sum, na.rm=FALSE)
summist2007<-aggregate(mist2007$TOTAL,by=list(mist2007$subplot,mist2007$plot2, mist2007$PLOT),sum, na.rm=FALSE)
colnames(summist2020)<-colnames(summist2007)<-c("plot-subplot","SITE_ID","PLOT","MISTTOTAL")
summist2020$YR<-2020
summist2007$YR<-2007

#create a new column for merging that removes B and replaces with V in all cases
summist2020$plot2<-gsub("VB","V",summist2020$SITE_ID)
summist2007$plot2<-gsub("VB","V",summist2007$SITE_ID)

summist2020sub<-summist2020[summist2020$plot2 %in% plots,]
summist2007sub<-summist2007[summist2007$plot2 %in% plots,]

summist2020sub$PLOT<-as.factor(summist2020sub$PLOT)
summist2007sub$PLOT<-as.factor(summist2007sub$PLOT)
treatdatsub$PLOT<-as.factor(treatdatsub$PLOT)
mist2020trt<-left_join(summist2020sub,treatdatsub,by=c("plot2","PLOT"),copy=TRUE)
mist2007trt<-left_join(summist2007sub,treatdatsub,by=c("plot2", "PLOT"),copy=TRUE)
mistalltrt<-rbind(mist2020trt,mist2007trt)
mistalltrtwide <- spread(mistalltrt, YR, MISTTOTAL)
colnames(mistalltrtwide)[35:36]<-c("MIST2007","MIST2020")
mistalltrtwide$MISTDIF<-mistalltrtwide$MIST2020-mistalltrtwide$MIST2007
mistalltrtwide$plot2<-as.factor(mistalltrtwide$plot2)

misttrt$TREAT<-as.factor(misttrt$TREAT)
misttrt$YR<-as.factor(misttrt$YR)
misttrt$plot2<-as.factor(misttrt$plot2)
misttrt$AGE<-as.numeric(misttrt$AGE)


