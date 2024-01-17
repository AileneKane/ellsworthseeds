
#Prep LWD for models
lwd2020$plot2<-gsub("VB","V",lwd2020$SITE_ID)
#lwd2007$plot2<-gsub("VB","V",lwd2007$SITE_ID)

#Add column for basal area in meters squared
lwd2020$DIAM<-as.numeric(lwd2020$DIAM)
#lwd2007$DIAM<-as.numeric(lwd2007$DIAM)

lwd2020$BA.LWD<-((lwd2020$DIAM/2)/100)*((lwd2020$DIAM/2)/100)*pi
#lwd2007$BA.LWD<-((lwd2007$DIAM/2)/100)*((lwd2007$DIAM/2)/100)*pi

#sum LWD across plots
sumlwd2020<-aggregate(lwd2020$BA.LWD,by=list(lwd2020$plot2),sum, na.rm=TRUE)
#sumlwd2007<-aggregate(lwd2007$BA.LWD,by=list(lwd2007$plot2),sum, na.rm=TRUE)
colnames(sumlwd2020)<-c("plot2","LWD_BA")
sumlwd2020$YR<-2020
#sumlwd2007$YR<-2007

sumlwd2020sub<-sumlwd2020[sumlwd2020$plot2 %in% plots,]
#sumlwd2007sub<-sumlwd2007[sumlwd2007$plot2 %in% plots,]
lwd2020trt<-left_join(sumlwd2020sub,treatdatsub,by=c("plot2"),copy=TRUE)
#lwd2007trt<-left_join(sumlwd2007sub,treatdatsub,by=c("plot2"),copy=TRUE)
#lwdalltrt<-rbind(lwd2020trt,lwd2007trt)
lwdalltrt<-lwd2020trt

lwdalltrt$YR<-as.factor(lwdalltrt$YR)

#add age in year of survey (2020 or 2007)
lwdalltrt$SURV.AGE[lwdalltrt$YR==2020]<-lwdalltrt$AGE[lwdalltrt$YR==2020]+1
#lwdalltrt$SURV.AGE[lwdalltrt$YR==2007]<-lwdalltrt$AGE[lwdalltrt$YR==2007]-12
