

sd2007sub<-sd2007[sd2007$plot2 %in% plots,]
sp2007sub<-sp2007[sp2007$plot2 %in% plots,]
#Do some basic looking at species found
#sort(unique(sp2007sub$SPP))
#sort(unique(sp2020$SPP))
#sort(unique(sd2007sub$SPP))
#sort(unique(sd2020$SPP))
#In 2007: 7 sapling spp ("ALRU2" "FRPU7" "MASY2" "PISI"  "PSME"  "THPL"  "TSHE"), 
#         6 seedling spp ("ALRU2" "FRPU7" "MASY2"  "PISI"  "THPL"  "TSHE")
#         Malus removed for simplicity        
#In 2020: 5 sapling (missing MASY2, PSME), 

#Combine data from both time periods in two different types of dataframes to analyze:
#First, in long format, with a column for year and rows for 2007 and 2020 kept separate
sp2007sub$YR<-2007
sp2020$YR<-2020
sd2007sub$YR<-2007
sd2020$YR<-2020
sp2007suba<-sp2007sub
sd2007suba<-sd2007sub
colnames(sp2020)[6]<-"COUNT"
colnames(sd2020)[6]<-"COUNT"

allspl<-rbind(sp2007suba,sp2020)
allsdl<-rbind(sd2007suba,sd2020)

#create new version of data that is just tshe dat
tshesdl<-allsdl
tshespl<-allspl
#Now Replace subplots that are blank or say "None" with TSHE and 0 for COUNT
tshesdl$SPP[tshesdl$SPP=="NONE"]<-"TSHE"
tshespl$SPP[tshespl$SPP=="NONE"]<-"TSHE"
tshesdl<-tshesdl[tshesdl$SPP=="TSHE",]
tshespl<-tshespl[tshespl$SPP=="TSHE",]
#replace NAs in sapling data
tshespl$COUNT[which(is.na(tshespl$COUNT))]<-0

#check dim (chould be 60 plots * 4 subplot s* 2 years = 480 rows)
#dim(tshespl)
#dim(tshesdl)
#only 458 and 459, so need to add in missing plots with 0s
#create dataframe with all species and subplots to merge in
sites<-as.data.frame(cbind(rep(plots, each=4),
                              rep(c("N","S","E","W"), times =length(plots))))

colnames(sites)<-c("plot2","SUBPLOT")
sites$BASIN<-substr(sites$plot2,1,2)
sites2007<-sites
sites2007$YR<-2007
sites2020<-sites
sites2020$YR<-2020
allsites<-rbind(sites2007,sites2020)
tshesddat<-left_join(allsites,tshesdl, by=c("plot2","BASIN","SUBPLOT","YR"))
#table(tshesddat$plot2,tshesddat$YR)#looks good!
tshespdat<-left_join(allsites,tshespl, by=c("plot2","BASIN","SUBPLOT","YR"))
#table(tshespdat$plot2,tshespdat$YR)#looks good

#Now summarize across all tree seedling data
allspsdl<-aggregate(allsdl$COUNT,by=list(allsdl$SITE_ID,allsdl$BASIN,allsdl$PLOT,allsdl$SUBPLOT,allsdl$plot2,allsdl$YR),sum,na.rm=TRUE)
allspspl<-aggregate(allspl$COUNT,by=list(allspl$SITE_ID,allspl$BASIN,allspl$PLOT,allspl$SUBPLOT,allspl$plot2,allspl$YR),sum,na.rm=TRUE)
colnames(allspsdl)<-colnames(allspspl)<-c("SITE_ID","BASIN","PLOT","SUBPLOT","plot2","YR","COUNT")

sdlmerge<-left_join(allsites,allspsdl, by=c("plot2","BASIN","SUBPLOT","YR"))
splmerge<-left_join(allsites,allspspl, by=c("plot2","BASIN","SUBPLOT","YR"))
#Fix A couple of NAs that should be 0s
splmerge$COUNT[which(is.na(splmerge$COUNT))]<-0
splmerge$PLOT[which(is.na(splmerge$COUNT))]<-"20"
splmerge$SITE_ID[which(is.na(splmerge$COUNT))]<-"N1-20V"

##Add treatment to all species
allspt<-left_join(splmerge,treatdatsub,by="plot2", copy=TRUE)
allsdt<-left_join(sdlmerge,treatdatsub,by="plot2", copy=TRUE)

#add age in year of survey (2020 or 2007)
allspt$SURV.AGE[allspt$YR==2020]<-allspt$AGE[allspt$YR==2020]+1
allspt$SURV.AGE[allspt$YR==2007]<-allspt$AGE[allspt$YR==2007]-12

allsdt$SURV.AGE[allsdt$YR==2020]<-allsdt$AGE[allsdt$YR==2020]+1
allsdt$SURV.AGE[allsdt$YR==2007]<-allsdt$AGE[allsdt$YR==2007]-12

#Create "Wide" format to fit models of change in seedling/sapling abundnace over time
allsptsub<-subset(allspt,select=c(plot2,SUBPLOT,BASIN.x,YR,COUNT,TREAT,AGE,thintype))
allsdtsub<-subset(allsdt,select=c(plot2,SUBPLOT,BASIN.x,YR,COUNT,TREAT,AGE,thintype))

allsdtwide <- spread(allsdtsub, YR, COUNT)

allsptwide <- spread(allsptsub, YR, COUNT)
colnames(allsdtwide)[7:8]<-colnames(allsptwide)[7:8]<-c("CT2007","CT2020")
allsdtwide$CTDIFF<-allsdtwide$CT2020-allsdtwide$CT2007
allsptwide$CTDIFF<-allsptwide$CT2020-allsptwide$CT2007
#boxplot(CTDIFF~TREAT,data=allsdtwide)
#boxplot(CTDIFF~TREAT,data=allsptwide)

#prepare the data for the model
allsdt$YR<-as.factor(allsdt$YR)
allsdt$plot2<-as.factor(allsdt$plot2)
allsdt$SURV.AGE<-as.numeric(allsdt$SURV.AGE)
allsdt$TREAT<-as.factor(allsdt$TREAT)
allsdt$COUNT<-as.integer(allsdt$COUNT)
allspt$YR<-as.factor(allspt$YR)
allspt$plot2<-as.factor(allspt$plot2)
allspt$SURV.AGE<-as.numeric(allspt$SURV.AGE)
allspt$TREAT<-as.factor(allspt$TREAT)
allspt$COUNT<-as.integer(allspt$COUNT)
head(allspt)
