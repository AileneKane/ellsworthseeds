#file that cleans the seedling and sapling ellsworth data from 2007 and 2020
sp2020<-sp2020[1:256,1:6]
sd2020<-sd2020[,1:6]
#sp2007<-sp2007[,1:6]
#sd2007<-sd2007[,1:6]

#Clean species names when applicable
sd2020$SPP[sd2020$SPP=="TSHE "]<-"TSHE"

#Clean plot names
sp2020$SITE_ID[sp2020$SITE_ID=="N2-26"]<-"N2-26VB"
sd2020$SITE_ID[sd2020$SITE_ID=="N2-26"]<-"N2-26VB"

#Change EAst, West, North South in 2020 dataset to match 2007 dataset (EWNS)
sp2020$SUBPLOT[sp2020$SUBPLOT=="EAST"]<-"E"
sp2020$SUBPLOT[sp2020$SUBPLOT=="WEST"]<-"W"
sp2020$SUBPLOT[sp2020$SUBPLOT=="NORTH"]<-"N"
sp2020$SUBPLOT[sp2020$SUBPLOT=="SOUTH"]<-"S"

sd2020$SUBPLOT[sd2020$SUBPLOT=="EAST"]<-"E"
sd2020$SUBPLOT[sd2020$SUBPLOT=="WEST"]<-"W"
sd2020$SUBPLOT[sd2020$SUBPLOT=="NORTH"]<-"N"
sd2020$SUBPLOT[sd2020$SUBPLOT=="SOUTH"]<-"S"

#one plot has 2 subplots listed as "N"- one of these should be east (guessing which one!)
sp2020$SUBPLOT[sp2020$SITE_ID=="N1-24VB" & sp2020$SUBPLOT=="N"]<-c("E","N")
#one plot as 2 subplots listed as "S"-used 2007 subplot data to name one E 
sp2020$SUBPLOT[sp2020$SITE_ID=="N1-14VB" & sp2020$SUBPLOT=="S"]<-c("E","S")
#one plot has 2 subplots listed as S- remove one that says "NONE" in SPP colun
sp2020<-sp2020[-which(sp2020$SITE_ID=="N2-1VB" & sp2020$SUBPLOT=="S" & sp2020$SPP=="",),]

#2020 data used NA instead of 0 for seedlings counts; replace to match 2007
sd2020$COUNT[which(is.na(sd2020$COUNT))]<-0
sp2020$COUNT[which(is.na(sp2020$COUNT))]<-0

#Make blank and "NONE" subplots all the same
sp2020$SPP[sp2020$SPP==""]<-"NONE"
sd2020$SPP[sd2020$SPP==""]<-"NONE"
#sp2007[sp2007$SPP=="NONE",]
#sd2007$SPP[sd2007$SPP=="none"]<-"NONE"

#create a new column for merging that removes B and replaces with V in all cases, use this for remainder of the analyses
#sd2007$plot2<-gsub("VB","V",sd2007$SITE_ID)
sd2020$plot2<-gsub("VB","V",sd2020$SITE_ID)
#sp2007$plot2<-gsub("VB","V",sp2007$SITE_ID)
sp2020$plot2<-gsub("VB","V",sp2020$SITE_ID)

