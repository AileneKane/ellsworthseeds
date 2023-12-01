#Put together the two different treatment files
#head(treatdat)
#head(moretreatdat)
#head(plotdat)
#add column for thinning type
pctplots$thintype<-"PCT"
ctplots$thintype<-"CT"
pctplotsub<-subset(pctplots,select=c(SITE_ID,thintype))
ctplotsub<-subset(ctplots,select=c(SITE_ID,thintype))
allthintypes<-rbind(pctplotsub,ctplotsub)

#create a new column for merging that removes B and replaces with V in all cases

allthintypes$plot2<-gsub("VB","V",allthintypes$SITE_ID)

#Add thin type to treatdat
treatdat2<-left_join(treatdat,allthintypes, by ="plot2")

#add stand type and basin
treatdat3<-left_join(treatdat2,plotdat, by = "plot2")
#treatdat3$thintype[treatdat3$TREAT=="CON"]
treatdat3$thintype[which(is.na(treatdat3$thintype))]<-"CON"#i think this is correct?

#Michael says the following plots have not been thinned yet, so even though they are in "thinned" basins they are still controls
treatdat3$treatment[treatdat3$plot2=="C1-25V"|treatdat3$plot2=="C1-28V"|treatdat3$plot2=="C1-3V"|treatdat3$plot2=="C1-4V"|treatdat3$plot2=="C1-15V"]<-"CON"
treatdat3$TRT[treatdat3$plot2=="C1-25V"|treatdat3$plot2=="C1-28V"|treatdat3$plot2=="C1-3V"|treatdat3$plot2=="C1-4V"|treatdat3$plot2=="C1-15V"]<-"CON"
