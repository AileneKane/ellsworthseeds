#Prep adult tree data for models

#Now merge adult data with treatment data
d2007sub<-d2007[d2007$plot2 %in% plots,]

#Create a combined dataframe to analyze:
colnames(d2007sub)[9:21]<-paste(colnames(d2007sub)[9:21],"2007",sep=".")
colnames(d2020)[9:23]<-paste(colnames(d2020)[9:23],"2020",sep=".")
colnames(d2020)[4]<-"TAG.2020"

#keep all rows
dall<-full_join(d2007sub,d2020,by=c("SITE_ID","BASIN","plot2","TAG.2007","SPP"))
dall<-dall[!dall$SPP=="NONE",]
#Remove Salix for now (not canopy tree)
dall<-dall[!dall$SPP=="SALIX",]

dalltrt<-left_join(dall,treatdatsub,by=c("plot2"),copy=TRUE)

#Calculate Basal Area for both time periods
dalltrt$BA.2007<-((as.numeric(dalltrt$DBH.2007)*.5)^2)*pi
dalltrt$BA.2020<-((as.numeric(dalltrt$DBH..CM..2020)*.5)^2)*pi


#See how DBH changed between the time periods
dalltrt$DBHDIFF<-as.numeric(dalltrt$DBH..CM..2020)-as.numeric(dalltrt$DBH.2007)
dalltrt$BAI<-dalltrt$BA.2020-dalltrt$BA.2007

#calculate relative growth to standardize by size/age
dalltrt$RG<-dalltrt$BAI/dalltrt$BA.2007
