#clean_overstory_litter

# drop untreated Active plots: C1-3, C1-4, C1-25, C1-28
cc2020 = subset(cc2020, SITE_ID!="C1-3V" & SITE_ID!="C1-4V" & SITE_ID!="C1-25VB" & SITE_ID!="C1-28VB"); unique(cc2020$SITE_ID)


#create a new column for merging that removes B and replaces with V in all cases
cc2020$plot2<-gsub("VB","V",cc2020$SITE_ID); head(cc2020)
#cc2007$plot2<-gsub("VB","V",cc2007$SITE_ID); head(cc2007)

#cc2007$plot_id<-cc2007$plot2
cc2020$plot_id<-cc2020$plot2
# drop untreated Active plots: C1-25, C1-3, C1-4, C1-28
litter2020 = subset(litter2020, SITE_ID!="C1-3V" & SITE_ID!="C1-4V" & SITE_ID!="C1-25VB" & SITE_ID!="C1-28VB")
#there are na's in 2020 data so remove those rows
litter2020$average=as.numeric(litter2020$average)
litter2020 = na.omit(litter2020)
any(is.na(litter2020))
#litter 2007 data had some na's but they moved the soil transect and took replacement data. I moved these replacement data over to the original columns (ie moved X_depth_0.5 to depth_0.5)
#create a new column for merging that removes B and replaces with V in all cases
litter2020$plot2<-gsub("VB","V",litter2020$SITE_ID); head(litter2020)
#litter2007$plot2<-gsub("VB","V",litter2007$SITE_ID); head(litter2007)

litter2020$SUBPLOT[litter2020$SUBPLOT=="EAST"]<-"E"
litter2020$SUBPLOT[litter2020$SUBPLOT=="WEST"]<-"W"
litter2020$SUBPLOT[litter2020$SUBPLOT=="SOUTH"]<-"S"
litter2020$SUBPLOT[litter2020$SUBPLOT=="NORTH"]<-"N"
#litter2007$plot_id<-litter2007$plot2
litter2020$plot_id<-litter2020$plot2
