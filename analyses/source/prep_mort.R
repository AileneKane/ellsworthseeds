#Prep adult tree mortality data for models
#Identify how many trees died in 2020 that were alive in 2007, and how many died naturally
dalltrt$dead2020<-NA
dalltrt$dead2020[dalltrt$LIVING.OR.DC.2020=="D"]<-1#Ask michael is this is correct? and what 1,2, and 4 mean
dalltrt$dead2020[dalltrt$LIVING.OR.DC.2020=="L"]<-0

#Identify how many trees died and were not harvested stumps
dalltrt$dead2020notharv<-dalltrt$dead2020
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="HARVESTED STUMP"]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="HARVESTED"]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="STUMP HARVESTED"]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="STUMP - HARVESTED"]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="HARVESTED STUMP "]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="NOT FOUND, LIKELY HARVESTED"]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="NOW HARVESTED STUMP"]<-NA  
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="NOW A STUMP - HARVESTED"]<-NA 
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="HARVEST STUMP"]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="NO TAG FOUND, BUT LIKELY HARVESTED STUMP AT LOCATION"]<-NA 
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="LIKELY REMOVED IN THINNING; TREE/TAG NOT FOUND "]<-NA
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="TAG FOUND ON GROUND, PRESUMED HARVESTED"]<-NA  
dalltrt$dead2020notharv[dalltrt$REMARKS.2020=="NOT FOUND"]<-NA  

mortdat<-dalltrt[-which(is.na(dalltrt$dead2020notharv)),]
mortdat<-mortdat[-which(is.na(mortdat$DBH.2007)),]#make sure we onyl include trees measured in 2007
mortdat$TREAT<-as.factor(mortdat$TREAT)
mortdat$AGE<-as.numeric(mortdat$AGE)
mortdat$plot2<-as.factor(mortdat$plot2)
mortdat$AGE.st<-(mortdat$AGE-mean(mortdat$AGE))/sd(mortdat$AGE)
mortdat$ELEV.st<-(mortdat$ELEV-mean(mortdat$ELEV))/sd(mortdat$ELEV)
mortdat$ASP.st<-(mortdat$ASPECT-mean(mortdat$ASPECT))/sd(mortdat$ASPECT)
mortdat$SLOPE.st<-(mortdat$SLOPE-mean(mortdat$SLOPE))/sd(mortdat$SLOPE)
dim(mortdat)#1697   99
