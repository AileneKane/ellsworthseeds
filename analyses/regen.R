# Analyze regeneration of trees using understory veg plot data at Ellsworth 
# Started by Ailene Ettinger
# June 2021

###NOTE: this file is not full functional. 
#The Most up to date regeration analyses can be found in ellsworth_restmoneval.R
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(tidyr)
require(gridExtra)
library(multcomp)

#set working directory
#setwd("C:/Users/ailene.ettinger.TNC/Box/ellsworthresearch")
setwd("C:/Users/ailene.ettinger/Box/ellsworthresearch")
#read in ellsworth tree seedling data
sd2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_seedlings.csv", header=TRUE)
sp2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_saplings.csv", header=TRUE)
sd2007<-read.csv("data/CLEAN_DATA_2006_2007/SEEDLING_CLEAN_04272008.csv", header=TRUE)
sp2007<-read.csv("data/CLEAN_DATA_2006_2007/SAPLING_CLEAN_04272008.csv", header=TRUE)

treatdat<-read.csv("data/Ellsworthvegplotagetreat.csv")
moretreatdat<-read.csv("data/Ellsworth_stands_treatment_data.csv")
pctplots<-read.csv("data/PCT_veg_plots_Ellsworth_stands_treatment_data_07082021.csv")
ctplots<-read.csv("data/CT_veg_plots_Ellsworth_stands_treatment_data_07082021.csv")

plotdat<-read.csv("data/CLEAN_DATA_2006_2007/PLOT_CLEAN_11062008.csv")



#Clean the data in these files
source("analysis/source/clean_seedsap.R")
source("analysis/source/clean_treatdat.R")
source("analysis/source/clean_plotdat.R")

#Merge the different treatment data files together to get type of thinning and year of thinning
source("analysis/source/merge_treatdat.R")

#compare only plots sampled in both time periods
plots<-unique(sd2020$plot2)#60
basins<-unique(sd2020$BASIN)

#add in age of stand and treatment
colnames(treatdat3)[1:3]<-c("SITE_ID","TREAT","AGE")
treatdatsub<-treatdat3[treatdat3$plot2 %in% plots,]
sd2007sub<-sd2007[sd2007$SITE_ID %in% plots,]
sp2007sub<-sp2007[sp2007$SITE_ID %in% plots,]

#write.csv(treatdatsub,"ellsworthplotages2020.csv", row.names=FALSE)

#Create to allow two different types of dataframes to analyze:
#First, in long format, with aa column for year and rows for 2007 and 2020 kept separate
#combine both time periods
sp2007sub$YR<-2007
sp2020$YR<-2020
sd2007sub$YR<-2007
sd2020$YR<-2020
sp2007suba<-sp2007sub
sd2007suba<-sd2007sub

allspl<-rbind(sp2007suba,sp2020)
allsdl<-rbind(sd2007suba,sd2020)

#First, in "wide" format, which one column for 2007 ocunts and one for 2020 counts
#Merge the two time periods together and add in treatments#
#change colnames to facilitate merging later
colnames(sd2007sub)[6]<-c("COUNT2007")
colnames(sp2007sub)[6]<-c("COUNT2007")
colnames(sd2020)[6]<-c("COUNT2020")
colnames(sp2020)[6]<-c("COUNT2020")
#Seedlings
sdall<-full_join(sd2007sub,sd2020,by=c("SITE_ID","BASIN","PLOT","SUBPLOT","SPP","plot2"))

#Saplings
spall<-full_join(sp2007sub,sp2020,by=c("SITE_ID","BASIN","PLOT","SUBPLOT","SPP","plot2"))


##Prep data so that all species in both types of dataframes have absences listed
source("analysis/source/prep_seedsap_spp.R")

##Change in seedling/sapling counts between two time periods

sddiffmod<-lmer(CTDIFF~TREAT*AGE+(1|SITE_ID),data=sdalltrt)
summary(sddiffmod)#higher seedling counts in older plots
Anova(sddiffmod)

sddifflm<-lm(COUNTDIFF~TREAT*AGE,data=sdalltrt)
summary(sddifflm)#higher seedling counts in older plots
Anova(sddifflm)

spdiffmod<-lmer(COUNTDIFF~TREAT*AGE+(1|SITE_ID),data=spalltrt)
summary(spdiffmod)#Higher in older plots 
Anova(spdiffmod) #sig TREAT*AGE interaction but we need to add time since treatment
spdifflm<-lm(COUNTDIFF~TREAT*AGE,data=spalltrt)
summary(spdifflm)#higher seedling counts in older plots
Anova(spdifflm)
png(file="analysis/figures/seedlingsaplingboxplots.png",width = 1200, height =600 )
par(mfrow=c(1,2))
boxplot(sdalltrt$COUNTDIFF~sdalltrt$TREAT, 
        xlab="Treatment",ylab="Change in Seedling Count",
        boxwex=.25, col = "lightgreen", cex.lab=1.7, cex.axis=1.7)

boxplot(spalltrt$COUNTDIFF~spalltrt$TREAT, 
        xlab="Treatment",ylab="Change in Sapling Count",
        boxwex=.25, col = "lightgreen", cex.lab=1.7, cex.axis=1.7)
dev.off()
sdalltrt$SPP<-as.factor(sdalltrt$SPP)
spalltrt$SPP<-as.factor(spalltrt$SPP)

psd <- qplot(AGE,COUNTDIFF, data = sdalltrt, colour = TREAT) +
  geom_smooth()+
  geom_point(aes(shape=SPP, color=TREAT))+
  labs(title = "Tree seedlings")


psp <- qplot(AGE,COUNTDIFF, data = spalltrt, colour = TREAT) +
  geom_smooth()+
  geom_point(aes(shape=SPP, color=TREAT)) +
   labs(title = "Tree saplings")
png(file="analysis/figures/seedlingsaplingcountdiff.png",width = 1200, height =600 )
par(mfrow=c(1,2))
grid.arrange(psd, psp, ncol=2)
dev.off()

#Look at differences across species now
#Seedlings
png(file="analysis/figures/seedlingsaplingboxplots_byspp.png",width = 1400, height =600 )
par(mfrow=c(1,2))
boxplot(sdalltrt$COUNTDIFF[sdalltrt$TREAT=="THIN"]~sdalltrt$SPP[sdalltrt$TREAT=="THIN"], 
        xlab="Species",ylab="Change in Seedling Count",
        boxwex=.25, col = "white",at =seq(from =1,to=6,by=1)+.15, cex.lab=1.5, cex.axis=1.5)
boxplot(sdalltrt$COUNTDIFF[sdalltrt$TREAT=="CON"]~sdalltrt$SPP[sdalltrt$TREAT=="CON"], 
        xaxt = "n", add = TRUE,
        boxwex=.25, col = "lightgreen",at =seq(from =1,to=6,by=1)-.15, cex.lab=1.5, cex.axis=1.5)
legend("bottomleft", legend=c("CON","THIN"),fill =c("lightgreen","white"), cex=1.5)

#Saplings
boxplot(spalltrt$COUNTDIFF[spalltrt$TREAT=="THIN"]~spalltrt$SPP[spalltrt$TREAT=="THIN"], 
        xlab="Species",ylab="Change in Sapling Count",
        boxwex=.25, col = "white",at =seq(from =1,to=6,by=1)+.15, cex.lab=1.5, cex.axis=1.5)
boxplot(spalltrt$COUNTDIFF[spalltrt$TREAT=="CON"]~spalltrt$SPP[spalltrt$TREAT=="CON"], 
        xaxt = "n", add = TRUE,
        boxwex=.25, col = "lightgreen",at =seq(from =1,to=6,by=1)-.15, cex.lab=1.5, cex.axis=1.5)
legend("bottomleft", legend=c("CON","THIN"),fill =c("lightgreen","white"), cex=1.5)
dev.off()
#models accounting for species differences
spdiffmod<-lm(COUNTDIFF~TREAT*AGE*SPP,data=spalltrt)
Anova(spdiffmod)#differences in counts between two time periods differ by treatment, age, sp, treat*sp interaction
summary(spdiffmod)#biggest effect of age and thinning: age interaction was on TSHE

sddiffmod<-lm(COUNTDIFF~TREAT*AGE*SPP,data=sdalltrt)#age is the main driver for sd, also age*sp interaction
Anova(sddiffmod)
summary(sddiffmod)#biggest effect of age and thinning: age interaction was on TSHE

#should we used mixed models to account for non-independence of plots? probably!
spdiffmmod<-lmer(COUNTDIFF~TREAT*AGE*SPP+(1|SITE_ID),data=spalltrt)
Anova(spdiffmmod)#differences in counts between two time periods differ by treatment, age, sp, treat*sp interaction
summary(spdiffmmod)#biggest effect of age and thinning: age interaction was on TSHE

sddiffmmod<-lmer(COUNTDIFF~TREAT*AGE*SPP+(1|SITE_ID),data=sdalltrt)#age is the main driver for sd, also age*sp interaction
Anova(sddiffmmod)
summary(sddiffmmod)#biggest effect of age and thinning: age interaction was on TSHE

#Proportional change mods
sppropdiffmmod<-lmer(PROPDIFF~TREAT*AGE*SPP+(1|SITE_ID),data=spalltrt)
Anova(sppropdiffmmod)#differences in counts between two time periods differ by treatment, age, sp, treat*sp interaction
summary(sppropdiffmmod)#biggest effect of age and thinning: age interaction was on TSHE

sdpropdiffmmod<-lmer(PROPDIFF~TREAT*AGE*SPP+(1|SITE_ID),data=sdalltrt)#age is the main driver for sd, also age*sp interaction
Anova(sdpropdiffmmod)
summary(sdpropdiffmmod)#biggest effect of age and thinning: age interaction was on TSHE

###Now look at the data in a different way- not just change, but treatment and age effects similar to what I did with the understory

#merge in age of the stand in 2019

allspt<-left_join(allsp,treatdatsub,by="SITE_ID", copy=TRUE)
allsdt<-left_join(allsd,treatdatsub,by="SITE_ID", copy=TRUE)

#add age in year of survey (2020 or 2007)
allspt$SURV.AGE[allspt$YR==2020]<-allspt$AGE[allspt$YR==2020]+1
allspt$SURV.AGE[allspt$YR==2007]<-allspt$AGE[allspt$YR==2007]-12

allsdt$SURV.AGE[allsdt$YR==2020]<-allsdt$AGE[allsdt$YR==2020]+1
allsdt$SURV.AGE[allsdt$YR==2007]<-allsdt$AGE[allsdt$YR==2007]-12

#prepare the data for the model
allsdt$YR<-as.factor(allsdt$YR)
allsdt$SURV.AGE<-as.numeric(allsdt$SURV.AGE)
allsdt$TREAT<-as.factor(allsdt$TREAT)
allsdt$COUNT<-as.integer(allsdt$COUNT)
allspt$YR<-as.factor(allspt$YR)
allspt$SURV.AGE<-as.numeric(allspt$SURV.AGE)
allspt$TREAT<-as.factor(allspt$TREAT)
allspt$COUNT<-as.integer(allspt$COUNT)


#need to add years since treatment i think? and check that i have the ages right...

#mods
#Try some different random effects structures
sdcountmod<-glmer(COUNT~TREAT*SURV.AGE*SPP + (1|SITE_ID)+ (1|YR),family="poisson", data =allsdt)
sdcountmodyr<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|YR),family="poisson", data =allsdt)
sdcountmodsite<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|SITE_ID),family="poisson", data =allsdt)
AIC(sdcountmodsite,sdcountmodyr,sdcountmod)

spcountmod<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|SITE_ID)+ (1|YR),family="poisson", data =allspt)
spcountmodyr<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|YR),family="poisson", data =allspt)
spcountmodsite<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|SITE_ID),family="poisson", data =allspt)
AIC(spcountmodsite,spcountmodyr,spcountmod)

#Might be best to just fit model for tshe, since that is the most common? or, fit it separately from other species
#Models with all spp are not converging...
tshesddat<-allsdt[allsdt$SPP=="TSHE",]
tshespdat<-allspt[allspt$SPP=="TSHE",]
tshesdcountlmmod<-glm(COUNT~TREAT*SURV.AGE,family="poisson", data =tshesddat)

tshesdcountmod<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =tshesddat)
tshesdcountmodyr<-glmer(COUNT~TREAT*SURV.AGE+ (1|YR),family="poisson", data =tshesddat)
tshesdcountmodsite<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID),family="poisson", data =tshesddat)
AIC(tshesdcountmodsite,tshesdcountmodyr,tshesdcountmod,tshesdcountlmmod)
summary(tshesdcountmod)#model with lowest AIC, For just TSHE, thinning has the biggest effect
Anova(tshesdcountmod)
tshecountmod<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =tshespdat)
tshecountlmmod<-glm(COUNT~TREAT*SURV.AGE,family="poisson", data =tshespdat)
tshecountmodyr<-glmer(COUNT~TREAT*SURV.AGE+ (1|YR),family="poisson", data =tshespdat)
tshecountmodsite<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID),family="poisson", data =tshespdat)
AIC(tshecountmodsite,tshecountmodyr,tshecountmod,tshecountlmmod)
summary(tshecountmod)#model with lowest AIC, 
Anova(tshecountmod)
#For just TSHE, thinning has neg effect on count of saps,
#age of stand has negative effect on sapling count for controls, but 
# age had a positive effect on saplings count for thinned 
summary(tshesdcountmod)
Anova(tshesdcountmod)
summary(tshecountmod)
Anova(tshecountmod)
#Plot

tshepsd <- qplot(SURV.AGE,COUNT, data = tshesddat, colour = TREAT) +
  geom_smooth()+
  geom_point(aes(shape=SPP, color=TREAT))+
  labs(title = "Seedlings")


tshepsp <- qplot(SURV.AGE,COUNT, data = tshespdat, colour = TREAT) +
  geom_smooth()+
  geom_point(aes(shape=SPP, color=TREAT)) +
  labs(title = "Saplings")
#should add a panel for adult growth and mortality repsonses
# tshead<- qplot(SURV.AGE,COUNT, data = tshespdat, colour = TREAT) +
#   geom_smooth()+
#   geom_point(aes(shape=SPP, color=TREAT)) +
#   labs(title = "Adults")

png(file="analysis/figures/tsheseedlingsaplingcountdiff.png",width = 1200, height =600 )
par(mfrow=c(1,2))
grid.arrange(tshepsd, tshepsp, ncol=2)#add,tshead


dev.off()





#Maybe now fit all other species together?
#Try some different random effects structures
mostsdt<-allsdt[allsdt$SPP!="TSHE",]
mostspt<-allspt[allspt$SPP!="TSHE",]

sdcountmod<-glmer(COUNT~TREAT*SURV.AGE*SPP + (1|SITE_ID)+ (1|YR),family="poisson", data =mostsdt)
sdcountmodyr<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|YR),family="poisson", data =mostsdt)
sdcountmodsite<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|SITE_ID),family="poisson", data =mostsdt)
AIC(sdcountmodsite,sdcountmodyr,sdcountmod)


spcountmod<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|SITE_ID)+ (1|YR),family="poisson", data =mostspt)
spcountmodyr<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|YR),family="poisson", data =mostspt)
spcountmodsite<-glmer(COUNT~TREAT*SURV.AGE*SPP+ (1|SITE_ID),family="poisson", data =mostspt)
AIC(spcountmodsite,spcountmodyr,spcountmod)
#not great -lack of convergenct/singular fit errors for most models

#MAybe try THPLE and PSME separately too?

thplsddat<-allsdt[allsdt$SPP=="THPL",]
thplspdat<-allspt[allspt$SPP=="THPL",]
thplsdcountlmmod<-glm(COUNT~TREAT*SURV.AGE,family="poisson", data =thplsddat)

thplsdcountmod<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =thplsddat)
thplsdcountmodyr<-glmer(COUNT~TREAT*SURV.AGE+ (1|YR),family="poisson", data =thplsddat)
thplsdcountmodsite<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID),family="poisson", data =thplsddat)
AIC(thplsdcountmodsite,thplsdcountmodyr,thplsdcountmod,thplsdcountlmmod)
summary(thplsdcountlmmod)#model with lowest AIC, For just THPL, thinning has the biggest (-) effect

thplcountmod<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =thplspdat)
thplcountlmmod<-glm(COUNT~TREAT*SURV.AGE,family="poisson", data =thplspdat)
thplcountmodyr<-glmer(COUNT~TREAT*SURV.AGE+ (1|YR),family="poisson", data =thplspdat)
thplcountmodsite<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID),family="poisson", data =thplspdat)
AIC(thplcountmodsite,thplcountmodyr,thplcountmod,thplcountlmmod)
summary(thplcountlmmod)#model with lowest AIC, 
#thinning treatment has a negative effect on THPL sapling count
Anova(thplcountlmmod)#tre

hist(thplspdat$COUNT)
hist(thplsddat$COUNT)



#PSME
psmesddat<-allsdt[allsdt$SPP=="PSME",]
psmespdat<-allspt[allspt$SPP=="PSME",]
psmesdcountlmmod<-glm(COUNT~TREAT*SURV.AGE,family="poisson", data =psmesddat)

psmesdcountmod<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =psmesddat)
psmesdcountmodyr<-glmer(COUNT~TREAT*SURV.AGE+ (1|YR),family="poisson", data =psmesddat)
psmesdcountmodsite<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID),family="poisson", data =psmesddat)
#no models could be fit!

psmecountmod<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =psmespdat)
psmecountlmmod<-glm(COUNT~TREAT*SURV.AGE,family="poisson", data =psmespdat)
psmecountmodyr<-glmer(COUNT~TREAT*SURV.AGE+ (1|YR),family="poisson", data =psmespdat)
psmecountmodsite<-glmer(COUNT~TREAT*SURV.AGE+ (1|SITE_ID),family="poisson", data =psmespdat)
AIC(psmecountmodsite,psmecountlmmod)
summary(psmecountlmmod)#model with lowest AIC, 
#no sig effects of age or ttreatment
Anova(psmecountlmmod)

hist(psmespdat$COUNT)
hist(psmesddat$COUNT)

#See if type of thinning affects regen of tshe
tshecountmod<-glmer(COUNT~thintype*SURV.AGE+ (1|SITE_ID)+ (1|YR),family="poisson", data =tshespdat)

#Next steps/questions:
# 1) IS increased seedling abundance due to better conditions or more seeds (who cares?)
# 2) Incorporate treatment year?- Michael doesn't think that treatment year will have a big effect but it might
# 3) Look at seedling regeneration in combination with diversity?
# 4) Add slope, or other factors
# 5) July 28, 2021: Note that of plots have not recieved thinning, we can assume that nothing has been done.