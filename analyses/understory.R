#Analyze understory data at Ellsworth 
#Started by Ailene
#May 2021

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
library(ggplot2)

#set working directory
setwd("C:/Users/ailene.ettinger.TNC/Box/ellsworthresearch")

#load helper functions
source("analysis/source/helper_functions.R")


#read in ellsworth veg plot understory data
d2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_under_veg.csv", header=TRUE)
d2007<-read.csv("data/CLEAN_DATA_2006_2007/UNDER_VEG_CLEAN_04272008.csv", header=TRUE)

treatdat<-read.csv("data/Ellsworthvegplotagetreat.csv")
#compare only plots sampled in both time periods
plots<-unique(d2020$SITE_ID)
basins<-unique(d2020$BASIN)

#Clean the data in these files
source("analysis/source/clean_understory.R")

#add in age of stand and treatment
colnames(treatdat)<-c("SITE_ID","TREAT","AGE")
treatdatsub<-treatdat[treatdat$SITE_ID %in% plots,]

d2007sub<-d2007[d2007$SITE_ID %in% plots,]

d2020trt<-left_join(d2020,treatdatsub,by=c("SITE_ID"),copy=TRUE)
d2007trt<-left_join(d2007sub,treatdatsub,by=c("SITE_ID"),copy=TRUE)

#How does beta diversity compare between 2007 and 2020? (move this to understory file- left here tempoerarily because wanted to discuss with michael)

length(unique(d2007sub$SPP))#78 species found in 2007
length(unique(d2020$SPP))#48 species found in 2020
#Questions:
#Is this change in diversity due to observation/survey differences or ecological differences?
#What species have disappeared?
#Is alpha richness also lower in 2020 vs 2007?

#calculate species richness, mean height, and mean cover in each plot and time period
sum2007<-get.sum(d2007sub)
sum2020<-get.sum(d2020)
sum2007$year<-2007
sum2020$year<-2020

#merge in age of the stand in 2019
colnames(sum2020)[1]<-colnames(sum2007)[1]<-"SITE_ID"
sum2020t<-left_join(sum2020,treatdatsub,by="SITE_ID", copy=TRUE)
sum2007t<-left_join(sum2007,treatdatsub,by="SITE_ID", copy=TRUE)

#add age in year of survey (2020 or 2007)
sum2020t$SURV.AGE<-sum2020t$AGE+1
sum2007t$SURV.AGE<-sum2007t$AGE-12

#create one summary file
allsumt<-rbind(sum2007t,sum2020t)

allsumt<-allsumt[!allsumt$SITE_ID=="",]

#prepare the data for the model
allsumt$year<-as.factor(allsumt$year)
allsumt$SURV.AGE<-as.numeric(allsumt$SURV.AGE)
allsumt$allmnht<-as.numeric(allsumt$allmnht)
allsumt$allmncov<-as.numeric(allsumt$allmncov)
allsumt$alltotcov<-as.numeric(allsumt$alltotcov)
allsumt$SURV.AGE<-as.numeric(allsumt$SURV.AGE)
allsumt$TREAT<-as.factor(allsumt$TREAT)
allsumt$allnumsp<-as.numeric(allsumt$allnumsp)
allsumt$SITE_ID<-as.factor(allsumt$SITE_ID)

#need to add years since treatment i think? and check that i have the ages right...

#mods
sprichmod<-lmer(allnumsp~TREAT*SURV.AGE + (1|year), data =allsumt)
summary(sprichmod)
sprichmod2<-lm(allnumsp~TREAT*SURV.AGE, data =allsumt)
summary(sprichmod2)
#richness is (marginally) higher in thinned plots (age has a negative relationahips with richness)

#Now mean cover
covmod<-lmer(allmncov~TREAT*SURV.AGE + (1|year), data =allsumt)
summary(covmod)
covmod2<-lm(allmncov~TREAT*SURV.AGE, data =allsumt)
summary(covmod2)

#totalcover
tcovmod<-lmer(alltotcov~TREAT*SURV.AGE + (1|year), data =allsumt)
summary(tcovmod)
tcovmod2<-lm(alltotcov~TREAT*SURV.AGE, data =allsumt)
summary(tcovmod2)


#slightly higher mn cov in thinned plots, with increasing age

htmod<-lmer(allmnht~TREAT*SURV.AGE + (1|year), data =allsumt)
summary(htmod)
htmod2<-lm(allmnht~TREAT*SURV.AGE, data =allsumt)
summary(htmod2)
#biggest efect is neg effect of age on ht?but lm doesn't make sense

richplot1 <- ggplot(allsumt, aes(SURV.AGE, allnumsp,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Species richness", x = "Stand Age")+
  geom_smooth()
richplot1 # print plot
richplot2 <- ggplot(allsumt, aes(SURV.AGE, allnumsp,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Species richness", x = "Stand Age")+
  geom_smooth(method = 'lm')
richplot2 # print plot

#Now mean cover
covplot1 <- ggplot(allsumt, aes(SURV.AGE,allmncov,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Mean Cover (%)", x = "Stand Age")+
  geom_smooth()
covplot1 # print plot
covplot2 <- ggplot(allsumt, aes(SURV.AGE,allmncov,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Mean Cover (%)", x = "Stand Age")+
  geom_smooth(method = 'lm')
covplot2 # print plot

#Now total cover
tcovplot1 <- ggplot(allsumt, aes(SURV.AGE,alltotcov,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Total Cover (%)", x = "Stand Age")+
  geom_smooth()
tcovplot1 # print plot
tcovplot2 <- ggplot(allsumt, aes(SURV.AGE,alltotcov,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Total Cover (%)", x = "Stand Age")+
  geom_smooth(method = 'lm')
tcovplot2 # print plot

#Now mean ht
htplot1 <- ggplot(allsumt, aes(SURV.AGE,allmnht,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Mean height (m)", x = "Stand Age")+
  geom_smooth()
htplot1 # print plot
htplot2 <- ggplot(allsumt, aes(SURV.AGE,allmnht,col =TREAT)) + 
  geom_point(size = 3) + # change size and colour
  labs(y = "Mean height (m)", x = "Stand Age")+
  geom_smooth(method = 'lm')
htplot2 # print plot


#Now, multivariat
#Next steps/questions:
# 1) **Incorporate treatment year?- Michael doesn't think that treatment year will have a big effect but it might
# 2)** Look at diversity, and perhaps phylogenetic distinctness?
# 3) **Look at seedling/sapling presence, abundance- are there are species we care about and want to pull out?
# 4) Add slope, or other factors
# 5) Multivariate analysis- how have plots shifted?
# 6) Do we want to account for Basin as a random effect  (wasn't able to in above models)
