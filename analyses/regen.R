# Analyze regeneration of trees using understory veg plot data at Ellsworth 
# by Ailene Ettinger
# started with ellsworth_restomoneval.R code from Case et al 2023 paper

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
require(gridExtra)
library(tidyr)
library(sjPlot)
library(multcomp)

setwd("~/GitHub/ellsworthseeds")

#read in ellsworth tree seedling, sapling, adult tree, and lwd data
#seedling data
sd2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_seedlings.csv", header=TRUE)

#sapling data 
sp2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_saplings.csv", header=TRUE)

#adult tree growth data
d2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_TREES.csv", header=TRUE)

#adult tree mortality data
oldtrees<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_OLDTREES.csv", header=TRUE)

#Large woody debris data
lwd2020<-read.csv("data/LCWD_Ellsworth_Creek_2020_ALL_5Jan2021.csv", header=TRUE)

#SNAGS
snag2020<-read.csv("data/SNAGS_Ellsworth_Creek_2020_ALL_19Jan2022.csv", header=TRUE)


#Overstory/Canopy data
cc2020<-read.csv("data/Ellsworth_Creek_2020_overstory_cover.csv", header=TRUE)

#Litter data
litter2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_litterlayer.csv", header=TRUE)

#Understory vegetation data
und2020<-read.csv("data/Ellsworth_Creek_2020_ALL_5Jan2021_under_veg.csv", header=TRUE)

# treatment data
treatdat<-read.csv("data/Ellsworthvegplotagetreat_13Jan22.csv")
moretreatdat<-read.csv("data/Ellsworth_stands_treatment_data_07082021.csv")
pctplots<-read.csv("data/PCT_veg_plots_Ellsworth_stands_treatment_data_07082021.csv")
ctplots<-read.csv("data/CT_veg_plots_Ellsworth_stands_treatment_data_07082021.csv")
plotdat<-read.csv("data/PLOT_CLEAN_11062008.csv")

#Clean the data in these files
source("analyses/source/clean_seedsap.R")
source("analyses/source/clean_treatdat.R")
source("analyses/source/clean_plotdat.R")
source("analyses/source/clean_trees.R")
source("analyses/source/clean_oldtrees.R")
source("analyses/source/clean_lwd.R")
source("analyses/source/clean_overstory_litter.R")

#Merge different treatment data files together to get type of thinning and year of thinning
source("analyses/source/merge_treatdat.R")

plots<-unique(sd2020$plot2)#60
basins<-unique(sd2020$BASIN)

#add in age of stand and treatment to seedling and sapling data
colnames(treatdat3)[1:3]<-c("SITE_ID","TREAT","AGE")
treatdat3<-treatdat3[,-5]
treatdatsub<-treatdat3[treatdat3$plot2 %in% plots,]
#write.csv(treatdatsub,"treatmentdat.csv",row.names=FALSE)

##Prep data so that models can be fit
source("analyses/source/prep_seedsap_spp.R")
source("analyses/source/prep_lwd.R")
source("analyses/source/prep_adulttrees.R")
#source("analysis/source/prep_mort.R")


#Create categorical value for young and mature forests:
source("analyses/source/get_agecat.R")

#Fit models and make plots of effects of treatment across stand age, for different life stages

#Jan 17, 2024 To do:
#1) Make plots of 
