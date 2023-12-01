#Understory richness analysis
########## looking at understory diversity ##########

# 1. Prepare workspace and packages ####
# clear workspace
rm(list=ls())

setwd("C:/Users/ailene.ettinger.TNC/Box/ellsworthresearch/analysis")

# load necessary package
library(ggplot2)
library(reshape2)
library(ggrepel)
library(stats)
library(vegan)
library(lme4)
indir<-"C:/Users/ailene.ettinger.TNC/Box/ellsworthresearch/data/"

# 2. Get experimental information information####
exp_DF <- read.csv(paste0(indir, "Ellsworthvegplotagetreat_13Jan22.csv"))
exp_DF <- data.frame(exp_DF, do.call(rbind,strsplit(exp_DF$plot,'V') ))
exp_DF <- exp_DF[,-c(1,5)]
colnames(exp_DF) <- c("TRT", "Age", "SITE_ID")
# Change treatment into dummy variable
exp_DF$Thinned <- 0
exp_DF$Thinned[exp_DF$TRT == "THIN"] <- 1
exp_DF$Control <- 0
exp_DF$Control[exp_DF$TRT == "CON"] <- 1
exp_DF <- exp_DF[, -which(colnames(exp_DF) == "TRT")]
exp_DF <- exp_DF[,c(2,3,4,1)] # reorder just to make it neater
row.names(exp_DF) <- exp_DF$SITE_ID
exp_DF <- exp_DF[,-1]


# 3. Load clean and prep forest data: Understory abundance ####
undRaw <- read.csv(paste0(indir, "Ellsworth_Creek_2020_ALL_5Jan2021_under_veg.csv"))
undRaw$COVER[which(undRaw$COVER == "")] <- NA
undAbund <- undRaw
# remove trace species
undAbund <- undAbund[-which(undAbund$COVER == "T"),]
# change class of cover column
undAbund$COVER <- as.numeric(undAbund$COVER)
# remove rows with all NA values for COVER
undAbund <-  undAbund[-which(is.na(undAbund$COVER)),]
# convert to wide format and aggrgate by calculating the sum of cover for each subplot per species
undAbundWide <- dcast(undAbund, SITE_ID ~ SPP, value.var="COVER", fun.aggregate = sum)
# divide all sums by 4
undAbundWide[,c(2:38)] <- undAbundWide[,c(2:38)]/4
# remove unknowns
undAbundWide <- undAbundWide[,-(which(colnames(undAbundWide)%in% c("UNK1", "UNKGRAM", "UNKHERB", "UNKNOWN", "NONE")))] 
# remove "V" or "VB" from the end of the Site ID and assign this as row names
row.names(undAbundWide) <- do.call(rbind,strsplit(undAbundWide$SITE_ID,'V') )[,1]
# remove the "SITE_ID" column
undAbundWide <- undAbundWide[,-1]
# reorder based on site id
undAbundWide <- undAbundWide[order(row.names(undAbundWide)),] #
#remove the column with "NONE" species
undAbund_noNA <- undAbundWide

# convert into presence absence ####
undAbundPA <- undAbund_noNA
undAbundPA[undAbundPA >0] <- 1

# calcualte diversity ####
divInd <- undAbund_noNA
divInd$Richness <- rowSums(undAbundPA)
# subset envrionmental data based on age ####
# only keep sites that are younger than 75 years. This is to keep it consistent with the rest of the veg analysis
exp_DF_75 <- exp_DF[which(exp_DF$Age <= 75),] # 87
# Change age into dummy variable
exp_DF_cat <- exp_DF_75
exp_DF_cat$Young <- 0
exp_DF_cat$Young[exp_DF_cat$Age < 40 ] <- 1
exp_DF_cat$Old <- 0
exp_DF_cat$Old[exp_DF_cat$Age > 40] <- 1
env <- exp_DF_75[which(row.names(exp_DF_75)%in%row.names(divInd)),]

# create the group object and combine
grp <- env
row.names(grp) <- row.names(env)
grp$EXP <- "CONTROL"
grp$EXP[which(grp$Thinned == 1)] <- "TREATMENT"
grp$COLOR <- "red"
grp$COLOR[which(grp$Thinned == 1)] <- "blue"
grp$Age_catagory <- "YOUNG"
grp$Age_catagory[which(grp$Age > 40)] <- "MATURE"
grp <- merge(grp, divInd, by = "row.names")
row.names(grp) <- grp[, 1]
grp <- grp[, -1]
grp$EXP <- as.factor(grp$EXP)

# lm models ####
grp$Plot_ID <- row.names(grp)
grp$age_trt <- paste(grp$Age_catagory, grp$EXP)
grp$age_trt <- factor(grp$age_trt, levels = c("YOUNG CONTROL", "YOUNG TREATMENT", "MATURE CONTROL", "MATURE TREATMENT"))
grp$Age_catagory <- as.factor(grp$Age_catagory)
grp$EXP <- as.factor(grp$EXP)

modRich <- glm(Richness ~ Age_catagory * EXP, data = grp, family = poisson)
summary(modRich)
modRich2 <- glm(Richness ~ -1+age_trt, data = grp, family = poisson)


png(file="figures/rich_barplot.png",width = 1000, height =800 ,res =200)
#pdf(file="figures/rich_barplot.pdf",width = 10, height =8)

error<-exp(confint(modRich2))
x<-barplot(exp(coef(modRich2)),names.arg=c("","","",""),col=c("goldenrod","darkgreen","goldenrod","darkgreen"),
           ylab="# Species", ylim=c(0,12))
for(i in 1:length(x)){
  arrows(x[i],error[i,1],x[i],error[i,2], code=3, angle=90, length=0.05,  lwd=1)
}
axis(side=1,at=c(1.2,3.7),labels=c("young","mature"),line=2,lwd=NA)
abline(h=0)
legend("bottomright",legend=c("control","treated"),fill=c("goldenrod","darkgreen"))
dev.off()
