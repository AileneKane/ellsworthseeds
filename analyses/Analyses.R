###### Final Project Analyses #######
###### By Eileen Arata ######
library(car)
library(MuMIn)
library(MASS)
library(AER)
library(lme4)
library(colorspace)

setwd("~/GitHub/ellsworthseeds/analyses")
seeds <- read.csv("../data/completed_plots.csv")

#Explore Data
seeds$treat <- as.factor(seeds$treat)
seeds$stand <- as.factor(seeds$stand)
seeds$subplot <- as.factor(seeds$subplot)

colnames(seeds)
dim(seeds)

par(mfrow=c(1,1))
plot(seedct ~ age, dat=seeds, pch=20, main='Seeds vs. Age', xlab='Stand Age (yrs)',
     ylab='Seed Count')


#Simple Linear Models - forward stepwise
null <- lm(seedct ~ 1, data=seeds)

#One predictor
age.slm <- lm(seedct ~ age, data=seeds)
slope.slm <- lm(seedct ~ slope, data=seeds)
asp.slm <- lm(seedct ~ aspect, data=seeds)
elev.slm <- lm(seedct ~ elev, data=seeds)
treat.slm <- lm(seedct ~ as.factor(treat), data=seeds)

aicc.slm <- data.frame(Model = c('Null','Age','Slope','Aspect','Elevation','Treatment'),
                       AICc = c(AICc(null),AICc(age.slm), AICc(slope.slm), AICc(asp.slm), 
                                AICc(elev.slm), AICc(treat.slm)))
aicc.slm[order(aicc.slm$AICc),]

#Two Predictors
agslope.slm <- lm(seedct ~ age + slope, data=seeds)
agasp.slm <- lm(seedct ~ age + aspect, data=seeds)
agelev.slm <- lm(seedct ~ age + elev, data=seeds)
agtreat.slm <- lm(seedct ~ age + as.factor(treat), data=seeds)

aicc.slm <- rbind(aicc.slm, data.frame(
  Model = c('Age + Slope', 'Age + Aspect', 'Age + Elevation', 'Age + Treatment'),
  AICc = c(c(AICc(agslope.slm), AICc(agasp.slm), 
             AICc(agelev.slm), AICc(agtreat.slm)))
))
aicc.slm[order(aicc.slm$AICc),]

#Three Predictors
agaspslope.slm <- lm(seedct ~ age + slope + aspect, data=seeds)
agaspelev.slm <- lm(seedct ~ age + slope + elev, data=seeds)
agasptreat.slm <- lm(seedct ~ age + slope + treat, data=seeds)

aicc.slm <- rbind(aicc.slm, data.frame(
  Model = c('Age + Aspect + Slope', 'Age + Aspect + Elevation', 
            'Age + Aspect + Treatment'),
  AICc = c(c(AICc(agaspslope.slm), AICc(agaspelev.slm), 
             AICc(agasptreat.slm)))
))
aicc.slm[order(aicc.slm$AICc),]

#Best model: Age + Aspect as predictors
par(mfrow=c(1,2))
plot(seedct ~ age, dat=seeds, pch=20, main='Seeds vs. Age', xlab='Stand Age (yrs)',
     ylab='Seed Count')
plot(seedct ~ aspect, dat=seeds, pch=20, main='Seeds vs. Aspect', xlab='Aspect (degrees)',
     ylab='Seed Count')

#Explore best model (seedct ~ age)
par(mfrow=c(2,2))
plot(agasp.slm, pch=20)
shapiro.test(residuals(agasp.slm))

#Cook's Distance
cd.slm <- cooks.distance(agasp.slm)

par(mfrow=c(1,2))
plot(cd.slm, type="h", lend=1, lwd=6, main="Cooks distance", 
     ylab="Cooks Distance", xlab="Observation number")
abline(h=c(3,2,1)*mean(cd.slm), col=c("red", "orange", "blue"), lty=2, lwd=2)
legend("topright", col=c("red", "orange", "blue"), lty=2, lwd=2,
       legend=c("3*mean", "2*mean", "1*mean"), bty="n")

#DFFITS
df.slm <- dffits(agasp.slm)
df.crit <- 2*sqrt(2/nrow(seeds))


plot(df.slm, type="h", lend=1, lwd=4, main="DFFITS",
     xlab="Observation number", ylab="DFFITS")
abline(h=c(-1,0,1)*df.crit, col=c("red", "black", "red"), lty=2, lwd=2)
legend("topright", col=c("red", "black", "red"), lty=2, lwd=2,
       legend=c("-crit", "0", "+crit"), bty="n")

seeds[44,c('stand','subplot','seedct')]
#Influential Points: 44

#Transforms

#Transforming the response
par(mfrow=c(1,1))
boxcox(agasp.slm)
abline(v=c(0,0.25,0.5), col=c("red", "blue", "orange"), lwd=2)
legend("bottomright", col=c("red", "blue", "orange"), lwd=2, 
       legend=c("Log", "Fourth-root", "Square-root"), bty="n")

#Log transform the response
seeds$log.seedct <- log(seeds$seedct)

log.mr <- lm(log.seedct ~ age + aspect, data=seeds)

par(mfrow=c(1,2))
plot(seedct ~ age, data=seeds, 
     pch=20, ylab="Seed Count", 
     xlab="Age (years)", main="Original Data")
abline(agasp.slm, col='red')

plot(log.seedct ~ age, data=seeds, 
     pch=20, ylab="Log(Seed Count)", 
     xlab="Age (years)", main="Transformed Response")
abline(log.slm, col='red')

# Transforming the predictor

#Age
par(mfrow=c(2,2))
plot(seedct ~ age, dat=seeds, pch=20, main='Original', xlab='Stand Age (yrs)',
     ylab='Seed Count')
abline(age.slm, col='red')

sqrtage.slm <- lm(seedct ~ I(age^0.5), data=seeds)
plot(seedct ~ sqrt(age), data=seeds, type="p", 
     pch=20, ylab="Seed Count", 
     xlab="sqrt(Age (years))", main="Sqrt")
abline(sqrtage.slm, col='red')

logage.slm <- lm(seedct ~ I(log(age)), data=seeds)
plot(seedct ~ log(age), data=seeds, type="p", 
     pch=20, ylab="Seed Count", 
     xlab="log(Age (years))", main="Log")
abline(logage.slm, col='red')

sqrdage.slm <- lm(seedct ~ I(age^2), data=seeds)
plot(seedct ~ age^2, data=seeds, type="p", 
     pch=20, ylab="Seed Count", 
     xlab="(Age (years))^2", main="Squared")
abline(sqrdage.slm, col='red')
#Not much difference; if any, sqrt

#Aspect
par(mfrow=c(2,2))
plot(seedct ~ aspect, dat=seeds, pch=20, main='Original', xlab='Aspect (degrees)',
     ylab='Seed Count')
abline(asp.slm, col='red')

sqrtasp.slm <- lm(seedct ~ I(aspect^0.5), data=seeds)
plot(seedct ~ sqrt(aspect), data=seeds, type="p", 
     pch=20, ylab="Seed Count", 
     xlab="sqrt(Aspect (degrees))", main="Sqrt")
abline(sqrtasp.slm, col='red')

logasp.slm <- lm(seedct ~ I(log(aspect)), data=seeds)
plot(seedct ~ log(aspect), data=seeds, type="p", 
     pch=20, ylab="Seed Count", 
     xlab="log(Aspect (degrees))", main="Log")
abline(logasp.slm, col='red')

sqrdasp.slm <- lm(seedct ~ I(aspect^2), data=seeds)
plot(seedct ~ aspect^2, data=seeds, type="p", 
     pch=20, ylab="Seed Count", 
     xlab="(Aspect (degrees))^2", main="Squared")
abline(sqrdasp.slm, col='red')
#No difference

#Check diagnostic differences with transformation
par(mfrow=c(2, 2))
plot(agasp.slm, pch=20, main='Original')
plot(log.slm, pch=20, main='Transformed')

shapiro.test(residuals(logseed.slm))

#Multicollinearity
cor(seeds$age, seeds$aspect)
pairs(seeds[,c(8, 9)], pch=20, main='Pairs Plot')

# Log-transformed models
log.full <- lm(log.seedct ~ age + slope + aspect + elev + treat, data=seeds,
               na.action="na.fail")
dredge(log.full)

# Multiple Regression Log Model
summary(log.mr)
confint(log.mr)

par(mfrow=c(2, 2))
plot(log.mr, pch=20)

age.seq <- seq(from=min(seeds$age), to=max(seeds$age), length.out=100)
logcon.pred <- data.frame(age=age.seq, aspect=mean(seeds$aspect),treat="CON")
logthin.pred <- data.frame(age=age.seq, aspect=mean(seeds$aspect), treat="THIN")
logpred.dat <- rbind(logcon.pred, logthin.pred)

withCI <- data.frame(predict(log.mr, newdata=logpred.dat, interval='confidence'))

pred.dat <- data.frame(age=age.seq, aspect=mean(seeds$aspect),
                       treat=pred.dat$treat, fit=withCI$fit,
                       lwr=withCI$lwr, upr=withCI$upr)

par(mfrow=c(1, 2))
crPlots(log.mr, variable='age', pch=20)
crPlots(log.mr, variable='aspect', pch=20)

# Polynomials
poly.all <- lm(log.seedct ~ age + I(age^2) + aspect + I(aspect^2), data=seeds,
              na.action="na.fail")
dredge(poly.all)

#Best model: age, age^2
#Close second: age, age^2, aspect^2
#Aspect terms aren't steadily in the best models, so just use age, age^2

log.poly <- lm(log.seedct ~ age + I(age^2), data=seeds)
summary(log.poly)
AICc(log.poly)
confint(log.poly)

par(mfrow=c(2,2))
plot(log.poly, pch=20)
shapiro.test(residuals(log.poly))

# Poissons
poi.all <- glm(seedct ~ age + aspect + treat + slope + elev,
               data=seeds, family=poisson(link="log"), na.action="na.fail")
dredge(poi.all)

# Best: age + aspect + slope + treat
poi <- glm(seedct ~ age + aspect + slope + treat, data=seeds,
           family=poisson(link="log"))
summary(poi)

#Poisson diagnostics
dispersiontest(poi)

cd.poi <- cooks.distance(poi)

par(mfrow=c(1,1))
plot(cd.poi, type="h", lend=1, lwd=6, main="Cooks distance", 
     ylab="Cooks Distance", xlab="Observation number")
abline(h=mean(cd.poi), col='red', lty=2, lwd=2)
legend("topright", col='red', lty=2, lwd=2,
       legend="1*mean", bty="n")
#Outliers: 16, 21, 22, 34, 42, 44

#Residuals vs. fitted
poi.res <- residuals(poi, type="deviance")
poi.fit <- predict(poi, type="link")

plot(poi.res ~ poi.fit, type="p", pch=20, ylab="Residuals", xlab="Fitted")
abline(h=0, lty=2)

#Goodness of Fit Test
pchisq(poi$deviance, df=poi$df.residual, lower.tail=F)
#Reject H0 that this is a good fit


# Negative binomial since both were over/under dispersed
nb.all <- glm.nb(seedct ~ age + I(age^2) + aspect + treat + slope + elev,
               data=seeds, na.action="na.fail")
dredge(nb.all) #doesn't work well for this model type - do by hand

#Forward stepwise model selection
#One predictor
nb.age <- glm.nb(seedct ~ age, data=seeds)
nb.asp <- glm.nb(seedct ~ aspect, data=seeds)
nb.slope <- glm.nb(seedct ~ slope, data=seeds)
nb.elev <- glm.nb(seedct ~ elev, data=seeds)
nb.treat <- glm.nb(seedct ~ treat, data=seeds)

aicc.nb <- data.frame(Model = c('Age','Aspect', 'Slope','Elevation','Treatment'),
                       AICc = c(AICc(nb.age), AICc(nb.asp), AICc(nb.slope), 
                                AICc(nb.elev), AICc(nb.treat)))
aicc.nb[order(aicc.nb$AICc),]
#Best NB: age

#Two predictors
nb.agasp <- glm.nb(seedct ~ age + aspect, data=seeds)
nb.agslope <- glm.nb(seedct ~ age + slope, data=seeds)
nb.agelev <- glm.nb(seedct ~ age + elev, data=seeds)
nb.agtreat <- glm.nb(seedct ~ age + treat, data=seeds)

aicc.nb <- rbind(aicc.nb, data.frame(
  Model = c('Age + Aspect', 'Age + Slope', 
            'Age + Elev', 'Age + Treatment'),
  AICc = c(c(AICc(nb.agasp), AICc(nb.agslope),
             AICc(nb.agelev), AICc(nb.agtreat)))
))
aicc.nb[order(aicc.nb$AICc),]
#Best NB: age + aspect

#Three predictors
nb.agaspslope <- glm.nb(seedct ~ age + aspect + slope, data=seeds)
nb.agaspelev <- glm.nb(seedct ~ age + aspect + elev, data=seeds)
nb.agasptreat <- glm.nb(seedct ~ age + aspect + treat, data=seeds)

aicc.nb <- rbind(aicc.nb, data.frame(
  Model = c('Age + Asp + Slope', 'Age + Asp + Elev', 'Age + Asp + Treatment'),
  AICc = c(c(AICc(nb.agaspslope), AICc(nb.agaspelev), AICc(nb.agasptreat)))
))
aicc.nb[order(aicc.nb$AICc),]
#Best NB: Age + Asp + Treat

#Four Predictors
nb.agasptrslope <- glm.nb(seedct ~ age + aspect + treat + slope, data=seeds)
#Model error, 'algorithm did not converge'
#Use 3 predictor model

#Explore age + asp + treatment model
nb <- nb.agasptreat
summary(nb)
confint(nb)

AICc(poi) - AICc(nb)
data.frame(Model=c('Polynomial', 'Poisson', 'NB'),
           AICc=c(AICc(log.poly), AICc(poi), AICc(nb)))

#NB Goodness of Fit
pchisq(nb$deviance, df=nb$df.residual, lower.tail=F)
#Fail to reject H0 that this is a good fit

#NB Pearson's Goodness of Fit
fit_mu <- predict(nb.ag2asptreat, type="response")
(X2 <- sum(((seeds$seedct - fit_mu)^2)/fit_mu))

pchisq(X2, df=nb.ag2asptreat$df.residual, lower.tail=F)
#Reject H0 that this is a good fit


# examine mean:variance relationship
vals <- seq(from=min(fitted(nb)), to=max(fitted(nb)), 
               length.out=100)

nb.var <- vals + (vals^2)/nb$theta 
poi.var <- vals

par(mfrow=c(1,1))
plot(nb.var ~ vals, type="l", lwd=2, main='Poisson & NB Variance vs. Mean',
     ylab="Variance", xlab="Mean")
lines(poi.var ~ vals, type="l", col=2, lwd=2)
legend("topleft",col=c(1,2), legend=c("NB", "Poisson"), lwd=2)

# Likelihood Ratio Test for treatment intercepts
lmtest::lrtest(nb, nb.agasptreat)
#Treatment intercepts aren't valid inclusion

#Plot Negative Binomial
#Hold aspect at mean
range(seeds$age)
agepred.dat <- rbind(data.frame(age=seq(from=15, to=85, length.out=100), aspect=mean(seeds$aspect),
                             treat="CON"),
                  data.frame(age=seq(from=15, to=85, length.out=100), aspect=mean(seeds$aspect),
                             treat="THIN"))
agepred.dat$treat <- factor(agepred.dat$treat, levels=c("CON", "THIN"))

# Predictions on link scale
pred.age <- predict(nb, newdata=agepred.dat, se.fit=T)

# 95% CI + Conversion
agepred.dat$fit <- exp(pred.age$fit)
agepred.dat$lwr <- exp(pred.age$fit - 1.96*pred.age$se.fit)
agepred.dat$upr <- exp(pred.age$fit + 1.96*pred.age$se.fit)

head(agepred.dat)

#Hold age at mean
range(seeds$aspect)
asppred.dat <- rbind(data.frame(aspect=seq(from=20, to=352, length.out=100), age=mean(seeds$age),
                               treat="CON"),
                    data.frame(aspect=seq(from=20, to=352, length.out=100), age=mean(seeds$age),
                               treat="THIN"))
asppred.dat$treat <- factor(asppred.dat$treat, levels=c("CON", "THIN"))

# Predictions on link scale
pred.asp <- predict(nb, newdata=asppred.dat, se.fit=T)

# 95% CI + Conversion
asppred.dat$fit <- exp(pred.asp$fit)
asppred.dat$lwr <- exp(pred.asp$fit - 1.96*pred.asp$se.fit)
asppred.dat$upr <- exp(pred.asp$fit + 1.96*pred.asp$se.fit)

head(asppred.dat)

# Plot
par(mfrow=c(1,2))
plot(seedct ~ age, data=seeds, type="p", pch=20, main="Negative Binomial Model: Seeds vs. Age",
     ylab="Seed Count", xlab="Age (years)", col=as.numeric(seeds$treat))
legend("topright", col=1:2, pch=20, legend=c("Control", "Thin"), bty='n')

lines(fit ~ age, data=agepred.dat[agepred.dat$treat=="CON",], type="l", col=1, lwd=2)
lines(lwr ~ age, data=agepred.dat[agepred.dat$treat=="CON",], type="l", 
      col=rgb(0,0,0,80, maxColorValue=255), lwd=2, lty=2)
lines(upr ~ age, data=agepred.dat[agepred.dat$treat=="CON",], type="l",
      col=rgb(0,0,0,80, maxColorValue=255), lwd=2, lty=2)

lines(fit ~ age, data=agepred.dat[agepred.dat$treat=="THIN",], type="l", col=2, lwd=2)
lines(lwr ~ age, data=agepred.dat[agepred.dat$treat=="THIN",], type="l",
      col=rgb(255,0,0,80, maxColorValue=255), lwd=2, lty=2)
lines(upr ~ age, data=agepred.dat[agepred.dat$treat=="THIN",], type="l",
      col=rgb(255,0,0,80, maxColorValue=255), lwd=2, lty=2)

plot(seedct ~ aspect, data=seeds, type="p", pch=20, main="Negative Binomial Model: Seeds vs. Aspect",
     ylab="Seed Count", xlab="Aspect (degrees)", col=as.numeric(seeds$treat))
legend("topleft", col=1:2, pch=20, legend=c("Control", "Thin"), bty='n')

lines(fit ~ aspect, data=asppred.dat[asppred.dat$treat=="CON",], type="l", col=1, lwd=2)
lines(lwr ~ aspect, data=asppred.dat[asppred.dat$treat=="CON",], type="l", 
      col=rgb(0,0,0,80, maxColorValue=255), lwd=2, lty=2)
lines(upr ~ aspect, data=asppred.dat[asppred.dat$treat=="CON",], type="l",
      col=rgb(0,0,0,80, maxColorValue=255), lwd=2, lty=2)

lines(fit ~ aspect, data=asppred.dat[asppred.dat$treat=="THIN",], type="l", col=2, lwd=2)
lines(lwr ~ aspect, data=asppred.dat[asppred.dat$treat=="THIN",], type="l",
      col=rgb(255,0,0,80, maxColorValue=255), lwd=2, lty=2)
lines(upr ~ aspect, data=asppred.dat[asppred.dat$treat=="THIN",], type="l",
      col=rgb(255,0,0,80, maxColorValue=255), lwd=2, lty=2)

# Random effects: plot and subplot
age.re <- lmer(log.seedct ~ age + (age|stand), data=seeds)
age.re <- lmer(log.seedct ~ age + (age|subplot), data=seeds)
age.re <- lmer(log.seedct ~ age + (1|subplot), data=seeds)
# None work, likely because of collinarity between age and stand/subplot

age.re <- lmer(log.seedct ~ age + (1|stand), data=seeds)


