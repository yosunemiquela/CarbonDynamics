library("reshape")
library("ggplot2")
library("nlme")


#####################
#Load data#
##################### 
Spring60 <- load("FRI60_Spring.Rdata", .GlobalEnv)  
Summer60 <- load("FRI60_Summer.Rdata", .GlobalEnv)
FRI60Spring <- data.frame(FRI60Spring)
names(FRI60Spring)
FRI60Spring$Season <- 1
FRI60<-data.frame(rbind(FRI60Spring, FRI60Summer))
length(FRI60[, 1])
str(FRI60)
FRI60$Season


Spring100<-load("FRI100_Spring.Rdata", .GlobalEnv)
FRI100Spring <- data.frame(FRI100Spring)
FRI100Spring$Season <- 1
Summer100<-load("FRI100_Summer.Rdata", .GlobalEnv)
FRI100<-data.frame(rbind(FRI100Spring, FRI100Summer))
length(FRI100[,1])
FRI100$Season


Spring150<-load("FRI150_Spring.Rdata", .GlobalEnv)
FRI150Spring <- data.frame(FRI150Spring)
FRI150Spring$Season <- 1
Summer150<-load("FRI150_Summer.Rdata", .GlobalEnv)
FRI150<-data.frame(rbind(FRI150Spring,FRI150Summer))
str(FRI150)
FRI150$Season


Spring300<-load("FRI300_Spring.Rdata", .GlobalEnv)
FRI300Spring <- data.frame(FRI300Spring)
FRI300Spring$Season <- 1
Summer300<-load("FRI300_Summer.Rdata", .GlobalEnv)
FRI300<-data.frame(rbind(FRI300Spring,FRI300Summer))
FRI300$Season


Spring700<-load("FRI700_Spring.Rdata", .GlobalEnv)
FRI700Spring <- data.frame(FRI700Spring)
FRI700Spring$Season <- 1
Summer700<-load("FRI700_Summer.Rdata", .GlobalEnv)
FRI700Summer <- data.frame(FRI700Summer)
FRI700Summer <- FRI700Summer[,-c(21,22,23)]
FRI700<-data.frame(rbind(FRI700Spring,FRI700Summer))
FRI700$Season

Spring1200<-load("FRI1200_Spring.Rdata", .GlobalEnv)
FRI1200Spring <- data.frame(FRI1200Spring)
FRI1200Spring$Season <- 1
Summer1200<-load("FRI1200_Summer.Rdata", .GlobalEnv)
FRI1200<-data.frame(rbind(FRI1200Spring,FRI1200Summer))


SpringNF<-load("FRINF_Spring.Rdata", .GlobalEnv)
FRINFSpring <- data.frame(FRINFSpring)
FRINFSpring$Season <- 1
SummerNF<-load("FRINF_Summer.Rdata", .GlobalEnv)
FRINF<-data.frame(rbind(FRINFSpring,FRINFSummer))
str(FRINF)
FRINF$Season

All<-rbind(FRI60,FRI100, FRI150,FRI300,FRI700,FRI1200,FRINF)
All$FireReturnInterval <- factor(All$FireReturnInterval)
levels(All$FireReturnInterval)[levels(All$FireReturnInterval)== "3e+17"] <- "NF"
levels(All$FireReturnInterval)[levels(All$FireReturnInterval)== "3e+17"] <- "NF"
length(All[,1])
head(All)
All$FireReturnInterval
as.factor(All$FireReturnInterval)
All$Season<-as.factor(All$Season)
as.factor(All$FireReturnInterval)
KgMg<-(0.001)
kghatogm2<-0.1
All[,5] <- with(All, All[,5]*KgMg)
All[,7:20] <- with(All, All[,7:20]*KgMg)
All$FireReturnInterval <- factor(All$FireReturnInterval,
                                 levels=c('60','100','150','300','700','1200','NF'), ordered=TRUE)
All$Organic<-(All$AGveryfast+All$AGslow)
All$Mineral<-(All$BGveryfast+All$BGslow)

All$WoodyDebris<-(All$Snags+All$SnagBranch+All$AGfast+All$AGMedium)
All$Structure<-ifelse(All$StandStructure>=1.7,'Irregular','Regular')
All$Structure<-as.factor(All$Structure)

#####################
#Statistical Analysis#
#####################
##Total Ecosystem C
mod.ecs <- lme(EcosystemCStock~FireReturnInterval+Season+
                  FireReturnInterval:Season, random=~1|Structure,
                  weights=varIdent(form=~1 | FireReturnInterval), data=All)
mod.ecs1 <- lme(EcosystemCStock~FireReturnInterval+Season, random=~1|Structure,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)

## AIC
loglikratio <- mod.ecs$logLik/mod.ecs1$logLik
## 
plot(residuals(mod.ecs, type="pearson")~All$FireReturnInterval, ylab="Pearson residuals", xlab="FireReturnInterval")
plot(residuals(mod.ecs, level=1, type="pearson")~fitted(mod.ecs, level=1), ylab="R?sidus de Pearson", xlab="Valeurs pr?dites", main="R?sidus vs valeurs pr?dites")
abline(h=0, lty=2)
anova(mod.ecs)
summary(mod.ecs)
##check relative variability of intercept with respect to the corresponding fixed effect (Pinheiro and Bates 2001, Section 4.3.2, p. 191)
(17.9)/abs(fixef(mod.ecs)[1])*100
ranef(mod.ecs)
fixef(mod.ecs)
coef(mod.ecs1)[2,]
coef(mod.ecs1)
Var_Random_effect <- as.numeric(VarCorr(mod.ecs))
varests <- as.numeric(VarCorr(mod.ecs)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)


##Total DOM soil C

mod.scs <- lme(SoilCStock~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)
mod.scs1 <- lme(SoilCStock~FireReturnInterval+Season, random=~1|Structure,
                weights=varIdent(form=~1 | FireReturnInterval), data=All)
## AIC
loglikratio <- mod.scs$logLik/mod.scs1$logLik
## 
anova(mod.scs)
summary(mod.scs)
varests <- as.numeric(VarCorr(mod.scs)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.scs)

##Biomass
ctrl <- lmeControl(opt='optim')
mod.bcs <- lme(BiomassLiveCStock~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,control=ctrl,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)

mod.bcs1 <- lme(BiomassLiveCStock~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)

## AIC
loglikratio <- mod.bcs$logLik/mod.bcs1$logLik
## 
anova(mod.bcs)
summary(mod.bcs)
varests <- as.numeric(VarCorr(mod.bcs)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.bcs)

###Organic
mod.org <- lme(Organic~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)
anova(mod.org)
summary(mod.org)
coef(mod.org)
varests <- as.numeric(VarCorr(mod.org)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.org)

###Mineral
mod.min<- lme(Mineral~FireReturnInterval+Season+FireReturnInterval*Season, 
              random=~1|Structure,control=ctrl,
              weights=varIdent(form=~1 |FireReturnInterval), data=All)
anova(mod.min)
summary(mod.min)
varests <- as.numeric(VarCorr(mod.min)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.min)

###Woody debris
mod.wood<- lme(WoodyDebris~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
                 weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.wood<- lme(WoodyDebris~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,
               data=All)
anova(mod.wood)
summary(mod.wood)
varests <- as.numeric(VarCorr(mod.wood)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.wood)




library(plyr)
library(ggplot2)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




head(All)

npp <- summarySE(All, measurevar="NPP", groupvars=c("FireReturnInterval"))
nep <- summarySE(All, measurevar="NEP", groupvars=c("FireReturnInterval"))
rh<- summarySE(All, measurevar="SoilRespiration", groupvars=c("FireReturnInterval"))
nbp <- summarySE(All, measurevar="NBP", groupvars=c("FireReturnInterval"))
ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval","Season"))
ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval"))
scs<- summarySE(All, measurevar="SoilCStock", groupvars=c("FireReturnInterval","Season"))
bcs<-summarySE(All, measurevar="BiomassLiveCStock", groupvars=c("FireReturnInterval"))
mscs<-summarySE(All, measurevar="Mineral", groupvars=c("FireReturnInterval","Season"))
mscs<-summarySE(All, measurevar="Mineral", groupvars=c("FireReturnInterval"))
ol<-summarySE(All, measurevar="Organic", groupvars=c("FireReturnInterval"))
wd<-summarySE(All, measurevar="WoodyDebris", groupvars=c("FireReturnInterval"))
