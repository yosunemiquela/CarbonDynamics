library("reshape")
library("ggplot2")
library("nlme")
library("plyr")
library("grid")


#####################
#Load data#
##################### 
Spring60 <- load("FRI60_Spring.Rdata", .GlobalEnv)  
Summer60 <- load("FRI60_Summer.Rdata", .GlobalEnv)
FRI60Spring <- data.frame(FRI60Spring)
names(FRI60Spring)
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
FRI700<-data.frame(rbind(FRI700Spring,FRI700Summer))
FRI700$Season

Spring1200<-load("FRI1200_Spring.Rdata", .GlobalEnv)
FRI1200Spring <- data.frame(FRI1200Spring)
FRI1200Spring$Season <- 1
Summer1200<-load("FRI1200_Summer.Rdata", .GlobalEnv)
FRI1200<-data.frame(rbind(FRI1200Spring,FRI1200Summer))
FRI1200$Season

SpringNF<-load("FRINF_Spring.Rdata", .GlobalEnv)
FRINFSpring <- data.frame(FRINFSpring)
FRINFSpring$Season <- 1
SummerNF<-load("FRINF_Summer.Rdata", .GlobalEnv)
FRINF<-data.frame(rbind(FRINFSpring,FRINFSummer))
str(FRINF)
FRINF$Season
FRINF$NBP <- FRINF$NEP 
FRINF <- FRINF[c(1,2,3,4,5,6,7,8,21,9,10,11,12,13,14,15,16,17,18,19,20)]

               
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
All[,7:21] <- with(All, All[,7:21]*KgMg)
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
ctrl <- lmeControl(opt='optim')

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
(14.6)/abs(fixef(mod.ecs)[1])*100
ranef(mod.ecs)
fixef(mod.ecs)
coef(mod.ecs)[2,]
coef(mod.ecs)
Var_Random_effect <- as.numeric(VarCorr(mod.ecs))
varests <- as.numeric(VarCorr(mod.ecs)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
#test significance of random effect
mod.ecsre <- lme(EcosystemCStock~FireReturnInterval+Season+
                 FireReturnInterval:Season, random=~1|Structure,
                , data=All, method="ML")
mod.ecsre2 <- lm(EcosystemCStock~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.ecsre)[1]-logLik(mod.ecsre2)[1]
p <- 1-pchisq(chi,df=1)

##Total DOM soil C

mod.scs <- lme(SoilCStock~FireReturnInterval+Season +
                 FireReturnInterval:Season,control=ctrl, random=~1|Structure,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)
mod.scs1 <- lme(SoilCStock~FireReturnInterval+Season, random=~1|Structure,
                weights=varIdent(form=~1 | FireReturnInterval), data=All)
## AIC
loglikratio <- mod.scs$logLik/mod.scs1$logLik
## 
anova(mod.scs)
summary(mod.scs)
varests <- as.numeric(VarCorr(mod.scs)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.scs)
#test significance of random effect
mod.scsre <- lme(SoilCStock~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.scsre2 <- lm(SoilCStock~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.scsre )[1]-logLik(mod.scsre2)[1]
p <- 1-pchisq(chi,df=1)

##Biomass

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
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.bcs)
#test significance of random effect
mod.bcsre <- lme(BiomassLiveCStock~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.bcsre2 <- lm(BiomassLiveCStock~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.bcsre )[1]-logLik(mod.bcsre2)[1]
p <- 1-pchisq(chi,df=1)

###Organic
mod.org <- lme(Organic~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,
                 weights=varIdent(form=~1 | FireReturnInterval), data=All)

mod.org1 <- lme(Organic~FireReturnInterval+Season, random=~1|Structure,
               weights=varIdent(form=~1 | FireReturnInterval), data=All)

## AIC
loglikratio <- mod.org$logLik/mod.org1$logLik
## 
anova(mod.org)
summary(mod.org)
coef(mod.org)
varests <- as.numeric(VarCorr(mod.org)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.org)
#test significance of random effect

mod.orgre <- lme(Organic~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.orgre2 <- lm(Organic~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.orgre)[1]-logLik(mod.orgre2)[1]
p <- 1-pchisq(chi,df=1)

###Mineral
mod.min<- lme(Mineral~FireReturnInterval+Season+FireReturnInterval*Season, 
              random=~1|Structure,control=ctrl,
              weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.min1<- lme(Mineral~FireReturnInterval+Season, 
              random=~1|Structure,control=ctrl,
              weights=varIdent(form=~1 |FireReturnInterval), data=All)

## AIC
loglikratio <- mod.min$logLik/mod.min1$logLik
## 
anova(mod.min)
summary(mod.min)
varests <- as.numeric(VarCorr(mod.min)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.min)
#test significance of random effect

mod.minre <- lme(Mineral~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.minre2 <- lm(Mineral~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.minre)[1]-logLik(mod.minre2)[1]
p <- 1-pchisq(chi,df=1)
###Woody debris
mod.wood<- lme(WoodyDebris~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
                 weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.wood1<- lme(WoodyDebris~FireReturnInterval+Season, random=~1|Structure,
               data=All)

## AIC
loglikratio <- mod.wood$logLik/mod.wood1$logLik
## 
anova(mod.wood)
summary(mod.wood)
varests <- as.numeric(VarCorr(mod.wood)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.wood)
#test significance of random effect

mod.woodre <- lme(WoodyDebris~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.woodre2 <- lm(WoodyDebris~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.woodre)[1]-logLik(mod.woodre2)[1]
p <- 1-pchisq(chi,df=1)

###NPP
mod.npp<- lme(NPP~FireReturnInterval+Season +
                 FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
               weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.npp1<- lme(NPP~FireReturnInterval+Season, random=~1|Structure,
                data=All)

## AIC
loglikratio <- mod.npp$logLik/mod.npp1$logLik
## 
anova(mod.npp)
summary(mod.npp)
varests <- as.numeric(VarCorr(mod.npp)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.npp)

#test significance of random effect

mod.nppre <- lme(NPP~FireReturnInterval+Season+
                    FireReturnInterval:Season, random=~1|Structure,
                  , data=All, method="ML")
mod.nppre2 <- lm(NPP~FireReturnInterval+Season+
                    FireReturnInterval:Season,
                  , data=All)

chi <- -2*logLik(mod.nppre )[1]-logLik(mod.nppre2)[1]
p <- 1-pchisq(chi,df=1)


###NEP
mod.nep<- lme(NEP~FireReturnInterval+Season +
                FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
              weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.nep1<- lme(NEP~FireReturnInterval+Season, random=~1|Structure,
               data=All)

## AIC
loglikratio <- mod.nep$logLik/mod.nep1$logLik
## 
anova(mod.nep)
summary(mod.nep)
varests <- as.numeric(VarCorr(mod.nep)[1:2])# vector of variance estimates
#variance explained by structure
ICC <- varests[1]/sum(varests)
coef(mod.nep)

#test significance of random effect

mod.nepre <- lme(NEP~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.nepre2 <- lm(NEP~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.nepre )[1]-logLik(mod.nepre2)[1]
p <- 1-pchisq(chi,df=1)


###NBP
mod.nbp<- lme(NBP~FireReturnInterval+Season +
                FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
              weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.nbp1<- lme(NBP~FireReturnInterval+Season, random=~1|Structure,
               data=All)

## AIC
loglikratio <- mod.nbp$logLik/mod.nbp1$logLik
## 
anova(mod.nbp)
summary(mod.nbp)
varests <- as.numeric(VarCorr(mod.nbp)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.nbp)

#test significance of random effect

mod.nbpre <- lme(NBP~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.nbpre2 <- lm(NBP~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.nbpre )[1]-logLik(mod.nbpre2)[1]
p <- 1-pchisq(chi,df=1)
###Rh
mod.rh<- lme(SoilRespiration~FireReturnInterval+Season +
                FireReturnInterval:Season, random=~1|Structure,,control=ctrl,
              weights=varIdent(form=~1 |FireReturnInterval), data=All)

mod.rh1<- lme(SoilRespiration~FireReturnInterval+Season, random=~1|Structure,
               data=All)

## AIC
loglikratio <- mod.nbp$logLik/mod.nbp1$logLik
## 
anova(mod.rh)
summary(mod.rh)
varests <- as.numeric(VarCorr(mod.rh)[1:2])# vector of variance estimates
ICC <- varests[1]/sum(varests)
coef(mod.rh)

#test significance of random effect

mod.rhre <- lme(SoilRespiration~FireReturnInterval+Season+
                   FireReturnInterval:Season, random=~1|Structure,
                 , data=All, method="ML")
mod.rhre2 <- lm(SoilRespiration~FireReturnInterval+Season+
                   FireReturnInterval:Season,
                 , data=All)

chi <- -2*logLik(mod.rhre )[1]-logLik(mod.rhre2 )[1]
p <- 1-pchisq(chi,df=1)


##
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



npp <- summarySE(All, measurevar="NPP", groupvars=c("FireReturnInterval"))
nep <- summarySE(All, measurevar="NEP", groupvars=c("FireReturnInterval"))
nbp <- summarySE(All, measurevar="NBP", groupvars=c("FireReturnInterval"))
rh<- summarySE(All, measurevar="SoilRespiration", groupvars=c("FireReturnInterval"))
nbp <- summarySE(All, measurevar="NBP", groupvars=c("FireReturnInterval","Season"))
ecs <- summarySE(All, measurevar="EcosystemCStock", groupvars=c("FireReturnInterval","Season"))
scs<- summarySE(All, measurevar="SoilCStock", groupvars=c("FireReturnInterval","Season"))
bcs<-summarySE(All, measurevar="BiomassLiveCStock", groupvars=c("FireReturnInterval","Season"))
mscs<-summarySE(All, measurevar="Mineral", groupvars=c("FireReturnInterval","Season"))
ol<-summarySE(All, measurevar="Organic", groupvars=c("FireReturnInterval","Season"))
wd<-summarySE(All, measurevar="WoodyDebris", groupvars=c("FireReturnInterval"))


# # # # # # # #
#Graphics
# # # # # # # #

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


my_grobA <- grobTree(textGrob("A)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobB <- grobTree(textGrob("B)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobC <- grobTree(textGrob("C)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobD <- grobTree(textGrob("D)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobE <- grobTree(textGrob("E)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=10, fontface="bold")))
my_grobF <- grobTree(textGrob("F)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=10, fontface="bold")))




pd <- position_dodge(0.1)
pn <-ggplot(npp, aes(x = FireReturnInterval, y = NPP),
            ylim = c(3.0,4.0)) + labs(x="Fire Return Interval",y="NPP (MgC/ha*yr)")+
  geom_errorbar(aes(ymin = NPP-ci, ymax=NPP+ci), colour="black", width=.1, position=pd)
pnp <- pn + geom_line(position=pd) + geom_point(size=2, colour="black") 
pnpp <-pnp + theme(axis.text = element_text(colour = "black", size = 12),
                  axis.title = element_text(size = 12, color = "black"))
pnpp1 <- pnpp + theme_bw()
pnpp2 <- pnpp1+theme(legend.position="none")
pnpp2 <- pnpp2+annotation_custom(my_grobA)
pnpp3 <- pnpp2+scale_y_continuous(limits=c(3.0,4.0))


pne <- ggplot(nep, aes(x=FireReturnInterval, y=NEP, colour=FireReturnInterval)) +
  labs(x ="Fire Return Interval",y ="NEP (MgC/ha*yr)")+
  geom_errorbar(aes(ymin = NEP-ci, ymax = NEP+ci), colour ="black", width=.1, position=pd)
pnep <- pne+geom_line(position=pd) + geom_point(size =2, colour="black") +
  geom_hline(yintercept = 0)
pnep1 <- pnep + theme(axis.text = element_text(colour = "black", size = 12),
                    axis.title = element_text(size = 12, color = "black"))
pnep1 <- pnep1 + theme_bw()
pnep2 <- pnep1+theme(legend.position="none")
pnep2 <- pnep2+annotation_custom(my_grobC)


pnb <- ggplot(nbp, aes(x = FireReturnInterval, y = NBP, colour = FireReturnInterval)) + 
  labs(x="Fire Return Interval",y="NBP (MgC/ha*yr)") +
  geom_errorbar(aes(ymin = NBP-ci, ymax=NBP+ci), colour = "black", width=.1, position=pd)
pnbp <- pnb+geom_line(position = pd) + geom_point(size=2, colour="black") + 
  geom_hline(yintercept = 0)
pnbp1 <- pnbp + theme(axis.text = element_text(colour = "black", size = 12),
                    axis.title = element_text(size = 12, color = "black"))
pnbp1 <- pnbp1 + theme_bw()
pnbp2 <- pnbp1+theme(legend.position="none")
pnbp2 <- pnbp2+annotation_custom(my_grobD)



pso <- ggplot(rh, aes(x=FireReturnInterval, y=SoilRespiration, colour=FireReturnInterval), ylim = c(2.7,4.0))+labs(x ="Fire Return Interval",y ="Heterotrophic respiration (MgC/ha*yr)")+ geom_errorbar(aes(ymin=SoilRespiration-ci, ymax=SoilRespiration+ci), colour="black", 
                width=.1)
psoi <- pso+geom_line() + geom_point(size=2, colour="black") 
psoil <- psoi + theme(axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(size = 12, color = "black"))
psoil1 <- psoil + theme_bw()
psoil1 <- psoil1 + theme(legend.position ="none")
psoil1 <- psoil1 + annotation_custom(my_grobB)
psoil2 <- psoil1+scale_y_continuous(limits=c(3.0,4.0))

fluxes <- multiplot(pnpp3,pnep2,psoil2,pnbp2,cols=2)


black.12.text <- element_text(face="plain",color = "black", size =12)
black.bold.text <- element_text(face = "plain", color = "black", size=14)

##Ecosystem C stock
pd <- position_dodge(0.1)
p <- ggplot(ecs, aes(x=FireReturnInterval, y=EcosystemCStock, group=Season)) + ylim(200,300) +
  labs(x="Fire Return Interval",y="Total ecosystem C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=EcosystemCStock-ci, ymax=EcosystemCStock+ci), colour="black", width=.1, position=pd)
p1 <- p+geom_point(aes(shape=Season), size=2,position=pd)
p2 <- p1+ scale_shape_manual(values = c(19,5))+
annotate("text", x=1.3, y=240, label= "1", size=4) +
annotate("text", x = 1.3, y=232, label = "2", size=4)
p2<-p2+ theme_bw()
p2a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p3<-p2a+annotation_custom(my_grobA)


pd <- position_dodge(0.1)
p <- ggplot(bcs, aes(x=FireReturnInterval, y=BiomassLiveCStock, group=Season)) + ylim(35,60) +
  labs(x="Fire Return Interval",y="Live Biomass C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=BiomassLiveCStock-ci, ymax=BiomassLiveCStock+ci), colour="black", width=.1, position=pd)
p1 <- p+geom_point(aes(shape=Season),size=2,position=pd) 
p6a <- p1+ scale_shape_manual(values = c(19,5))+
  annotate("text", x=1.3, y=43, label= "1", size=4) +
  annotate("text", x = 1.3, y=41, label = "2", size=4)
p6a1 <- p6a + theme_bw()
p6ab <- p6a1+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p6 <- p6ab+annotation_custom(my_grobB)


pd <- position_dodge(0.1)
p <- ggplot(ol, aes(x=FireReturnInterval, y=Organic, group=Season)) + ylim(40,55) +
labs(x="Fire Return Interval",y="Organic Layer C Stock (Mg C ha-1)")+
geom_errorbar(aes(ymin=Organic-se, ymax=Organic+se), colour="black", width=.1)
p1 <- p+geom_point(aes(shape=Season),position=pd, size=2) 
p6a <- p1+ scale_shape_manual(values = c(19,5))+
  annotate("text", x=1.3, y=45, label= "1", size=4) +
  annotate("text", x = 1.3, y=44, label = "2", size=4)
p2 <- p6a+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p7<-p7a+annotation_custom(my_grobC)


##Woody debris stock
pd <- position_dodge(0.1)
p<-ggplot(wd, aes(x=FireReturnInterval, y=WoodyDebris))+ ylim(45,55)  +
  labs(x="Fire Return Interval",y="Woody debris C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=WoodyDebris-ci, ymax=WoodyDebris+ci), colour="black", width=.1, position=pd)
p1<-p+geom_point(position=pd,size=2) 
p2<-p1 + theme_bw()
p9a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p9<-p9a+annotation_custom(my_grobD)


##Mineral stock
pd <- position_dodge(0.1)
p<-ggplot(mscs, aes(x=FireReturnInterval, y=Mineral,  group=Season)) + ylim(90, 126) +
  labs(x="Fire Return Interval",y="Mineral Soil C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=Mineral-ci, ymax=Mineral+ci), colour="black", width=.1, position=pd)
p1<-p+geom_point(aes(shape=Season),position=pd,size=2) +
  scale_shape_manual(values=c(19,5))+
  annotate("text", x=1.3, y=102, label= "1", size=4) +
  annotate("text", x = 1.3, y=98, label = "2", size=4)
p2<-p1 + theme_bw()
p8a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p8<-p8a+annotation_custom(my_grobE)


pd <- position_dodge(0.1)
p4<-ggplot(scs, aes(x=FireReturnInterval, y=SoilCStock, group=Season)) + ylim(180, 240) +
  labs(x="Fire Return Interval",y="DOM C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=SoilCStock-ci, ymax=SoilCStock+ci), colour="black", width=.1, position=pd)
p4a<-p4 +geom_point(aes(shape=Season),position=pd,size=2) +
  scale_shape_manual(values=c(19, 5))+
  annotate("text", x=1.3, y=197, label= "1", size=4) +
  annotate("text", x = 1.3, y=191, label = "2", size=4)
p4ab<-p4a+theme_bw()
p5<-p4ab + theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p5<-p5+annotation_custom(my_grobF)


a<-multiplot(p3,p9,p6,p8,p7,p5, cols=3)


##########################################################
#Obtain descriptive statistics by FRI, Season and Structure#
##########################################################

summaries <- function (fri, season, structure){
 tmp <- All[which(All$FireReturnInterval==fri & All$Season == season & All$Structure ==structure), ]
    listmean <- lapply(as.list(tmp[,5:24]),mean)
    listsd <- lapply(as.list(tmp[,5:24]),sd)
    l <- length(tmp[,1])
    return(c(l=l,listmean=listmean,listsd=listsd))
 }


summariesoverall <- function (fri){
  tmp <- All[which(All$FireReturnInterval==fri), ]
  listmean <- lapply(as.list(tmp[,5:24]),mean)
  listsd <- lapply(as.list(tmp[,5:24]),sd)
  l <- length(tmp[,1])
  return(c(l=l,listmean=listmean,listsd=listsd))
}

# Do this for levels of interest
s <- summaries(fri= "NF",season=2,structure="Irregular")
overallmean <- summariesoverall(fri="NF")


##########################################################
#Supplementary material#
##########################################################
anova(lm(INT~SprSummer, data=Intensities))#F=5.84,p=0.01
anova(lm(ISec~INT*SprSummer, data=Intensities))#F=12.9 p<0.00001 
#strong difference between Spring and Summer, and the relation with intensity varies with season

par(mfrow=c(1,2))
plot(SpringFires$DR~SpringFires$INT, xlab="Intensity (kW/m2)", ylab="DC", main="Spring",ylim=c(0,500),xlim=c(0,35000) )
abline(lm(SpringFires$DR~SpringFires$INT),col = "red")
plot(SummerFires$DR~SummerFires$INT, xlab="Intensity (kW/m2)", ylab="DC", main="Summer",ylim=c(0,500),xlim=c(0,35000))
abline(lm(SummerFires$DR~SummerFires$INT),col = "red")

##Load data on C emissions trends among FRI

load("~/Desktop/ManuscriptEcosystems/Final2/NewSimulationsCarbonCombustion/60/FNBP60.RData")
load("~/Desktop/ManuscriptEcosystems/Final2/NewSimulationsCarbonCombustion/100/FNBP100.RData")
load("~/Desktop/ManuscriptEcosystems/Final2/NewSimulationsCarbonCombustion/150/FNBP150.RData")
load("~/Desktop/ManuscriptEcosystems/Final2/NewSimulationsCarbonCombustion/300/FNBP300.RData")
load("~/Desktop/ManuscriptEcosystems/Final2/NewSimulationsCarbonCombustion/700/NBP700.RData")
load("~/Desktop/ManuscriptEcosystems/Final2/NewSimulationsCarbonCombustion/1200/NBP1200.RData")


NBP60 <- data.frame(NBP60)
NBP100 <- data.frame(NBP100)
NBP150 <- data.frame(NBP150)
NBP300 <- data.frame(NBP300)
NBP700 <- data.frame(NBP700)
NBP1200 <- data.frame(NBP1200)


Emissions60 <- colMeans(NBP60*KgMg)
Emissions100 <- colMeans(NBP100*KgMg)
Emissions150 <- colMeans(NBP150*KgMg)
Emissions300 <- colMeans(NBP300*KgMg)
Emissions700 <- colMeans(NBP700*KgMg)
Emissions1200 <- colMeans(NBP1200*KgMg)

plot(Emissions60[20:4800],type="l",col="red", xlab="Simulation time", ylab="Total emissions (MgC/ha*yr)",ylim=c(-0.7,1), xlim=c(0,5000))
lines(Emissions100[20:4800],col="orange")
lines(Emissions150[20:4800],col="green")
lines(Emissions300[20:4800],col="blue")
lines(Emissions700[20:4800],col="purple")
lines(Emissions1200[20:4800],col="black")
abline(h = 0)
legend(500,0.9, legend=c("60","100","150","300","700","1200"),
horiz=T,lty=c(1,1,1,1,1),bty = "n",col=c("red","orange","green","blue","purple","black")) 




sn<-summarySE(All, measurevar="Snags", groupvars=c("FireReturnInterval"))
sb<-summarySE(All, measurevar="SnagBranch", groupvars=c("FireReturnInterval"))
am<-summarySE(All, measurevar="AGMedium", groupvars=c("FireReturnInterval"))
af<-summarySE(All, measurevar="AGfast", groupvars=c("FireReturnInterval"))
avf<-summarySE(All, measurevar="AGveryfast", groupvars=c("FireReturnInterval"))
as<-summarySE(All, measurevar="AGslow", groupvars=c("FireReturnInterval"))
bgvf<-summarySE(All, measurevar="BGveryfast", groupvars=c("FireReturnInterval"))
bgf<-summarySE(All, measurevar="BGfast", groupvars=c("FireReturnInterval"))
bgs<-summarySE(All, measurevar="BGslow", groupvars=c("FireReturnInterval"))



my_grobA = grobTree(textGrob("A)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobB = grobTree(textGrob("B)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobC = grobTree(textGrob("C)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobD = grobTree(textGrob("D)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobE = grobTree(textGrob("E)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobF = grobTree(textGrob("F)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobG = grobTree(textGrob("G)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobH = grobTree(textGrob("H)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))
my_grobI = grobTree(textGrob("I)", x=0.01,  y=0.95, hjust=0,
                             gp=gpar(col="black", fontsize=12, fontface="bold")))

pd <- position_dodge(0.1)
p<-ggplot(sn, aes(x=FireReturnInterval, y=Snags)) + ylim(5,7.5) +
  labs(x="Fire Return Interval",y="Snags (MgC/ha)")+
  geom_errorbar(aes(ymin=Snags-ci, ymax=Snags+ci), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p7<-p7a+annotation_custom(my_grobA)


pd <- position_dodge(0.1)
p<-ggplot(sb, aes(x=FireReturnInterval, y=SnagBranch)) + ylim(1.5,2.7) +
  labs(x="Fire Return Interval",y="SnagBranch (MgC/ha)")+
  geom_errorbar(aes(ymin=SnagBranch-ci, ymax=SnagBranch+ci), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p8<-p7a+annotation_custom(my_grobB)


pd <- position_dodge(0.1)
p<-ggplot(am, aes(x=FireReturnInterval, y=AGMedium)) + ylim(27,34) +
  labs(x="Fire Return Interval",y="AG Medium (MgC/ha)")+
  geom_errorbar(aes(ymin=AGMedium-se, ymax=AGMedium+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p9<-p7a+annotation_custom(my_grobC)

pd <- position_dodge(0.1)
p<-ggplot(af, aes(x=FireReturnInterval, y=AGfast)) + ylim(9,12) +
  labs(x="Fire Return Interval",y="AG fast (MgC/ha)")+
  geom_errorbar(aes(ymin=AGfast-se, ymax=AGfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p10<-p7a+annotation_custom(my_grobD)


pd <- position_dodge(0.1)
p<-ggplot(avf, aes(x=FireReturnInterval, y=AGveryfast)) + ylim(7,10) +
  labs(x="Fire Return Interval",y="AG very fast (MgC/ha)")+
  geom_errorbar(aes(ymin=AGveryfast-se, ymax=AGveryfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p11<-p7a+annotation_custom(my_grobE)


pd <- position_dodge(0.1)
p<-ggplot(as, aes(x=FireReturnInterval, y=AGslow)) + ylim(35,44) +
  labs(x="Fire Return Interval",y="AG slow (MgC/ha)")+
  geom_errorbar(aes(ymin=AGslow-se, ymax=AGslow+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p12<-p7a+annotation_custom(my_grobF)


pd <- position_dodge(0.1)
p<-ggplot(bgvf, aes(x=FireReturnInterval, y=BGveryfast)) + ylim(1,2) +
  labs(x="Fire Return Interval",y="BG very fast (MgC/ha)")+
  geom_errorbar(aes(ymin=BGveryfast-se, ymax=BGveryfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p13<-p7a+annotation_custom(my_grobG)


pd <- position_dodge(0.1)
p<-ggplot(bgf, aes(x=FireReturnInterval, y=BGfast)) + ylim(3,5) +
  labs(x="Fire Return Interval",y="BG fast (MgC/ha)")+
  geom_errorbar(aes(ymin=BGfast-se, ymax=BGfast+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p14<-p7a+annotation_custom(my_grobH)



pd <- position_dodge(0.1)
p<-ggplot(bgs, aes(x=FireReturnInterval, y=BGslow)) + ylim(90,122) +
  labs(x="Fire Return Interval",y="BG slow (MgC/ha)")+
  geom_errorbar(aes(ymin=BGslow-se, ymax=BGslow+se), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p15<-p7a+annotation_custom(my_grobI)


nnn<-multiplot(p7,p10,p13,p8,p11,p14,p9,p12,p15,cols=3)

#####################################
#Load data on fire regime attributes#
##################################### 
SpringNF60 <- load("FRINF60Extra_Spring.Rdata", .GlobalEnv)
SpringBAL60 <- load("FRIBAL60Extra_Spring.Rdata", .GlobalEnv)  
SpringDC60 <- load("FRIDC60Extra_Spring.Rdata", .GlobalEnv)
SpringINT60 <- load("FRIINT60Extra_Spring.Rdata", .GlobalEnv)
SpringSCP60 <- load("FRISCP60Extra_Spring.Rdata", .GlobalEnv)
SpringFC60 <- load("FRIFuelCons60Extra_Spring.Rdata", .GlobalEnv)
SpringFS60 <- load("FRIFS60Extra_Spring.Rdata", .GlobalEnv)


SummerNF60 <- load("FRINF60Extra_Summer.Rdata", .GlobalEnv)
SummerBAL60 <- load("FRIBAL60Extra_Summer.Rdata", .GlobalEnv)  
SummerDC60 <- load("FRIDC60Extra_Summer.Rdata", .GlobalEnv)
SummerINT60 <- load("FRIINT60Extra_Summer.Rdata", .GlobalEnv)
SummerSCP60 <- load("FRISCP60Extra_Summer.Rdata", .GlobalEnv)
SummerFC60 <- load("FRIFuelCons60Extra_Summer.Rdata", .GlobalEnv)
SummerFS60 <- load("FRIFS60Extra_Summer.Rdata", .GlobalEnv)

SpringNF1200 <- load("FRINF1200Extra_Spring.Rdata", .GlobalEnv)
SpringBAL1200 <- load("FRIBAL1200Extra_Spring.Rdata", .GlobalEnv)  
SpringDC1200 <- load("FRIDC1200Extra_Spring.Rdata", .GlobalEnv)
SpringINT1200 <- load("FRIINT1200Extra_Spring.Rdata", .GlobalEnv)
SpringSCP1200 <- load("FRISCP1200Extra_Spring.Rdata", .GlobalEnv)
SpringFC1200 <- load("FRIFuelCons1200Extra_Spring.Rdata", .GlobalEnv)
SpringFS1200 <- load("FRIFS1200Extra_Spring.Rdata", .GlobalEnv)

SummerNF1200<- load("FRINF1200Extra_Summer.Rdata", .GlobalEnv)
SummerBAL1200 <- load("FRIBAL1200Extra_Summer.Rdata", .GlobalEnv)  
SummerDC1200 <- load("FRIDC1200Extra_Summer.Rdata", .GlobalEnv)
SummerINT1200 <- load("FRIINT1200Extra_Summer.Rdata", .GlobalEnv)
SummerSCP1200 <- load("FRISCP1200Extra_Summer.Rdata", .GlobalEnv)
SummerFC1200 <- load("FRIFuelCons1200Extra_Summer.Rdata", .GlobalEnv)
SummerFS1200<- load("FRIFS1200Extra_Summer.Rdata", .GlobalEnv)
SummerCOMB1200<- load("FRICOMB1200Extra_Summer.Rdata", .GlobalEnv)
BaLost
DC
Intensities
SnagCProduction
fuelconsumed
FS

mean(x,na.rm=TRUE)
sd(NumberFires)

x <- BaLost[,4790]
valuesm <- rep(0,length(x[,1]))
valuess <- rep(0,length(x[,1]))
means <- 0
esde <- 0
  for (i in 1:length(x[,1])){
    tmp <- x[,i]
    tmp2 <- subset(tmp,tmp>0)   
    valuesm[i] <- mean(tmp2)
    valuess[i] <- sd(tmp2)
    
means <- mean(valuesm, na.rm=TRUE)
esde <- mean(valuess,na.rm=TRUE)
}
means 
esde