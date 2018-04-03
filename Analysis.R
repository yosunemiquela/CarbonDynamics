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
ICC <- varests[1]/sum(varests)
coef(mod.scs)

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
ICC <- varests[1]/sum(varests)
coef(mod.bcs)

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
ICC <- varests[1]/sum(varests)
coef(mod.org)

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
ICC <- varests[1]/sum(varests)
coef(mod.min)

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
ICC <- varests[1]/sum(varests)
coef(mod.wood)


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
ICC <- varests[1]/sum(varests)
coef(mod.npp)

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
ICC <- varests[1]/sum(varests)
coef(mod.nep)

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
#p<-ggplot(ecs, aes(x=FireReturnInterval, y=EcosystemCStock, group=Season)) + ylim(180, 260) +
p<-ggplot(ecs, aes(x=FireReturnInterval, y=EcosystemCStock)) + ylim(190, 290) +
  labs(x="Fire Return Interval",y="Total ecosystem C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=EcosystemCStock-ci, ymax=EcosystemCStock+ci), colour="black", width=.1, position=pd)
#p1<-p+geom_line(aes(linetype=Season),position=pd, size=0.5) +
p1<-p+geom_point(position=pd,size=2) 
#annotate("text", x=0.8, y=190, label= "1", size=4) +
#annotate("text", x = 0.8, y=200, label = "2", size=4)
p2<-p1+ theme_bw()
p2a<-p2+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p3<-p2a+annotation_custom(my_grobA)
pdf("Eco.pdf", width=6,height=4, paper='special')
p3
dev.off()


pd <- position_dodge(0.1)
p<-ggplot(bcs, aes(x=FireReturnInterval, y=BiomassLiveCStock)) + ylim(38,60) +
  labs(x="Fire Return Interval",y="Live Biomass C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=BiomassLiveCStock-ci, ymax=BiomassLiveCStock+ci), colour="black", width=.1, position=pd)
p1<-p+geom_point(position=pd,size=2) 
p6a<-p1 + theme_bw()
p6ab<-p6a+theme(legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p6<-p6ab+annotation_custom(my_grobB)
pdf("Bio.pdf", width=6,height=4, paper='special')
p6
dev.off()

pd <- position_dodge(0.1)
p<-ggplot(ol, aes(x=FireReturnInterval, y=Organic)) + ylim(38,54) +
  labs(x="Fire Return Interval",y="Organic Layer C Stock (MgC/ha)")+
  geom_errorbar(aes(ymin=Organic-ci, ymax=Organic+ci), colour="black", width=.1)
p1<-p+geom_point(size=2) 
p2<-p1+theme_bw()
p7a<-p2+theme( legend.position="none", title=black.12.text, axis.title = black.12.text, text=black.12.text)
p7<-p7a+annotation_custom(my_grobC)
pdf("C:/Users/psladmin/Documents/ModelOutput2/Defense_graphs/Org.pdf", width=6,height=4, paper='special')
p7
dev.off()
