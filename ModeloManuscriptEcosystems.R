
library("reshape")
library("plotrix")
library("relaimpo")
library("MASS")
library("car")
library("ggplot2")
library("multcomp")
library("class")
library("Hmisc")
library("scatterplot3d")
library("reshape")
library("fitdistrplus")
library("compare")
library("truncreg")
library("truncdist")
library("msm") ## for dtnorm
library("actuar")
library("moments")
library("vegan")
library("stats")



### Sampling functions
subpop  <-  function(reg,  st,  d=Plots) {
  res  <-  subset(d,  FIRE_CODE %in% reg & stands %in% st, 
                select="ID_PEP_MES")
  return(res)
}


GetTrees  <-  function(T,  Plots) {
  res  <-  NULL
  for (i in Plots) {
    res <- rbind(res,  subset(T,  ID_PEP_MES %in% i))
  }
  return(res)
}


subintensity  <-  function(reg,  Int,  season) {
  intense  <-  subset(Int, 
                    FIRE_CODE %in% reg&SprSummer %in% season, 
                    select=c("INT",  "IH",  "ISec",  "Season",  "Dates"))
  return(intense)
}


##From ARTEMIS
sigma_plotsquare <- 0.09399156344241
sd_random_plot <- sqrt(sigma_plotsquare)
u_i <- rnorm(1000, mean=0, sd=sd_random_plot)
sigma_intervalsquare <- 0.01444625473266
sd_interval_plot <- sqrt(sigma_intervalsquare)
u_ik <- rnorm(1000, mean=0, sd=sd_interval_plot)



diameter_growth  <-  function (g,  dbh,  b, u_i, u_ik){
  Ui  <-  sample(u_i,  size=1,  replace=T)
  Uik <-  sample(u_ik,  size=1,  replace=T)
  n  <-  length(g) ### Total 15 classes. scalar
  ba  <-  numeric(n) ###initialize basal area per diameter class
  ba  <-  g * b   ######total basal area per diameter class for a particular stand with a given number of trees.
  G  <-  sum(ba)  #####Total stand basal area (m2/ha)
  LNDI <- exp(-0.4961+ 0.06078+ (0.0208*(dbh))+(-0.00068*(dbh^2))+
              (0.5257* 2.3025)+(-0.01267*(G))+Ui+Uik)
  DI <- LNDI/10
  return(DI)
}

#The validation of newly developed models for
#FVSOntario using independent data from Quebec # Lacerte, 2006

diametersmall  <-  function(stand, dbhq, baq){
  bad  <-  stand*baq
  cba  <-  rev(cumsum(rev(bad)))-bad
  BAL  <-  ifelse(cba<0.3, 0.3, cba)
  growth  <-  0.6944+(0.0838*dbhq)+(-0.00942*dbhq^2)+(-0.2548*log(BAL))
  growth  <-  ifelse(growth>0, growth, 0)
  return(growth)
}

##ARTEMIS
mortality  <-  function(g, dbh, ba){
  tmp1  <-  g*ba
  cba  <-  rev(cumsum(rev(tmp1)))-tmp1 #cumulative basal area
  p  <-  1-exp(-exp(-1.624+(-2.6181)+(0.1229*dbh)+(-0.8280*log(dbh))+
                    (0.0098*0*log(10))+(0.0208*(cba))+log(10)))
  pa  <-  p^1/10
  survan  <-  1-pa
  return(survan)
}

###Recruitment after fire as a function of seed abscission schedule, 
#granivory,  seed mortality by fire and optimal seedbeds. Predicts number of 3 yr old seedlings (1.5 and 7 cm),  unless seedbed#and drainage are optimal,  then it may exceptionally reach 10 to 15 cm) (VanBoagaert et al. 2015)

SeedProd <- function(stand, baq){
  m2Ha  <- 1e4
  Bd  <-  sum(baq[5:15]*stand[5:15])/10000 #pre-fire basal area/area
  Qd  <-  163400*Bd^0.95 #germinable seeds/m2 in the aerial seed bank
  Pq  <-  1   #proportion of seed abscised
  Sas  <-  0.58     # fraction of seed surviving pass fire
  m  <-  0.0012 ##black spruce seed mass (g)
  w  <-  0.14 #proportion of optimal seedbeds(Boifin and Munson 2013)
  Sj  <-  0.43*(w*(1-exp(-1.83*m^0.43))+(1-w)*(1-exp(-0.33*m^0.76)))
  Fd  <-  Qd*Sj*Sas*Pq #the number of expected 3 year recuits/m2
  return(round(Fd*m2Ha)) ##per m2
}


#Peng
Height  <-  function(dbh){
  1.3+  1.065*(dbh^0.8868)
}


## Holdaway
crownratio  <-  function(dbh, BA){
  b1  <-  5.54
  b2  <-  0.0072
  b3  <-  4.2
  b4  <-  0.053
  y  <-  b1/(1+(b2*BA)) + (b3*(1-exp(-b4*dbh)))
  round((y-.45)/10,  digits=4)
}


updateCR  <-  function(pcr, ccr, dbh, TopHeight, di, BA, DBA, stand){
  b1  <-  5.54
  b2  <-  0.0072
  b3  <-  4.2
  b4  <-  0.053
  if(sum(is.na(stand))>0)
    stop(message="updateCR stand NA")
  if(sum(is.na(di))>0)
    stop(message="updateCR di NA")
  if(sum(is.na(DBA))>0)
    stop(message="updateCR DBA NA")
  d1  <-  (b3 * b4 * exp(-b4 * dbh))* di
  d2  <- (-b1 * b2/((1 + b2 * BA)^2))* DBA
  xH  <-  TopHeight+di #Heightgrowth
  dH  <-  xH-TopHeight  #deltaheight
  MaximumCR  <- (ccr*TopHeight+dH)/xH   #maximumCR
  ccr  <-  ccr+ d1 + ifelse(ccr < pcr,  0,  d2)
  DBA  <-  rep(DBA, 15)
  newcr  <-  pmin(MaximumCR, ccr)
  ccr  <-  ifelse(DBA<0, newcr, ccr)
  ccr  <-  ifelse(stand>0, ccr, pcr) #deals with all/ANY zeros
  ccr
}



####CARBON DYNAMICS##
Stemwood  <-  function(DBH){
  stembiomass  <-  0.0477*DBH^2.5147
  return(stembiomass)
}

Bark  <-  function(DBH){
  barkbiomass  <-  0.0153*DBH^2.2429
  return(barkbiomass)
}

Branches  <-  function(DBH){
  branchbiomass  <-  0.0278*DBH^2.0839
  return(branchbiomass)
}

Needles  <-  function(DBH){
  needlesbiomass  <- 0.1648*DBH^1.4143
  return(needlesbiomass)
}


####Coarse root equation is from Ouimet et al. 2008.
Coarse  <-  function(DBH){ ####only roots >5 mm
  rootbiomass  <- 0.0085*1.036*(DBH^2.87) ###all classes
  fineroot  <-  0.00153*2.40*(DBH^1.123) #### 2mm-5mm
  coarsebiomass  <- rootbiomass-fineroot
  return(coarsebiomass)
}
Fineroots  <- function(dbh){  #### <5mm. kg/tree Chen et al. 2004
  finerootbiomass  <- 0.011*(dbh^1.9748)
  return(finerootbiomass)
}
#

SnagsCarbon <- function(mortality, BioMassCarbon){
  snags  <-  mortality
  Stemwoodbig  <-  BioMassCarbon[1, 5:15]%*%(snags[5:15])
  Barkmerchantable  <-  BioMassCarbon[2, 5:15]%*%(snags[5:15])
  Stemwoodsmall  <-  BioMassCarbon[1, 1:4]%*%(snags[1:4])
  Barksmall  <-  BioMassCarbon[2, 1:4]%*%(snags[1:4])
  Branches  <-  BioMassCarbon[3, 1:15]%*%(snags[1:15])
  Foliage  <-  BioMassCarbon[4, 1:15]%*%(snags[1:15])
  CRoot  <-  BioMassCarbon[5, 1:15]%*%(snags[1:15])
  FRoot  <-  BioMassCarbon[6, 1:15]%*%(snags[1:15])
  SnagC  <-  Stemwoodbig+Barkmerchantable
  SnagbranchC  <-  Branches+Stemwoodsmall+Barksmall
  SnagFoliage  <-  Foliage
  SnagCoarse  <-  CRoot
  SnagFine  <-  FRoot
  return(list(SnagC=SnagC, SnagbranchC=SnagbranchC, SnagFoliage=SnagFoliage, SnagCoarse=SnagCoarse, SnagFine=SnagFine))
}


UpdateCarbonPool3  <- function(CPool, ICB, BiomassGrowth, LiveBiomass, Snags, Snagbranches, SnagFoliage, SnagCoarse, SnagFine, CarbonPoolTransferMatrix, BiomassMortality ){
  BiomassLostTurnover  <-  0
  delta  <-  (BiomassGrowth-ICB)##NPP delta bio#1
  tmp  <-  as.vector(t(CPool)%*%CarbonPoolTransferMatrix)
  ctmp  <-  as.vector(t(LiveBiomass)%*%Input_Matrix2) ##turnover
  BiomassLostTurnover  <-  sum(ctmp[8:14])##only litterfall and root turnover
  Biomass  <-  sum(LiveBiomass)-BiomassLostTurnover
  ctmp[6]  <-  ctmp[6] + Snags ##adding C from snags
  ctmp[7]  <-  ctmp[7] + Snagbranches 
  ctmp[9]  <-  ctmp[9] + SnagCoarse * (0.5)  ##adding C from coarse roots to AG fast
  ctmp[10]  <-  ctmp[10] + SnagFine * (0.5) + SnagFoliage
  ctmp[12]  <-  ctmp[12] + SnagFine * (0.5)
  ctmp[13]  <-  ctmp[13] + SnagCoarse * (0.5)##BG fast
  Inputs  <-  ctmp[6:14]  ##how much C is incorporated into the DOMCpools including snags (mortality)
  CPool  <-  tmp[2:10]+Inputs
  SoilCAtmFlux  <-  tmp[1]
  NPP <-  delta+BiomassLostTurnover
  NEP  <- (NPP-SoilCAtmFlux)
  return(list(NEP=NEP,  NPP=NPP,  CPool=CPool, SoilCAtmFlux=SoilCAtmFlux, Inputs=Inputs, 
              Biomass=Biomass, BiomassLostTurnover=BiomassLostTurnover))
}




Decayrates  <-  function(MAT){
  REFT  <-  10 ##reference temperature
  Reduction  <-  MAT-REFT ##reduction
  Q10  <-  c(2, 2, 2, 2, 2.65, 2.65, 2, 2, 1)
  BDR  <-  c(0.0187, 0.072, 0.034, 0.1435, 0.355, 0.015, 0.5, 0.1435, 0.0033) #base decay rates at 10C
  TEMPMOD  <-  exp((Reduction)*log(Q10)*0.1)
  #TEMPMOD2  <-  exp(1)^((Reduction)*log(Q10)*0.1)
  STANDMOD  <-  1
  ADR  <-  BDR*TEMPMOD*STANDMOD
  ADR
}




Input_Matrix2  <-  matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                          0, (1-0.04), 0, 0, 0, 0, (0.04*0.25), 0, (0.04*0.75), 0, 0, 0, 0, 0, 
                          0, 0, (1-0.16), 0, 0, 0, 0, 0, 0, (0.16*1), 0, 0, 0, 0, 
                          0, 0, 0, (1-0.02), 0, 0, 0, 0, (0.02*0.5), 0, 0, 0, (0.02*0.5), 0, 
                          0, 0, 0, 0, (1-0.64), 0, 0, 0, 0, (0.64*0.5), 0, (0.64*0.5), 0, 0
), nrow=5,  ncol=14,  byrow=TRUE)
colnames(Input_Matrix2) <- c("Merchantable", "Otherwood", "Needles", "Coarse", "Fine", "Snags", "Snagbranch", "Medium", "AGfast", "AGveryfast", "AGslow", "BGveryfast", "BGfast", "BGslow")
rownames(Input_Matrix2) <- c("Merchantable", "Otherwood", "Needles", "Coarse", "Fine")


###Fire module###

CanopyFuelStocksbs  <-  function(dbh){
  w  <-  0.6329+0.02*dbh^2.2
  return(w)
}


FuelClass  <-  function(stand, dbh){  ## fuel per DBH class kg/ha  using Stocks allometry
  w  <-  CanopyFuelStocksbs(dbh)*stand
  return(w)
}

VerticalFuelProfile  <-  function(c, Top, Base){ #Base<Top must be quaranteed
  MaxTop  <-  max(Top)#maximum height of dbh class
  f  <-  numeric(MaxTop)#number of 1m height sections up to the maximum height
  for (i in seq(1:length(Top))){ #for each  dbh class we do the following:
    v  <-  seq(Base[i]+1, Top[i]) # 1 m sections per diameter class
    x  <-  1 / length(v)  #here we approximate the shape as cylinder. each section gets the same proportion of biomass fuel
    f[v] <-  f[v]+(c[i]*x) ## the vector f accumulates  for each 1m section the fuel of every dbh classes per hectare
  }
  f/10000 # since the units of f are  kg/ha*m we have to convert to kg/m3  that means convert hectares to square meters...  (fuel in a section of 1 ha in area and 1m in height)
}

## this gives me the critical crown base height for crowning
#based on the initial fire intensity (Van Wagner)
Zc  <-  function(I){
  C  <-  0.010
  m  <-  100  #moisture
  h  <-  460 + 26*m
  criticalbase  <-  I^(2/3)/(C*h)
  return(criticalbase)
}

Crowning  <-  function(I, Top, Base, Bulk, b, DenCrit){
  zc  <-  ceiling(Zc(I))   #Critical base height at intensity (I)
  cb  <-  Base[Base<=zc]   # which dbh classes are burning
  ct  <-  Top[Base<=zc]     # flames extend to top of ct...
  CrownLayer  <-  0        # initialise
  k  <- length(cb)         # k= number of dbh classes that affected by fire
  if (k > 0){
    fc  <-  ct[k]              # Height of the last dbh class affected by surface fire. Returns the  flame length value that corresponds to the highest strata with fire. Maxflamelength
    ct  <-  Top[Base<=fc]      #  We evaluate again the CBH of dbh classes. Which ones have a CBH lower than the flame height. return the dbh classes with crown base height smaller than  max flame
    kk  <-  length(ct)
    if (kk>0){    ###We evaluate then if crowning can be sustained
      for (i in seq(kk, 1, by=-1)){   #we are checking from top to down
        t  <-  max(ct[i], b)    ##maximum height of the last dbh class with crowning
        den  <-  sum(Bulk[b:t])/(t-b+1)   #running mean "Available CBD for combustion"
        if (den > DenCrit){
          CrownLayer  <-  i
          break
        }
      }
    }
  }
  CrownLayer
}


UpdateCrownLayer <- function(cl, Top, Base, Bulk, b, DenCrit){
  newcl <- 0
  if (cl > 0) {
    for (i in seq(cl, 1, by=-1)){
      t  <-  max(Top[i], b)
      den  <-  sum(Bulk[b:t])/(t-b+1)
      if (den>DenCrit){
        newcl <- i
        break
      }
    }
  }
  newcl
}


FlameLength <- function(I){
  0.0775*((I)^0.46)
}

UpdateIntensity <- function(I, Ht){
  h <- 0
  h <- h+(Ht*0.5)	#add 1/2 mean canopy height for crown fires (Byram)
  259.83*(h^2.174) ##flame length intensity relationship
}

ScorchHeight <- function(I){
  0.1483*(I^0.667) #Van Wagner (1973)
}

CrownKill <- function(I, Top, CCR){
  z <- ScorchHeight(I)   ##sh
  ht <- Top
  cbh <- ht*(1-CCR)     #CBH
  cl <- ht-cbh
  tmp <- z-(ht-cl)
  tcls <- ifelse(z<cbh, 0, tmp)
  cls <- ifelse(z>ht, cl, tcls)
  cvscy <- 100*(cls/cl)
  return(cvscy)
}

ScorchMortality <- function(Bark, CK){
  bc <- 6.316*(1-exp(-Bark))
  cc <-  -0.000535*(CK^2) #
  1/(1+exp(-1.941+bc+ cc ))
}

#####Total Basal area lost
Basalost <- function (stand,  baq,  newstand){
  a <- sum(stand*baq)    ### Total basal area before
  b <- sum(newstand*baq)
  Percentage_T_Basalost <-  100*(1-(b/a))
  return(Percentage_T_Basalost)
}




#######full model

exe <- function(stand, Y, FRI, Season){
  Sampled  <-  subpop (reg=c("A2", "D4", "B3", "C3", "E1", "E3"),  st=c("EnEn"),  d=Plots)
  Sampled  <-  Sampled[sample(1:dim(Sampled)[1],  size=1,  replace=T), ]
  Tree.List  <-  GetTrees (Tree, Plots=Sampled)
  as.character(Tree.List$DBH)
  as.factor(Tree.List$ESSENCE)
  Tree.List  <-  as.data.frame(lapply(Tree.List[, ], function(x)rep(x, 25)))
  range.DBH <- c(seq(1, 30,  by=2),  100)
  #Resume the results by class
  Tree.List$DBH  <-  as.numeric(Tree.List$DBH)
  #stand  <-  table(cut(Tree.List$DBH,  breaks=range.DBH,  labels=seq(1, 15)))
  #stand[1:4]  <-  stand[1:4]*10
  ###Partition the basal area of big trees >31 cm and add number of trees that the surplus of basal area represents
  basal_big_class  <-  0.0707905544
  BAB  <-  rep(0, 100)
  TBA  <-  3.142*(Tree.List[Tree.List[, 3]>31, 3]/200)^2
  BAB  <-  round(TBA/basal_big_class, digits=0)
  y  <-  sum(BAB)
  #stand[15]  <-  stand[15]+y
  n  <-  length(stand)
  N1s  <-  rep(1, n)
  N0s  <-  rep(0, n)
  BurnMortP  <-  numeric(n)
  BurnMortPpar  <-  numeric(n)
  shannon  <-  0
  ScH  <-  0
  cl  <-  0
  stand <- rep(0, 15)
  ###################################
  ############################################################
  dbhl  <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29) ###dbh lower limit
  dbhu  <-  c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31) ##dbh upper limit
  dbhq  <- sqrt((dbhu^3-dbhl^3)/((dbhu-dbhl)*3)) ###assuming a uniform dbh distribution over interval
  #and the basal area at the stage mean dbh is...
  baq  <- (dbhq/2)^2*pi/1e4
  ######INITIALISE VARIABLES (TOP, BASE, CROWN RATIOS, INTENSITY, BARK)
  Top  <-  ceiling(Height(dbhq))##ceiling gives the upper integer of the height at diameter class i
  Top  <-  ifelse(Top<2, rep(2, length(Top)), Top)	#Top>=2m
  HeightUpdated  <-  Top
  PCR  <-  crownratio(dbhq, sum(baq*stand))
  CCR  <-  PCR
  Base  <-  floor(HeightUpdated*(1-CCR)) #####gives crown base height
  Bark  <-  0.032*2.54*dbhq ##inches to cm!! Black spruce Equation taken from Behave plus fire modelling system  (thickness in cm)
  hatom2  <-  1e4
  b  <-  3
  DenCrit  <-  0.11
  iw  <-  dbhu-dbhl
  BarkThickness  <-  0.032*2.54*dbhq
  n  <-  length(stand)
  N1s  <-  rep(1, n)
  N0s  <-  rep(0, n)
  n  <- length(stand)
  m2Ha  <- 1e4
  biocar_factor  <-  0.5
  MgKg  <-  1000
  SaplingSurvival  <-  0.98 ###sapling survival of different initial sizes (Matthias et al. 2003)
  pBurn  <-  1/FRI
  NF  <-  0
  fire.year  <-  NULL
  RegenLagTime  <-  27   ## Assume it takes a natural seedling 30 years to grow to 214 cm (DBH 1.0 cm),  assume a 27 year delay for a 3-yr old natural black spruce seedling( 20 years was rather optimistic; VanBoagaert et al. 2015)
  RegenRegular  <-  60 ##calibration exercise 11/11/2014
  RegenIrregular  <-  55 ##calibration exercise 11/11/2014
  shannon  <-  diversity(stand[5:15],  index="shannon",  MARGIN=1,  base=exp(1)) ##updated shannon
  RegenCohorts  <-  rpois(RegenLagTime,  ifelse(shannon<1.7, RegenRegular, RegenIrregular))
  #CPool  <-  c(5.8, 2.1, 24, 9.89, 9.0, 36, 1.69, 3.75, 74) * MgKg
  CPool  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0) * MgKg
  Snags  <-  0
  Snagbranches  <-  0
  SnagFoliage  <-  0
  SnagCoarse  <-  0
  SnagFine  <-  0
  CC <- 0
  
  
  #This is to get the simulated plots stand structure characteristics
  ###Initialize object variables to save simulation results####
  
  Size  <-  matrix(0, Y, n, byrow=TRUE)
  PreFireStand  <-  matrix(0, Y, n, byrow=TRUE)
  Recruits  <-  numeric(Y)
  BA  <-  numeric(Y)
  FireDeaths  <-  matrix(0, Y, n, byrow=TRUE)
  Senescence  <-  matrix(0, Y, n, byrow=TRUE)
  Transition <- matrix(0, Y, n, byrow=TRUE) ##transition probability
  Crecimiento <- matrix(0, Y, n, byrow=TRUE)
  Parcela <- matrix(0, Y, n, byrow=TRUE)
  Muertos <- matrix(0, Y, n, byrow=TRUE)
  Mortality <- matrix(0, Y, n, byrow=TRUE)##probability of mortality
  Structure <- numeric(Y)
  InitialIntensity <- numeric(Y)
  FireSeason <- numeric(Y)
  BALost <- numeric(Y)
  Delta_BA <- numeric(Y)
  DeltaN <- matrix(0, Y, n, byrow=TRUE)
  CR <- matrix(0, Y, n, byrow=TRUE)
  Heights <- matrix(0, Y, n, byrow=TRUE)
  Heights <- matrix(0, Y, n, byrow=TRUE)
  ShiftCrownratio <- matrix(0, Y, n, byrow=TRUE)
  ShiftHeights <- matrix(0, Y, n, byrow=TRUE)
  DiameterGrowth <- matrix(0, Y, n, byrow=TRUE)
  SnagCProduction <- numeric(Y)
  Turnover <- numeric(Y) ##turnover
  DOMC_Pool <- matrix(0, Y, length(CPool), byrow=TRUE)
  DOM_Flux <- numeric(Y)
  DOM_Inputs <- matrix(0, Y, 9, byrow=TRUE)
  BioMass  <-  matrix(c(Stemwood(dbhq), Bark(dbhq), Branches(dbhq), Needles(dbhq), Coarse(dbhq), Fineroots(dbhq)), nrow=6, 
                    ncol=length(dbhq), byrow=TRUE)
  BioMassCarbon  <-  BioMass*biocar_factor ##Biomass C per diameter class
  InitialCBiomass  <-  sum(BioMassCarbon%*%as.matrix(stand)) ##bio1 for the site
  ICB  <-  InitialCBiomass ##bio1 for the site
  NetPrimaryProductivity <- numeric(Y)
  TotalLiveBiomass <- numeric(Y)
  AppliedDecayRates <- matrix(0, Y, 9, byrow=TRUE)
  EmpiricalTemperature <- numeric(Y)
  NetEcosystemProduction <- numeric(Y)
  Rh  <-  numeric(Y)
  CarbonCombusted  <-  numeric(Y) 
  AnnualBiomassRecruits  <-  numeric(Y)
  
  for (y in 1:Y){ #Things that have to be reinitialized and updated
    
    HeadIntensity  <-  sample(Season,  size=1,  replace=F)
    I <- HeadIntensity
    Istart  <- I
    shannon <- diversity(stand[5:15],  index="shannon",  MARGIN=1,  base=exp(1)) ##updated shannon
    Structure[y] <- shannon
    Firedeaths <- rep(0, 15)
    BALost[y] <- 0
    bay <- sum(stand*baq)
    
    
    #MAT <- as.numeric(sample(Historical$température.moyenne.annuelle,  1,  replace = FALSE,  prob = NULL))
    MAT <- 0.36
    EmpiricalTemperature[y] <- MAT
    AppDecayRates <- Decayrates(MAT)
    AppliedDecayRates[y, ] <- AppDecayRates
    Decayrate <- rep(0, 9)
    Decayrate[1] <- AppDecayRates[1]
    Decayrate[2] <- AppDecayRates[2]
    Decayrate[3] <- AppDecayRates[3]
    Decayrate[4] <- AppDecayRates[4]
    Decayrate[5] <- AppDecayRates[5]
    Decayrate[6] <- AppDecayRates[6]
    Decayrate[7] <- AppDecayRates[7]
    Decayrate[8] <- AppDecayRates[8]
    Decayrate[9] <- AppDecayRates[9]
    
    CarbonPoolTransferMatrix  <-  matrix(c(
      Decayrate[1]*0.83, (1-Decayrate[1]-0.08), 0, 0.08, 0, 0, Decayrate[1]*(1-0.83), 0, 0, 0, 
      Decayrate[2]*0.83, 0, (1-Decayrate[2]-0.10), 0, 0.10, 0, Decayrate[2]*(1-0.83), 0, 0, 0, 
      Decayrate[3]*0.83, 0, 0, 1-Decayrate[3], 0, 0, Decayrate[3]*(1-0.83), 0, 0, 0, 
      Decayrate[4]*0.83, 0, 0, 0, (1-Decayrate[4]), 0, Decayrate[4]*(1-0.83), 0, 0, 0, 
      Decayrate[5]*0.815, 0, 0, 0, 0, (1-Decayrate[5]), Decayrate[5]*(1-0.815), 0, 0, 0, 
      Decayrate[6]*1, 0, 0, 0, 0, 0, (1-Decayrate[6]-0.006), 0, 0, 0.006, 
      Decayrate[7]*0.83, 0, 0, 0, 0, 0, 0, (1-Decayrate[7]), 0, Decayrate[7]*(1-0.83), 
      Decayrate[8]*0.83, 0, 0, 0, 0, 0, 0, 0, (1-Decayrate[8]), Decayrate[8]*(1-0.83), 
      Decayrate[9]*1, 0, 0, 0, 0, 0, 0, 0, 0, 1-Decayrate[9]
    ), nrow=9, ncol=10, byrow=TRUE)
    colnames(CarbonPoolTransferMatrix)  <-  c("Atm", "Snags", "Snagbranch", "Medium", "AGfast", "AGveryfast", "AGslow", "BGveryfast", "BGfast", "BGslow")
    rownames(CarbonPoolTransferMatrix)  <-  c("Snags", "Snagbranch", "Medium", "AGfast", "AGveryfast", "AGslow", "BGveryfast", "BGfast", "BGslow")
    
    ###Apply decay rate to CPools
    tmp  <-  as.vector(t(CPool)%*%CarbonPoolTransferMatrix)
    SoilCAtmFlux  <-  tmp[1] 
    
    
    
    #GROWTH
    annual_diam_incre <- diameter_growth(stand, dbhq, baq, u_i, u_ik)
    annual_diam_incre_small  <-  diametersmall(stand, dbhq, baq)
    annual_diam_incre[1:4]  <-  annual_diam_incre_small[1:4]
    adi <- annual_diam_incre
    graduating  <-  1/(iw/adi) #Transition probabilities
    growth  <-  rbinom(N1s,  stand,  graduating) #stohastically
    stand  <-  stand-growth
    stand  <-  stand+c(0, growth[1:n-1]) #after growth
    LastDCT <-  growth[15]##prevents loosing trees in last diameter class
    stand [15] <-  stand[15]+LastDCT
    BAIncrement  <-  sum(growth*baq)    #patch BA increment due to growth
    CCRgrowth  <-  growth*CCR           #the trees that grow bring their crown ratio
    Heightgrowth  <-  growth*HeightUpdated
    
    ###Calculate Biomass due to growth 
    GrowthCBiomass  <-  sum(BioMassCarbon%*%as.matrix(stand))##calculate biomass due to growth..Biomass that has not been lost to turnover or mortality
    Parcela[y, ]  <-  stand ##stand after regeneration..should capture pulses
    delta  <-  (GrowthCBiomass-ICB)
    
    ####Apply turnover 
    #Corrected to match IPCC Good Practice Guidance
    Stemwoodsmall  <-  BioMassCarbon[1, 1:4]%*%(stand[1:4])
    Barkmerchantable  <-  BioMassCarbon[2, 5:15]%*%(stand[5:15])
    LiveBiomassCPools  <-  BioMassCarbon%*%(stand) #biomass C kg/ha  ###after growth
    LiveBiomassCPoolsCorrected  <- matrix(0, nrow=5, ncol=1)
    LiveBiomassCPoolsCorrected[1, 1]  <-  LiveBiomassCPools[1, 1]-Stemwoodsmall+Barkmerchantable#
    LiveBiomassCPoolsCorrected[2, 1]  <-  LiveBiomassCPools[2, 1]-Barkmerchantable+LiveBiomassCPools[3, 1]+
      Stemwoodsmall   #Otherwood+Bark
    LiveBiomassCPoolsCorrected[3, 1]  <-  LiveBiomassCPools[4, 1]#Foliage
    LiveBiomassCPoolsCorrected[4, 1]  <-  LiveBiomassCPools[5, 1]#Coarse
    LiveBiomassCPoolsCorrected[5, 1]  <-  LiveBiomassCPools[6, 1]#Fine
    ctmp  <-  as.vector(t(LiveBiomassCPoolsCorrected)%*%Input_Matrix2) ##turnover
    BiomassLostTurnover  <-  sum(ctmp[6:14]) 
    
    NPP  <-  delta + BiomassLostTurnover
    NEP  <-  (NPP-SoilCAtmFlux)
    
    
    
    
    ##MORTALITY
    surviving  <-  mortality(stand, dbhq, baq)
    surviving[1:4]  <-  SaplingSurvival
    Senescencedeaths  <-  rbinom(N1s, stand, 1-surviving)
    deaths  <-  Senescencedeaths+Firedeaths
    Muertos[y, ]  <-  deaths
    Senescence[y, ]  <-  Senescencedeaths
    SnagCpools  <-  SnagsCarbon(deaths, BioMassCarbon)
    Snags  <-  SnagCpools$SnagC
    Snagbranches  <-  SnagCpools$SnagbranchC
    SnagFoliage  <-  SnagCpools$SnagFoliage
    SnagCoarse  <-  SnagCpools$SnagCoarse
    SnagFine  <-  SnagCpools$SnagFine
    stand  <-  stand-Senescencedeaths  ###fire deaths are taken care in the fire module
    DeltaBA  <-  BAIncrement ##net basal area increment after mortality (fire and senescence) and growth
    DeltaStand  <-  sum(stand)  ##net change in density after mortality (fire and senescence) and growth
    if(sum(stand<0)>0)
      stop(message="neg count 3")
    Delta_BA[y]  <-  DeltaBA
    DeltaN[y, ]  <-  deaths+growth
    
    CMortality  <-  Snags+Snagbranches+SnagFoliage+SnagCoarse+SnagFine 
    
    
    
    ##FIRE MODULE
    #Evaluates if a fire arrives its intensity,  crowning,  and post-fire tree mortality and regeneration
    
    if(runif(1) < pBurn){ ## Determine if a fire happens
      fire.year  <-  c(fire.year, y)
      NF  <-  NF+1 ## Update number of fires that occurred during the simulation
      RegenCohorts  <-  rep(0, RegenLagTime) ## KILL ALL regenerating trees
      prefirestand  <-  stand
      PreFireStand[y, ]  <-  prefirestand
      NewRegen  <-  SeedProd(prefirestand, baq)
      Fuel  <-  as.numeric(FuelClass(stand, dbhq)) #Kg/ha per dbh class
      VF  <-  VerticalFuelProfile(Fuel, HeightUpdated, Base)
      cl  <-  Crowning(I, HeightUpdated, Base, VF, b, DenCrit)
      #if there is crowning,  then update the crown layer affected by fire
      cl  <-  ifelse (cl>0, UpdateCrownLayer(cl, HeightUpdated, Base, VF, b, DenCrit), 
                    Crowning(I, HeightUpdated, Base, VF, b, DenCrit))
      u  <-  UpdateIntensity(I, HeightUpdated[cl])
      I  <-  ifelse(cl>0, max(UpdateIntensity(I, HeightUpdated[cl])), Istart)
      ScH  <-  ScorchHeight(I)
      CK  <-  CrownKill(I, HeightUpdated, CCR)
      BurnMortP  <-  ScorchMortality(BarkThickness, CrownKill(I, HeightUpdated, CCR))
      Firedeaths  <-  rbinom(N1s, stand, BurnMortP)
      Firedeaths  <-  ifelse(is.na(Firedeaths), N0s, Firedeaths)
      FireDeaths[y, ]  <-  Firedeaths
      newstand  <-  stand-Firedeaths
      severity  <-  Basalost(stand, baq, newstand)
      stand  <-  newstand ###update stand after a fire
      InitialIntensity[y]  <-  I #adjusted intensity using Catchpole et.al 1992
      #FireSeason[y] <- Season
      BALost[y] <- severity
      tmp3  <-  CPool[3]*0.392140  ##carbon consumed in the medium carbon pool
      tmp4  <-  CPool[4]*0.6415 ##carbon consumed in the Ag fast carbon pool
      tmp5  <-  CPool[5]*0.968533 ##carbon consumed in the Ag very fast pool
      tmp6  <-  CPool[6]*0.09001 ##carbon consumed in the Ag slowpool
      CPool[3]  <-  CPool[3]-tmp3
      CPool[4]  <-  CPool[4]-tmp4
      CPool[5]  <-  CPool[5]-tmp5
      CPool[6]  <-  CPool[6]-tmp6
      CC  <-  tmp3+tmp4+tmp5+tmp6
      
    }
    else {
      NewRegen  <-  rpois(1, ifelse(shannon<1.7, RegenRegular, RegenIrregular))
    }
    
    ##Add recruitment
    Recruits[y]  <-  RegenCohorts[RegenLagTime]
    RegenCohorts  <-  c(NewRegen, RegenCohorts[1:RegenLagTime-1])
    stand[1]  <-  stand[1] + Recruits[y]
    CCRRecruits  <- Recruits[y]*PCR[1]
    HeightRecruits  <- Recruits[y]*Top[1]
    
    
    ###update biomass
    Biomass  <-  sum(BioMassCarbon%*%as.matrix(stand))
    ICB  <-  Biomass
    
    ##Dynamically updating crown ratios and heights of natural (all, regenerated and fire derived recruits)
    xH  <-  Top+adi ##Heightgrowth
    dH  <-  xH-Top  ##deltaheight
    MaximumCR  <-  (CCR*Top+dH)/xH
    TotalN  <-  stand+c(0, growth[1:n-1])
    TotalN[1] <- TotalN[1]+Recruits[y]
    CCR  <-  updateCR(PCR, CCR, dbhq, Top, adi, bay, DeltaBA, stand)##CCR after recruitment, growth, mortality
    CCRnow  <-  stand*CCR
    ShiftCR  <-  CCRnow+c(0, CCRgrowth[1:n-1])
    ShiftCR[1]  <-  ShiftCR[1]+CCRRecruits
    CCR  <-  ifelse(stand>0, pmin(MaximumCR, ShiftCR/TotalN), PCR)
    ##Updating heights
    Top  <-  Height(dbhq)
    HeightUpdated  <-  Top+adi ##Heightgrowth
    Heightnow  <-  stand*HeightUpdated
    ShiftHeight  <-  Heightnow+c(0, Heightgrowth[1:n-1])
    ShiftHeight[1]  <-  ShiftHeight[1]+HeightRecruits
    HeightUpdated  <-  ifelse(stand>0, ShiftHeight/TotalN, Top)
    Base  <-  HeightUpdated*(1-CCR) #####gives crown base height
    
    
    
    #Distribute turnover and carbon to pools
    ctmp[6]  <-  ctmp[6] + Snags ##adding C from snags
    ctmp[7]  <-  ctmp[7] +  Snagbranches ##adding C from small trees
    ctmp[9]  <-  ctmp[9] + SnagCoarse *(0.5) ##adding C from coarse roots to AG fast
    ctmp[10]  <-  ctmp[10] + SnagFine *(0.5) + SnagFoliage*(0.85)
    ctmp[12]  <-  ctmp[12] + SnagFine *(0.5)
    ctmp[13]  <-  ctmp[13] + SnagCoarse *(0.5)
    Inputs  <-  ctmp[6:14]  ##how much C is incorporated into the DOMCpools including snags (mortality)
    CPool  <-  tmp[2:10]+Inputs #update the pools
    
    
    #Save results into objects
    CarbonCombusted[y]  <-  CC
    SnagCProduction[y]  <-  CMortality
    #Save results into objects
    TotalLiveBiomass[y]  <-    Biomass
    DOMC_Pool[y, ]  <-  CPool #Nine DOM carbon pools (snag=snagbracnhes etc.)
    DOM_Flux[y]  <-  SoilCAtmFlux
    DOM_Inputs[y, ]  <-  Inputs #includes carbon turnover+carbon from snags
    Turnover[y]  <-  BiomassLostTurnover #how much biomassC is lost due only to litterfall
    NetPrimaryProductivity[y]  <-  NPP
    NetEcosystemProduction[y]  <-  NEP
    #AnnualBiomassRecruits[y]  <-  BiomassRecruits
    Rh[y]  <-  SoilCAtmFlux
    BA[y]  <-  bay
    CR[y, ]  <-  CCR
    Heights[y, ]  <-  HeightUpdated
    ShiftCrownratio[y, ]  <-  ShiftCR
    ShiftHeights[y, ]  <-  ShiftHeight
    DiameterGrowth[y, ]  <-  adi ##annual diamater increment
    Transition[y, ]  <-  graduating ##transition probabilities
    Mortality[y, ]  <-  1-surviving ###probability of mortality per DBH class
    Crecimiento[y, ]  <-  growth
    Muertos[y, ]  <-  deaths
    Size[y, ]  <-  stand ###  after all processes..FINAL size..or initial one
    
  }
  
  res <- list(Parcela=Parcela, Size=Size, DeltaN=DeltaN, BA=BA, PreFireStand=PreFireStand, Senescence=Senescence, AppliedDecayRates=AppliedDecayRates, 
            EmpiricalTemperature=EmpiricalTemperature, FireDeaths=FireDeaths, Muertos=Muertos, Structure=Structure, Crecimiento=Crecimiento
            , Mortality=Mortality, Transition=Transition, Structure=Structure, DiameterGrowth=DiameterGrowth, CR=CR, 
            CMortality=CMortality, ShiftCrownratio=ShiftCrownratio, ShiftHeights=ShiftHeights, Heights=Heights, BALost=BALost,  NF=NF, InitialIntensity=InitialIntensity, fire.year=fire.year, Recruits=Recruits, TotalLiveBiomass=TotalLiveBiomass, 
            CarbonCombusted=CarbonCombusted, Turnover=Turnover, SnagCProduction=SnagCProduction, DOMC_Pool=DOMC_Pool, DOM_Flux=DOM_Flux, DOM_Inputs=DOM_Inputs, NetPrimaryProductivity=NetPrimaryProductivity,  Rh=Rh, NetEcosystemProduction=NetEcosystemProduction)
  
  return(res)
}



###Run simulations
set.seed(10)
n.iter <- 1000#plots to check
Y <- 4800
FRI <- 2400000000
#stand dynamics
Ba_s  <-  matrix(0, n.iter, Y, byrow=T)
#AnnualBiomassRec_s  <-  matrix(0, n.iter, Y, byrow=T)
Size_list  <-  vector("list",  n.iter) # create list
PreFireStand_s <- vector("list",  n.iter)
Transition_list  <-  vector("list",  n.iter)
Structure_s <- matrix(0, n.iter, Y, byrow=T)
Recruits_s <- matrix(0, n.iter, Y, byrow=T)
Heights_list <- vector("list",  n.iter)
CR_list <- vector("list",  n.iter)
#carbon dynamics
DOM_Pool_list <-  vector("list",  n.iter)
DOM_Inputs_list <-  vector("list",  n.iter)
DOM_Flux_s <- matrix(0, n.iter, Y, byrow=T)
PrimaryProductivity_s  <- matrix(0, n.iter, Y, byrow=T)
NetEcosystemProduction_s  <- matrix(0, n.iter, Y, byrow=T)
Rh_s  <- matrix(0, n.iter, Y, byrow=T)
Turnover_s  <- matrix(0, n.iter, Y, byrow=T)
TotalLiveBiomass_s  <-  matrix(0, n.iter, Y, byrow=T)
SnagCProduction_s  <-  matrix(0, n.iter, Y, byrow=T)

###Fire disturbance
Severity_s <- matrix(0, n.iter, Y, byrow=T)
InitialIntensity_s <- matrix(0, n.iter, Y, byrow=T)
NF_s <- numeric(n.iter)

for(i in 1:n.iter){
  Size_list[[i]]  <-  matrix(0, Y, 15, byrow=T)
  PreFireStand_s[[i]] <- matrix(0, Y, 15, byrow=T)
  Transition_list[[i]] <- matrix(0, Y, 15, byrow=T)
  Heights_list[[i]] <- matrix(0, Y, 15, byrow=T)
  CR_list[[i]] <- matrix(0, Y, 15, byrow=T)
  DOM_Pool_list[[i]] <-  matrix(0, Y, 9, byrow=T)
  DOM_Inputs_list[[i]] <-  matrix(0, Y, 9, byrow=T)
}

do.call(rbind,  Size_list)
do.call(rbind,  Transition_list)
do.call(rbind,  Heights_list)
do.call(rbind,  CR_list)
do.call(rbind,  DOM_Pool_list)
do.call(rbind,  DOM_Inputs_list)


for (i in 1:n.iter){
  CarbonModel <- exe(stand, Y, FRI, SpringWeightedCatch)
  Ba_s[i, ] <- CarbonModel$BA
  #AnnualBiomassRec_s[i, ] <- CarbonModel$AnnualBiomassRecruits
  Structure_s[i, ] <- CarbonModel$Structure
  Recruits_s[i, ] <- CarbonModel$Recruits
  Size_list[[i]] <- CarbonModel$Size
  PreFireStand_s[[i]]  <- CarbonModel$PreFireStand
  Transition_list[[i]] <- CarbonModel$Transition
  Heights_list[[i]] <- CarbonModel$Heights
  CR_list[[i]]  <- CarbonModel$CR
  SnagCProduction_s [i, ] <-  CarbonModel$SnagCProduction
  TotalLiveBiomass_s[i, ] <- CarbonModel$TotalLiveBiomass
  PrimaryProductivity_s[i, ]  <-  CarbonModel$NetPrimaryProductivity
  NetEcosystemProduction_s[i, ] <- CarbonModel$NetEcosystemProduction
  Rh_s[i, ] <- CarbonModel$Rh
  Turnover_s[i, ] <- CarbonModel$Turnover
  DOM_Pool_list[[i]] <- CarbonModel$DOMC_Pool
  DOM_Flux_s[i, ]  <- CarbonModel$DOM_Flux
  DOM_Inputs_list[[i]]  <- CarbonModel$DOM_Inputs
  Severity_s[i, ] <- CarbonModel$BALost
  InitialIntensity_s[i, ] <- CarbonModel$InitialIntensity
  NF_s[i] <- CarbonModel$NF
  print(i)
}

Year <- 4800
plots <- 1000
#Season 1 Spring #2 Summer
BasalArea <- Ba_s[, Year]
Recruitment <- Recruits_s[, Year]
BiomassTurnover <- Turnover_s[1:plots, Year]
Inputs <- sapply(DOM_Inputs_list,  rowSums)
Litterfall <- Inputs[Year, ]
BiomassLiveCStock <- TotalLiveBiomass_s[1:plots, Year] #1:1000 plots @ yr500
BiomassLiveCStock1 <- t(BiomassLiveCStock)
NPP <- PrimaryProductivity_s[1:plots, Year]
SoilRespiration <- Rh_s[1:plots, Year]
NEP <- NetEcosystemProduction_s[1:plots, Year]
StandStructure <- Structure_s[1:plots, Year]
Intensities <- InitialIntensity_s[1:plots, 1:Year]
Snags <- sapply(DOM_Pool_list,  function(m) m[Year, 1]) #
SnagBranch <- sapply(DOM_Pool_list,  function(m) m[Year, 2])
AGMedium <- sapply(DOM_Pool_list,  function(m) m[Year, 3])
AGfast <- sapply(DOM_Pool_list,  function(m) m[Year, 4])
AGveryfast <- sapply(DOM_Pool_list,  function(m) m[Year, 5])
AGslow <- sapply(DOM_Pool_list,  function(m) m[Year, 6])
BGveryfast <- sapply(DOM_Pool_list,  function(m) m[Year, 7])
BGfast <- sapply(DOM_Pool_list,  function(m) m[Year, 8])
BGslow <- sapply(DOM_Pool_list,  function(m) m[Year, 9])
SoilCStock <- Snags+SnagBranch+AGMedium+AGfast+AGveryfast+AGslow+BGveryfast+BGfast+BGslow
EcosystemCStock <- BiomassLiveCStock+SoilCStock
Season <- rep(1, plots)
FireReturnInterval <- rep(FRI, plots)
NumberFires  <-  mean(NF_s)


###Save results

FRI60Summer  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                      NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                      AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                      EcosystemCStock)
save("FRI60Summer",  file= "FRI60_Summer_Newest.RData")
FRI100Summer  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                       NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                       EcosystemCStock)
save("FRI100Summer",  file= "FRI100_Summer_Newest.RData")
FRI150Summer  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                       NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                       EcosystemCStock)
save("FRI150Summer",  file= "FRI150_Summer_Newest.RData")
FRI300Summer  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                       NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                       EcosystemCStock)
save("FRI300Summer",  file= "FRI300_Summer_Newest.RData")

FRI700Summer  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                       NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                       AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                       EcosystemCStock)
save("FRI700Summer",  file= "FRI700_Summer_Newest.RData")
FRI1200Summer  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                        NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                        AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                        EcosystemCStock)
save("FRI1200Summer",  file= "FRI1200_Summer_Newest.RData")
FRINFSpring  <-  cbind (Season, FireReturnInterval, StandStructure, BasalArea, BiomassLiveCStock, Recruitment, 
                      NPP,  NEP, SoilRespiration, BiomassTurnover, Snags, SnagBranch, 
                      AGMedium, AGfast, AGveryfast, AGslow, BGveryfast, BGfast, BGslow, SoilCStock, 
                      EcosystemCStock)
save("FRINFSpring",  file= "FRINF_Spring_Newest.RData")